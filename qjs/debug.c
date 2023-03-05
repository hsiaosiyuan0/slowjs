#include "include/quickjs-libc.h"
#include "include/quickjs.h"
#include "libs/cutils.h"
#include "libs/libregexp.h"
#include "libs/list.h"
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <unistd.h>

#ifndef FD_COPY
#define FD_COPY(dest, src) memcpy((dest), (src), sizeof *(dest))
#endif

typedef struct msg_t {
  struct list_head link;
  DynBuf buf;
} msg_t;

msg_t *new_msg() {
  msg_t *msg = calloc(1, sizeof(msg_t));
  if (!msg)
    return NULL;

  init_list_head(&msg->link);
  dbuf_init(&msg->buf);
  return msg;
}

void free_msg(msg_t *msg) {
  dbuf_free(&msg->buf);
  free(msg);
}

typedef struct serv_t serv_t;

//        1:1             1:1                   1:1
// conn -------> sess_t -------> event thread -------> user script thread
typedef struct sess_t {
  struct list_head link;

  struct {
    struct sockaddr_in addr;
    socklen_t addr_len;
    int fd;
  } conn;

  pthread_cond_t wakeup;

  pthread_mutex_t in_msgs_lock;
  struct list_head in_msgs; /* msg_t list */

  pthread_mutex_t out_msgs_lock;
  struct list_head out_msgs; /* msg_t list */

  serv_t *serv;
  pthread_t thead_id;

  // rt/ctx for encode/decode client messages
  JSRuntime *rt;
  JSContext *ctx;

  // rt/ctx for eval the requested script
  JSRuntime *eval_rt;
  JSContext *eval_ctx;
  const char *eval_file;
  pthread_t eval_thread_id;
  int eval_retval; // -2: running, -1: error, 0: ok:
  char *eval_errmsg;

  BOOL invalid;
  BOOL closing;
} sess_t;

JSContext *JS_NewCustomContext(JSRuntime *rt);
int eval_file(JSContext *ctx, const char *filename, int module);

void rt_setup(JSRuntime **rtp, JSContext **ctxp) {
  *rtp = JS_NewRuntime();

  js_std_set_worker_new_context_func(JS_NewCustomContext);
  js_std_init_handlers(*rtp);

  *ctxp = JS_NewCustomContext(*rtp);

  /* loader for ES6 modules */
  JS_SetModuleLoaderFunc(*rtp, NULL, js_module_loader, NULL);
  JS_SetHostPromiseRejectionTracker(*rtp, js_std_promise_rejection_tracker,
                                    NULL);

  js_std_add_helpers(*ctxp, -1, NULL);
}

sess_t *new_sess() {
  sess_t *sess = calloc(1, sizeof(sess_t));
  if (!sess)
    return NULL;

  pthread_cond_init(&sess->wakeup, NULL);

  pthread_mutex_init(&sess->in_msgs_lock, NULL);
  init_list_head(&sess->in_msgs);

  pthread_mutex_init(&sess->out_msgs_lock, NULL);
  init_list_head(&sess->out_msgs);

  return sess;
}

void free_sess(sess_t *sess) {
  struct list_head *el, *el1;
  msg_t *msg;

  if (sess->conn.fd)
    close(sess->conn.fd);

  pthread_cond_destroy(&sess->wakeup);

  pthread_mutex_lock(&sess->in_msgs_lock);
  list_for_each_safe(el, el1, &sess->in_msgs) {
    msg = list_entry(el, msg_t, link);
    free_msg(msg);
  }
  pthread_mutex_unlock(&sess->in_msgs_lock);
  pthread_mutex_destroy(&sess->in_msgs_lock);

  pthread_mutex_lock(&sess->out_msgs_lock);
  list_for_each_safe(el, el1, &sess->out_msgs) {
    msg = list_entry(el, msg_t, link);
    free_msg(msg);
  }
  pthread_mutex_unlock(&sess->out_msgs_lock);
  pthread_mutex_destroy(&sess->out_msgs_lock);

  free((void *)sess->eval_file);
  js_std_free_handlers(sess->rt);
  JS_FreeContext(sess->ctx);
  JS_FreeRuntime(sess->rt);

  free(sess);
}

void fatal(const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  printf(fmt, ap);
  va_end(ap);
  exit(-1);
}

void serv_free_sess(serv_t *serv, sess_t *sess);

int sess_enqueue_in_msg(sess_t *sess) {
  uint8_t buf[512];
  int len;
  msg_t *msg = new_msg();

  if (!msg)
    return -1;

  while (1) {
    len = recv(sess->conn.fd, buf, sizeof(buf), 0);
    if (len < 0) {
      if (errno == EAGAIN || errno == EWOULDBLOCK) {
        if (len)
          break; // there is some data arrived
        return 0;
      }
      return -1;
    }

    // conn closed
    if (len == 0) {
      sess->closing = TRUE;
      return -1;
    }

    if (dbuf_put(&msg->buf, buf, len))
      return -1;
  }

  pthread_mutex_lock(&sess->in_msgs_lock);
  list_add_tail(&msg->link, &sess->in_msgs);
  pthread_mutex_unlock(&sess->in_msgs_lock);

  pthread_cond_signal(&sess->wakeup);
  return 0;
}

msg_t *sess_dequeue_in_msg(sess_t *sess) {
  msg_t *msg = NULL;
  struct list_head *next;
  if (!list_empty(&sess->in_msgs)) {
    next = sess->in_msgs.next;
    list_del(next);
    msg = list_entry(next, msg_t, link);
  }
  return msg;
}

void sess_enqueue_out_msg(sess_t *sess, msg_t *msg) {
  pthread_mutex_lock(&sess->out_msgs_lock);
  list_add_tail(&msg->link, &sess->out_msgs);
  pthread_mutex_unlock(&sess->out_msgs_lock);
}

msg_t *sess_dequeue_out_msg(sess_t *sess) {
  msg_t *msg = NULL;
  struct list_head *next;
  pthread_mutex_lock(&sess->out_msgs_lock);
  if (!list_empty(&sess->out_msgs)) {
    next = sess->out_msgs.next;
    list_del(next);
    msg = list_entry(next, msg_t, link);
  }
  pthread_mutex_unlock(&sess->out_msgs_lock);
  return msg;
}

typedef struct serv_t {
  struct sockaddr_in addr;
  int fd;
  struct list_head sessions;
} serv_t;

serv_t *new_serv(uint64_t port) {
  int on = 1;

  serv_t *serv = calloc(1, sizeof(serv_t));
  if (!serv)
    return NULL;

  serv->fd = socket(PF_INET, SOCK_STREAM, 0);
  serv->addr.sin_family = AF_INET;
  serv->addr.sin_port = htons(port);
  serv->addr.sin_addr.s_addr = INADDR_ANY;

  if (setsockopt(serv->fd, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on)))
    fatal("failed to setup serv socket\n");

  if (ioctl(serv->fd, FIONBIO, (char *)&on))
    fatal("failed to turn serv socket into no-blocking\n");

  init_list_head(&serv->sessions);

  return serv;
}

sess_t *accept_conn(serv_t *serv) {
  sess_t *sess = new_sess();
  if (!sess)
    fatal("failed to make sess\n");

  sess->conn.fd = accept(serv->fd, (struct sockaddr *)&sess->conn.addr,
                         &sess->conn.addr_len);
  if (sess->conn.fd < 0)
    fatal("failed to accept new conn\n");

  list_add_tail(&sess->link, &serv->sessions);
  return sess;
};

void serv_free_sess(serv_t *serv, sess_t *sess) {
  pthread_cond_signal(&sess->wakeup);
  list_del(&sess->link);
  free_sess(sess);
}

// `replay` will be freed
int sess_event_send_replay(sess_t *sess, JSValue replay) {
  JSValue str;
  const char *cstr;
  size_t len;
  msg_t *msg;

  str = JS_JSONStringify(sess->ctx, replay, JS_NULL, JS_NewInt32(sess->ctx, 2));
  if (JS_IsException(str))
    return -1;

  cstr = JS_ToCStringLen(sess->ctx, &len, str);
  if (!cstr)
    return -1;

  msg = new_msg();
  dbuf_put(&msg->buf, (uint8_t *)cstr, strlen(cstr));

  JS_FreeCString(sess->ctx, cstr);
  JS_FreeValue(sess->ctx, str);
  JS_FreeValue(sess->ctx, replay);

  sess_enqueue_out_msg(sess, msg);
  return 0;
}

JSValue new_event_err(sess_t *sess, int code, const char *msg) {
  JSValue obj;
  obj = JS_NewObject(sess->ctx);
  if (JS_IsException(obj)) {
    sess->invalid = TRUE;
    printf("failed to new event error, stopping...\n");
    return JS_EXCEPTION;
  }
  JS_SetPropertyStr(sess->ctx, obj, "code", JS_NewInt32(sess->ctx, code));
  if (msg) {
    JS_SetPropertyStr(sess->ctx, obj, "msg",
                      JS_NewStringLen(sess->ctx, msg, strlen(msg)));
  }
  return obj;
}

void *debug_user_script(void *arg) {
  sess_t *sess = arg;

  rt_setup(&sess->eval_rt, &sess->eval_ctx);

  if (js_debug_init(sess->eval_ctx))
    goto fail;

  js_debug_wait_ready2start(sess->eval_ctx);
  if (eval_file(sess->eval_ctx, sess->eval_file, 1)) {
    sess->eval_errmsg = "failed to eval user script";
    goto fail;
  }

  goto succ;

done:
  js_free_debug(sess->eval_ctx);
  JS_FreeContext(sess->eval_ctx);
  JS_FreeRuntime(sess->eval_rt);

  sess->eval_ctx = NULL;
  sess->eval_rt = NULL;
  return NULL;

succ:
  sess->eval_retval = 0;
  goto done;

fail:
  sess->eval_retval = -1;
  goto done;
}

JSValue copy_val_cross_ctx(JSValue from_val, JSContext *from_ctx,
                           JSContext *to_ctx) {
  JSValue ser =
      JS_JSONStringify(from_ctx, from_val, JS_NULL, JS_NewInt32(from_ctx, 2));

  size_t len;
  const char *ser_cstr = JS_ToCStringLen(from_ctx, &len, ser);
  if (!ser_cstr) {
    JS_FreeValue(from_ctx, ser);
    return JS_NULL;
  }

  JSValue de = JS_ParseJSON(to_ctx, ser_cstr, len, "<input>");
  if (JS_IsException(de)) {
    de = JS_NULL;
  }
  
  JS_FreeValue(from_ctx, ser);
  JS_FreeCString(from_ctx, ser_cstr);
  return de;
}

void sess_event_handle(sess_t *sess, JSValue event) {
  JSValue act = JS_NULL, args = JS_NULL;
  const char *act_cstr;
  size_t act_len;
  int err_code = 500;
  const char *err_msg = "deformed request";

  act = JS_GetPropertyStr(sess->ctx, event, "type");
  act_cstr = JS_ToCString(sess->ctx, act);
  if (!act_cstr)
    goto fail;

  JSAtom data_atom = JS_NewAtom(sess->ctx, "data");
  args = JS_GetPropertyStr(sess->ctx, event, "data");
  JS_DeleteProperty(sess->ctx, event, data_atom, 0);

  if (!strcmp("ping", act_cstr)) {
    JS_SetPropertyStr(sess->ctx, event, "type",
                      JS_NewString(sess->ctx, "pong"));
    JS_SetPropertyStr(sess->ctx, event, "data", JS_DupValue(sess->ctx, args));
    goto succ;
  }

  if (!strcmp("launch", act_cstr)) {
    if (sess->eval_retval == -2) {
      err_msg = "previous script is still running";
      goto fail;
    }

    if (!JS_IsObject(args))
      goto fail;

    JSValue file = JS_GetPropertyStr(sess->ctx, args, "file");
    if (!JS_IsString(file)) {
      JS_FreeValue(sess->ctx, file);
      goto fail;
    }

    const char *file_cstr = JS_ToCString(sess->ctx, file);
    if (!file_cstr) {
      JS_FreeValue(sess->ctx, file);
      goto fail;
    }

    sess->eval_file = malloc(strlen(file_cstr) + 1);
    if (!sess->eval_file) {
      JS_FreeValue(sess->ctx, file);
      JS_FreeCString(sess->ctx, file_cstr);
      goto fail;
    }
    memcpy((void *)sess->eval_file, file_cstr, strlen(file_cstr) + 1);
    JS_FreeCString(sess->ctx, file_cstr);
    JS_FreeValue(sess->ctx, file);

    int s =
        pthread_create(&sess->eval_thread_id, NULL, &debug_user_script, sess);
    if (s) {
      err_msg = "failed to eval user script";
      goto fail;
    }

    sess->eval_retval = -2;

    goto succ;
  }

  if (!strcmp("setBreakpoint", act_cstr)) {
    if (!JS_IsObject(args))
      goto fail;

    JSValue file = JS_GetPropertyStr(sess->ctx, args, "file");
    if (!JS_IsString(file)) {
      JS_FreeValue(sess->ctx, file);
      goto fail;
    }

    const char *file_cstr = JS_ToCString(sess->ctx, file);
    if (!file_cstr) {
      JS_FreeValue(sess->ctx, file);
      goto fail;
    }

    JSValue line = JS_GetPropertyStr(sess->ctx, args, "line");
    if (!JS_IsNumber(line)) {
      JS_FreeValue(sess->ctx, file);
      JS_FreeValue(sess->ctx, line);
      goto fail;
    }

    JSValue col = JS_GetPropertyStr(sess->ctx, args, "col");
    if (!JS_IsNumber(col)) {
      JS_FreeValue(sess->ctx, file);
      JS_FreeValue(sess->ctx, line);
      JS_FreeValue(sess->ctx, col);
      goto fail;
    }

    int line_num, col_num;
    JS_ToInt32(sess->ctx, &line_num, line);
    JS_ToInt32(sess->ctx, &col_num, col);
    js_debug_set_breakpoint(sess->eval_ctx, file_cstr, line_num, col_num);
    JS_FreeCString(sess->ctx, file_cstr);
    JS_FreeValue(sess->ctx, file);

    goto succ;
  }

  if (!strcmp("run", act_cstr)) {
    if (sess->eval_retval != -2) {
      err_msg = "debugger does not launch yet";
      goto fail;
    }

    js_debug_on(sess->eval_ctx);
    js_debug_ready2start(sess->eval_ctx);
    goto succ;
  }

  if (!strcmp("continue", act_cstr)) {
    if (sess->eval_retval != -2 || sess->eval_ctx == NULL) {
      err_msg = "user script is not running";
      goto fail;
    }

    js_debug_continue(sess->eval_ctx);
    goto succ;
  }

  if (!strcmp("availableBreakpoints", act_cstr)) {
    JSValue bps = js_debug_list_breakpoints(sess->eval_ctx);
    JS_SetPropertyStr(sess->ctx, event, "data",
                      copy_val_cross_ctx(bps, sess->eval_ctx, sess->ctx));
    JS_FreeValue(sess->eval_ctx, bps);
    goto succ;
  }

  if (!strcmp("dumpStackframe", act_cstr)) {
    JSValue info = js_debug_dump_stackframe(sess->eval_ctx);
    JS_SetPropertyStr(sess->ctx, event, "data",
                      copy_val_cross_ctx(info, sess->eval_ctx, sess->ctx));
    JS_FreeValue(sess->eval_ctx, info);
    goto succ;
  }

  goto fail;

done:
  JS_FreeAtom(sess->ctx, data_atom);
  JS_FreeValue(sess->ctx, args);
  JS_FreeCString(sess->ctx, act_cstr);
  JS_FreeValue(sess->ctx, act);
  return;

succ:
  sess_event_send_replay(sess, event);
  goto done;

fail:
  JS_FreeValue(sess->ctx, event);
  sess_event_send_replay(sess, new_event_err(sess, err_code, err_msg));
  goto done;
}

void msg_trim_end(msg_t *msg) {
  while (msg->buf.size) {
    if (lre_is_space(msg->buf.buf[msg->buf.size - 1])) {
      msg->buf.size -= 1;
    } else {
      break;
    }
  }
}

void *sess_handler(void *arg) {
  sess_t *sess = arg;
  msg_t *msg;
  JSValue event, err;

  printf("new sess thread is running...\n");
  sess->invalid = FALSE;

  // setup rt in thread
  rt_setup(&sess->rt, &sess->ctx);

  pthread_mutex_lock(&sess->in_msgs_lock);
  while (1) {
    if (sess->invalid) {
      pthread_mutex_unlock(&sess->in_msgs_lock);
      if (sess->closing) {
        printf("client closed, stopping sess thread...\n");
      } else {
        printf("sess thread runs into invalid, stopping...\n");
      }
      break;
    }

    msg = sess_dequeue_in_msg(sess);
    if (msg) {
      msg_trim_end(msg);
      dbuf_putc(&msg->buf, 0);
      event = JS_ParseJSON(sess->ctx, (char *)msg->buf.buf, msg->buf.size - 1,
                           "<input>");
      if (JS_IsException(event)) {
        err = new_event_err(sess, 500, "deformed request");
        if (JS_IsException(err))
          return NULL;

        sess_event_send_replay(sess, err);
        continue;
      }

      sess_event_handle(sess, event);
      continue;
    }
    pthread_cond_wait(&sess->wakeup, &sess->in_msgs_lock);
  }
  pthread_mutex_unlock(&sess->in_msgs_lock);
  return NULL;
}

void sess_start(sess_t *sess) {
  int s = pthread_create(&sess->thead_id, NULL, &sess_handler, sess);
  if (s)
    fatal("failed to start sess\n");
}

void serve_debug(uint64_t port) {
  int max_fd, s, len;
  serv_t *serv;
  fd_set rfds, wfds, rfds1, wfds1;
  sess_t *sess;
  size_t iter = 0;
  void *iter_item;
  struct list_head *sess_iter, *sess_iter1, *msg_iter, *msg_iter1;
  msg_t *msg;
  struct timeval timeout;

  timeout.tv_sec = 3 * 60;
  timeout.tv_usec = 0;

  serv = new_serv(port);
  if (!serv)
    fatal("failed to make serv\n");

  s = bind(serv->fd, (struct sockaddr *)&serv->addr, sizeof(serv->addr));
  if (s)
    fatal("failed to bind: [%s]\n", strerror(errno));

  s = listen(serv->fd, 512);
  if (s)
    fatal("failed to listen: [%s]\n", strerror(errno));

  printf("server is running at: %llu\n", port);

  FD_ZERO(&rfds);
  FD_ZERO(&wfds);

  FD_SET(serv->fd, &rfds);

  max_fd = serv->fd;
  while (1) {
    FD_COPY(&rfds, &rfds1);
    FD_COPY(&wfds, &wfds1);

    s = select(max_fd + 1, &rfds1, &wfds1, NULL, &timeout);

    if (s < 0)
      fatal("failed to select: [%s]\n", strerror(errno));

    if (FD_ISSET(serv->fd, &rfds1)) {
      sess = accept_conn(serv);

      FD_SET(sess->conn.fd, &rfds);
      FD_SET(sess->conn.fd, &wfds);

      if (sess->conn.fd > max_fd) {
        max_fd = sess->conn.fd;
      }

      // simple thread model - one sess per thread, for complicated situation a
      // thread pool can be introduced and the numer of threads in that pool can
      // be set to the number of CPU cores
      sess_start(sess);
    }

    if (!list_empty(&serv->sessions)) {
      list_for_each_safe(sess_iter, sess_iter1, &serv->sessions) {
        sess = list_entry(sess_iter, sess_t, link);
        if (FD_ISSET(sess->conn.fd, &rfds1)) {
          if (sess_enqueue_in_msg(sess)) {
            printf("failed to read msg: %s\n", strerror(errno));
            FD_CLR(sess->conn.fd, &rfds);
            if (sess->conn.fd == max_fd)
              max_fd -= 1;
            sess->invalid = TRUE;
          }
        }

        if (FD_ISSET(sess->conn.fd, &wfds1)) {
          msg = sess_dequeue_out_msg(sess);
          if (msg) {
            len = send(sess->conn.fd, msg->buf.buf, msg->buf.size, 0);
            if (len < 0) {
              printf("failed to send msg: %s\n", strerror(errno));
              FD_CLR(sess->conn.fd, &wfds);
              if (sess->conn.fd == max_fd)
                max_fd -= 1;
              sess->invalid = TRUE;
            }
          }
        }

        if (sess->invalid)
          serv_free_sess(serv, sess);
      }
    }
  }
}