#include "include/quickjs-libc.h"
#include "libs/cutils.h"
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
  pthread_t theadId;

  BOOL invalid;
} sess_t;

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

    if (len == 0)
      return 0;

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
  int flags;

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

void *sess_thread_handler(void *arg) {
  sess_t *sess = arg;
  struct list_head *el, *el1;
  msg_t *msg;
  int len;

  printf("sess thread is running...\n");
  sess->invalid = FALSE;

  pthread_mutex_lock(&sess->in_msgs_lock);
  while (1) {
    if (sess->invalid) {
      pthread_mutex_unlock(&sess->in_msgs_lock);
      break;
    }

    msg = sess_dequeue_in_msg(sess);
    if (msg) {
      sess_enqueue_out_msg(sess, msg);
      continue;
    }
    pthread_cond_wait(&sess->wakeup, &sess->in_msgs_lock);
  }
  return NULL;
}

void sess_start(sess_t *sess) {
  int s = pthread_create(&sess->theadId, NULL, &sess_thread_handler, sess);
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

      sess_start(sess);
    }

    if (!list_empty(&serv->sessions)) {
      list_for_each_safe(sess_iter, sess_iter1, &serv->sessions) {
        sess = list_entry(sess_iter, sess_t, link);
        if (FD_ISSET(sess->conn.fd, &rfds1)) {
          if (sess_enqueue_in_msg(sess)) {
            printf("failed to read msg: %s\n", strerror(errno));
            sess->invalid = TRUE;
          }
        }

        if (FD_ISSET(sess->conn.fd, &wfds1)) {
          msg = sess_dequeue_out_msg(sess);
          if (msg) {
            len = send(sess->conn.fd, msg->buf.buf, msg->buf.size, 0);
            if (len < 0) {
              printf("failed to send msg: %s\n", strerror(errno));
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