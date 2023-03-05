#include "debug.h"
#include "include/quickjs.h"
#include "libs/cutils.h"
#include "libs/list.h"
#include "utils/dbuf.h"
#include "vm/cfunc.h"
#include "vm/conv.h"
#include "vm/func.h"
#include "vm/instr.h"
#include "vm/intrins/intrins.h"
#include "vm/mod.h"
#include "vm/obj.h"
#include "vm/vm.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

static JSValue sort_bp_col_asc(JSContext *ctx, JSValueConst this_val, int argc,
                               JSValueConst *argv) {
  JSValue a = JS_GetPropertyStr(ctx, argv[0], "col");
  JSValue b = JS_GetPropertyStr(ctx, argv[1], "col");
  int aa = 0;
  int bb = 0;
  JS_ToInt32Free(ctx, &aa, a);
  JS_ToInt32Free(ctx, &bb, b);
  JS_FreeValue(ctx, b);
  JS_FreeValue(ctx, a);
  return JS_NewInt32(ctx, aa - bb);
}

JSValue js_debug_pc2line(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv) {
  JSValue bps, pc2line, ret, line2bps, pc2bp, line_bps, line_bps_arr, sort_fn,
      pc2bp_bps, pc2bp_first, line_bps_arr_len;
  uint8_t *buf = NULL, *buf_pos = NULL;
  const uint8_t *buf_end = NULL;
  JSObject *p = NULL;
  JSFunctionBytecode *b = NULL;
  int pl, pc = 0, line, op, len = 0;

  uint32_t uv, i = 0;
  int32_t v, col;
  const JSOpCode *oi;

  if (argc == 0 || !JS_IsFunction(ctx, argv[0])) {
    return JS_ThrowTypeError(ctx, "argument should be a function type");
  }

  bps = JS_NewArray(ctx);
  if (JS_IsException(bps))
    goto fail;

  line2bps = JS_NewObject(ctx);
  if (JS_IsException(line2bps)) {
    goto fail1;
  }

  pc2bp = JS_NewObject(ctx);
  if (JS_IsException(pc2bp)) {
    goto fail2;
  }

  p = JS_VALUE_GET_OBJ(argv[0]);
  b = p->u.func.function_bytecode;
  if (!b->has_debug) {
    JS_ThrowTypeError(ctx,
                      "function does not have debug info, maybe in strip mode");
    goto fail3;
  }

  buf = b->debug.pc2line_buf;
  buf_pos = buf;
  buf_end = buf + b->debug.pc2line_len;
  line = b->debug.line_num;

  while (buf_pos < buf_end) {
    pc2line = JS_NewObject(ctx);
    if (JS_IsException(pc2line)) {
      goto fail3;
    }

    pl = *buf_pos++;
    if (pl == 0) {
      len = get_leb128(&uv, buf_pos, buf_end);
      if (len < 0)
        goto fail;

      pc += uv;
      buf_pos += len;

      len = get_sleb128(&v, buf_pos, buf_end);
      if (len < 0) {
        JS_FreeValue(ctx, pc2line);
        JS_ThrowTypeError(ctx, "invalid pc2line pos=%d", (int)(buf_pos - buf));
        goto fail3;
      }

      line += v;
      buf_pos += len;
    } else {
      pl -= PC2LINE_OP_FIRST;
      pc += (pl / PC2LINE_RANGE);
      line += (pl % PC2LINE_RANGE) + PC2LINE_BASE;
    }

    len = get_sleb128(&col, buf_pos, buf_end);
    if (len < 0) {
      goto fail;
    }
    buf_pos += len;

    op = b->byte_code_buf[pc];
    oi = &short_opcode_info(op);

    JS_SetPropertyStr(ctx, pc2line, "op", JS_NewString(ctx, oi->name));
    JS_SetPropertyStr(ctx, pc2line, "pc", JS_NewInt32(ctx, pc));
    JS_SetPropertyStr(ctx, pc2line, "line", JS_NewInt32(ctx, line));
    JS_SetPropertyStr(ctx, pc2line, "col", JS_NewInt32(ctx, col));
    JS_SetPropertyUint32(ctx, bps, i++, pc2line);

    pc2bp_bps = JS_GetPropertyUint32(ctx, pc2bp, pc);
    if (JS_IsUndefined(pc2bp_bps)) {
      pc2bp_bps = JS_NewArray(ctx);
      if (JS_IsException(pc2bp_bps))
        goto fail;
      JS_SetPropertyUint32(ctx, pc2bp, pc, pc2bp_bps);
      JS_DupValue(ctx, pc2bp_bps); // to balance refcount
    }
    js_array_push(ctx, pc2bp_bps, 1, (JSValueConst *)&pc2line, 0);
    JS_FreeValue(ctx, pc2bp_bps);

    line_bps = JS_GetPropertyUint32(ctx, line2bps, line);
    if (JS_IsUndefined(line_bps)) {
      line_bps = JS_NewArray(ctx);
      if (JS_IsException(line_bps))
        goto fail3;
      JS_SetPropertyUint32(ctx, line2bps, line, line_bps);
      JS_DupValue(ctx, line_bps); // to balance refcount

      pc2bp_first = JS_NewObject(ctx);
      if (JS_IsException(pc2bp_first)) {
        JS_FreeValue(ctx, pc2bp_bps);
        goto fail;
      }

      JS_SetPropertyStr(ctx, pc2bp_first, "op", JS_NewString(ctx, oi->name));
      JS_SetPropertyStr(ctx, pc2bp_first, "pc", JS_NewInt32(ctx, pc));
      JS_SetPropertyStr(ctx, pc2bp_first, "line", JS_NewInt32(ctx, line));
      JS_SetPropertyStr(ctx, pc2bp_first, "col", JS_NewInt32(ctx, 0));
      js_array_push(ctx, pc2bp_bps, 1, (JSValueConst *)&pc2bp_first, 0);
      JS_FreeValue(ctx, pc2bp_first);
    }
    js_array_push(ctx, line_bps, 1, (JSValueConst *)&pc2line, 0);
    JS_FreeValue(ctx, line_bps);
  }

  line_bps_arr = js_object_keys(ctx, JS_NULL, 1, (JSValueConst *)&line2bps,
                                JS_ITERATOR_KIND_VALUE);
  sort_fn = JS_NewCFunction2(ctx, &sort_bp_col_asc, "sort_bp_col_asc", 2,
                             JS_CFUNC_generic, 0);
  line_bps_arr_len = JS_GetPropertyStr(ctx, line_bps_arr, "length");
  JS_ToInt32Free(ctx, &len, line_bps_arr_len);
  for (i = 0; i < len; i++) {
    line_bps = JS_GetPropertyUint32(ctx, line_bps_arr, i);
    JS_FreeValue(ctx,
                 js_array_sort(ctx, line_bps, 1, (JSValueConst *)&sort_fn));
    JS_FreeValue(ctx, line_bps);
  }
  JS_FreeValue(ctx, line_bps_arr_len);
  JS_FreeValue(ctx, line_bps_arr);
  JS_FreeValue(ctx, sort_fn);

  ret = JS_NewObject(ctx);
  if (JS_IsException(ret))
    goto fail3;

  JS_SetPropertyStr(ctx, ret, "file", JS_AtomToString(ctx, b->debug.filename));
  JS_SetPropertyStr(ctx, ret, "line", JS_NewInt32(ctx, b->debug.line_num));
  JS_SetPropertyStr(ctx, ret, "bps", bps);
  JS_SetPropertyStr(ctx, ret, "line2bps", line2bps);
  JS_SetPropertyStr(ctx, ret, "pc2bp", pc2bp);

  return ret;

fail3:
  JS_FreeValue(ctx, pc2bp);
fail2:
  JS_FreeValue(ctx, line2bps);
fail1:
  JS_FreeValue(ctx, bps);
fail:
  return JS_EXCEPTION;
}

// list all the available breakpoints of currently paused function
// Note: only use this method when vm is paused by breakpoint hit
JSValue js_debug_list_breakpoints(JSContext *ctx) {
  struct JSStackFrame *sf = ctx->rt->current_stack_frame;
  if (!ctx->debug.paused || !sf)
    return JS_NULL;

  return js_debug_pc2line(ctx, JS_NULL, 1, (JSValue *)&sf->cur_func);
}

// list the args, vars and var_refs in the stackframe of current paused
// function execution
// Note: only use this method when vm is paused by breakpoint hit
JSValue js_debug_dump_stackframe(JSContext *ctx) {
  struct JSStackFrame *sf = ctx->rt->current_stack_frame;
  if (!ctx->debug.paused || !sf)
    return JS_NULL;

  JSValue fn = sf->cur_func;
  if (JS_IsUndefined(fn))
    return JS_NULL;

  JSObject *p = JS_VALUE_GET_OBJ(fn);
  JSFunctionBytecode *b = p->u.func.function_bytecode;

  JSValue ret = JS_NewObject(ctx);
  if (JS_IsException(ret))
    return JS_NULL;

  JSValue args = JS_NewArray(ctx);
  if (JS_IsException(args))
    goto fail;
  JS_SetPropertyStr(ctx, ret, "args", args);

  JSValue vars = JS_NewArray(ctx);
  if (JS_IsException(vars))
    goto fail;
  JS_SetPropertyStr(ctx, ret, "vars", vars);

  JSValue closure_vars = JS_NewArray(ctx);
  if (JS_IsException(vars))
    goto fail;
  JS_SetPropertyStr(ctx, ret, "closure_vars", closure_vars);

  for (int i = 0; i < sf->arg_count; i++) {
    JSValue arg = JS_NewObject(ctx);
    if (JS_IsException(arg))
      goto fail;

    JSVarDef vd = b->vardefs[i];
    JS_SetPropertyStr(ctx, arg, "name", JS_AtomToString(ctx, vd.var_name));
    JS_SetPropertyStr(ctx, arg, "value", JS_DupValue(ctx, sf->arg_buf[i]));
    js_array_push(ctx, args, 1, (JSValueConst *)&arg, 0);
    JS_FreeValue(ctx, arg);
  }

  for (int i = 0; i < b->var_count; i++) {
    JSValue var = JS_NewObject(ctx);
    if (JS_IsException(var))
      goto fail;

    JSVarDef vd = b->vardefs[b->arg_count + i];
    JS_SetPropertyStr(ctx, var, "name", JS_AtomToString(ctx, vd.var_name));
    JS_SetPropertyStr(ctx, var, "value", JS_DupValue(ctx, sf->var_buf[i]));
    js_array_push(ctx, vars, 1, (JSValueConst *)&var, 0);
    JS_FreeValue(ctx, var);
  }

  JSVarRef **var_refs = p->u.func.var_refs;
  for (int i = 0; i < b->closure_var_count; i++) {
    JSValue var = JS_NewObject(ctx);
    if (JS_IsException(var))
      goto fail;

    JSClosureVar cv = b->closure_var[i];
    JS_SetPropertyStr(ctx, var, "name", JS_AtomToString(ctx, cv.var_name));

    JS_SetPropertyStr(ctx, var, "value",
                      JS_DupValue(ctx, *var_refs[i]->pvalue));
    js_array_push(ctx, closure_vars, 1, (JSValueConst *)&var, 0);
    JS_FreeValue(ctx, var);
  }

  return ret;

fail:
  JS_FreeValue(ctx, ret);
  return JS_NULL;
}

// retrieve breakpoints at pc, the length of returned array is [0, 2]
static JSBreakpoint *js_debug_bps_from_pc(const uint8_t *pc, JSRuntime *rt,
                                          JSContext *ctx, uint32_t *len) {

  JSBreakpoint *ret = NULL;
  JSValue bps, bp, line, col;
  struct JSStackFrame *sf = rt->current_stack_frame;
  *len = 0;

  if (!sf)
    return NULL;

  JSValue fn = sf->cur_func;
  JSFunctionBytecode *b = JS_VALUE_GET_OBJ(fn)->u.func.function_bytecode;

  size_t delta = pc - b->byte_code_buf;
  JSAtom file = b->debug.filename;
  JSValue debug_info = js_debug_pc2line(ctx, JS_NULL, 1, (JSValueConst *)&fn);
  if (JS_IsException(debug_info))
    return NULL;

  // `debug_info` always has `pc2bp`
  JSValue pc2bp = JS_GetPropertyStr(ctx, debug_info, "pc2bp");
  bps = JS_GetPropertyUint32(ctx, pc2bp, delta);
  if (JS_IsUndefined(bps))
    goto done;

  if (js_get_length32(ctx, len, bps) || *len == 0)
    goto done;

  ret = calloc(*len, sizeof(JSBreakpoint));
  if (!ret)
    goto done;

  for (uint32_t i = 0; i < *len; i++) {
    bp = JS_GetPropertyUint32(ctx, bps, i);

    ret[i].file = file;
    line = JS_GetPropertyStr(ctx, bp, "line");
    JS_ToInt32Free(ctx, &ret[i].line, line);
    JS_FreeValue(ctx, line);

    col = JS_GetPropertyStr(ctx, bp, "col");
    JS_ToInt32Free(ctx, &ret[i].col, col);
    JS_FreeValue(ctx, line);

    JS_FreeValue(ctx, bp);
  }

done:
  JS_FreeValue(ctx, bps);
  JS_FreeValue(ctx, pc2bp);
  JS_FreeValue(ctx, debug_info);
  return ret;
}

JSBreakpoint *js_debug_get_bp(JSContext *ctx, JSBreakpoint bp);
static int js_debug_interrupt(const uint8_t *pc, JSRuntime *rt, void *opaque) {
  JSContext *ctx;

  if (unlikely(rt->debug)) {
    ctx = opaque;

    uint32_t len;
    JSBreakpoint *bps = js_debug_bps_from_pc(pc, rt, ctx, &len);
    if (len == 0)
      return 0;

    for (uint32_t i = 0; i < len; i++) {
      if (bps[i].file && js_debug_get_bp(ctx, bps[i])) {
        pthread_mutex_lock(&ctx->debug.bp_mutex);
        ctx->debug.paused = TRUE;
        pthread_cond_wait(&ctx->debug.bp_cond, &ctx->debug.bp_mutex);
        ctx->debug.paused = FALSE;
        pthread_mutex_unlock(&ctx->debug.bp_mutex);
        break;
      }
    }
  }
  return 0;
}

int js_debug_init(JSContext *ctx) {
#if defined(__APPLE__)
  ctx->debug.ready2start = dispatch_semaphore_create(0);
#else
  if (sem_init(&ctx->debug.ready2start, 0, 0))
    return -1;
#endif

  if (pthread_mutex_init(&ctx->debug.bp_mutex, NULL))
    return -1;

  if (pthread_cond_init(&ctx->debug.bp_cond, NULL))
    return -1;

  init_list_head(&ctx->debug.bps);

  JS_SetPCInterruptHandler(ctx->rt, &js_debug_interrupt, ctx);
  return 0;
}

void js_debug_wait_ready2start(JSContext *ctx) {
#if defined(__APPLE__)
  dispatch_semaphore_wait(ctx->debug.ready2start, DISPATCH_TIME_FOREVER);
#else
  int r;
  do {
    r = sem_wait(&ctx->debug.ready2start);
  } while (r == -1 && errno == EINTR);
#endif
}

void js_debug_ready2start(JSContext *ctx) {
#if defined(__APPLE__)
  dispatch_semaphore_signal(ctx->debug.ready2start);
#else
  sem_post(&ctx->debug.ready2start);
#endif
}

void js_debug_on(JSContext *ctx) { ctx->rt->debug = TRUE; }
void js_debug_off(JSContext *ctx) { ctx->rt->debug = FALSE; }

void js_debug_free_bp(JSContext *ctx, JSBreakpoint *bp) {
  JS_FreeAtom(ctx, bp->file);
  free(bp);
}

void js_free_debug(JSContext *ctx) {
  struct list_head *el;
  JSBreakpoint *bp;

  pthread_mutex_destroy(&ctx->debug.bp_mutex);
  pthread_cond_destroy(&ctx->debug.bp_cond);

  list_for_each(el, &ctx->debug.bps) {
    bp = list_entry(el, JSBreakpoint, link);
    js_debug_free_bp(ctx, bp);
  }
}

// get breakpoint from the breakpoints collection manually specified by user
JSBreakpoint *js_debug_get_bp(JSContext *ctx, JSBreakpoint bp) {
  struct list_head *el;
  JSBreakpoint *iter = NULL;
  JSBreakpoint *bpp = NULL;

  list_for_each(el, &ctx->debug.bps) {
    iter = list_entry(el, JSBreakpoint, link);
    if (iter->file == bp.file && iter->line == bp.line && iter->col == bp.col) {
      bpp = iter;
      break;
    }
  }
  return bpp;
}

// add breakpoint to the breakpoints collection manually specified by user
int js_debug_add_bp(JSContext *ctx, JSBreakpoint bp) {
  JSBreakpoint *bpp;
  if (js_debug_get_bp(ctx, bp))
    return 0;

  bpp = malloc(sizeof(*bpp));
  if (!bpp)
    return -1;

  memcpy(bpp, &bp, sizeof(bp));
  list_add_tail(&bpp->link, &ctx->debug.bps);
  return 0;
}

void js_debug_del_bp(JSContext *ctx, JSBreakpoint bp) {
  JSBreakpoint *bpp = js_debug_get_bp(ctx, bp);
  if (bpp) {
    list_del(&bpp->link);
    js_debug_free_bp(ctx, bpp);
  }
}

int js_debug_set_breakpoint(JSContext *ctx, const char *file, int line,
                            int col) {
  return js_debug_add_bp(ctx, (JSBreakpoint){(struct list_head){},
                                             JS_NewAtom(ctx, file), line, col});
}

void js_debug_continue(JSContext *ctx) {
  if (!ctx->debug.paused)
    return;

  pthread_cond_signal(&ctx->debug.bp_cond);
}
