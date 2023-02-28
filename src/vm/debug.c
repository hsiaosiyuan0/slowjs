#include "debug.h"
#include "include/quickjs.h"
#include "utils/dbuf.h"
#include "vm/cfunc.h"
#include "vm/conv.h"
#include "vm/func.h"
#include "vm/instr.h"
#include "vm/intrins/intrins.h"
#include "vm/obj.h"

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
      line_bps_arr_len;
  BOOL line_bps_new = FALSE;
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

    JS_SetPropertyUint32(ctx, pc2bp, pc, JS_DupValue(ctx, pc2line));

    line_bps_new = FALSE;
    line_bps = JS_GetPropertyUint32(ctx, line2bps, line);
    if (JS_IsUndefined(line_bps)) {
      line_bps = JS_NewArray(ctx);
      if (JS_IsException(line_bps))
        goto fail3;
      line_bps_new = TRUE;
      JS_SetPropertyUint32(ctx, line2bps, line, line_bps);
    }
    js_array_push(ctx, line_bps, 1, (JSValueConst *)&pc2line, 0);
    if (!line_bps_new) { // balance ref_count
      JS_FreeValue(ctx, line_bps);
    }
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

static inline int js_debug(JSRuntime *rt, void *opaque) {
  JSContext *ctx = opaque;
  if (ctx->debug.pause) {
    pthread_mutex_lock(&ctx->debug.bp_mutex);
    printf("paused\n");
    pthread_cond_wait(&ctx->debug.bp_cond, &ctx->debug.bp_mutex);
  }
  return 0;
}

void js_debug_continue(JSContext *ctx) {
  pthread_cond_signal(&ctx->debug.bp_cond);
}

void js_debug_init(JSContext *ctx) {
  pthread_mutex_init(&ctx->debug.bp_mutex, NULL);
  pthread_cond_init(&ctx->debug.bp_cond, NULL);
  JS_SetInterruptHandler(ctx->rt, &js_debug, ctx);
}

void js_debug_pause(JSContext *ctx) { ctx->debug.pause = TRUE; }
