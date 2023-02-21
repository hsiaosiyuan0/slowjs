#include "debug.h"
#include "include/quickjs.h"
#include "utils/dbuf.h"
#include "vm/func.h"
#include "vm/instr.h"
#include "vm/obj.h"

JSValue js_debug_pc2line(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv) {
  JSValue pc2lineArr, pc2line;

  uint8_t *buf = NULL, *buf_pos = NULL;
  const uint8_t *buf_end = NULL;
  JSObject *p = NULL;
  JSFunctionBytecode *b = NULL;
  int pl, pc = 0, line, len, op;

  uint32_t uv, i = 0;
  int32_t v, col;
  const JSOpCode *oi;

  if (argc == 0 || !JS_IsFunction(ctx, argv[0])) {
    return JS_ThrowTypeError(ctx, "argument should be a function type");
  }

  pc2lineArr = JS_NewArray(ctx);
  if (JS_IsException(pc2lineArr))
    return JS_EXCEPTION;

  p = JS_VALUE_GET_OBJ(argv[0]);
  b = p->u.func.function_bytecode;
  if (!b->has_debug) {
    JS_FreeValue(ctx, pc2lineArr);
    return JS_ThrowTypeError(
        ctx, "function does not have debug info, maybe in strip mode");
  }

  buf = b->debug.pc2line_buf;
  buf_pos = buf;
  buf_end = buf + b->debug.pc2line_len;
  line = b->debug.line_num;

  while (buf_pos < buf_end) {
    pc2line = JS_NewObject(ctx);
    if (JS_IsException(pc2line))
      return JS_EXCEPTION;

    pl = *buf_pos++;
    if (pl == 0) {
      len = get_leb128(&uv, buf_pos, buf_end);
      if (len < 0)
        goto fail;

      pc += uv;
      buf_pos += len;

      len = get_sleb128(&v, buf_pos, buf_end);
      if (len < 0) {
      fail:
        JS_FreeValue(ctx, pc2lineArr);
        JS_FreeValue(ctx, pc2line);
        return JS_ThrowTypeError(ctx, "invalid pc2line pos=%d",
                                 (int)(buf_pos - buf));
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
    JS_SetPropertyStr(ctx, pc2line, "column", JS_NewInt32(ctx, col));
    JS_SetPropertyUint32(ctx, pc2lineArr, i++, pc2line);
  }

  return pc2lineArr;
}