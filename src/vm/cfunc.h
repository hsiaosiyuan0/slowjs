#ifndef QUICKJS_CFUNC_H
#define QUICKJS_CFUNC_H

#include "def.h"

typedef struct JSCFunctionDataRecord {
  JSCFunctionData *func;
  uint8_t length;
  uint8_t data_len;
  uint16_t magic;
  JSValue data[0];
} JSCFunctionDataRecord;

/* Note: at least 'length' arguments will be readable in 'argv' */
JSValue JS_NewCFunction3(JSContext *ctx, JSCFunction *func, const char *name,
                         int length, JSCFunctionEnum cproto, int magic,
                         JSValueConst proto_val);

/* Note: at least 'length' arguments will be readable in 'argv' */
JSValue JS_NewCFunction2(JSContext *ctx, JSCFunction *func, const char *name,
                         int length, JSCFunctionEnum cproto, int magic);

void js_c_function_data_finalizer(JSRuntime *rt, JSValue val);

void js_c_function_data_mark(JSRuntime *rt, JSValueConst val,
                             JS_MarkFunc *mark_func);

JSValue js_c_function_data_call(JSContext *ctx, JSValueConst func_obj,
                                JSValueConst this_val, int argc,
                                JSValueConst *argv, int flags);

JSValue JS_NewCFunctionData(JSContext *ctx, JSCFunctionData *func, int length,
                            int magic, int data_len, JSValueConst *data);
#endif