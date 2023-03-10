#include "cfunc.h"

#include "class.h"
#include "func.h"
#include "obj.h"

/* Note: at least 'length' arguments will be readable in 'argv' */
JSValue JS_NewCFunction3(JSContext *ctx, JSCFunction *func, const char *name,
                         int length, JSCFunctionEnum cproto, int magic,
                         JSValueConst proto_val) {
  JSValue func_obj;
  JSObject *p;
  JSAtom name_atom;

  func_obj = JS_NewObjectProtoClass(ctx, proto_val, JS_CLASS_C_FUNCTION);
  if (JS_IsException(func_obj))
    return func_obj;
  p = JS_VALUE_GET_OBJ(func_obj);
  p->u.cfunc.realm = JS_DupContext(ctx);
  p->u.cfunc.c_function.generic = func;
  p->u.cfunc.length = length;
  p->u.cfunc.cproto = cproto;
  p->u.cfunc.magic = magic;
  p->is_constructor =
      (cproto == JS_CFUNC_constructor || cproto == JS_CFUNC_constructor_magic ||
       cproto == JS_CFUNC_constructor_or_func ||
       cproto == JS_CFUNC_constructor_or_func_magic);
  if (!name)
    name = "";
  name_atom = JS_NewAtom(ctx, name);
  js_function_set_properties(ctx, func_obj, name_atom, length);
  JS_FreeAtom(ctx, name_atom);
  return func_obj;
}

/* Note: at least 'length' arguments will be readable in 'argv' */
JSValue JS_NewCFunction2(JSContext *ctx, JSCFunction *func, const char *name,
                         int length, JSCFunctionEnum cproto, int magic) {
  return JS_NewCFunction3(ctx, func, name, length, cproto, magic,
                          ctx->function_proto);
}

void js_c_function_data_finalizer(JSRuntime *rt, JSValue val) {
  JSCFunctionDataRecord *s = JS_GetOpaque(val, JS_CLASS_C_FUNCTION_DATA);
  int i;

  if (s) {
    for (i = 0; i < s->data_len; i++) {
      JS_FreeValueRT(rt, s->data[i]);
    }
    js_free_rt(rt, s);
  }
}

void js_c_function_data_mark(JSRuntime *rt, JSValueConst val,
                             JS_MarkFunc *mark_func) {
  JSCFunctionDataRecord *s = JS_GetOpaque(val, JS_CLASS_C_FUNCTION_DATA);
  int i;

  if (s) {
    for (i = 0; i < s->data_len; i++) {
      JS_MarkValue(rt, s->data[i], mark_func);
    }
  }
}

// TODO:
void js_c_function_data_walk(JSRuntime *rt, JSValueConst val,
                             JS_WalkFunc *walk_func, void *uctx) {}

JSValue js_c_function_data_call(JSContext *ctx, JSValueConst func_obj,
                                JSValueConst this_val, int argc,
                                JSValueConst *argv, int flags) {
  JSCFunctionDataRecord *s = JS_GetOpaque(func_obj, JS_CLASS_C_FUNCTION_DATA);
  JSValueConst *arg_buf;
  int i;

  /* XXX: could add the function on the stack for debug */
  if (unlikely(argc < s->length)) {
    arg_buf = alloca(sizeof(arg_buf[0]) * s->length);
    for (i = 0; i < argc; i++)
      arg_buf[i] = argv[i];
    for (i = argc; i < s->length; i++)
      arg_buf[i] = JS_UNDEFINED;
  } else {
    arg_buf = argv;
  }

  return s->func(ctx, this_val, argc, arg_buf, s->magic, s->data);
}

JSValue JS_NewCFunctionData(JSContext *ctx, JSCFunctionData *func, int length,
                            int magic, int data_len, JSValueConst *data) {
  JSCFunctionDataRecord *s;
  JSValue func_obj;
  int i;

  func_obj = JS_NewObjectProtoClass(ctx, ctx->function_proto,
                                    JS_CLASS_C_FUNCTION_DATA);
  if (JS_IsException(func_obj))
    return func_obj;
  s = js_malloc(ctx, sizeof(*s) + data_len * sizeof(JSValue));
  if (!s) {
    JS_FreeValue(ctx, func_obj);
    return JS_EXCEPTION;
  }
  s->func = func;
  s->length = length;
  s->data_len = data_len;
  s->magic = magic;
  for (i = 0; i < data_len; i++)
    s->data[i] = JS_DupValue(ctx, data[i]);
  JS_SetOpaque(func_obj, s);
  js_function_set_properties(ctx, func_obj, JS_ATOM_empty_string, length);
  return func_obj;
}