#include "intrins.h"

#include "vm/conv.h"
#include "vm/error.h"
#include "vm/iter.h"
#include "vm/obj.h"
#include "vm/str.h"
#include "vm/vm.h"

static JSValue iterator_to_array(JSContext *ctx, JSValueConst items) {
  JSValue iter, next_method = JS_UNDEFINED;
  JSValue v, r = JS_UNDEFINED;
  int64_t k;
  BOOL done;

  iter = JS_GetIterator(ctx, items, FALSE);
  if (JS_IsException(iter))
    goto exception;
  next_method = JS_GetProperty(ctx, iter, JS_ATOM_next);
  if (JS_IsException(next_method))
    goto exception;
  r = JS_NewArray(ctx);
  if (JS_IsException(r))
    goto exception;
  for (k = 0;; k++) {
    v = JS_IteratorNext(ctx, iter, next_method, 0, NULL, &done);
    if (JS_IsException(v))
      goto exception_close;
    if (done)
      break;
    if (JS_DefinePropertyValueInt64(ctx, r, k, v,
                                    JS_PROP_C_W_E | JS_PROP_THROW) < 0)
      goto exception_close;
  }
done:
  JS_FreeValue(ctx, next_method);
  JS_FreeValue(ctx, iter);
  return r;
exception_close:
  JS_IteratorClose(ctx, iter, TRUE);
exception:
  JS_FreeValue(ctx, r);
  r = JS_EXCEPTION;
  goto done;
}

JSValue js_error_constructor(JSContext *ctx, JSValueConst new_target, int argc,
                             JSValueConst *argv, int magic) {
  JSValue obj, msg, proto;
  JSValueConst message;

  if (JS_IsUndefined(new_target))
    new_target = JS_GetActiveFunction(ctx);
  proto = JS_GetProperty(ctx, new_target, JS_ATOM_prototype);
  if (JS_IsException(proto))
    return proto;
  if (!JS_IsObject(proto)) {
    JSContext *realm;
    JSValueConst proto1;

    JS_FreeValue(ctx, proto);
    realm = JS_GetFunctionRealm(ctx, new_target);
    if (!realm)
      return JS_EXCEPTION;
    if (magic < 0) {
      proto1 = realm->class_proto[JS_CLASS_ERROR];
    } else {
      proto1 = realm->native_error_proto[magic];
    }
    proto = JS_DupValue(ctx, proto1);
  }
  obj = JS_NewObjectProtoClass(ctx, proto, JS_CLASS_ERROR);
  JS_FreeValue(ctx, proto);
  if (JS_IsException(obj))
    return obj;
  if (magic == JS_AGGREGATE_ERROR) {
    message = argv[1];
  } else {
    message = argv[0];
  }

  if (!JS_IsUndefined(message)) {
    msg = JS_ToString(ctx, message);
    if (unlikely(JS_IsException(msg)))
      goto exception;
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_message, msg,
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  }

  if (magic == JS_AGGREGATE_ERROR) {
    JSValue error_list = iterator_to_array(ctx, argv[0]);
    if (JS_IsException(error_list))
      goto exception;
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_errors, error_list,
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  }

  /* skip the Error() function in the backtrace */
  build_backtrace(ctx, obj, NULL, 0, JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL);
  return obj;
exception:
  JS_FreeValue(ctx, obj);
  return JS_EXCEPTION;
}

JSValue js_error_toString(JSContext *ctx, JSValueConst this_val, int argc,
                          JSValueConst *argv) {
  JSValue name, msg;

  if (!JS_IsObject(this_val))
    return JS_ThrowTypeErrorNotAnObject(ctx);
  name = JS_GetProperty(ctx, this_val, JS_ATOM_name);
  if (JS_IsUndefined(name))
    name = JS_AtomToString(ctx, JS_ATOM_Error);
  else
    name = JS_ToStringFree(ctx, name);
  if (JS_IsException(name))
    return JS_EXCEPTION;

  msg = JS_GetProperty(ctx, this_val, JS_ATOM_message);
  if (JS_IsUndefined(msg))
    msg = JS_AtomToString(ctx, JS_ATOM_empty_string);
  else
    msg = JS_ToStringFree(ctx, msg);
  if (JS_IsException(msg)) {
    JS_FreeValue(ctx, name);
    return JS_EXCEPTION;
  }
  if (!JS_IsEmptyString(name) && !JS_IsEmptyString(msg))
    name = JS_ConcatString3(ctx, "", name, ": ");
  return JS_ConcatString(ctx, name, msg);
}

JSValue js_throw_type_error(JSContext *ctx, JSValueConst this_val, int argc,
                            JSValueConst *argv) {
  return JS_ThrowTypeError(ctx, "invalid property access");
};

const JSCFunctionListEntry js_error_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_error_toString),
    JS_PROP_STRING_DEF("name", "Error",
                       JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE),
    JS_PROP_STRING_DEF("message", "", JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE),
};

const char *const native_error_name[JS_NATIVE_ERROR_COUNT] = {
    "EvalError", "RangeError", "ReferenceError", "SyntaxError",
    "TypeError", "URIError",   "InternalError",  "AggregateError",
};
