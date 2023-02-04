#include "intrins.h"

#include "vm/error.h"
#include "vm/obj.h"
#include "vm/str.h"
#include "vm/vm.h"

JSValue js_function_proto(JSContext *ctx, JSValueConst this_val, int argc,
                          JSValueConst *argv) {
  return JS_UNDEFINED;
}

/* XXX: add a specific eval mode so that Function("}), ({") is rejected */
JSValue js_function_constructor(JSContext *ctx, JSValueConst new_target,
                                int argc, JSValueConst *argv, int magic) {
  JSFunctionKindEnum func_kind = magic;
  int i, n, ret;
  JSValue s, proto, obj = JS_UNDEFINED;
  StringBuffer b_s, *b = &b_s;

  string_buffer_init(ctx, b, 0);
  string_buffer_putc8(b, '(');

  if (func_kind == JS_FUNC_ASYNC || func_kind == JS_FUNC_ASYNC_GENERATOR) {
    string_buffer_puts8(b, "async ");
  }
  string_buffer_puts8(b, "function");

  if (func_kind == JS_FUNC_GENERATOR || func_kind == JS_FUNC_ASYNC_GENERATOR) {
    string_buffer_putc8(b, '*');
  }
  string_buffer_puts8(b, " anonymous(");

  n = argc - 1;
  for (i = 0; i < n; i++) {
    if (i != 0) {
      string_buffer_putc8(b, ',');
    }
    if (string_buffer_concat_value(b, argv[i]))
      goto fail;
  }
  string_buffer_puts8(b, "\n) {\n");
  if (n >= 0) {
    if (string_buffer_concat_value(b, argv[n]))
      goto fail;
  }
  string_buffer_puts8(b, "\n})");
  s = string_buffer_end(b);
  if (JS_IsException(s))
    goto fail1;

  obj = JS_EvalObject(ctx, ctx->global_obj, s, JS_EVAL_TYPE_INDIRECT, -1);
  JS_FreeValue(ctx, s);
  if (JS_IsException(obj))
    goto fail1;
  if (!JS_IsUndefined(new_target)) {
    /* set the prototype */
    proto = JS_GetProperty(ctx, new_target, JS_ATOM_prototype);
    if (JS_IsException(proto))
      goto fail1;
    if (!JS_IsObject(proto)) {
      JSContext *realm;
      JS_FreeValue(ctx, proto);
      realm = JS_GetFunctionRealm(ctx, new_target);
      if (!realm)
        goto fail1;
      proto = JS_DupValue(ctx,
                          realm->class_proto[func_kind_to_class_id[func_kind]]);
    }
    ret = JS_SetPrototypeInternal(ctx, obj, proto, TRUE);
    JS_FreeValue(ctx, proto);
    if (ret < 0)
      goto fail1;
  }
  return obj;

fail:
  string_buffer_free(b);
fail1:
  JS_FreeValue(ctx, obj);
  return JS_EXCEPTION;
}

/* XXX: not 100% compatible, but mozilla seems to use a similar
   implementation to ensure that caller in non strict mode does not
   throw (ES5 compatibility) */
JSValue js_function_proto_caller(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv) {
  JSFunctionBytecode *b = JS_GetFunctionBytecode(this_val);
  if (!b || (b->js_mode & JS_MODE_STRICT) || !b->has_prototype) {
    return js_throw_type_error(ctx, this_val, 0, NULL);
  }
  return JS_UNDEFINED;
};

JSValue js_function_call(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv) {
  if (argc <= 0) {
    return JS_Call(ctx, this_val, JS_UNDEFINED, 0, NULL);
  } else {
    return JS_Call(ctx, this_val, argv[0], argc - 1, argv + 1);
  }
}

JSValue js_function_bind(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv) {
  JSBoundFunction *bf;
  JSValue func_obj, name1, len_val;
  JSObject *p;
  int arg_count, i, ret;

  if (check_function(ctx, this_val))
    return JS_EXCEPTION;

  func_obj =
      JS_NewObjectProtoClass(ctx, ctx->function_proto, JS_CLASS_BOUND_FUNCTION);
  if (JS_IsException(func_obj))
    return JS_EXCEPTION;
  p = JS_VALUE_GET_OBJ(func_obj);
  p->is_constructor = JS_IsConstructor(ctx, this_val);
  arg_count = max_int(0, argc - 1);
  bf = js_malloc(ctx, sizeof(*bf) + arg_count * sizeof(JSValue));
  if (!bf)
    goto exception;
  bf->func_obj = JS_DupValue(ctx, this_val);
  bf->this_val = JS_DupValue(ctx, argv[0]);
  bf->argc = arg_count;
  for (i = 0; i < arg_count; i++) {
    bf->argv[i] = JS_DupValue(ctx, argv[i + 1]);
  }
  p->u.bound_function = bf;

  /* XXX: the spec could be simpler by only using GetOwnProperty */
  ret = JS_GetOwnProperty(ctx, NULL, this_val, JS_ATOM_length);
  if (ret < 0)
    goto exception;
  if (!ret) {
    len_val = JS_NewInt32(ctx, 0);
  } else {
    len_val = JS_GetProperty(ctx, this_val, JS_ATOM_length);
    if (JS_IsException(len_val))
      goto exception;
    if (JS_VALUE_GET_TAG(len_val) == JS_TAG_INT) {
      /* most common case */
      int len1 = JS_VALUE_GET_INT(len_val);
      if (len1 <= arg_count)
        len1 = 0;
      else
        len1 -= arg_count;
      len_val = JS_NewInt32(ctx, len1);
    } else if (JS_VALUE_GET_NORM_TAG(len_val) == JS_TAG_FLOAT64) {
      double d = JS_VALUE_GET_FLOAT64(len_val);
      if (isnan(d)) {
        d = 0.0;
      } else {
        d = trunc(d);
        if (d <= (double)arg_count)
          d = 0.0;
        else
          d -= (double)arg_count; /* also converts -0 to +0 */
      }
      len_val = JS_NewFloat64(ctx, d);
    } else {
      JS_FreeValue(ctx, len_val);
      len_val = JS_NewInt32(ctx, 0);
    }
  }
  JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_length, len_val,
                         JS_PROP_CONFIGURABLE);

  name1 = JS_GetProperty(ctx, this_val, JS_ATOM_name);
  if (JS_IsException(name1))
    goto exception;
  if (!JS_IsString(name1)) {
    JS_FreeValue(ctx, name1);
    name1 = JS_AtomToString(ctx, JS_ATOM_empty_string);
  }
  name1 = JS_ConcatString3(ctx, "bound ", name1, "");
  if (JS_IsException(name1))
    goto exception;
  JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_name, name1,
                         JS_PROP_CONFIGURABLE);
  return func_obj;
exception:
  JS_FreeValue(ctx, func_obj);
  return JS_EXCEPTION;
}

static JSValue js_function_toString(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv) {
  JSObject *p;
  JSFunctionKindEnum func_kind = JS_FUNC_NORMAL;

  if (check_function(ctx, this_val))
    return JS_EXCEPTION;

  p = JS_VALUE_GET_OBJ(this_val);
  if (js_class_has_bytecode(p->class_id)) {
    JSFunctionBytecode *b = p->u.func.function_bytecode;
    if (b->has_debug && b->debug.source) {
      return JS_NewStringLen(ctx, b->debug.source, b->debug.source_len);
    }
    func_kind = b->func_kind;
  }
  {
    JSValue name;
    const char *pref, *stuff;

    switch (func_kind) {
    default:
    case JS_FUNC_NORMAL:
      pref = "function ";
      break;
    case JS_FUNC_GENERATOR:
      pref = "function *";
      break;
    case JS_FUNC_ASYNC:
      pref = "async function ";
      break;
    case JS_FUNC_ASYNC_GENERATOR:
      pref = "async function *";
      break;
    }
    stuff = "() {\n    [native code]\n}";
    name = JS_GetProperty(ctx, this_val, JS_ATOM_name);
    if (JS_IsUndefined(name))
      name = JS_AtomToString(ctx, JS_ATOM_empty_string);
    return JS_ConcatString3(ctx, pref, name, stuff);
  }
}

static JSValue js_function_hasInstance(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv) {
  int ret;
  ret = JS_OrdinaryIsInstanceOf(ctx, argv[0], this_val);
  if (ret < 0)
    return JS_EXCEPTION;
  else
    return JS_NewBool(ctx, ret);
}

static JSValue js_function_proto_fileName(JSContext *ctx,
                                          JSValueConst this_val) {
  JSFunctionBytecode *b = JS_GetFunctionBytecode(this_val);
  if (b && b->has_debug) {
    return JS_AtomToString(ctx, b->debug.filename);
  }
  return JS_UNDEFINED;
}

static JSValue js_function_proto_lineNumber(JSContext *ctx,
                                            JSValueConst this_val) {
  JSFunctionBytecode *b = JS_GetFunctionBytecode(this_val);
  if (b && b->has_debug) {
    return JS_NewInt32(ctx, b->debug.line_num);
  }
  return JS_UNDEFINED;
}

const JSCFunctionListEntry js_function_proto_funcs[] = {
    JS_CFUNC_DEF("call", 1, js_function_call),
    JS_CFUNC_MAGIC_DEF("apply", 2, js_function_apply, 0),
    JS_CFUNC_DEF("bind", 1, js_function_bind),
    JS_CFUNC_DEF("toString", 0, js_function_toString),
    JS_CFUNC_DEF("[Symbol.hasInstance]", 1, js_function_hasInstance),
    JS_CGETSET_DEF("fileName", js_function_proto_fileName, NULL),
    JS_CGETSET_DEF("lineNumber", js_function_proto_lineNumber, NULL),
};
