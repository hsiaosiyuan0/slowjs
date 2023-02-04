#include "intrins.h"

#include "vm/cfunc.h"
#include "vm/conv.h"
#include "vm/obj.h"
#include "vm/shape.h"
#include "vm/vm.h"

/* Global functions */

static JSValue js_parseInt(JSContext *ctx, JSValueConst this_val, int argc,
                           JSValueConst *argv) {
  const char *str, *p;
  int radix, flags;
  JSValue ret;

  str = JS_ToCString(ctx, argv[0]);
  if (!str)
    return JS_EXCEPTION;
  if (JS_ToInt32(ctx, &radix, argv[1])) {
    JS_FreeCString(ctx, str);
    return JS_EXCEPTION;
  }
  if (radix != 0 && (radix < 2 || radix > 36)) {
    ret = JS_NAN;
  } else {
    p = str;
    p += skip_spaces(p);
    flags = ATOD_INT_ONLY | ATOD_ACCEPT_PREFIX_AFTER_SIGN;
    ret = js_atof(ctx, p, NULL, radix, flags);
  }
  JS_FreeCString(ctx, str);
  return ret;
}

static JSValue js_parseFloat(JSContext *ctx, JSValueConst this_val, int argc,
                             JSValueConst *argv) {
  const char *str, *p;
  JSValue ret;

  str = JS_ToCString(ctx, argv[0]);
  if (!str)
    return JS_EXCEPTION;
  p = str;
  p += skip_spaces(p);
  ret = js_atof(ctx, p, NULL, 10, 0);
  JS_FreeCString(ctx, str);
  return ret;
}

JSValue js_global_eval(JSContext *ctx, JSValueConst this_val, int argc,
                       JSValueConst *argv) {
  return JS_EvalObject(ctx, ctx->global_obj, argv[0], JS_EVAL_TYPE_INDIRECT,
                       -1);
}

JSValue js_global_isNaN(JSContext *ctx, JSValueConst this_val, int argc,
                        JSValueConst *argv) {
  double d;

  /* XXX: does this work for bigfloat? */
  if (unlikely(JS_ToFloat64(ctx, &d, argv[0])))
    return JS_EXCEPTION;
  return JS_NewBool(ctx, isnan(d));
}

JSValue js_global_isFinite(JSContext *ctx, JSValueConst this_val, int argc,
                           JSValueConst *argv) {
  BOOL res;
  double d;
  if (unlikely(JS_ToFloat64(ctx, &d, argv[0])))
    return JS_EXCEPTION;
  res = isfinite(d);
  return JS_NewBool(ctx, res);
}

static const JSCFunctionListEntry js_global_funcs[] = {
    JS_CFUNC_DEF("parseInt", 2, js_parseInt),
    JS_CFUNC_DEF("parseFloat", 1, js_parseFloat),
    JS_CFUNC_DEF("isNaN", 1, js_global_isNaN),
    JS_CFUNC_DEF("isFinite", 1, js_global_isFinite),

    JS_CFUNC_MAGIC_DEF("decodeURI", 1, js_global_decodeURI, 0),
    JS_CFUNC_MAGIC_DEF("decodeURIComponent", 1, js_global_decodeURI, 1),
    JS_CFUNC_MAGIC_DEF("encodeURI", 1, js_global_encodeURI, 0),
    JS_CFUNC_MAGIC_DEF("encodeURIComponent", 1, js_global_encodeURI, 1),
    JS_CFUNC_DEF("escape", 1, js_global_escape),
    JS_CFUNC_DEF("unescape", 1, js_global_unescape),
    JS_PROP_DOUBLE_DEF("Infinity", 1.0 / 0.0, 0),
    JS_PROP_DOUBLE_DEF("NaN", NAN, 0), JS_PROP_UNDEFINED_DEF("undefined", 0),

    /* for the 'Date' implementation */
    JS_CFUNC_DEF("__date_clock", 0, js___date_clock),
    // JS_CFUNC_DEF("__date_now", 0, js___date_now ),
    // JS_CFUNC_DEF("__date_getTimezoneOffset", 1, js___date_getTimezoneOffset
    // ), JS_CFUNC_DEF("__date_create", 3, js___date_create ),
};

/* -- Eval ----------------------------------- */

void JS_AddIntrinsicEval(JSContext *ctx) {
  ctx->eval_internal = __JS_EvalInternal;
}

/* -- AggregateError ----------------------------------- */

/* used by C code. */
JSValue js_aggregate_error_constructor(JSContext *ctx, JSValueConst errors) {
  JSValue obj;

  obj = JS_NewObjectProtoClass(ctx, ctx->native_error_proto[JS_AGGREGATE_ERROR],
                               JS_CLASS_ERROR);
  if (JS_IsException(obj))
    return obj;
  JS_DefinePropertyValue(ctx, obj, JS_ATOM_errors, JS_DupValue(ctx, errors),
                         JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  return obj;
}

/* -- Basic objects ----------------------------------- */

/* Minimum amount of objects to be able to compile code and display
   error messages. No JSAtom should be allocated by this function. */
void JS_AddIntrinsicBasicObjects(JSContext *ctx) {
  JSValue proto;
  int i;

  ctx->class_proto[JS_CLASS_OBJECT] = JS_NewObjectProto(ctx, JS_NULL);
  ctx->function_proto =
      JS_NewCFunction3(ctx, js_function_proto, "", 0, JS_CFUNC_generic, 0,
                       ctx->class_proto[JS_CLASS_OBJECT]);
  ctx->class_proto[JS_CLASS_BYTECODE_FUNCTION] =
      JS_DupValue(ctx, ctx->function_proto);
  ctx->class_proto[JS_CLASS_ERROR] = JS_NewObject(ctx);
#if 0
    /* these are auto-initialized from js_error_proto_funcs,
       but delaying might be a problem */
    JS_DefinePropertyValue(ctx, ctx->class_proto[JS_CLASS_ERROR], JS_ATOM_name,
                           JS_AtomToString(ctx, JS_ATOM_Error),
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    JS_DefinePropertyValue(ctx, ctx->class_proto[JS_CLASS_ERROR], JS_ATOM_message,
                           JS_AtomToString(ctx, JS_ATOM_empty_string),
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
#endif
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_ERROR],
                             js_error_proto_funcs,
                             countof(js_error_proto_funcs));

  for (i = 0; i < JS_NATIVE_ERROR_COUNT; i++) {
    proto = JS_NewObjectProto(ctx, ctx->class_proto[JS_CLASS_ERROR]);
    JS_DefinePropertyValue(ctx, proto, JS_ATOM_name,
                           JS_NewAtomString(ctx, native_error_name[i]),
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    JS_DefinePropertyValue(ctx, proto, JS_ATOM_message,
                           JS_AtomToString(ctx, JS_ATOM_empty_string),
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    ctx->native_error_proto[i] = proto;
  }

  /* the array prototype is an array */
  ctx->class_proto[JS_CLASS_ARRAY] = JS_NewObjectProtoClass(
      ctx, ctx->class_proto[JS_CLASS_OBJECT], JS_CLASS_ARRAY);

  ctx->array_shape =
      js_new_shape2(ctx, get_proto_obj(ctx->class_proto[JS_CLASS_ARRAY]),
                    JS_PROP_INITIAL_HASH_SIZE, 1);
  add_shape_property(ctx, &ctx->array_shape, NULL, JS_ATOM_length,
                     JS_PROP_WRITABLE | JS_PROP_LENGTH);

  /* XXX: could test it on first context creation to ensure that no
     new atoms are created in JS_AddIntrinsicBasicObjects(). It is
     necessary to avoid useless renumbering of atoms after
     JS_EvalBinary() if it is done just after
     JS_AddIntrinsicBasicObjects(). */
  //    assert(ctx->rt->atom_count == JS_ATOM_END);
}

void JS_AddIntrinsicBaseObjects(JSContext *ctx) {
  int i;
  JSValueConst obj, number_obj;
  JSValue obj1;

  ctx->throw_type_error = JS_NewCFunction(ctx, js_throw_type_error, NULL, 0);

  /* add caller and arguments properties to throw a TypeError */
  obj1 = JS_NewCFunction(ctx, js_function_proto_caller, NULL, 0);
  JS_DefineProperty(ctx, ctx->function_proto, JS_ATOM_caller, JS_UNDEFINED,
                    obj1, ctx->throw_type_error,
                    JS_PROP_HAS_GET | JS_PROP_HAS_SET |
                        JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE);
  JS_DefineProperty(ctx, ctx->function_proto, JS_ATOM_arguments, JS_UNDEFINED,
                    obj1, ctx->throw_type_error,
                    JS_PROP_HAS_GET | JS_PROP_HAS_SET |
                        JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE);
  JS_FreeValue(ctx, obj1);
  JS_FreeValue(ctx, js_object_seal(ctx, JS_UNDEFINED, 1,
                                   (JSValueConst *)&ctx->throw_type_error, 1));

  ctx->global_obj = JS_NewObject(ctx);
  ctx->global_var_obj = JS_NewObjectProto(ctx, JS_NULL);

  /* Object */
  obj = JS_NewGlobalCConstructor(ctx, "Object", js_object_constructor, 1,
                                 ctx->class_proto[JS_CLASS_OBJECT]);
  JS_SetPropertyFunctionList(ctx, obj, js_object_funcs,
                             countof(js_object_funcs));
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_OBJECT],
                             js_object_proto_funcs,
                             countof(js_object_proto_funcs));

  /* Function */
  JS_SetPropertyFunctionList(ctx, ctx->function_proto, js_function_proto_funcs,
                             countof(js_function_proto_funcs));
  ctx->function_ctor =
      JS_NewCFunctionMagic(ctx, js_function_constructor, "Function", 1,
                           JS_CFUNC_constructor_or_func_magic, JS_FUNC_NORMAL);
  JS_NewGlobalCConstructor2(ctx, JS_DupValue(ctx, ctx->function_ctor),
                            "Function", ctx->function_proto);

  /* Error */
  obj1 = JS_NewCFunctionMagic(ctx, js_error_constructor, "Error", 1,
                              JS_CFUNC_constructor_or_func_magic, -1);
  JS_NewGlobalCConstructor2(ctx, obj1, "Error",
                            ctx->class_proto[JS_CLASS_ERROR]);

  for (i = 0; i < JS_NATIVE_ERROR_COUNT; i++) {
    JSValue func_obj;
    int n_args;
    n_args = 1 + (i == JS_AGGREGATE_ERROR);
    func_obj = JS_NewCFunction3(ctx, (JSCFunction *)js_error_constructor,
                                native_error_name[i], n_args,
                                JS_CFUNC_constructor_or_func_magic, i, obj1);
    JS_NewGlobalCConstructor2(ctx, func_obj, native_error_name[i],
                              ctx->native_error_proto[i]);
  }

  /* Iterator prototype */
  ctx->iterator_proto = JS_NewObject(ctx);
  JS_SetPropertyFunctionList(ctx, ctx->iterator_proto, js_iterator_proto_funcs,
                             countof(js_iterator_proto_funcs));

  /* Array */
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_ARRAY],
                             js_array_proto_funcs,
                             countof(js_array_proto_funcs));

  obj = JS_NewGlobalCConstructor(ctx, "Array", js_array_constructor, 1,
                                 ctx->class_proto[JS_CLASS_ARRAY]);
  ctx->array_ctor = JS_DupValue(ctx, obj);
  JS_SetPropertyFunctionList(ctx, obj, js_array_funcs, countof(js_array_funcs));

  /* XXX: create auto_initializer */
  {
    /* initialize Array.prototype[Symbol.unscopables] */
    // clang-format off
    char const unscopables[] = "copyWithin" "\0" "entries" "\0" "fill" "\0" "find" "\0"
        "findIndex" "\0" "flat" "\0" "flatMap" "\0" "includes" "\0" "keys" "\0" "values" "\0";
    // clang-format on 
    const char *p = unscopables;
    obj1 = JS_NewObjectProto(ctx, JS_NULL);
    for (p = unscopables; *p; p += strlen(p) + 1) {
      JS_DefinePropertyValueStr(ctx, obj1, p, JS_TRUE, JS_PROP_C_W_E);
    }
    JS_DefinePropertyValue(ctx, ctx->class_proto[JS_CLASS_ARRAY],
                           JS_ATOM_Symbol_unscopables, obj1,
                           JS_PROP_CONFIGURABLE);
  }

  /* needed to initialize arguments[Symbol.iterator] */
  ctx->array_proto_values =
      JS_GetProperty(ctx, ctx->class_proto[JS_CLASS_ARRAY], JS_ATOM_values);

  ctx->class_proto[JS_CLASS_ARRAY_ITERATOR] =
      JS_NewObjectProto(ctx, ctx->iterator_proto);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_ARRAY_ITERATOR],
                             js_array_iterator_proto_funcs,
                             countof(js_array_iterator_proto_funcs));

  /* parseFloat and parseInteger must be defined before Number
     because of the Number.parseFloat and Number.parseInteger
     aliases */
  JS_SetPropertyFunctionList(ctx, ctx->global_obj, js_global_funcs,
                             countof(js_global_funcs));

  /* Number */
  ctx->class_proto[JS_CLASS_NUMBER] = JS_NewObjectProtoClass(
      ctx, ctx->class_proto[JS_CLASS_OBJECT], JS_CLASS_NUMBER);
  JS_SetObjectData(ctx, ctx->class_proto[JS_CLASS_NUMBER], JS_NewInt32(ctx, 0));
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_NUMBER],
                             js_number_proto_funcs,
                             countof(js_number_proto_funcs));
  number_obj = JS_NewGlobalCConstructor(ctx, "Number", js_number_constructor, 1,
                                        ctx->class_proto[JS_CLASS_NUMBER]);
  JS_SetPropertyFunctionList(ctx, number_obj, js_number_funcs,
                             countof(js_number_funcs));

  /* Boolean */
  ctx->class_proto[JS_CLASS_BOOLEAN] = JS_NewObjectProtoClass(
      ctx, ctx->class_proto[JS_CLASS_OBJECT], JS_CLASS_BOOLEAN);
  JS_SetObjectData(ctx, ctx->class_proto[JS_CLASS_BOOLEAN],
                   JS_NewBool(ctx, FALSE));
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_BOOLEAN],
                             js_boolean_proto_funcs,
                             countof(js_boolean_proto_funcs));
  JS_NewGlobalCConstructor(ctx, "Boolean", js_boolean_constructor, 1,
                           ctx->class_proto[JS_CLASS_BOOLEAN]);

  /* String */
  ctx->class_proto[JS_CLASS_STRING] = JS_NewObjectProtoClass(
      ctx, ctx->class_proto[JS_CLASS_OBJECT], JS_CLASS_STRING);
  JS_SetObjectData(ctx, ctx->class_proto[JS_CLASS_STRING],
                   JS_AtomToString(ctx, JS_ATOM_empty_string));
  obj = JS_NewGlobalCConstructor(ctx, "String", js_string_constructor, 1,
                                 ctx->class_proto[JS_CLASS_STRING]);
  JS_SetPropertyFunctionList(ctx, obj, js_string_funcs,
                             countof(js_string_funcs));
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_STRING],
                             js_string_proto_funcs,
                             countof(js_string_proto_funcs));

  ctx->class_proto[JS_CLASS_STRING_ITERATOR] =
      JS_NewObjectProto(ctx, ctx->iterator_proto);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_STRING_ITERATOR],
                             js_string_iterator_proto_funcs,
                             countof(js_string_iterator_proto_funcs));

  /* Math: create as autoinit object */
  js_random_init(ctx);
  JS_SetPropertyFunctionList(ctx, ctx->global_obj, js_math_obj,
                             countof(js_math_obj));

  /* ES6 Reflect: create as autoinit object */
  JS_SetPropertyFunctionList(ctx, ctx->global_obj, js_reflect_obj,
                             countof(js_reflect_obj));

  /* ES6 Symbol */
  ctx->class_proto[JS_CLASS_SYMBOL] = JS_NewObject(ctx);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_SYMBOL],
                             js_symbol_proto_funcs,
                             countof(js_symbol_proto_funcs));
  obj = JS_NewGlobalCConstructor(ctx, "Symbol", js_symbol_constructor, 0,
                                 ctx->class_proto[JS_CLASS_SYMBOL]);
  JS_SetPropertyFunctionList(ctx, obj, js_symbol_funcs,
                             countof(js_symbol_funcs));
  for (i = JS_ATOM_Symbol_toPrimitive; i < JS_ATOM_END; i++) {
    char buf[ATOM_GET_STR_BUF_SIZE];
    const char *str, *p;
    str = JS_AtomGetStr(ctx, buf, sizeof(buf), i);
    /* skip "Symbol." */
    p = strchr(str, '.');
    if (p)
      str = p + 1;
    JS_DefinePropertyValueStr(ctx, obj, str, JS_AtomToValue(ctx, i), 0);
  }

  /* ES6 Generator */
  ctx->class_proto[JS_CLASS_GENERATOR] =
      JS_NewObjectProto(ctx, ctx->iterator_proto);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_GENERATOR],
                             js_generator_proto_funcs,
                             countof(js_generator_proto_funcs));

  ctx->class_proto[JS_CLASS_GENERATOR_FUNCTION] =
      JS_NewObjectProto(ctx, ctx->function_proto);
  obj1 = JS_NewCFunctionMagic(ctx, js_function_constructor, "GeneratorFunction",
                              1, JS_CFUNC_constructor_or_func_magic,
                              JS_FUNC_GENERATOR);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_GENERATOR_FUNCTION],
                             js_generator_function_proto_funcs,
                             countof(js_generator_function_proto_funcs));
  JS_SetConstructor2(ctx, ctx->class_proto[JS_CLASS_GENERATOR_FUNCTION],
                     ctx->class_proto[JS_CLASS_GENERATOR], JS_PROP_CONFIGURABLE,
                     JS_PROP_CONFIGURABLE);
  JS_SetConstructor2(ctx, obj1, ctx->class_proto[JS_CLASS_GENERATOR_FUNCTION],
                     0, JS_PROP_CONFIGURABLE);
  JS_FreeValue(ctx, obj1);

  /* global properties */
  ctx->eval_obj = JS_NewCFunction(ctx, js_global_eval, "eval", 1);
  JS_DefinePropertyValue(ctx, ctx->global_obj, JS_ATOM_eval,
                         JS_DupValue(ctx, ctx->eval_obj),
                         JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);

  JS_DefinePropertyValue(ctx, ctx->global_obj, JS_ATOM_globalThis,
                         JS_DupValue(ctx, ctx->global_obj),
                         JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE);
}

