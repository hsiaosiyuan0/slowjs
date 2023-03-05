#include "func.h"

#include "class.h"
#include "error.h"
#include "exec.h"
#include "intrins/intrins.h"
#include "iter.h"
#include "obj.h"
#include "ops.h"
#include "utils/dbuf.h"
#include "vm/vm.h"

/* -- Arguments ----------------------------------- */

const JSClassExoticMethods js_arguments_exotic_methods = {
    .define_own_property = js_arguments_define_own_property,
};

int js_arguments_define_own_property(JSContext *ctx, JSValueConst this_obj,
                                     JSAtom prop, JSValueConst val,
                                     JSValueConst getter, JSValueConst setter,
                                     int flags) {
  JSObject *p;
  uint32_t idx;
  p = JS_VALUE_GET_OBJ(this_obj);
  /* convert to normal array when redefining an existing numeric field */
  if (p->fast_array && JS_AtomIsArrayIndex(ctx, &idx, prop) &&
      idx < p->u.array.count) {
    if (convert_fast_array_to_array(ctx, p))
      return -1;
  }
  /* run the default define own property */
  return JS_DefineProperty(ctx, this_obj, prop, val, getter, setter,
                           flags | JS_PROP_NO_EXOTIC);
}

JSValue js_build_arguments(JSContext *ctx, int argc, JSValueConst *argv) {
  JSValue val, *tab;
  JSProperty *pr;
  JSObject *p;
  int i;

  val = JS_NewObjectProtoClass(ctx, ctx->class_proto[JS_CLASS_OBJECT],
                               JS_CLASS_ARGUMENTS);
  if (JS_IsException(val))
    return val;
  p = JS_VALUE_GET_OBJ(val);

  /* add the length field (cannot fail) */
  pr = add_property(ctx, p, JS_ATOM_length,
                    JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  pr->u.value = JS_NewInt32(ctx, argc);

  /* initialize the fast array part */
  tab = NULL;
  if (argc > 0) {
    tab = js_malloc(ctx, sizeof(tab[0]) * argc);
    if (!tab) {
      JS_FreeValue(ctx, val);
      return JS_EXCEPTION;
    }
    for (i = 0; i < argc; i++) {
      tab[i] = JS_DupValue(ctx, argv[i]);
    }
  }
  p->u.array.u.values = tab;
  p->u.array.count = argc;

  JS_DefinePropertyValue(ctx, val, JS_ATOM_Symbol_iterator,
                         JS_DupValue(ctx, ctx->array_proto_values),
                         JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE);
  /* add callee property to throw a TypeError in strict mode */
  JS_DefineProperty(ctx, val, JS_ATOM_callee, JS_UNDEFINED,
                    ctx->throw_type_error, ctx->throw_type_error,
                    JS_PROP_HAS_GET | JS_PROP_HAS_SET);
  return val;
}

/* legacy arguments object: add references to the function arguments */
JSValue js_build_mapped_arguments(JSContext *ctx, int argc, JSValueConst *argv,
                                  JSStackFrame *sf, int arg_count) {
  JSValue val;
  JSProperty *pr;
  JSObject *p;
  int i;

  val = JS_NewObjectProtoClass(ctx, ctx->class_proto[JS_CLASS_OBJECT],
                               JS_CLASS_MAPPED_ARGUMENTS);
  if (JS_IsException(val))
    return val;
  p = JS_VALUE_GET_OBJ(val);

  /* add the length field (cannot fail) */
  pr = add_property(ctx, p, JS_ATOM_length,
                    JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  pr->u.value = JS_NewInt32(ctx, argc);

  for (i = 0; i < arg_count; i++) {
    JSVarRef *var_ref;
    var_ref = get_var_ref(ctx, sf, i, TRUE);
    if (!var_ref)
      goto fail;
    pr = add_property(ctx, p, __JS_AtomFromUInt32(i),
                      JS_PROP_C_W_E | JS_PROP_VARREF);
    if (!pr) {
      free_var_ref(ctx->rt, var_ref);
      goto fail;
    }
    pr->u.var_ref = var_ref;
  }

  /* the arguments not mapped to the arguments of the function can
     be normal properties */
  for (i = arg_count; i < argc; i++) {
    if (JS_DefinePropertyValueUint32(ctx, val, i, JS_DupValue(ctx, argv[i]),
                                     JS_PROP_C_W_E) < 0)
      goto fail;
  }

  JS_DefinePropertyValue(ctx, val, JS_ATOM_Symbol_iterator,
                         JS_DupValue(ctx, ctx->array_proto_values),
                         JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE);
  /* callee returns this function in non strict mode */
  JS_DefinePropertyValue(
      ctx, val, JS_ATOM_callee,
      JS_DupValue(ctx, ctx->rt->current_stack_frame->cur_func),
      JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE);
  return val;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

JSValue js_build_rest(JSContext *ctx, int first, int argc, JSValueConst *argv) {
  JSValue val;
  int i, ret;

  val = JS_NewArray(ctx);
  if (JS_IsException(val))
    return val;
  for (i = first; i < argc; i++) {
    ret = JS_DefinePropertyValueUint32(
        ctx, val, i - first, JS_DupValue(ctx, argv[i]), JS_PROP_C_W_E);
    if (ret < 0) {
      JS_FreeValue(ctx, val);
      return JS_EXCEPTION;
    }
  }
  return val;
}

void free_arg_list(JSContext *ctx, JSValue *tab, uint32_t len) {
  uint32_t i;
  for (i = 0; i < len; i++) {
    JS_FreeValue(ctx, tab[i]);
  }
  js_free(ctx, tab);
}

/* XXX: should use ValueArray */
JSValue *build_arg_list(JSContext *ctx, uint32_t *plen,
                        JSValueConst array_arg) {
  uint32_t len, i;
  JSValue *tab, ret;
  JSObject *p;

  if (JS_VALUE_GET_TAG(array_arg) != JS_TAG_OBJECT) {
    JS_ThrowTypeError(ctx, "not a object");
    return NULL;
  }
  if (js_get_length32(ctx, &len, array_arg))
    return NULL;
  if (len > JS_MAX_LOCAL_VARS) {
    JS_ThrowInternalError(ctx, "too many arguments");
    return NULL;
  }
  /* avoid allocating 0 bytes */
  tab = js_mallocz(ctx, sizeof(tab[0]) * max_uint32(1, len));
  if (!tab)
    return NULL;
  p = JS_VALUE_GET_OBJ(array_arg);
  if ((p->class_id == JS_CLASS_ARRAY || p->class_id == JS_CLASS_ARGUMENTS) &&
      p->fast_array && len == p->u.array.count) {
    for (i = 0; i < len; i++) {
      tab[i] = JS_DupValue(ctx, p->u.array.u.values[i]);
    }
  } else {
    for (i = 0; i < len; i++) {
      ret = JS_GetPropertyUint32(ctx, array_arg, i);
      if (JS_IsException(ret)) {
        free_arg_list(ctx, tab, i);
        return NULL;
      }
      tab[i] = ret;
    }
  }
  *plen = len;
  return tab;
}

/* -- Call ----------------------------------- */

JSValue JS_Call(JSContext *ctx, JSValueConst func_obj, JSValueConst this_obj,
                int argc, JSValueConst *argv) {
  return JS_CallInternal(ctx, func_obj, this_obj, JS_UNDEFINED, argc,
                         (JSValue *)argv, JS_CALL_FLAG_COPY_ARGV);
}

JSValue JS_CallFree(JSContext *ctx, JSValue func_obj, JSValueConst this_obj,
                    int argc, JSValueConst *argv) {
  JSValue res = JS_CallInternal(ctx, func_obj, this_obj, JS_UNDEFINED, argc,
                                (JSValue *)argv, JS_CALL_FLAG_COPY_ARGV);
  JS_FreeValue(ctx, func_obj);
  return res;
}

/* warning: the refcount of the context is not incremented. Return
   NULL in case of exception (case of revoked proxy only) */
JSContext *JS_GetFunctionRealm(JSContext *ctx, JSValueConst func_obj) {
  JSObject *p;
  JSContext *realm;

  if (JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)
    return ctx;
  p = JS_VALUE_GET_OBJ(func_obj);
  switch (p->class_id) {
  case JS_CLASS_C_FUNCTION:
    realm = p->u.cfunc.realm;
    break;
  case JS_CLASS_BYTECODE_FUNCTION:
  case JS_CLASS_GENERATOR_FUNCTION:
  case JS_CLASS_ASYNC_FUNCTION:
  case JS_CLASS_ASYNC_GENERATOR_FUNCTION: {
    JSFunctionBytecode *b;
    b = p->u.func.function_bytecode;
    realm = b->realm;
  } break;
  case JS_CLASS_PROXY: {
    JSProxyData *s = p->u.opaque;
    if (!s)
      return ctx;
    if (s->is_revoked) {
      JS_ThrowTypeErrorRevokedProxy(ctx);
      return NULL;
    } else {
      realm = JS_GetFunctionRealm(ctx, s->target);
    }
  } break;
  case JS_CLASS_BOUND_FUNCTION: {
    JSBoundFunction *bf = p->u.bound_function;
    realm = JS_GetFunctionRealm(ctx, bf->func_obj);
  } break;
  default:
    realm = ctx;
    break;
  }
  return realm;
}

JSValue js_create_from_ctor(JSContext *ctx, JSValueConst ctor, int class_id) {
  JSValue proto, obj;
  JSContext *realm;

  if (JS_IsUndefined(ctor)) {
    proto = JS_DupValue(ctx, ctx->class_proto[class_id]);
  } else {
    proto = JS_GetProperty(ctx, ctor, JS_ATOM_prototype);
    if (JS_IsException(proto))
      return proto;
    if (!JS_IsObject(proto)) {
      JS_FreeValue(ctx, proto);
      realm = JS_GetFunctionRealm(ctx, ctor);
      if (!realm)
        return JS_EXCEPTION;
      proto = JS_DupValue(ctx, realm->class_proto[class_id]);
    }
  }
  obj = JS_NewObjectProtoClass(ctx, proto, class_id);
  JS_FreeValue(ctx, proto);
  return obj;
}

/* argv[] is modified if (flags & JS_CALL_FLAG_COPY_ARGV) = 0. */
JSValue JS_CallConstructorInternal(JSContext *ctx, JSValueConst func_obj,
                                   JSValueConst new_target, int argc,
                                   JSValue *argv, int flags) {
  JSObject *p;
  JSFunctionBytecode *b;

  if (js_poll_interrupts(ctx))
    return JS_EXCEPTION;
  flags |= JS_CALL_FLAG_CONSTRUCTOR;
  if (unlikely(JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT))
    goto not_a_function;
  p = JS_VALUE_GET_OBJ(func_obj);
  if (unlikely(!p->is_constructor))
    return JS_ThrowTypeError(ctx, "not a constructor");
  if (unlikely(p->class_id != JS_CLASS_BYTECODE_FUNCTION)) {
    JSClassCall *call_func;
    call_func = ctx->rt->class_array[p->class_id].call;
    if (!call_func) {
    not_a_function:
      return JS_ThrowTypeError(ctx, "not a function");
    }
    return call_func(ctx, func_obj, new_target, argc, (JSValueConst *)argv,
                     flags);
  }

  b = p->u.func.function_bytecode;
  if (b->is_derived_class_constructor) {
    return JS_CallInternal(ctx, func_obj, JS_UNDEFINED, new_target, argc, argv,
                           flags);
  } else {
    JSValue obj, ret;
    /* legacy constructor behavior */
    obj = js_create_from_ctor(ctx, new_target, JS_CLASS_OBJECT);
    if (JS_IsException(obj))
      return JS_EXCEPTION;
    ret = JS_CallInternal(ctx, func_obj, obj, new_target, argc, argv, flags);
    if (JS_VALUE_GET_TAG(ret) == JS_TAG_OBJECT || JS_IsException(ret)) {
      JS_FreeValue(ctx, obj);
      return ret;
    } else {
      JS_FreeValue(ctx, ret);
      return obj;
    }
  }
}

JSValue JS_CallConstructor2(JSContext *ctx, JSValueConst func_obj,
                            JSValueConst new_target, int argc,
                            JSValueConst *argv) {
  return JS_CallConstructorInternal(ctx, func_obj, new_target, argc,
                                    (JSValue *)argv, JS_CALL_FLAG_COPY_ARGV);
}

JSValue JS_CallConstructor(JSContext *ctx, JSValueConst func_obj, int argc,
                           JSValueConst *argv) {
  return JS_CallConstructorInternal(ctx, func_obj, func_obj, argc,
                                    (JSValue *)argv, JS_CALL_FLAG_COPY_ARGV);
}

JSValue JS_Invoke(JSContext *ctx, JSValueConst this_val, JSAtom atom, int argc,
                  JSValueConst *argv) {
  JSValue func_obj;
  func_obj = JS_GetProperty(ctx, this_val, atom);
  if (JS_IsException(func_obj))
    return func_obj;
  return JS_CallFree(ctx, func_obj, this_val, argc, argv);
}

JSValue JS_InvokeFree(JSContext *ctx, JSValue this_val, JSAtom atom, int argc,
                      JSValueConst *argv) {
  JSValue res = JS_Invoke(ctx, this_val, atom, argc, argv);
  JS_FreeValue(ctx, this_val);
  return res;
}

/* magic value: 0 = normal apply, 1 = apply for constructor, 2 =
   Reflect.apply */
JSValue js_function_apply(JSContext *ctx, JSValueConst this_val, int argc,
                          JSValueConst *argv, int magic) {
  JSValueConst this_arg, array_arg;
  uint32_t len;
  JSValue *tab, ret;

  if (check_function(ctx, this_val))
    return JS_EXCEPTION;
  this_arg = argv[0];
  array_arg = argv[1];
  if ((JS_VALUE_GET_TAG(array_arg) == JS_TAG_UNDEFINED ||
       JS_VALUE_GET_TAG(array_arg) == JS_TAG_NULL) &&
      magic != 2) {
    return JS_Call(ctx, this_val, this_arg, 0, NULL);
  }
  tab = build_arg_list(ctx, &len, array_arg);
  if (!tab)
    return JS_EXCEPTION;
  if (magic & 1) {
    ret =
        JS_CallConstructor2(ctx, this_val, this_arg, len, (JSValueConst *)tab);
  } else {
    ret = JS_Call(ctx, this_val, this_arg, len, (JSValueConst *)tab);
  }
  free_arg_list(ctx, tab, len);
  return ret;
}

JSValue js_call_bound_function(JSContext *ctx, JSValueConst func_obj,
                               JSValueConst this_obj, int argc,
                               JSValueConst *argv, int flags) {
  JSObject *p;
  JSBoundFunction *bf;
  JSValueConst *arg_buf, new_target;
  int arg_count, i;

  p = JS_VALUE_GET_OBJ(func_obj);
  bf = p->u.bound_function;
  arg_count = bf->argc + argc;
  if (js_check_stack_overflow(ctx->rt, sizeof(JSValue) * arg_count))
    return JS_ThrowStackOverflow(ctx);
  arg_buf = alloca(sizeof(JSValue) * arg_count);
  for (i = 0; i < bf->argc; i++) {
    arg_buf[i] = bf->argv[i];
  }
  for (i = 0; i < argc; i++) {
    arg_buf[bf->argc + i] = argv[i];
  }
  if (flags & JS_CALL_FLAG_CONSTRUCTOR) {
    new_target = this_obj;
    if (js_same_value(ctx, func_obj, new_target))
      new_target = bf->func_obj;
    return JS_CallConstructor2(ctx, bf->func_obj, new_target, arg_count,
                               arg_buf);
  } else {
    return JS_Call(ctx, bf->func_obj, bf->this_val, arg_count, arg_buf);
  }
}

JSValue js_call_c_function(JSContext *ctx, JSValueConst func_obj,
                           JSValueConst this_obj, int argc, JSValueConst *argv,
                           int flags) {
  JSRuntime *rt = ctx->rt;
  JSCFunctionType func;
  JSObject *p;
  JSStackFrame sf_s, *sf = &sf_s, *prev_sf;
  JSValue ret_val;
  JSValueConst *arg_buf;
  int arg_count, i;
  JSCFunctionEnum cproto;

  p = JS_VALUE_GET_OBJ(func_obj);
  cproto = p->u.cfunc.cproto;
  arg_count = p->u.cfunc.length;

  /* better to always check stack overflow */
  if (js_check_stack_overflow(rt, sizeof(arg_buf[0]) * arg_count))
    return JS_ThrowStackOverflow(ctx);

  prev_sf = rt->current_stack_frame;
  sf->prev_frame = prev_sf;
  rt->current_stack_frame = sf;
  ctx = p->u.cfunc.realm; /* change the current realm */

#ifdef CONFIG_BIGNUM
  /* we only propagate the bignum mode as some runtime functions
     test it */
  if (prev_sf)
    sf->js_mode = prev_sf->js_mode & JS_MODE_MATH;
  else
    sf->js_mode = 0;
#else
  sf->js_mode = 0;
#endif
  sf->cur_func = (JSValue)func_obj;
  sf->arg_count = argc;
  arg_buf = argv;

  if (unlikely(argc < arg_count)) {
    /* ensure that at least argc_count arguments are readable */
    arg_buf = alloca(sizeof(arg_buf[0]) * arg_count);
    for (i = 0; i < argc; i++)
      arg_buf[i] = argv[i];
    for (i = argc; i < arg_count; i++)
      arg_buf[i] = JS_UNDEFINED;
    sf->arg_count = arg_count;
  }
  sf->arg_buf = (JSValue *)arg_buf;

  func = p->u.cfunc.c_function;
  switch (cproto) {
  case JS_CFUNC_constructor:
  case JS_CFUNC_constructor_or_func:
    if (!(flags & JS_CALL_FLAG_CONSTRUCTOR)) {
      if (cproto == JS_CFUNC_constructor) {
      not_a_constructor:
        ret_val = JS_ThrowTypeError(ctx, "must be called with new");
        break;
      } else {
        this_obj = JS_UNDEFINED;
      }
    }
    /* here this_obj is new_target */
    /* fall thru */
  case JS_CFUNC_generic:
    ret_val = func.generic(ctx, this_obj, argc, arg_buf);
    break;
  case JS_CFUNC_constructor_magic:
  case JS_CFUNC_constructor_or_func_magic:
    if (!(flags & JS_CALL_FLAG_CONSTRUCTOR)) {
      if (cproto == JS_CFUNC_constructor_magic) {
        goto not_a_constructor;
      } else {
        this_obj = JS_UNDEFINED;
      }
    }
    /* fall thru */
  case JS_CFUNC_generic_magic:
    ret_val =
        func.generic_magic(ctx, this_obj, argc, arg_buf, p->u.cfunc.magic);
    break;
  case JS_CFUNC_getter:
    ret_val = func.getter(ctx, this_obj);
    break;
  case JS_CFUNC_setter:
    ret_val = func.setter(ctx, this_obj, arg_buf[0]);
    break;
  case JS_CFUNC_getter_magic:
    ret_val = func.getter_magic(ctx, this_obj, p->u.cfunc.magic);
    break;
  case JS_CFUNC_setter_magic:
    ret_val = func.setter_magic(ctx, this_obj, arg_buf[0], p->u.cfunc.magic);
    break;
  case JS_CFUNC_f_f: {
    double d1;

    if (unlikely(JS_ToFloat64(ctx, &d1, arg_buf[0]))) {
      ret_val = JS_EXCEPTION;
      break;
    }
    ret_val = JS_NewFloat64(ctx, func.f_f(d1));
  } break;
  case JS_CFUNC_f_f_f: {
    double d1, d2;

    if (unlikely(JS_ToFloat64(ctx, &d1, arg_buf[0]))) {
      ret_val = JS_EXCEPTION;
      break;
    }
    if (unlikely(JS_ToFloat64(ctx, &d2, arg_buf[1]))) {
      ret_val = JS_EXCEPTION;
      break;
    }
    ret_val = JS_NewFloat64(ctx, func.f_f_f(d1, d2));
  } break;
  case JS_CFUNC_iterator_next: {
    int done;
    ret_val = func.iterator_next(ctx, this_obj, argc, arg_buf, &done,
                                 p->u.cfunc.magic);
    if (!JS_IsException(ret_val) && done != 2) {
      ret_val = js_create_iterator_result(ctx, ret_val, done);
    }
  } break;
  default:
    abort();
  }

  rt->current_stack_frame = sf->prev_frame;
  return ret_val;
}

/* -- Function utils ----------------------------------- */

int find_line_num(JSContext *ctx, JSFunctionBytecode *b, uint32_t pc_value) {
  const uint8_t *p_end, *p;
  int new_line_num, line_num, pc, v, ret;
  unsigned int op;

  if (!b->has_debug || !b->debug.pc2line_buf) {
    /* function was stripped */
    return -1;
  }

  p = b->debug.pc2line_buf;
  p_end = p + b->debug.pc2line_len;
  pc = 0;
  line_num = b->debug.line_num;
  while (p < p_end) {
    op = *p++;
    if (op == 0) {
      uint32_t val;
      ret = get_leb128(&val, p, p_end);
      if (ret < 0)
        goto fail;
      pc += val;
      p += ret;
      ret = get_sleb128(&v, p, p_end);
      if (ret < 0) {
      fail:
        /* should never happen */
        return b->debug.line_num;
      }
      p += ret;
      new_line_num = line_num + v;
    } else {
      op -= PC2LINE_OP_FIRST;
      pc += (op / PC2LINE_RANGE);
      new_line_num = line_num + (op % PC2LINE_RANGE) + PC2LINE_BASE;
    }
    if (pc_value < pc)
      return line_num;
    line_num = new_line_num;
    // read `col_num`
    get_sleb128(&v, p, p_end);
  }
  return line_num;
}

/* in order to avoid executing arbitrary code during the stack trace
   generation, we only look at simple 'name' properties containing a
   string. */
const char *get_func_name(JSContext *ctx, JSValueConst func) {
  JSProperty *pr;
  JSShapeProperty *prs;
  JSValueConst val;

  if (JS_VALUE_GET_TAG(func) != JS_TAG_OBJECT)
    return NULL;
  prs = find_own_property(&pr, JS_VALUE_GET_OBJ(func), JS_ATOM_name);
  if (!prs)
    return NULL;
  if ((prs->flags & JS_PROP_TMASK) != JS_PROP_NORMAL)
    return NULL;
  val = pr->u.value;
  if (JS_VALUE_GET_TAG(val) != JS_TAG_STRING)
    return NULL;
  return JS_ToCString(ctx, val);
}

BOOL JS_IsFunction(JSContext *ctx, JSValueConst val) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
    return FALSE;
  p = JS_VALUE_GET_OBJ(val);
  switch (p->class_id) {
  case JS_CLASS_BYTECODE_FUNCTION:
    return TRUE;
  case JS_CLASS_PROXY:
    return p->u.proxy_data->is_func;
  default:
    return (ctx->rt->class_array[p->class_id].call != NULL);
  }
}

BOOL JS_IsCFunction(JSContext *ctx, JSValueConst val, JSCFunction *func,
                    int magic) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
    return FALSE;
  p = JS_VALUE_GET_OBJ(val);
  if (p->class_id == JS_CLASS_C_FUNCTION)
    return (p->u.cfunc.c_function.generic == func && p->u.cfunc.magic == magic);
  else
    return FALSE;
}

BOOL JS_IsConstructor(JSContext *ctx, JSValueConst val) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
    return FALSE;
  p = JS_VALUE_GET_OBJ(val);
  return p->is_constructor;
}

BOOL JS_SetConstructorBit(JSContext *ctx, JSValueConst func_obj, BOOL val) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)
    return FALSE;
  p = JS_VALUE_GET_OBJ(func_obj);
  p->is_constructor = val;
  return TRUE;
}

JSValue js_get_function_name(JSContext *ctx, JSAtom name) {
  JSValue name_str;

  name_str = JS_AtomToString(ctx, name);
  if (JS_AtomSymbolHasDescription(ctx, name)) {
    name_str = JS_ConcatString3(ctx, "[", name_str, "]");
  }
  return name_str;
}

void js_method_set_home_object(JSContext *ctx, JSValueConst func_obj,
                               JSValueConst home_obj) {
  JSObject *p, *p1;
  JSFunctionBytecode *b;

  if (JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)
    return;
  p = JS_VALUE_GET_OBJ(func_obj);
  if (!js_class_has_bytecode(p->class_id))
    return;
  b = p->u.func.function_bytecode;
  if (b->need_home_object) {
    p1 = p->u.func.home_object;
    if (p1) {
      JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p1));
    }
    if (JS_VALUE_GET_TAG(home_obj) == JS_TAG_OBJECT)
      p1 = JS_VALUE_GET_OBJ(JS_DupValue(ctx, home_obj));
    else
      p1 = NULL;
    p->u.func.home_object = p1;
  }
}

void js_function_set_properties(JSContext *ctx, JSValueConst func_obj,
                                JSAtom name, int len) {
  /* ES6 feature non compatible with ES5.1: length is configurable */
  JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_length, JS_NewInt32(ctx, len),
                         JS_PROP_CONFIGURABLE);
  JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_name,
                         JS_AtomToString(ctx, name), JS_PROP_CONFIGURABLE);
}

/* return NULL without exception if not a function or no bytecode */
JSFunctionBytecode *JS_GetFunctionBytecode(JSValueConst val) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
    return NULL;
  p = JS_VALUE_GET_OBJ(val);
  if (!js_class_has_bytecode(p->class_id))
    return NULL;
  return p->u.func.function_bytecode;
}

/* Modify the name of a method according to the atom and
   'flags'. 'flags' is a bitmask of JS_PROP_HAS_GET and
   JS_PROP_HAS_SET. Also set the home object of the method.
   Return < 0 if exception. */
int js_method_set_properties(JSContext *ctx, JSValueConst func_obj, JSAtom name,
                             int flags, JSValueConst home_obj) {
  JSValue name_str;

  name_str = js_get_function_name(ctx, name);
  if (flags & JS_PROP_HAS_GET) {
    name_str = JS_ConcatString3(ctx, "get ", name_str, "");
  } else if (flags & JS_PROP_HAS_SET) {
    name_str = JS_ConcatString3(ctx, "set ", name_str, "");
  }
  if (JS_IsException(name_str))
    return -1;
  if (JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_name, name_str,
                             JS_PROP_CONFIGURABLE) < 0)
    return -1;
  js_method_set_home_object(ctx, func_obj, home_obj);
  return 0;
}

/* -- Closure ----------------------------------- */

JSVarRef *get_var_ref(JSContext *ctx, JSStackFrame *sf, int var_idx,
                      BOOL is_arg) {
  JSVarRef *var_ref;
  struct list_head *el;

  list_for_each(el, &sf->var_ref_list) {
    var_ref = list_entry(el, JSVarRef, header.link);
    if (var_ref->var_idx == var_idx && var_ref->is_arg == is_arg) {
      var_ref->header.ref_count++;
      return var_ref;
    }
  }
  /* create a new one */
  var_ref = js_malloc(ctx, sizeof(JSVarRef));
  if (!var_ref)
    return NULL;
  var_ref->header.ref_count = 1;
  var_ref->is_detached = FALSE;
  var_ref->is_arg = is_arg;
  var_ref->var_idx = var_idx;
  list_add_tail(&var_ref->header.link, &sf->var_ref_list);
  if (is_arg)
    var_ref->pvalue = &sf->arg_buf[var_idx];
  else
    var_ref->pvalue = &sf->var_buf[var_idx];
  var_ref->value = JS_UNDEFINED;
  return var_ref;
}

JSValue js_closure2(JSContext *ctx, JSValue func_obj, JSFunctionBytecode *b,
                    JSVarRef **cur_var_refs, JSStackFrame *sf) {
  JSObject *p;
  JSVarRef **var_refs;
  int i;

  p = JS_VALUE_GET_OBJ(func_obj);
  p->u.func.function_bytecode = b;
  p->u.func.home_object = NULL;
  p->u.func.var_refs = NULL;
  if (b->closure_var_count) {
    var_refs = js_mallocz(ctx, sizeof(var_refs[0]) * b->closure_var_count);
    if (!var_refs)
      goto fail;
    p->u.func.var_refs = var_refs;
    for (i = 0; i < b->closure_var_count; i++) {
      JSClosureVar *cv = &b->closure_var[i];
      JSVarRef *var_ref;
      if (cv->is_local) {
        /* reuse the existing variable reference if it already exists */
        var_ref = get_var_ref(ctx, sf, cv->var_idx, cv->is_arg);
        if (!var_ref)
          goto fail;
      } else {
        var_ref = cur_var_refs[cv->var_idx];
        var_ref->header.ref_count++;
      }
      var_refs[i] = var_ref;
    }
  }
  return func_obj;
fail:
  /* bfunc is freed when func_obj is freed */
  JS_FreeValue(ctx, func_obj);
  return JS_EXCEPTION;
}

const uint16_t func_kind_to_class_id[] = {
    [JS_FUNC_NORMAL] = JS_CLASS_BYTECODE_FUNCTION,
    [JS_FUNC_GENERATOR] = JS_CLASS_GENERATOR_FUNCTION,
    [JS_FUNC_ASYNC] = JS_CLASS_ASYNC_FUNCTION,
    [JS_FUNC_ASYNC_GENERATOR] = JS_CLASS_ASYNC_GENERATOR_FUNCTION,
};

JSValue js_closure(JSContext *ctx, JSValue bfunc, JSVarRef **cur_var_refs,
                   JSStackFrame *sf) {
  JSFunctionBytecode *b;
  JSValue func_obj;
  JSAtom name_atom;

  b = JS_VALUE_GET_PTR(bfunc);
  func_obj = JS_NewObjectClass(ctx, func_kind_to_class_id[b->func_kind]);
  if (JS_IsException(func_obj)) {
    JS_FreeValue(ctx, bfunc);
    return JS_EXCEPTION;
  }
  func_obj = js_closure2(ctx, func_obj, b, cur_var_refs, sf);
  if (JS_IsException(func_obj)) {
    /* bfunc has been freed */
    goto fail;
  }
  name_atom = b->func_name;
  if (name_atom == JS_ATOM_NULL)
    name_atom = JS_ATOM_empty_string;
  js_function_set_properties(ctx, func_obj, name_atom, b->defined_arg_count);

  if (b->func_kind & JS_FUNC_GENERATOR) {
    JSValue proto;
    int proto_class_id;
    /* generators have a prototype field which is used as
       prototype for the generator object */
    if (b->func_kind == JS_FUNC_ASYNC_GENERATOR)
      proto_class_id = JS_CLASS_ASYNC_GENERATOR;
    else
      proto_class_id = JS_CLASS_GENERATOR;
    proto = JS_NewObjectProto(ctx, ctx->class_proto[proto_class_id]);
    if (JS_IsException(proto))
      goto fail;
    JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_prototype, proto,
                           JS_PROP_WRITABLE);
  } else if (b->has_prototype) {
    /* add the 'prototype' property: delay instantiation to avoid
       creating cycles for every javascript function. The prototype
       object is created on the fly when first accessed */
    JS_SetConstructorBit(ctx, func_obj, TRUE);
    JS_DefineAutoInitProperty(ctx, func_obj, JS_ATOM_prototype,
                              JS_AUTOINIT_ID_PROTOTYPE, NULL, JS_PROP_WRITABLE);
  }
  return func_obj;
fail:
  /* bfunc is freed when func_obj is freed */
  JS_FreeValue(ctx, func_obj);
  return JS_EXCEPTION;
}

void close_var_refs(JSRuntime *rt, JSStackFrame *sf) {
  struct list_head *el, *el1;
  JSVarRef *var_ref;
  int var_idx;

  list_for_each_safe(el, el1, &sf->var_ref_list) {
    var_ref = list_entry(el, JSVarRef, header.link);
    var_idx = var_ref->var_idx;
    if (var_ref->is_arg)
      var_ref->value = JS_DupValueRT(rt, sf->arg_buf[var_idx]);
    else
      var_ref->value = JS_DupValueRT(rt, sf->var_buf[var_idx]);
    var_ref->pvalue = &var_ref->value;
    /* the reference is no longer to a local variable */
    var_ref->is_detached = TRUE;
    add_gc_object(rt, &var_ref->header, JS_GC_OBJ_TYPE_VAR_REF);
  }
}

void close_lexical_var(JSContext *ctx, JSStackFrame *sf, int idx, int is_arg) {
  struct list_head *el, *el1;
  JSVarRef *var_ref;
  int var_idx = idx;

  list_for_each_safe(el, el1, &sf->var_ref_list) {
    var_ref = list_entry(el, JSVarRef, header.link);
    if (var_idx == var_ref->var_idx && var_ref->is_arg == is_arg) {
      var_ref->value = JS_DupValue(ctx, sf->var_buf[var_idx]);
      var_ref->pvalue = &var_ref->value;
      list_del(&var_ref->header.link);
      /* the reference is no longer to a local variable */
      var_ref->is_detached = TRUE;
      add_gc_object(ctx->rt, &var_ref->header, JS_GC_OBJ_TYPE_VAR_REF);
    }
  }
}

/* -- AsyncFunction ----------------------------------- */

/* JSAsyncFunctionState (used by generator and async functions) */
__exception int async_func_init(JSContext *ctx, JSAsyncFunctionState *s,
                                JSValueConst func_obj, JSValueConst this_obj,
                                int argc, JSValueConst *argv) {
  JSObject *p;
  JSFunctionBytecode *b;
  JSStackFrame *sf;
  int local_count, i, arg_buf_len, n;

  sf = &s->frame;
  init_list_head(&sf->var_ref_list);
  p = JS_VALUE_GET_OBJ(func_obj);
  b = p->u.func.function_bytecode;
  sf->js_mode = b->js_mode;
  sf->cur_pc = b->byte_code_buf;
  arg_buf_len = max_int(b->arg_count, argc);
  local_count = arg_buf_len + b->var_count + b->stack_size;
  sf->arg_buf = js_malloc(ctx, sizeof(JSValue) * max_int(local_count, 1));
  if (!sf->arg_buf)
    return -1;
  sf->cur_func = JS_DupValue(ctx, func_obj);
  s->this_val = JS_DupValue(ctx, this_obj);
  s->argc = argc;
  sf->arg_count = arg_buf_len;
  sf->var_buf = sf->arg_buf + arg_buf_len;
  sf->cur_sp = sf->var_buf + b->var_count;
  for (i = 0; i < argc; i++)
    sf->arg_buf[i] = JS_DupValue(ctx, argv[i]);
  n = arg_buf_len + b->var_count;
  for (i = argc; i < n; i++)
    sf->arg_buf[i] = JS_UNDEFINED;
  return 0;
}

void async_func_mark(JSRuntime *rt, JSAsyncFunctionState *s,
                     JS_MarkFunc *mark_func) {
  JSStackFrame *sf;
  JSValue *sp;

  sf = &s->frame;
  JS_MarkValue(rt, sf->cur_func, mark_func);
  JS_MarkValue(rt, s->this_val, mark_func);
  if (sf->cur_sp) {
    /* if the function is running, cur_sp is not known so we
       cannot mark the stack. Marking the variables is not needed
       because a running function cannot be part of a removable
       cycle */
    for (sp = sf->arg_buf; sp < sf->cur_sp; sp++)
      JS_MarkValue(rt, *sp, mark_func);
  }
}

void async_func_free(JSRuntime *rt, JSAsyncFunctionState *s) {
  JSStackFrame *sf;
  JSValue *sp;

  sf = &s->frame;

  /* close the closure variables. */
  close_var_refs(rt, sf);

  if (sf->arg_buf) {
    /* cannot free the function if it is running */
    assert(sf->cur_sp != NULL);
    for (sp = sf->arg_buf; sp < sf->cur_sp; sp++) {
      JS_FreeValueRT(rt, *sp);
    }
    js_free_rt(rt, sf->arg_buf);
  }
  JS_FreeValueRT(rt, sf->cur_func);
  JS_FreeValueRT(rt, s->this_val);
}

JSValue async_func_resume(JSContext *ctx, JSAsyncFunctionState *s) {
  JSValue func_obj;

  if (js_check_stack_overflow(ctx->rt, 0))
    return JS_ThrowStackOverflow(ctx);

  /* the tag does not matter provided it is not an object */
  func_obj = JS_MKPTR(JS_TAG_INT, s);
  return JS_CallInternal(ctx, func_obj, s->this_val, JS_UNDEFINED, s->argc,
                         s->frame.arg_buf, JS_CALL_FLAG_GENERATOR);
}

void js_async_function_terminate(JSRuntime *rt, JSAsyncFunctionData *s) {
  if (s->is_active) {
    async_func_free(rt, &s->func_state);
    s->is_active = FALSE;
  }
}

void js_async_function_free0(JSRuntime *rt, JSAsyncFunctionData *s) {
  js_async_function_terminate(rt, s);
  JS_FreeValueRT(rt, s->resolving_funcs[0]);
  JS_FreeValueRT(rt, s->resolving_funcs[1]);
  remove_gc_object(&s->header);
  js_free_rt(rt, s);
}

void js_async_function_free(JSRuntime *rt, JSAsyncFunctionData *s) {
  if (--s->header.ref_count == 0) {
    js_async_function_free0(rt, s);
  }
}

void js_async_function_resolve_finalizer(JSRuntime *rt, JSValue val) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JSAsyncFunctionData *s = p->u.async_function_data;
  if (s) {
    js_async_function_free(rt, s);
  }
}

void js_async_function_resolve_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JSAsyncFunctionData *s = p->u.async_function_data;
  if (s) {
    mark_func(rt, &s->header);
  }
}

int js_async_function_resolve_create(JSContext *ctx, JSAsyncFunctionData *s,
                                     JSValue *resolving_funcs) {
  int i;
  JSObject *p;

  for (i = 0; i < 2; i++) {
    resolving_funcs[i] = JS_NewObjectProtoClass(
        ctx, ctx->function_proto, JS_CLASS_ASYNC_FUNCTION_RESOLVE + i);
    if (JS_IsException(resolving_funcs[i])) {
      if (i == 1)
        JS_FreeValue(ctx, resolving_funcs[0]);
      return -1;
    }
    p = JS_VALUE_GET_OBJ(resolving_funcs[i]);
    s->header.ref_count++;
    p->u.async_function_data = s;
  }
  return 0;
}

void js_async_function_resume(JSContext *ctx, JSAsyncFunctionData *s) {
  JSValue func_ret, ret2;

  func_ret = async_func_resume(ctx, &s->func_state);
  if (JS_IsException(func_ret)) {
    JSValue error;
  fail:
    error = JS_GetException(ctx);
    ret2 = JS_Call(ctx, s->resolving_funcs[1], JS_UNDEFINED, 1,
                   (JSValueConst *)&error);
    JS_FreeValue(ctx, error);
    js_async_function_terminate(ctx->rt, s);
    JS_FreeValue(ctx, ret2); /* XXX: what to do if exception ? */
  } else {
    JSValue value;
    value = s->func_state.frame.cur_sp[-1];
    s->func_state.frame.cur_sp[-1] = JS_UNDEFINED;
    if (JS_IsUndefined(func_ret)) {
      /* function returned */
      ret2 = JS_Call(ctx, s->resolving_funcs[0], JS_UNDEFINED, 1,
                     (JSValueConst *)&value);
      JS_FreeValue(ctx, ret2); /* XXX: what to do if exception ? */
      JS_FreeValue(ctx, value);
      js_async_function_terminate(ctx->rt, s);
    } else {
      JSValue promise, resolving_funcs[2], resolving_funcs1[2];
      int i, res;

      /* await */
      JS_FreeValue(ctx, func_ret); /* not used */
      promise = js_promise_resolve(ctx, ctx->promise_ctor, 1,
                                   (JSValueConst *)&value, 0);
      JS_FreeValue(ctx, value);
      if (JS_IsException(promise))
        goto fail;
      if (js_async_function_resolve_create(ctx, s, resolving_funcs)) {
        JS_FreeValue(ctx, promise);
        goto fail;
      }

      /* Note: no need to create 'thrownawayCapability' as in
         the spec */
      for (i = 0; i < 2; i++)
        resolving_funcs1[i] = JS_UNDEFINED;
      res = perform_promise_then(ctx, promise, (JSValueConst *)resolving_funcs,
                                 (JSValueConst *)resolving_funcs1);
      JS_FreeValue(ctx, promise);
      for (i = 0; i < 2; i++)
        JS_FreeValue(ctx, resolving_funcs[i]);
      if (res)
        goto fail;
    }
  }
}

JSValue js_async_function_resolve_call(JSContext *ctx, JSValueConst func_obj,
                                       JSValueConst this_obj, int argc,
                                       JSValueConst *argv, int flags) {
  JSObject *p = JS_VALUE_GET_OBJ(func_obj);
  JSAsyncFunctionData *s = p->u.async_function_data;
  BOOL is_reject = p->class_id - JS_CLASS_ASYNC_FUNCTION_RESOLVE;
  JSValueConst arg;

  if (argc > 0)
    arg = argv[0];
  else
    arg = JS_UNDEFINED;
  s->func_state.throw_flag = is_reject;
  if (is_reject) {
    JS_Throw(ctx, JS_DupValue(ctx, arg));
  } else {
    /* return value of await */
    s->func_state.frame.cur_sp[-1] = JS_DupValue(ctx, arg);
  }
  js_async_function_resume(ctx, s);
  return JS_UNDEFINED;
}

JSValue js_async_function_call(JSContext *ctx, JSValueConst func_obj,
                               JSValueConst this_obj, int argc,
                               JSValueConst *argv, int flags) {
  JSValue promise;
  JSAsyncFunctionData *s;

  s = js_mallocz(ctx, sizeof(*s));
  if (!s)
    return JS_EXCEPTION;
  s->header.ref_count = 1;
  add_gc_object(ctx->rt, &s->header, JS_GC_OBJ_TYPE_ASYNC_FUNCTION);
  s->is_active = FALSE;
  s->resolving_funcs[0] = JS_UNDEFINED;
  s->resolving_funcs[1] = JS_UNDEFINED;

  promise = JS_NewPromiseCapability(ctx, s->resolving_funcs);
  if (JS_IsException(promise))
    goto fail;

  if (async_func_init(ctx, &s->func_state, func_obj, this_obj, argc, argv)) {
  fail:
    JS_FreeValue(ctx, promise);
    js_async_function_free(ctx->rt, s);
    return JS_EXCEPTION;
  }
  s->is_active = TRUE;

  js_async_function_resume(ctx, s);

  js_async_function_free(ctx->rt, s);

  return promise;
}