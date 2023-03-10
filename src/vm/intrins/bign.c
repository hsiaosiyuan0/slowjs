#include "intrins.h"

#include "vm/conv.h"
#include "vm/error.h"
#include "vm/num.h"
#include "vm/ops.h"

#ifdef CONFIG_BIGNUM

/* -- Operators ----------------------------------- */

void js_operator_set_finalizer(JSRuntime *rt, JSValue val) {
  JSOperatorSetData *opset = JS_GetOpaque(val, JS_CLASS_OPERATOR_SET);
  int i, j;
  JSBinaryOperatorDefEntry *ent;

  if (opset) {
    for (i = 0; i < JS_OVOP_COUNT; i++) {
      if (opset->self_ops[i])
        JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, opset->self_ops[i]));
    }
    for (j = 0; j < opset->left.count; j++) {
      ent = &opset->left.tab[j];
      for (i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
        if (ent->ops[i])
          JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, ent->ops[i]));
      }
    }
    js_free_rt(rt, opset->left.tab);
    for (j = 0; j < opset->right.count; j++) {
      ent = &opset->right.tab[j];
      for (i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
        if (ent->ops[i])
          JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, ent->ops[i]));
      }
    }
    js_free_rt(rt, opset->right.tab);
    js_free_rt(rt, opset);
  }
}

void js_operator_set_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func) {
  JSOperatorSetData *opset = JS_GetOpaque(val, JS_CLASS_OPERATOR_SET);
  int i, j;
  JSBinaryOperatorDefEntry *ent;

  if (opset) {
    for (i = 0; i < JS_OVOP_COUNT; i++) {
      if (opset->self_ops[i])
        JS_MarkValue(rt, JS_MKPTR(JS_TAG_OBJECT, opset->self_ops[i]),
                     mark_func);
    }
    for (j = 0; j < opset->left.count; j++) {
      ent = &opset->left.tab[j];
      for (i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
        if (ent->ops[i])
          JS_MarkValue(rt, JS_MKPTR(JS_TAG_OBJECT, ent->ops[i]), mark_func);
      }
    }
    for (j = 0; j < opset->right.count; j++) {
      ent = &opset->right.tab[j];
      for (i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
        if (ent->ops[i])
          JS_MarkValue(rt, JS_MKPTR(JS_TAG_OBJECT, ent->ops[i]), mark_func);
      }
    }
  }
}

// TODO:
void js_operator_set_walk(JSRuntime *rt, JSValueConst val,
                          JS_WalkFunc *walk_func, void *uctx) {}

/* create an OperatorSet object */
static JSValue js_operators_create_internal(JSContext *ctx, int argc,
                                            JSValueConst *argv,
                                            BOOL is_primitive) {
  JSValue opset_obj, prop, obj;
  JSOperatorSetData *opset, *opset1;
  JSBinaryOperatorDef *def;
  JSValueConst arg;
  int i, j;
  JSBinaryOperatorDefEntry *new_tab;
  JSBinaryOperatorDefEntry *ent;
  uint32_t op_count;

  if (ctx->rt->operator_count == UINT32_MAX) {
    return JS_ThrowTypeError(ctx, "too many operators");
  }
  opset_obj = JS_NewObjectProtoClass(ctx, JS_NULL, JS_CLASS_OPERATOR_SET);
  if (JS_IsException(opset_obj))
    goto fail;
  opset = js_mallocz(ctx, sizeof(*opset));
  if (!opset)
    goto fail;
  JS_SetOpaque(opset_obj, opset);
  if (argc >= 1) {
    arg = argv[0];
    /* self operators */
    for (i = 0; i < JS_OVOP_COUNT; i++) {
      prop = JS_GetPropertyStr(ctx, arg, js_overloadable_operator_names[i]);
      if (JS_IsException(prop))
        goto fail;
      if (!JS_IsUndefined(prop)) {
        if (check_function(ctx, prop)) {
          JS_FreeValue(ctx, prop);
          goto fail;
        }
        opset->self_ops[i] = JS_VALUE_GET_OBJ(prop);
      }
    }
  }
  /* left & right operators */
  for (j = 1; j < argc; j++) {
    arg = argv[j];
    prop = JS_GetPropertyStr(ctx, arg, "left");
    if (JS_IsException(prop))
      goto fail;
    def = &opset->right;
    if (JS_IsUndefined(prop)) {
      prop = JS_GetPropertyStr(ctx, arg, "right");
      if (JS_IsException(prop))
        goto fail;
      if (JS_IsUndefined(prop)) {
        JS_ThrowTypeError(ctx, "left or right property must be present");
        goto fail;
      }
      def = &opset->left;
    }
    /* get the operator set */
    obj = JS_GetProperty(ctx, prop, JS_ATOM_prototype);
    JS_FreeValue(ctx, prop);
    if (JS_IsException(obj))
      goto fail;
    prop = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_operatorSet);
    JS_FreeValue(ctx, obj);
    if (JS_IsException(prop))
      goto fail;
    opset1 = JS_GetOpaque2(ctx, prop, JS_CLASS_OPERATOR_SET);
    if (!opset1) {
      JS_FreeValue(ctx, prop);
      goto fail;
    }
    op_count = opset1->operator_counter;
    JS_FreeValue(ctx, prop);

    /* we assume there are few entries */
    new_tab = js_realloc(ctx, def->tab, (def->count + 1) * sizeof(def->tab[0]));
    if (!new_tab)
      goto fail;
    def->tab = new_tab;
    def->count++;
    ent = def->tab + def->count - 1;
    memset(ent, 0, sizeof(def->tab[0]));
    ent->operator_index = op_count;

    for (i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
      prop = JS_GetPropertyStr(ctx, arg, js_overloadable_operator_names[i]);
      if (JS_IsException(prop))
        goto fail;
      if (!JS_IsUndefined(prop)) {
        if (check_function(ctx, prop)) {
          JS_FreeValue(ctx, prop);
          goto fail;
        }
        ent->ops[i] = JS_VALUE_GET_OBJ(prop);
      }
    }
  }
  opset->is_primitive = is_primitive;
  opset->operator_counter = ctx->rt->operator_count++;
  return opset_obj;
fail:
  JS_FreeValue(ctx, opset_obj);
  return JS_EXCEPTION;
}

static JSValue js_operators_create(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv) {
  return js_operators_create_internal(ctx, argc, argv, FALSE);
}

static JSValue js_operators_updateBigIntOperators(JSContext *ctx,
                                                  JSValueConst this_val,
                                                  int argc,
                                                  JSValueConst *argv) {
  JSValue opset_obj, prop;
  JSOperatorSetData *opset;
  const JSOverloadableOperatorEnum ops[2] = {JS_OVOP_DIV, JS_OVOP_POW};
  JSOverloadableOperatorEnum op;
  int i;

  opset_obj = JS_GetProperty(ctx, ctx->class_proto[JS_CLASS_BIG_INT],
                             JS_ATOM_Symbol_operatorSet);
  if (JS_IsException(opset_obj))
    goto fail;
  opset = JS_GetOpaque2(ctx, opset_obj, JS_CLASS_OPERATOR_SET);
  if (!opset)
    goto fail;
  for (i = 0; i < countof(ops); i++) {
    op = ops[i];
    prop = JS_GetPropertyStr(ctx, argv[0], js_overloadable_operator_names[op]);
    if (JS_IsException(prop))
      goto fail;
    if (!JS_IsUndefined(prop)) {
      if (!JS_IsNull(prop) && check_function(ctx, prop)) {
        JS_FreeValue(ctx, prop);
        goto fail;
      }
      if (opset->self_ops[op])
        JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, opset->self_ops[op]));
      if (JS_IsNull(prop)) {
        opset->self_ops[op] = NULL;
      } else {
        opset->self_ops[op] = JS_VALUE_GET_PTR(prop);
      }
    }
  }
  JS_FreeValue(ctx, opset_obj);
  return JS_UNDEFINED;
fail:
  JS_FreeValue(ctx, opset_obj);
  return JS_EXCEPTION;
}

static int js_operators_set_default(JSContext *ctx, JSValueConst obj) {
  JSValue opset_obj;

  if (!JS_IsObject(obj)) /* in case the prototype is not defined */
    return 0;
  opset_obj = js_operators_create_internal(ctx, 0, NULL, TRUE);
  if (JS_IsException(opset_obj))
    return -1;
  /* cannot be modified by the user */
  JS_DefinePropertyValue(ctx, obj, JS_ATOM_Symbol_operatorSet, opset_obj, 0);
  return 0;
}

static JSValue js_dummy_operators_ctor(JSContext *ctx, JSValueConst new_target,
                                       int argc, JSValueConst *argv) {
  return js_create_from_ctor(ctx, new_target, JS_CLASS_OBJECT);
}

static JSValue js_global_operators(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv) {
  JSValue func_obj, proto, opset_obj;

  func_obj = JS_UNDEFINED;
  proto = JS_NewObject(ctx);
  if (JS_IsException(proto))
    return JS_EXCEPTION;
  opset_obj = js_operators_create_internal(ctx, argc, argv, FALSE);
  if (JS_IsException(opset_obj))
    goto fail;
  JS_DefinePropertyValue(ctx, proto, JS_ATOM_Symbol_operatorSet, opset_obj,
                         JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  func_obj = JS_NewCFunction2(ctx, js_dummy_operators_ctor, "Operators", 0,
                              JS_CFUNC_constructor, 0);
  if (JS_IsException(func_obj))
    goto fail;
  JS_SetConstructor2(ctx, func_obj, proto, 0,
                     JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  JS_FreeValue(ctx, proto);
  return func_obj;
fail:
  JS_FreeValue(ctx, proto);
  JS_FreeValue(ctx, func_obj);
  return JS_EXCEPTION;
}

static const JSCFunctionListEntry js_operators_funcs[] = {
    JS_CFUNC_DEF("create", 1, js_operators_create),
    JS_CFUNC_DEF("updateBigIntOperators", 2,
                 js_operators_updateBigIntOperators),
};

/* must be called after all overloadable base types are initialized */
void JS_AddIntrinsicOperators(JSContext *ctx) {
  JSValue obj;

  ctx->allow_operator_overloading = TRUE;
  obj = JS_NewCFunction(ctx, js_global_operators, "Operators", 1);
  JS_SetPropertyFunctionList(ctx, obj, js_operators_funcs,
                             countof(js_operators_funcs));
  JS_DefinePropertyValue(ctx, ctx->global_obj, JS_ATOM_Operators, obj,
                         JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  /* add default operatorSets */
  js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_BOOLEAN]);
  js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_NUMBER]);
  js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_STRING]);
  js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_BIG_INT]);
  js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_BIG_FLOAT]);
  js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_BIG_DECIMAL]);
}

/* -- BigInt ----------------------------------- */

static JSValue JS_ToBigIntCtorFree(JSContext *ctx, JSValue val) {
  uint32_t tag;

redo:
  tag = JS_VALUE_GET_NORM_TAG(val);
  switch (tag) {
  case JS_TAG_INT:
  case JS_TAG_BOOL:
    val = JS_NewBigInt64(ctx, JS_VALUE_GET_INT(val));
    break;
  case JS_TAG_BIG_INT:
    break;
  case JS_TAG_FLOAT64:
  case JS_TAG_BIG_FLOAT: {
    bf_t *a, a_s;

    a = JS_ToBigFloat(ctx, &a_s, val);
    if (!bf_is_finite(a)) {
      JS_FreeValue(ctx, val);
      val = JS_ThrowRangeError(ctx, "cannot convert NaN or Infinity to bigint");
    } else {
      JSValue val1 = JS_NewBigInt(ctx);
      bf_t *r;
      int ret;
      if (JS_IsException(val1)) {
        JS_FreeValue(ctx, val);
        return JS_EXCEPTION;
      }
      r = JS_GetBigInt(val1);
      ret = bf_set(r, a);
      ret |= bf_rint(r, BF_RNDZ);
      JS_FreeValue(ctx, val);
      if (ret & BF_ST_MEM_ERROR) {
        JS_FreeValue(ctx, val1);
        val = JS_ThrowOutOfMemory(ctx);
      } else if (ret & BF_ST_INEXACT) {
        JS_FreeValue(ctx, val1);
        val =
            JS_ThrowRangeError(ctx, "cannot convert to bigint: not an integer");
      } else {
        val = JS_CompactBigInt(ctx, val1);
      }
    }
    if (a == &a_s)
      bf_delete(a);
  } break;
  case JS_TAG_BIG_DECIMAL:
    val = JS_ToStringFree(ctx, val);
    if (JS_IsException(val))
      break;
    goto redo;
  case JS_TAG_STRING:
    val = JS_StringToBigIntErr(ctx, val);
    break;
  case JS_TAG_OBJECT:
    val = JS_ToPrimitiveFree(ctx, val, HINT_NUMBER);
    if (JS_IsException(val))
      break;
    goto redo;
  case JS_TAG_NULL:
  case JS_TAG_UNDEFINED:
  default:
    JS_FreeValue(ctx, val);
    return JS_ThrowTypeError(ctx, "cannot convert to bigint");
  }
  return val;
}

static JSValue js_bigint_constructor(JSContext *ctx, JSValueConst new_target,
                                     int argc, JSValueConst *argv) {
  if (!JS_IsUndefined(new_target))
    return JS_ThrowTypeError(ctx, "not a constructor");
  return JS_ToBigIntCtorFree(ctx, JS_DupValue(ctx, argv[0]));
}

static JSValue js_thisBigIntValue(JSContext *ctx, JSValueConst this_val) {
  if (JS_IsBigInt(ctx, this_val))
    return JS_DupValue(ctx, this_val);

  if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
    JSObject *p = JS_VALUE_GET_OBJ(this_val);
    if (p->class_id == JS_CLASS_BIG_INT) {
      if (JS_IsBigInt(ctx, p->u.object_data))
        return JS_DupValue(ctx, p->u.object_data);
    }
  }
  return JS_ThrowTypeError(ctx, "not a bigint");
}

static JSValue js_bigint_toString(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv) {
  JSValue val;
  int base;
  JSValue ret;

  val = js_thisBigIntValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  if (argc == 0 || JS_IsUndefined(argv[0])) {
    base = 10;
  } else {
    base = js_get_radix(ctx, argv[0]);
    if (base < 0)
      goto fail;
  }
  ret = js_bigint_to_string1(ctx, val, base);
  JS_FreeValue(ctx, val);
  return ret;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static JSValue js_bigint_valueOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv) {
  return js_thisBigIntValue(ctx, this_val);
}

static JSValue js_bigint_div(JSContext *ctx, JSValueConst this_val, int argc,
                             JSValueConst *argv, int magic) {
  bf_t a_s, b_s, *a, *b, *r, *q;
  int status;
  JSValue q_val, r_val;

  q_val = JS_NewBigInt(ctx);
  if (JS_IsException(q_val))
    return JS_EXCEPTION;
  r_val = JS_NewBigInt(ctx);
  if (JS_IsException(r_val))
    goto fail;
  b = NULL;
  a = JS_ToBigInt(ctx, &a_s, argv[0]);
  if (!a)
    goto fail;
  b = JS_ToBigInt(ctx, &b_s, argv[1]);
  if (!b) {
    JS_FreeBigInt(ctx, a, &a_s);
    goto fail;
  }
  q = JS_GetBigInt(q_val);
  r = JS_GetBigInt(r_val);
  status = bf_divrem(q, r, a, b, BF_PREC_INF, BF_RNDZ, magic & 0xf);
  JS_FreeBigInt(ctx, a, &a_s);
  JS_FreeBigInt(ctx, b, &b_s);
  if (unlikely(status)) {
    throw_bf_exception(ctx, status);
    goto fail;
  }
  q_val = JS_CompactBigInt(ctx, q_val);
  if (magic & 0x10) {
    JSValue ret;
    ret = JS_NewArray(ctx);
    if (JS_IsException(ret))
      goto fail;
    JS_SetPropertyUint32(ctx, ret, 0, q_val);
    JS_SetPropertyUint32(ctx, ret, 1, JS_CompactBigInt(ctx, r_val));
    return ret;
  } else {
    JS_FreeValue(ctx, r_val);
    return q_val;
  }
fail:
  JS_FreeValue(ctx, q_val);
  JS_FreeValue(ctx, r_val);
  return JS_EXCEPTION;
}

static JSValue js_bigint_sqrt(JSContext *ctx, JSValueConst this_val, int argc,
                              JSValueConst *argv, int magic) {
  bf_t a_s, *a, *r, *rem;
  int status;
  JSValue r_val, rem_val;

  r_val = JS_NewBigInt(ctx);
  if (JS_IsException(r_val))
    return JS_EXCEPTION;
  rem_val = JS_NewBigInt(ctx);
  if (JS_IsException(rem_val))
    return JS_EXCEPTION;
  r = JS_GetBigInt(r_val);
  rem = JS_GetBigInt(rem_val);

  a = JS_ToBigInt(ctx, &a_s, argv[0]);
  if (!a)
    goto fail;
  status = bf_sqrtrem(r, rem, a);
  JS_FreeBigInt(ctx, a, &a_s);
  if (unlikely(status & ~BF_ST_INEXACT)) {
    throw_bf_exception(ctx, status);
    goto fail;
  }
  r_val = JS_CompactBigInt(ctx, r_val);
  if (magic) {
    JSValue ret;
    ret = JS_NewArray(ctx);
    if (JS_IsException(ret))
      goto fail;
    JS_SetPropertyUint32(ctx, ret, 0, r_val);
    JS_SetPropertyUint32(ctx, ret, 1, JS_CompactBigInt(ctx, rem_val));
    return ret;
  } else {
    JS_FreeValue(ctx, rem_val);
    return r_val;
  }
fail:
  JS_FreeValue(ctx, r_val);
  JS_FreeValue(ctx, rem_val);
  return JS_EXCEPTION;
}

static JSValue js_bigint_op1(JSContext *ctx, JSValueConst this_val, int argc,
                             JSValueConst *argv, int magic) {
  bf_t a_s, *a;
  int64_t res;

  a = JS_ToBigInt(ctx, &a_s, argv[0]);
  if (!a)
    return JS_EXCEPTION;
  switch (magic) {
  case 0: /* floorLog2 */
    if (a->sign || a->expn <= 0) {
      res = -1;
    } else {
      res = a->expn - 1;
    }
    break;
  case 1: /* ctz */
    if (bf_is_zero(a)) {
      res = -1;
    } else {
      res = bf_get_exp_min(a);
    }
    break;
  default:
    abort();
  }
  JS_FreeBigInt(ctx, a, &a_s);
  return JS_NewBigInt64(ctx, res);
}

static JSValue js_bigint_asUintN(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int asIntN) {
  uint64_t bits;
  bf_t a_s, *a = &a_s, *r, mask_s, *mask = &mask_s;
  JSValue res;

  if (JS_ToIndex(ctx, &bits, argv[0]))
    return JS_EXCEPTION;
  res = JS_NewBigInt(ctx);
  if (JS_IsException(res))
    return JS_EXCEPTION;
  r = JS_GetBigInt(res);
  a = JS_ToBigInt(ctx, &a_s, argv[1]);
  if (!a) {
    JS_FreeValue(ctx, res);
    return JS_EXCEPTION;
  }
  /* XXX: optimize */
  r = JS_GetBigInt(res);
  bf_init(ctx->bf_ctx, mask);
  bf_set_ui(mask, 1);
  bf_mul_2exp(mask, bits, BF_PREC_INF, BF_RNDZ);
  bf_add_si(mask, mask, -1, BF_PREC_INF, BF_RNDZ);
  bf_logic_and(r, a, mask);
  if (asIntN && bits != 0) {
    bf_set_ui(mask, 1);
    bf_mul_2exp(mask, bits - 1, BF_PREC_INF, BF_RNDZ);
    if (bf_cmpu(r, mask) >= 0) {
      bf_set_ui(mask, 1);
      bf_mul_2exp(mask, bits, BF_PREC_INF, BF_RNDZ);
      bf_sub(r, r, mask, BF_PREC_INF, BF_RNDZ);
    }
  }
  bf_delete(mask);
  JS_FreeBigInt(ctx, a, &a_s);
  return JS_CompactBigInt(ctx, res);
}

static const JSCFunctionListEntry js_bigint_funcs[] = {
    JS_CFUNC_MAGIC_DEF("asUintN", 2, js_bigint_asUintN, 0),
    JS_CFUNC_MAGIC_DEF("asIntN", 2, js_bigint_asUintN, 1),
    /* QuickJS extensions */
    JS_CFUNC_MAGIC_DEF("tdiv", 2, js_bigint_div, BF_RNDZ),
    JS_CFUNC_MAGIC_DEF("fdiv", 2, js_bigint_div, BF_RNDD),
    JS_CFUNC_MAGIC_DEF("cdiv", 2, js_bigint_div, BF_RNDU),
    JS_CFUNC_MAGIC_DEF("ediv", 2, js_bigint_div, BF_DIVREM_EUCLIDIAN),
    JS_CFUNC_MAGIC_DEF("tdivrem", 2, js_bigint_div, BF_RNDZ | 0x10),
    JS_CFUNC_MAGIC_DEF("fdivrem", 2, js_bigint_div, BF_RNDD | 0x10),
    JS_CFUNC_MAGIC_DEF("cdivrem", 2, js_bigint_div, BF_RNDU | 0x10),
    JS_CFUNC_MAGIC_DEF("edivrem", 2, js_bigint_div, BF_DIVREM_EUCLIDIAN | 0x10),
    JS_CFUNC_MAGIC_DEF("sqrt", 1, js_bigint_sqrt, 0),
    JS_CFUNC_MAGIC_DEF("sqrtrem", 1, js_bigint_sqrt, 1),
    JS_CFUNC_MAGIC_DEF("floorLog2", 1, js_bigint_op1, 0),
    JS_CFUNC_MAGIC_DEF("ctz", 1, js_bigint_op1, 1),
};

static const JSCFunctionListEntry js_bigint_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_bigint_toString),
    JS_CFUNC_DEF("valueOf", 0, js_bigint_valueOf),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "BigInt", JS_PROP_CONFIGURABLE),
};

void JS_AddIntrinsicBigInt(JSContext *ctx) {
  JSRuntime *rt = ctx->rt;
  JSValueConst obj1;

  rt->bigint_ops.to_string = js_bigint_to_string;
  rt->bigint_ops.from_string = js_string_to_bigint;
  rt->bigint_ops.unary_arith = js_unary_arith_bigint;
  rt->bigint_ops.binary_arith = js_binary_arith_bigint;
  rt->bigint_ops.compare = js_compare_bigfloat;

  ctx->class_proto[JS_CLASS_BIG_INT] = JS_NewObject(ctx);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_BIG_INT],
                             js_bigint_proto_funcs,
                             countof(js_bigint_proto_funcs));
  obj1 = JS_NewGlobalCConstructor(ctx, "BigInt", js_bigint_constructor, 1,
                                  ctx->class_proto[JS_CLASS_BIG_INT]);
  JS_SetPropertyFunctionList(ctx, obj1, js_bigint_funcs,
                             countof(js_bigint_funcs));
}

/* -- BigFloat ----------------------------------- */

static JSValue js_thisBigFloatValue(JSContext *ctx, JSValueConst this_val) {
  if (JS_IsBigFloat(this_val))
    return JS_DupValue(ctx, this_val);

  if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
    JSObject *p = JS_VALUE_GET_OBJ(this_val);
    if (p->class_id == JS_CLASS_BIG_FLOAT) {
      if (JS_IsBigFloat(p->u.object_data))
        return JS_DupValue(ctx, p->u.object_data);
    }
  }
  return JS_ThrowTypeError(ctx, "not a bigfloat");
}

static JSValue js_bigfloat_toString(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv) {
  JSValue val;
  int base;
  JSValue ret;

  val = js_thisBigFloatValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  if (argc == 0 || JS_IsUndefined(argv[0])) {
    base = 10;
  } else {
    base = js_get_radix(ctx, argv[0]);
    if (base < 0)
      goto fail;
  }
  ret = js_ftoa(ctx, val, base, 0, BF_RNDN | BF_FTOA_FORMAT_FREE_MIN);
  JS_FreeValue(ctx, val);
  return ret;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static JSValue js_bigfloat_valueOf(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv) {
  return js_thisBigFloatValue(ctx, this_val);
}

static int bigfloat_get_rnd_mode(JSContext *ctx, JSValueConst val) {
  int rnd_mode;
  if (JS_ToInt32Sat(ctx, &rnd_mode, val))
    return -1;
  if (rnd_mode < BF_RNDN || rnd_mode > BF_RNDF) {
    JS_ThrowRangeError(ctx, "invalid rounding mode");
    return -1;
  }
  return rnd_mode;
}

static JSValue js_bigfloat_toFixed(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv) {
  JSValue val, ret;
  int64_t f;
  int rnd_mode, radix;

  val = js_thisBigFloatValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  if (JS_ToInt64Sat(ctx, &f, argv[0]))
    goto fail;
  if (f < 0 || f > BF_PREC_MAX) {
    JS_ThrowRangeError(ctx, "invalid number of digits");
    goto fail;
  }
  rnd_mode = BF_RNDNA;
  radix = 10;
  /* XXX: swap parameter order for rounding mode and radix */
  if (argc > 1) {
    rnd_mode = bigfloat_get_rnd_mode(ctx, argv[1]);
    if (rnd_mode < 0)
      goto fail;
  }
  if (argc > 2) {
    radix = js_get_radix(ctx, argv[2]);
    if (radix < 0)
      goto fail;
  }
  ret = js_ftoa(ctx, val, radix, f, rnd_mode | BF_FTOA_FORMAT_FRAC);
  JS_FreeValue(ctx, val);
  return ret;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static BOOL js_bigfloat_is_finite(JSContext *ctx, JSValueConst val) {
  BOOL res;
  uint32_t tag;

  tag = JS_VALUE_GET_NORM_TAG(val);
  switch (tag) {
  case JS_TAG_BIG_FLOAT: {
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    res = bf_is_finite(&p->num);
  } break;
  default:
    res = FALSE;
    break;
  }
  return res;
}

static JSValue js_bigfloat_toExponential(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv) {
  JSValue val, ret;
  int64_t f;
  int rnd_mode, radix;

  val = js_thisBigFloatValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  if (JS_ToInt64Sat(ctx, &f, argv[0]))
    goto fail;
  if (!js_bigfloat_is_finite(ctx, val)) {
    ret = JS_ToString(ctx, val);
  } else if (JS_IsUndefined(argv[0])) {
    ret = js_ftoa(ctx, val, 10, 0,
                  BF_RNDN | BF_FTOA_FORMAT_FREE_MIN | BF_FTOA_FORCE_EXP);
  } else {
    if (f < 0 || f > BF_PREC_MAX) {
      JS_ThrowRangeError(ctx, "invalid number of digits");
      goto fail;
    }
    rnd_mode = BF_RNDNA;
    radix = 10;
    if (argc > 1) {
      rnd_mode = bigfloat_get_rnd_mode(ctx, argv[1]);
      if (rnd_mode < 0)
        goto fail;
    }
    if (argc > 2) {
      radix = js_get_radix(ctx, argv[2]);
      if (radix < 0)
        goto fail;
    }
    ret = js_ftoa(ctx, val, radix, f + 1,
                  rnd_mode | BF_FTOA_FORMAT_FIXED | BF_FTOA_FORCE_EXP);
  }
  JS_FreeValue(ctx, val);
  return ret;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static JSValue js_bigfloat_toPrecision(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv) {
  JSValue val, ret;
  int64_t p;
  int rnd_mode, radix;

  val = js_thisBigFloatValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  if (JS_IsUndefined(argv[0]))
    goto to_string;
  if (JS_ToInt64Sat(ctx, &p, argv[0]))
    goto fail;
  if (!js_bigfloat_is_finite(ctx, val)) {
  to_string:
    ret = JS_ToString(ctx, this_val);
  } else {
    if (p < 1 || p > BF_PREC_MAX) {
      JS_ThrowRangeError(ctx, "invalid number of digits");
      goto fail;
    }
    rnd_mode = BF_RNDNA;
    radix = 10;
    if (argc > 1) {
      rnd_mode = bigfloat_get_rnd_mode(ctx, argv[1]);
      if (rnd_mode < 0)
        goto fail;
    }
    if (argc > 2) {
      radix = js_get_radix(ctx, argv[2]);
      if (radix < 0)
        goto fail;
    }
    ret = js_ftoa(ctx, val, radix, p, rnd_mode | BF_FTOA_FORMAT_FIXED);
  }
  JS_FreeValue(ctx, val);
  return ret;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static const JSCFunctionListEntry js_bigfloat_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_bigfloat_toString),
    JS_CFUNC_DEF("valueOf", 0, js_bigfloat_valueOf),
    JS_CFUNC_DEF("toPrecision", 1, js_bigfloat_toPrecision),
    JS_CFUNC_DEF("toFixed", 1, js_bigfloat_toFixed),
    JS_CFUNC_DEF("toExponential", 1, js_bigfloat_toExponential),
};

static JSValue js_bigfloat_constructor(JSContext *ctx, JSValueConst new_target,
                                       int argc, JSValueConst *argv) {
  JSValue val;
  if (!JS_IsUndefined(new_target))
    return JS_ThrowTypeError(ctx, "not a constructor");
  if (argc == 0) {
    bf_t *r;
    val = JS_NewBigFloat(ctx);
    if (JS_IsException(val))
      return val;
    r = JS_GetBigFloat(val);
    bf_set_zero(r, 0);
  } else {
    val = JS_DupValue(ctx, argv[0]);
  redo:
    switch (JS_VALUE_GET_NORM_TAG(val)) {
    case JS_TAG_BIG_FLOAT:
      break;
    case JS_TAG_FLOAT64: {
      bf_t *r;
      double d = JS_VALUE_GET_FLOAT64(val);
      val = JS_NewBigFloat(ctx);
      if (JS_IsException(val))
        break;
      r = JS_GetBigFloat(val);
      if (bf_set_float64(r, d))
        goto fail;
    } break;
    case JS_TAG_INT: {
      bf_t *r;
      int32_t v = JS_VALUE_GET_INT(val);
      val = JS_NewBigFloat(ctx);
      if (JS_IsException(val))
        break;
      r = JS_GetBigFloat(val);
      if (bf_set_si(r, v))
        goto fail;
    } break;
    case JS_TAG_BIG_INT:
      /* We keep the full precision of the integer */
      {
        JSBigFloat *p = JS_VALUE_GET_PTR(val);
        val = JS_MKPTR(JS_TAG_BIG_FLOAT, p);
      }
      break;
    case JS_TAG_BIG_DECIMAL:
      val = JS_ToStringFree(ctx, val);
      if (JS_IsException(val))
        break;
      goto redo;
    case JS_TAG_STRING: {
      const char *str, *p;
      size_t len;
      int err;

      str = JS_ToCStringLen(ctx, &len, val);
      JS_FreeValue(ctx, val);
      if (!str)
        return JS_EXCEPTION;
      p = str;
      p += skip_spaces(p);
      if ((p - str) == len) {
        bf_t *r;
        val = JS_NewBigFloat(ctx);
        if (JS_IsException(val))
          break;
        r = JS_GetBigFloat(val);
        bf_set_zero(r, 0);
        err = 0;
      } else {
        val = js_atof(ctx, p, &p, 0,
                      ATOD_ACCEPT_BIN_OCT | ATOD_TYPE_BIG_FLOAT |
                          ATOD_ACCEPT_PREFIX_AFTER_SIGN);
        if (JS_IsException(val)) {
          JS_FreeCString(ctx, str);
          return JS_EXCEPTION;
        }
        p += skip_spaces(p);
        err = ((p - str) != len);
      }
      JS_FreeCString(ctx, str);
      if (err) {
        JS_FreeValue(ctx, val);
        return JS_ThrowSyntaxError(ctx, "invalid bigfloat literal");
      }
    } break;
    case JS_TAG_OBJECT:
      val = JS_ToPrimitiveFree(ctx, val, HINT_NUMBER);
      if (JS_IsException(val))
        break;
      goto redo;
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
    default:
      JS_FreeValue(ctx, val);
      return JS_ThrowTypeError(ctx, "cannot convert to bigfloat");
    }
  }
  return val;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static JSValue js_bigfloat_get_const(JSContext *ctx, JSValueConst this_val,
                                     int magic) {
  bf_t *r;
  JSValue val;
  val = JS_NewBigFloat(ctx);
  if (JS_IsException(val))
    return val;
  r = JS_GetBigFloat(val);
  switch (magic) {
  case 0: /* PI */
    bf_const_pi(r, ctx->fp_env.prec, ctx->fp_env.flags);
    break;
  case 1: /* LN2 */
    bf_const_log2(r, ctx->fp_env.prec, ctx->fp_env.flags);
    break;
  case 2: /* MIN_VALUE */
  case 3: /* MAX_VALUE */
  {
    slimb_t e_range, e;
    e_range = (limb_t)1 << (bf_get_exp_bits(ctx->fp_env.flags) - 1);
    bf_set_ui(r, 1);
    if (magic == 2) {
      e = -e_range + 2;
      if (ctx->fp_env.flags & BF_FLAG_SUBNORMAL)
        e -= ctx->fp_env.prec - 1;
      bf_mul_2exp(r, e, ctx->fp_env.prec, ctx->fp_env.flags);
    } else {
      bf_mul_2exp(r, ctx->fp_env.prec, ctx->fp_env.prec, ctx->fp_env.flags);
      bf_add_si(r, r, -1, ctx->fp_env.prec, ctx->fp_env.flags);
      bf_mul_2exp(r, e_range - ctx->fp_env.prec, ctx->fp_env.prec,
                  ctx->fp_env.flags);
    }
  } break;
  case 4: /* EPSILON */
    bf_set_ui(r, 1);
    bf_mul_2exp(r, 1 - ctx->fp_env.prec, ctx->fp_env.prec, ctx->fp_env.flags);
    break;
  default:
    abort();
  }
  return val;
}

static JSValue js_bigfloat_parseFloat(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv) {
  bf_t *a;
  const char *str;
  JSValue ret;
  int radix;
  JSFloatEnv *fe;

  str = JS_ToCString(ctx, argv[0]);
  if (!str)
    return JS_EXCEPTION;
  if (JS_ToInt32(ctx, &radix, argv[1])) {
  fail:
    JS_FreeCString(ctx, str);
    return JS_EXCEPTION;
  }
  if (radix != 0 && (radix < 2 || radix > 36)) {
    JS_ThrowRangeError(ctx, "radix must be between 2 and 36");
    goto fail;
  }
  fe = &ctx->fp_env;
  if (argc > 2) {
    fe = JS_GetOpaque2(ctx, argv[2], JS_CLASS_FLOAT_ENV);
    if (!fe)
      goto fail;
  }
  ret = JS_NewBigFloat(ctx);
  if (JS_IsException(ret))
    goto done;
  a = JS_GetBigFloat(ret);
  /* XXX: use js_atof() */
  bf_atof(a, str, NULL, radix, fe->prec, fe->flags);
done:
  JS_FreeCString(ctx, str);
  return ret;
}

static JSValue js_bigfloat_isFinite(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv) {
  JSValueConst val = argv[0];
  JSBigFloat *p;

  if (JS_VALUE_GET_NORM_TAG(val) != JS_TAG_BIG_FLOAT)
    return JS_FALSE;
  p = JS_VALUE_GET_PTR(val);
  return JS_NewBool(ctx, bf_is_finite(&p->num));
}

static JSValue js_bigfloat_isNaN(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv) {
  JSValueConst val = argv[0];
  JSBigFloat *p;

  if (JS_VALUE_GET_NORM_TAG(val) != JS_TAG_BIG_FLOAT)
    return JS_FALSE;
  p = JS_VALUE_GET_PTR(val);
  return JS_NewBool(ctx, bf_is_nan(&p->num));
}

enum {
  MATH_OP_ABS,
  MATH_OP_FLOOR,
  MATH_OP_CEIL,
  MATH_OP_ROUND,
  MATH_OP_TRUNC,
  MATH_OP_SQRT,
  MATH_OP_FPROUND,
  MATH_OP_ACOS,
  MATH_OP_ASIN,
  MATH_OP_ATAN,
  MATH_OP_ATAN2,
  MATH_OP_COS,
  MATH_OP_EXP,
  MATH_OP_LOG,
  MATH_OP_POW,
  MATH_OP_SIN,
  MATH_OP_TAN,
  MATH_OP_FMOD,
  MATH_OP_REM,
  MATH_OP_SIGN,

  MATH_OP_ADD,
  MATH_OP_SUB,
  MATH_OP_MUL,
  MATH_OP_DIV,
};

static JSValue js_bigfloat_fop(JSContext *ctx, JSValueConst this_val, int argc,
                               JSValueConst *argv, int magic) {
  bf_t a_s, *a, *r;
  JSFloatEnv *fe;
  int rnd_mode;
  JSValue op1, res;

  op1 = JS_ToNumeric(ctx, argv[0]);
  if (JS_IsException(op1))
    return op1;
  a = JS_ToBigFloat(ctx, &a_s, op1);
  fe = &ctx->fp_env;
  if (argc > 1) {
    fe = JS_GetOpaque2(ctx, argv[1], JS_CLASS_FLOAT_ENV);
    if (!fe)
      goto fail;
  }
  res = JS_NewBigFloat(ctx);
  if (JS_IsException(res)) {
  fail:
    if (a == &a_s)
      bf_delete(a);
    JS_FreeValue(ctx, op1);
    return JS_EXCEPTION;
  }
  r = JS_GetBigFloat(res);
  switch (magic) {
  case MATH_OP_ABS:
    bf_set(r, a);
    r->sign = 0;
    break;
  case MATH_OP_FLOOR:
    rnd_mode = BF_RNDD;
    goto rint;
  case MATH_OP_CEIL:
    rnd_mode = BF_RNDU;
    goto rint;
  case MATH_OP_ROUND:
    rnd_mode = BF_RNDNA;
    goto rint;
  case MATH_OP_TRUNC:
    rnd_mode = BF_RNDZ;
  rint:
    bf_set(r, a);
    fe->status |= bf_rint(r, rnd_mode);
    break;
  case MATH_OP_SQRT:
    fe->status |= bf_sqrt(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_FPROUND:
    bf_set(r, a);
    fe->status |= bf_round(r, fe->prec, fe->flags);
    break;
  case MATH_OP_ACOS:
    fe->status |= bf_acos(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_ASIN:
    fe->status |= bf_asin(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_ATAN:
    fe->status |= bf_atan(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_COS:
    fe->status |= bf_cos(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_EXP:
    fe->status |= bf_exp(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_LOG:
    fe->status |= bf_log(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_SIN:
    fe->status |= bf_sin(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_TAN:
    fe->status |= bf_tan(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_SIGN:
    if (bf_is_nan(a) || bf_is_zero(a)) {
      bf_set(r, a);
    } else {
      bf_set_si(r, 1 - 2 * a->sign);
    }
    break;
  default:
    abort();
  }
  if (a == &a_s)
    bf_delete(a);
  JS_FreeValue(ctx, op1);
  return res;
}

static JSValue js_bigfloat_fop2(JSContext *ctx, JSValueConst this_val, int argc,
                                JSValueConst *argv, int magic) {
  bf_t a_s, *a, b_s, *b, r_s, *r = &r_s;
  JSFloatEnv *fe;
  JSValue op1, op2, res;

  op1 = JS_ToNumeric(ctx, argv[0]);
  if (JS_IsException(op1))
    return op1;
  op2 = JS_ToNumeric(ctx, argv[1]);
  if (JS_IsException(op2)) {
    JS_FreeValue(ctx, op1);
    return op2;
  }
  a = JS_ToBigFloat(ctx, &a_s, op1);
  b = JS_ToBigFloat(ctx, &b_s, op2);
  fe = &ctx->fp_env;
  if (argc > 2) {
    fe = JS_GetOpaque2(ctx, argv[2], JS_CLASS_FLOAT_ENV);
    if (!fe)
      goto fail;
  }
  res = JS_NewBigFloat(ctx);
  if (JS_IsException(res)) {
  fail:
    if (a == &a_s)
      bf_delete(a);
    if (b == &b_s)
      bf_delete(b);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    return JS_EXCEPTION;
  }
  r = JS_GetBigFloat(res);
  switch (magic) {
  case MATH_OP_ATAN2:
    fe->status |= bf_atan2(r, a, b, fe->prec, fe->flags);
    break;
  case MATH_OP_POW:
    fe->status |= bf_pow(r, a, b, fe->prec, fe->flags | BF_POW_JS_QUIRKS);
    break;
  case MATH_OP_FMOD:
    fe->status |= bf_rem(r, a, b, fe->prec, fe->flags, BF_RNDZ);
    break;
  case MATH_OP_REM:
    fe->status |= bf_rem(r, a, b, fe->prec, fe->flags, BF_RNDN);
    break;
  case MATH_OP_ADD:
    fe->status |= bf_add(r, a, b, fe->prec, fe->flags);
    break;
  case MATH_OP_SUB:
    fe->status |= bf_sub(r, a, b, fe->prec, fe->flags);
    break;
  case MATH_OP_MUL:
    fe->status |= bf_mul(r, a, b, fe->prec, fe->flags);
    break;
  case MATH_OP_DIV:
    fe->status |= bf_div(r, a, b, fe->prec, fe->flags);
    break;
  default:
    abort();
  }
  if (a == &a_s)
    bf_delete(a);
  if (b == &b_s)
    bf_delete(b);
  JS_FreeValue(ctx, op1);
  JS_FreeValue(ctx, op2);
  return res;
}

static const JSCFunctionListEntry js_bigfloat_funcs[] = {
    JS_CGETSET_MAGIC_DEF("PI", js_bigfloat_get_const, NULL, 0),
    JS_CGETSET_MAGIC_DEF("LN2", js_bigfloat_get_const, NULL, 1),
    JS_CGETSET_MAGIC_DEF("MIN_VALUE", js_bigfloat_get_const, NULL, 2),
    JS_CGETSET_MAGIC_DEF("MAX_VALUE", js_bigfloat_get_const, NULL, 3),
    JS_CGETSET_MAGIC_DEF("EPSILON", js_bigfloat_get_const, NULL, 4),
    JS_CFUNC_DEF("parseFloat", 1, js_bigfloat_parseFloat),
    JS_CFUNC_DEF("isFinite", 1, js_bigfloat_isFinite),
    JS_CFUNC_DEF("isNaN", 1, js_bigfloat_isNaN),
    JS_CFUNC_MAGIC_DEF("abs", 1, js_bigfloat_fop, MATH_OP_ABS),
    JS_CFUNC_MAGIC_DEF("fpRound", 1, js_bigfloat_fop, MATH_OP_FPROUND),
    JS_CFUNC_MAGIC_DEF("floor", 1, js_bigfloat_fop, MATH_OP_FLOOR),
    JS_CFUNC_MAGIC_DEF("ceil", 1, js_bigfloat_fop, MATH_OP_CEIL),
    JS_CFUNC_MAGIC_DEF("round", 1, js_bigfloat_fop, MATH_OP_ROUND),
    JS_CFUNC_MAGIC_DEF("trunc", 1, js_bigfloat_fop, MATH_OP_TRUNC),
    JS_CFUNC_MAGIC_DEF("sqrt", 1, js_bigfloat_fop, MATH_OP_SQRT),
    JS_CFUNC_MAGIC_DEF("acos", 1, js_bigfloat_fop, MATH_OP_ACOS),
    JS_CFUNC_MAGIC_DEF("asin", 1, js_bigfloat_fop, MATH_OP_ASIN),
    JS_CFUNC_MAGIC_DEF("atan", 1, js_bigfloat_fop, MATH_OP_ATAN),
    JS_CFUNC_MAGIC_DEF("atan2", 2, js_bigfloat_fop2, MATH_OP_ATAN2),
    JS_CFUNC_MAGIC_DEF("cos", 1, js_bigfloat_fop, MATH_OP_COS),
    JS_CFUNC_MAGIC_DEF("exp", 1, js_bigfloat_fop, MATH_OP_EXP),
    JS_CFUNC_MAGIC_DEF("log", 1, js_bigfloat_fop, MATH_OP_LOG),
    JS_CFUNC_MAGIC_DEF("pow", 2, js_bigfloat_fop2, MATH_OP_POW),
    JS_CFUNC_MAGIC_DEF("sin", 1, js_bigfloat_fop, MATH_OP_SIN),
    JS_CFUNC_MAGIC_DEF("tan", 1, js_bigfloat_fop, MATH_OP_TAN),
    JS_CFUNC_MAGIC_DEF("sign", 1, js_bigfloat_fop, MATH_OP_SIGN),
    JS_CFUNC_MAGIC_DEF("add", 2, js_bigfloat_fop2, MATH_OP_ADD),
    JS_CFUNC_MAGIC_DEF("sub", 2, js_bigfloat_fop2, MATH_OP_SUB),
    JS_CFUNC_MAGIC_DEF("mul", 2, js_bigfloat_fop2, MATH_OP_MUL),
    JS_CFUNC_MAGIC_DEF("div", 2, js_bigfloat_fop2, MATH_OP_DIV),
    JS_CFUNC_MAGIC_DEF("fmod", 2, js_bigfloat_fop2, MATH_OP_FMOD),
    JS_CFUNC_MAGIC_DEF("remainder", 2, js_bigfloat_fop2, MATH_OP_REM),
};

/* -- FloatEnv ----------------------------------- */

static JSValue js_float_env_constructor(JSContext *ctx, JSValueConst new_target,
                                        int argc, JSValueConst *argv) {
  JSValue obj;
  JSFloatEnv *fe;
  int64_t prec;
  int flags, rndmode;

  prec = ctx->fp_env.prec;
  flags = ctx->fp_env.flags;
  if (!JS_IsUndefined(argv[0])) {
    if (JS_ToInt64Sat(ctx, &prec, argv[0]))
      return JS_EXCEPTION;
    if (prec < BF_PREC_MIN || prec > BF_PREC_MAX)
      return JS_ThrowRangeError(ctx, "invalid precision");
    flags = BF_RNDN; /* RNDN, max exponent size, no subnormal */
    if (argc > 1 && !JS_IsUndefined(argv[1])) {
      if (JS_ToInt32Sat(ctx, &rndmode, argv[1]))
        return JS_EXCEPTION;
      if (rndmode < BF_RNDN || rndmode > BF_RNDF)
        return JS_ThrowRangeError(ctx, "invalid rounding mode");
      flags = rndmode;
    }
  }

  obj = JS_NewObjectClass(ctx, JS_CLASS_FLOAT_ENV);
  if (JS_IsException(obj))
    return JS_EXCEPTION;
  fe = js_malloc(ctx, sizeof(*fe));
  if (!fe)
    return JS_EXCEPTION;
  fe->prec = prec;
  fe->flags = flags;
  fe->status = 0;
  JS_SetOpaque(obj, fe);
  return obj;
}

void js_float_env_finalizer(JSRuntime *rt, JSValue val) {
  JSFloatEnv *fe = JS_GetOpaque(val, JS_CLASS_FLOAT_ENV);
  js_free_rt(rt, fe);
}

static JSValue js_float_env_get_prec(JSContext *ctx, JSValueConst this_val) {
  return JS_NewInt64(ctx, ctx->fp_env.prec);
}

static JSValue js_float_env_get_expBits(JSContext *ctx, JSValueConst this_val) {
  return JS_NewInt32(ctx, bf_get_exp_bits(ctx->fp_env.flags));
}

static JSValue js_float_env_setPrec(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv) {
  JSValueConst func;
  int exp_bits, flags, saved_flags;
  JSValue ret;
  limb_t saved_prec;
  int64_t prec;

  func = argv[0];
  if (JS_ToInt64Sat(ctx, &prec, argv[1]))
    return JS_EXCEPTION;
  if (prec < BF_PREC_MIN || prec > BF_PREC_MAX)
    return JS_ThrowRangeError(ctx, "invalid precision");
  exp_bits = BF_EXP_BITS_MAX;

  if (argc > 2 && !JS_IsUndefined(argv[2])) {
    if (JS_ToInt32Sat(ctx, &exp_bits, argv[2]))
      return JS_EXCEPTION;
    if (exp_bits < BF_EXP_BITS_MIN || exp_bits > BF_EXP_BITS_MAX)
      return JS_ThrowRangeError(ctx, "invalid number of exponent bits");
  }

  flags = BF_RNDN | BF_FLAG_SUBNORMAL | bf_set_exp_bits(exp_bits);

  saved_prec = ctx->fp_env.prec;
  saved_flags = ctx->fp_env.flags;

  ctx->fp_env.prec = prec;
  ctx->fp_env.flags = flags;

  ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL);
  /* always restore the floating point precision */
  ctx->fp_env.prec = saved_prec;
  ctx->fp_env.flags = saved_flags;
  return ret;
}

#define FE_PREC (-1)
#define FE_EXP (-2)
#define FE_RNDMODE (-3)
#define FE_SUBNORMAL (-4)

static JSValue js_float_env_proto_get_status(JSContext *ctx,
                                             JSValueConst this_val, int magic) {
  JSFloatEnv *fe;
  fe = JS_GetOpaque2(ctx, this_val, JS_CLASS_FLOAT_ENV);
  if (!fe)
    return JS_EXCEPTION;
  switch (magic) {
  case FE_PREC:
    return JS_NewInt64(ctx, fe->prec);
  case FE_EXP:
    return JS_NewInt32(ctx, bf_get_exp_bits(fe->flags));
  case FE_RNDMODE:
    return JS_NewInt32(ctx, fe->flags & BF_RND_MASK);
  case FE_SUBNORMAL:
    return JS_NewBool(ctx, (fe->flags & BF_FLAG_SUBNORMAL) != 0);
  default:
    return JS_NewBool(ctx, (fe->status & magic) != 0);
  }
}

static JSValue js_float_env_proto_set_status(JSContext *ctx,
                                             JSValueConst this_val,
                                             JSValueConst val, int magic) {
  JSFloatEnv *fe;
  int b;
  int64_t prec;

  fe = JS_GetOpaque2(ctx, this_val, JS_CLASS_FLOAT_ENV);
  if (!fe)
    return JS_EXCEPTION;
  switch (magic) {
  case FE_PREC:
    if (JS_ToInt64Sat(ctx, &prec, val))
      return JS_EXCEPTION;
    if (prec < BF_PREC_MIN || prec > BF_PREC_MAX)
      return JS_ThrowRangeError(ctx, "invalid precision");
    fe->prec = prec;
    break;
  case FE_EXP:
    if (JS_ToInt32Sat(ctx, &b, val))
      return JS_EXCEPTION;
    if (b < BF_EXP_BITS_MIN || b > BF_EXP_BITS_MAX)
      return JS_ThrowRangeError(ctx, "invalid number of exponent bits");
    fe->flags = (fe->flags & ~(BF_EXP_BITS_MASK << BF_EXP_BITS_SHIFT)) |
                bf_set_exp_bits(b);
    break;
  case FE_RNDMODE:
    b = bigfloat_get_rnd_mode(ctx, val);
    if (b < 0)
      return JS_EXCEPTION;
    fe->flags = (fe->flags & ~BF_RND_MASK) | b;
    break;
  case FE_SUBNORMAL:
    b = JS_ToBool(ctx, val);
    fe->flags = (fe->flags & ~BF_FLAG_SUBNORMAL) | (b ? BF_FLAG_SUBNORMAL : 0);
    break;
  default:
    b = JS_ToBool(ctx, val);
    fe->status = (fe->status & ~magic) & ((-b) & magic);
    break;
  }
  return JS_UNDEFINED;
}

static JSValue js_float_env_clearStatus(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv) {
  JSFloatEnv *fe = JS_GetOpaque2(ctx, this_val, JS_CLASS_FLOAT_ENV);
  if (!fe)
    return JS_EXCEPTION;
  fe->status = 0;
  return JS_UNDEFINED;
}

static const JSCFunctionListEntry js_float_env_funcs[] = {
    JS_CGETSET_DEF("prec", js_float_env_get_prec, NULL),
    JS_CGETSET_DEF("expBits", js_float_env_get_expBits, NULL),
    JS_CFUNC_DEF("setPrec", 2, js_float_env_setPrec),
    JS_PROP_INT32_DEF("RNDN", BF_RNDN, 0),
    JS_PROP_INT32_DEF("RNDZ", BF_RNDZ, 0),
    JS_PROP_INT32_DEF("RNDU", BF_RNDU, 0),
    JS_PROP_INT32_DEF("RNDD", BF_RNDD, 0),
    JS_PROP_INT32_DEF("RNDNA", BF_RNDNA, 0),
    JS_PROP_INT32_DEF("RNDA", BF_RNDA, 0),
    JS_PROP_INT32_DEF("RNDF", BF_RNDF, 0),
    JS_PROP_INT32_DEF("precMin", BF_PREC_MIN, 0),
    JS_PROP_INT64_DEF("precMax", BF_PREC_MAX, 0),
    JS_PROP_INT32_DEF("expBitsMin", BF_EXP_BITS_MIN, 0),
    JS_PROP_INT32_DEF("expBitsMax", BF_EXP_BITS_MAX, 0),
};

static const JSCFunctionListEntry js_float_env_proto_funcs[] = {
    JS_CGETSET_MAGIC_DEF("prec", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, FE_PREC),
    JS_CGETSET_MAGIC_DEF("expBits", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, FE_EXP),
    JS_CGETSET_MAGIC_DEF("rndMode", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, FE_RNDMODE),
    JS_CGETSET_MAGIC_DEF("subnormal", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, FE_SUBNORMAL),
    JS_CGETSET_MAGIC_DEF("invalidOperation", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_INVALID_OP),
    JS_CGETSET_MAGIC_DEF("divideByZero", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_DIVIDE_ZERO),
    JS_CGETSET_MAGIC_DEF("overflow", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_OVERFLOW),
    JS_CGETSET_MAGIC_DEF("underflow", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_UNDERFLOW),
    JS_CGETSET_MAGIC_DEF("inexact", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_INEXACT),
    JS_CFUNC_DEF("clearStatus", 0, js_float_env_clearStatus),
};

void JS_AddIntrinsicBigFloat(JSContext *ctx) {
  JSRuntime *rt = ctx->rt;
  JSValueConst obj1;

  rt->bigfloat_ops.to_string = js_bigfloat_to_string;
  rt->bigfloat_ops.from_string = js_string_to_bigfloat;
  rt->bigfloat_ops.unary_arith = js_unary_arith_bigfloat;
  rt->bigfloat_ops.binary_arith = js_binary_arith_bigfloat;
  rt->bigfloat_ops.compare = js_compare_bigfloat;
  rt->bigfloat_ops.mul_pow10_to_float64 = js_mul_pow10_to_float64;
  rt->bigfloat_ops.mul_pow10 = js_mul_pow10;

  ctx->class_proto[JS_CLASS_BIG_FLOAT] = JS_NewObject(ctx);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_BIG_FLOAT],
                             js_bigfloat_proto_funcs,
                             countof(js_bigfloat_proto_funcs));
  obj1 = JS_NewGlobalCConstructor(ctx, "BigFloat", js_bigfloat_constructor, 1,
                                  ctx->class_proto[JS_CLASS_BIG_FLOAT]);
  JS_SetPropertyFunctionList(ctx, obj1, js_bigfloat_funcs,
                             countof(js_bigfloat_funcs));

  ctx->class_proto[JS_CLASS_FLOAT_ENV] = JS_NewObject(ctx);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_FLOAT_ENV],
                             js_float_env_proto_funcs,
                             countof(js_float_env_proto_funcs));
  obj1 =
      JS_NewGlobalCConstructorOnly(ctx, "BigFloatEnv", js_float_env_constructor,
                                   1, ctx->class_proto[JS_CLASS_FLOAT_ENV]);
  JS_SetPropertyFunctionList(ctx, obj1, js_float_env_funcs,
                             countof(js_float_env_funcs));
}

/* -- BigDecimal ----------------------------------- */

JSValue JS_ToBigDecimalFree(JSContext *ctx, JSValue val,
                            BOOL allow_null_or_undefined) {
redo:
  switch (JS_VALUE_GET_NORM_TAG(val)) {
  case JS_TAG_BIG_DECIMAL:
    break;
  case JS_TAG_NULL:
    if (!allow_null_or_undefined)
      goto fail;
    /* fall thru */
  case JS_TAG_BOOL:
  case JS_TAG_INT: {
    bfdec_t *r;
    int32_t v = JS_VALUE_GET_INT(val);

    val = JS_NewBigDecimal(ctx);
    if (JS_IsException(val))
      break;
    r = JS_GetBigDecimal(val);
    if (bfdec_set_si(r, v)) {
      JS_FreeValue(ctx, val);
      val = JS_EXCEPTION;
      break;
    }
  } break;
  case JS_TAG_FLOAT64:
  case JS_TAG_BIG_INT:
  case JS_TAG_BIG_FLOAT:
    val = JS_ToStringFree(ctx, val);
    if (JS_IsException(val))
      break;
    goto redo;
  case JS_TAG_STRING: {
    const char *str, *p;
    size_t len;
    int err;

    str = JS_ToCStringLen(ctx, &len, val);
    JS_FreeValue(ctx, val);
    if (!str)
      return JS_EXCEPTION;
    p = str;
    p += skip_spaces(p);
    if ((p - str) == len) {
      bfdec_t *r;
      val = JS_NewBigDecimal(ctx);
      if (JS_IsException(val))
        break;
      r = JS_GetBigDecimal(val);
      bfdec_set_zero(r, 0);
      err = 0;
    } else {
      val = js_atof(ctx, p, &p, 0, ATOD_TYPE_BIG_DECIMAL);
      if (JS_IsException(val)) {
        JS_FreeCString(ctx, str);
        return JS_EXCEPTION;
      }
      p += skip_spaces(p);
      err = ((p - str) != len);
    }
    JS_FreeCString(ctx, str);
    if (err) {
      JS_FreeValue(ctx, val);
      return JS_ThrowSyntaxError(ctx, "invalid bigdecimal literal");
    }
  } break;
  case JS_TAG_OBJECT:
    val = JS_ToPrimitiveFree(ctx, val, HINT_NUMBER);
    if (JS_IsException(val))
      break;
    goto redo;
  case JS_TAG_UNDEFINED: {
    bfdec_t *r;
    if (!allow_null_or_undefined)
      goto fail;
    val = JS_NewBigDecimal(ctx);
    if (JS_IsException(val))
      break;
    r = JS_GetBigDecimal(val);
    bfdec_set_nan(r);
  } break;
  default:
  fail:
    JS_FreeValue(ctx, val);
    return JS_ThrowTypeError(ctx, "cannot convert to bigdecimal");
  }
  return val;
}

static JSValue js_bigdecimal_constructor(JSContext *ctx,
                                         JSValueConst new_target, int argc,
                                         JSValueConst *argv) {
  JSValue val;
  if (!JS_IsUndefined(new_target))
    return JS_ThrowTypeError(ctx, "not a constructor");
  if (argc == 0) {
    bfdec_t *r;
    val = JS_NewBigDecimal(ctx);
    if (JS_IsException(val))
      return val;
    r = JS_GetBigDecimal(val);
    bfdec_set_zero(r, 0);
  } else {
    val = JS_ToBigDecimalFree(ctx, JS_DupValue(ctx, argv[0]), FALSE);
  }
  return val;
}

static JSValue js_thisBigDecimalValue(JSContext *ctx, JSValueConst this_val) {
  if (JS_IsBigDecimal(this_val))
    return JS_DupValue(ctx, this_val);

  if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
    JSObject *p = JS_VALUE_GET_OBJ(this_val);
    if (p->class_id == JS_CLASS_BIG_DECIMAL) {
      if (JS_IsBigDecimal(p->u.object_data))
        return JS_DupValue(ctx, p->u.object_data);
    }
  }
  return JS_ThrowTypeError(ctx, "not a bigdecimal");
}

static JSValue js_bigdecimal_toString(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv) {
  JSValue val;

  val = js_thisBigDecimalValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  return JS_ToStringFree(ctx, val);
}

static JSValue js_bigdecimal_valueOf(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv) {
  return js_thisBigDecimalValue(ctx, this_val);
}

static int js_bigdecimal_get_rnd_mode(JSContext *ctx, JSValueConst obj) {
  const char *str;
  size_t size;
  int rnd_mode;

  str = JS_ToCStringLen(ctx, &size, obj);
  if (!str)
    return -1;
  if (strlen(str) != size)
    goto invalid_rounding_mode;
  if (!strcmp(str, "floor")) {
    rnd_mode = BF_RNDD;
  } else if (!strcmp(str, "ceiling")) {
    rnd_mode = BF_RNDU;
  } else if (!strcmp(str, "down")) {
    rnd_mode = BF_RNDZ;
  } else if (!strcmp(str, "up")) {
    rnd_mode = BF_RNDA;
  } else if (!strcmp(str, "half-even")) {
    rnd_mode = BF_RNDN;
  } else if (!strcmp(str, "half-up")) {
    rnd_mode = BF_RNDNA;
  } else {
  invalid_rounding_mode:
    JS_FreeCString(ctx, str);
    JS_ThrowTypeError(ctx, "invalid rounding mode");
    return -1;
  }
  JS_FreeCString(ctx, str);
  return rnd_mode;
}

typedef struct {
  int64_t prec;
  bf_flags_t flags;
} BigDecimalEnv;

static int js_bigdecimal_get_env(JSContext *ctx, BigDecimalEnv *fe,
                                 JSValueConst obj) {
  JSValue prop;
  int64_t val;
  BOOL has_prec;
  int rnd_mode;

  if (!JS_IsObject(obj)) {
    JS_ThrowTypeErrorNotAnObject(ctx);
    return -1;
  }
  prop = JS_GetProperty(ctx, obj, JS_ATOM_roundingMode);
  if (JS_IsException(prop))
    return -1;
  rnd_mode = js_bigdecimal_get_rnd_mode(ctx, prop);
  JS_FreeValue(ctx, prop);
  if (rnd_mode < 0)
    return -1;
  fe->flags = rnd_mode;

  prop = JS_GetProperty(ctx, obj, JS_ATOM_maximumSignificantDigits);
  if (JS_IsException(prop))
    return -1;
  has_prec = FALSE;
  if (!JS_IsUndefined(prop)) {
    if (JS_ToInt64SatFree(ctx, &val, prop))
      return -1;
    if (val < 1 || val > BF_PREC_MAX)
      goto invalid_precision;
    fe->prec = val;
    has_prec = TRUE;
  }

  prop = JS_GetProperty(ctx, obj, JS_ATOM_maximumFractionDigits);
  if (JS_IsException(prop))
    return -1;
  if (!JS_IsUndefined(prop)) {
    if (has_prec) {
      JS_FreeValue(ctx, prop);
      JS_ThrowTypeError(ctx, "cannot provide both maximumSignificantDigits and "
                             "maximumFractionDigits");
      return -1;
    }
    if (JS_ToInt64SatFree(ctx, &val, prop))
      return -1;
    if (val < 0 || val > BF_PREC_MAX) {
    invalid_precision:
      JS_ThrowTypeError(ctx, "invalid precision");
      return -1;
    }
    fe->prec = val;
    fe->flags |= BF_FLAG_RADPNT_PREC;
    has_prec = TRUE;
  }
  if (!has_prec) {
    JS_ThrowTypeError(ctx, "precision must be present");
    return -1;
  }
  return 0;
}

static JSValue js_bigdecimal_fop(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic) {
  bfdec_t *a, *b, r_s, *r = &r_s;
  JSValue op1, op2, res;
  BigDecimalEnv fe_s, *fe = &fe_s;
  int op_count, ret;

  if (magic == MATH_OP_SQRT || magic == MATH_OP_ROUND)
    op_count = 1;
  else
    op_count = 2;

  op1 = JS_ToNumeric(ctx, argv[0]);
  if (JS_IsException(op1))
    return op1;
  a = JS_ToBigDecimal(ctx, op1);
  if (!a) {
    JS_FreeValue(ctx, op1);
    return JS_EXCEPTION;
  }
  if (op_count >= 2) {
    op2 = JS_ToNumeric(ctx, argv[1]);
    if (JS_IsException(op2)) {
      JS_FreeValue(ctx, op1);
      return op2;
    }
    b = JS_ToBigDecimal(ctx, op2);
    if (!b)
      goto fail;
  } else {
    op2 = JS_UNDEFINED;
    b = NULL;
  }
  fe->flags = BF_RNDZ;
  fe->prec = BF_PREC_INF;
  if (op_count < argc) {
    if (js_bigdecimal_get_env(ctx, fe, argv[op_count]))
      goto fail;
  }

  res = JS_NewBigDecimal(ctx);
  if (JS_IsException(res)) {
  fail:
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    return JS_EXCEPTION;
  }
  r = JS_GetBigDecimal(res);
  switch (magic) {
  case MATH_OP_ADD:
    ret = bfdec_add(r, a, b, fe->prec, fe->flags);
    break;
  case MATH_OP_SUB:
    ret = bfdec_sub(r, a, b, fe->prec, fe->flags);
    break;
  case MATH_OP_MUL:
    ret = bfdec_mul(r, a, b, fe->prec, fe->flags);
    break;
  case MATH_OP_DIV:
    ret = bfdec_div(r, a, b, fe->prec, fe->flags);
    break;
  case MATH_OP_FMOD:
    ret = bfdec_rem(r, a, b, fe->prec, fe->flags, BF_RNDZ);
    break;
  case MATH_OP_SQRT:
    ret = bfdec_sqrt(r, a, fe->prec, fe->flags);
    break;
  case MATH_OP_ROUND:
    ret = bfdec_set(r, a);
    if (!(ret & BF_ST_MEM_ERROR))
      ret = bfdec_round(r, fe->prec, fe->flags);
    break;
  default:
    abort();
  }
  JS_FreeValue(ctx, op1);
  JS_FreeValue(ctx, op2);
  ret &=
      BF_ST_MEM_ERROR | BF_ST_DIVIDE_ZERO | BF_ST_INVALID_OP | BF_ST_OVERFLOW;
  if (ret != 0) {
    JS_FreeValue(ctx, res);
    return throw_bf_exception(ctx, ret);
  } else {
    return res;
  }
}

static JSValue js_bigdecimal_toFixed(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv) {
  JSValue val, ret;
  int64_t f;
  int rnd_mode;

  val = js_thisBigDecimalValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  if (JS_ToInt64Sat(ctx, &f, argv[0]))
    goto fail;
  if (f < 0 || f > BF_PREC_MAX) {
    JS_ThrowRangeError(ctx, "invalid number of digits");
    goto fail;
  }
  rnd_mode = BF_RNDNA;
  if (argc > 1) {
    rnd_mode = js_bigdecimal_get_rnd_mode(ctx, argv[1]);
    if (rnd_mode < 0)
      goto fail;
  }
  ret = js_bigdecimal_to_string1(ctx, val, f, rnd_mode | BF_FTOA_FORMAT_FRAC);
  JS_FreeValue(ctx, val);
  return ret;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static JSValue js_bigdecimal_toExponential(JSContext *ctx,
                                           JSValueConst this_val, int argc,
                                           JSValueConst *argv) {
  JSValue val, ret;
  int64_t f;
  int rnd_mode;

  val = js_thisBigDecimalValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  if (JS_ToInt64Sat(ctx, &f, argv[0]))
    goto fail;
  if (JS_IsUndefined(argv[0])) {
    ret = js_bigdecimal_to_string1(
        ctx, val, 0, BF_RNDN | BF_FTOA_FORMAT_FREE_MIN | BF_FTOA_FORCE_EXP);
  } else {
    if (f < 0 || f > BF_PREC_MAX) {
      JS_ThrowRangeError(ctx, "invalid number of digits");
      goto fail;
    }
    rnd_mode = BF_RNDNA;
    if (argc > 1) {
      rnd_mode = js_bigdecimal_get_rnd_mode(ctx, argv[1]);
      if (rnd_mode < 0)
        goto fail;
    }
    ret = js_bigdecimal_to_string1(
        ctx, val, f + 1, rnd_mode | BF_FTOA_FORMAT_FIXED | BF_FTOA_FORCE_EXP);
  }
  JS_FreeValue(ctx, val);
  return ret;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static JSValue js_bigdecimal_toPrecision(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv) {
  JSValue val, ret;
  int64_t p;
  int rnd_mode;

  val = js_thisBigDecimalValue(ctx, this_val);
  if (JS_IsException(val))
    return val;
  if (JS_IsUndefined(argv[0])) {
    return JS_ToStringFree(ctx, val);
  }
  if (JS_ToInt64Sat(ctx, &p, argv[0]))
    goto fail;
  if (p < 1 || p > BF_PREC_MAX) {
    JS_ThrowRangeError(ctx, "invalid number of digits");
    goto fail;
  }
  rnd_mode = BF_RNDNA;
  if (argc > 1) {
    rnd_mode = js_bigdecimal_get_rnd_mode(ctx, argv[1]);
    if (rnd_mode < 0)
      goto fail;
  }
  ret = js_bigdecimal_to_string1(ctx, val, p, rnd_mode | BF_FTOA_FORMAT_FIXED);
  JS_FreeValue(ctx, val);
  return ret;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}

static const JSCFunctionListEntry js_bigdecimal_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_bigdecimal_toString),
    JS_CFUNC_DEF("valueOf", 0, js_bigdecimal_valueOf),
    JS_CFUNC_DEF("toPrecision", 1, js_bigdecimal_toPrecision),
    JS_CFUNC_DEF("toFixed", 1, js_bigdecimal_toFixed),
    JS_CFUNC_DEF("toExponential", 1, js_bigdecimal_toExponential),
};

static const JSCFunctionListEntry js_bigdecimal_funcs[] = {
    JS_CFUNC_MAGIC_DEF("add", 2, js_bigdecimal_fop, MATH_OP_ADD),
    JS_CFUNC_MAGIC_DEF("sub", 2, js_bigdecimal_fop, MATH_OP_SUB),
    JS_CFUNC_MAGIC_DEF("mul", 2, js_bigdecimal_fop, MATH_OP_MUL),
    JS_CFUNC_MAGIC_DEF("div", 2, js_bigdecimal_fop, MATH_OP_DIV),
    JS_CFUNC_MAGIC_DEF("mod", 2, js_bigdecimal_fop, MATH_OP_FMOD),
    JS_CFUNC_MAGIC_DEF("round", 1, js_bigdecimal_fop, MATH_OP_ROUND),
    JS_CFUNC_MAGIC_DEF("sqrt", 1, js_bigdecimal_fop, MATH_OP_SQRT),
};

void JS_AddIntrinsicBigDecimal(JSContext *ctx) {
  JSRuntime *rt = ctx->rt;
  JSValueConst obj1;

  rt->bigdecimal_ops.to_string = js_bigdecimal_to_string;
  rt->bigdecimal_ops.from_string = js_string_to_bigdecimal;
  rt->bigdecimal_ops.unary_arith = js_unary_arith_bigdecimal;
  rt->bigdecimal_ops.binary_arith = js_binary_arith_bigdecimal;
  rt->bigdecimal_ops.compare = js_compare_bigdecimal;

  ctx->class_proto[JS_CLASS_BIG_DECIMAL] = JS_NewObject(ctx);
  JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_BIG_DECIMAL],
                             js_bigdecimal_proto_funcs,
                             countof(js_bigdecimal_proto_funcs));
  obj1 = JS_NewGlobalCConstructor(ctx, "BigDecimal", js_bigdecimal_constructor,
                                  1, ctx->class_proto[JS_CLASS_BIG_DECIMAL]);
  JS_SetPropertyFunctionList(ctx, obj1, js_bigdecimal_funcs,
                             countof(js_bigdecimal_funcs));
}

void JS_EnableBignumExt(JSContext *ctx, BOOL enable) {
  ctx->bignum_ext = enable;
}

#endif /* CONFIG_BIGNUM */