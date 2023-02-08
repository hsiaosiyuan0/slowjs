#include "num.h"
#include "vm.h"

#ifdef CONFIG_BIGNUM

JSBigFloat *js_new_bf(JSContext *ctx) {
  JSBigFloat *p;
  p = js_malloc(ctx, sizeof(*p));
  if (!p)
    return NULL;
  p->header.ref_count = 1;
  bf_init(ctx->bf_ctx, &p->num);
  return p;
}

JSValue JS_NewBigFloat(JSContext *ctx) {
  JSBigFloat *p;
  p = js_malloc(ctx, sizeof(*p));
  if (!p)
    return JS_EXCEPTION;
  p->header.ref_count = 1;
  bf_init(ctx->bf_ctx, &p->num);
  return JS_MKPTR(JS_TAG_BIG_FLOAT, p);
}

JSValue JS_NewBigDecimal(JSContext *ctx) {
  JSBigDecimal *p;
  p = js_malloc(ctx, sizeof(*p));
  if (!p)
    return JS_EXCEPTION;
  p->header.ref_count = 1;
  bfdec_init(ctx->bf_ctx, &p->num);
  return JS_MKPTR(JS_TAG_BIG_DECIMAL, p);
}

JSValue JS_NewBigInt(JSContext *ctx) {
  JSBigFloat *p;
  p = js_malloc(ctx, sizeof(*p));
  if (!p)
    return JS_EXCEPTION;
  p->header.ref_count = 1;
  bf_init(ctx->bf_ctx, &p->num);
  return JS_MKPTR(JS_TAG_BIG_INT, p);
}

JSValue JS_CompactBigInt1(JSContext *ctx, JSValue val,
                          BOOL convert_to_safe_integer) {
  int64_t v;
  bf_t *a;

  if (JS_VALUE_GET_TAG(val) != JS_TAG_BIG_INT)
    return val; /* fail safe */
  a = JS_GetBigInt(val);
  if (convert_to_safe_integer && bf_get_int64(&v, a, 0) == 0 &&
      v >= -MAX_SAFE_INTEGER && v <= MAX_SAFE_INTEGER) {
    JS_FreeValue(ctx, val);
    return JS_NewInt64(ctx, v);
  } else if (a->expn == BF_EXP_ZERO && a->sign) {
#ifndef NDEBUG
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
#endif
    assert(p->header.ref_count == 1);
    a->sign = 0;
  }
  return val;
}

/* Convert the big int to a safe integer if in math mode. normalize
   the zero representation. Could also be used to convert the bigint
   to a short bigint value. The reference count of the value must be
   1. Cannot fail */
JSValue JS_CompactBigInt(JSContext *ctx, JSValue val) {
  return JS_CompactBigInt1(ctx, val, is_math_mode(ctx));
}

JSValue JS_NewBigInt64_1(JSContext *ctx, int64_t v) {
  JSValue val;
  bf_t *a;
  val = JS_NewBigInt(ctx);
  if (JS_IsException(val))
    return val;
  a = JS_GetBigInt(val);
  if (bf_set_si(a, v)) {
    JS_FreeValue(ctx, val);
    return JS_ThrowOutOfMemory(ctx);
  }
  return val;
}

JSValue JS_NewBigInt64(JSContext *ctx, int64_t v) {
  if (is_math_mode(ctx) && v >= -MAX_SAFE_INTEGER && v <= MAX_SAFE_INTEGER) {
    return JS_NewInt64(ctx, v);
  } else {
    return JS_NewBigInt64_1(ctx, v);
  }
}

JSValue JS_NewBigUint64(JSContext *ctx, uint64_t v) {
  JSValue val;
  if (is_math_mode(ctx) && v <= MAX_SAFE_INTEGER) {
    val = JS_NewInt64(ctx, v);
  } else {
    bf_t *a;
    val = JS_NewBigInt(ctx);
    if (JS_IsException(val))
      return val;
    a = JS_GetBigInt(val);
    if (bf_set_ui(a, v)) {
      JS_FreeValue(ctx, val);
      return JS_ThrowOutOfMemory(ctx);
    }
  }
  return val;
}

bf_t *JS_ToBigFloat(JSContext *ctx, bf_t *buf, JSValueConst val);
JSValue js_mul_pow10_to_float64(JSContext *ctx, const bf_t *a,
                                int64_t exponent) {
  bf_t r_s, *r = &r_s;
  double d;
  int ret;

  /* always convert to Float64 */
  bf_init(ctx->bf_ctx, r);
  ret = bf_mul_pow_radix(r, a, 10, exponent, 53,
                         bf_set_exp_bits(11) | BF_RNDN | BF_FLAG_SUBNORMAL);
  bf_get_float64(r, &d, BF_RNDN);
  bf_delete(r);
  if (ret & BF_ST_MEM_ERROR)
    return JS_ThrowOutOfMemory(ctx);
  else
    return __JS_NewFloat64(ctx, d);
}

no_inline int js_mul_pow10(JSContext *ctx, JSValue *sp) {
  bf_t a_s, *a, *r;
  JSValue op1, op2, res;
  int64_t e;
  int ret;

  res = JS_NewBigFloat(ctx);
  if (JS_IsException(res))
    return -1;
  r = JS_GetBigFloat(res);
  op1 = sp[-2];
  op2 = sp[-1];
  a = JS_ToBigFloat(ctx, &a_s, op1);
  if (!a)
    return -1;
  if (JS_IsBigInt(ctx, op2)) {
    ret = JS_ToBigInt64(ctx, &e, op2);
  } else {
    ret = JS_ToInt64(ctx, &e, op2);
  }
  if (ret) {
    if (a == &a_s)
      bf_delete(a);
    JS_FreeValue(ctx, res);
    return -1;
  }

  bf_mul_pow_radix(r, a, 10, e, ctx->fp_env.prec, ctx->fp_env.flags);
  if (a == &a_s)
    bf_delete(a);
  JS_FreeValue(ctx, op1);
  JS_FreeValue(ctx, op2);
  sp[-2] = res;
  return 0;
}
#else

JSValue JS_ThrowUnsupportedBigint(JSContext *ctx) {
  return JS_ThrowTypeError(ctx, "bigint is not supported");
}

JSValue JS_NewBigInt64(JSContext *ctx, int64_t v) {
  return JS_ThrowUnsupportedBigint(ctx);
}

JSValue JS_NewBigUint64(JSContext *ctx, uint64_t v) {
  return JS_ThrowUnsupportedBigint(ctx);
}

#endif /* !CONFIG_BIGNUM */

double js_pow(double a, double b) {
  if (unlikely(!isfinite(b)) && fabs(a) == 1) {
    /* not compatible with IEEE 754 */
    return JS_FLOAT64_NAN;
  } else {
    return pow(a, b);
  }
}

/* Note: can return an exception */
int JS_NumberIsInteger(JSContext *ctx, JSValueConst val) {
  double d;
  if (!JS_IsNumber(val))
    return FALSE;
  if (unlikely(JS_ToFloat64(ctx, &d, val)))
    return -1;
  return isfinite(d) && floor(d) == d;
}

BOOL JS_NumberIsNegativeOrMinusZero(JSContext *ctx, JSValueConst val) {
  uint32_t tag;

  tag = JS_VALUE_GET_NORM_TAG(val);
  switch (tag) {
  case JS_TAG_INT: {
    int v;
    v = JS_VALUE_GET_INT(val);
    return (v < 0);
  }
  case JS_TAG_FLOAT64: {
    JSFloat64Union u;
    u.d = JS_VALUE_GET_FLOAT64(val);
    return (u.u64 >> 63);
  }
#ifdef CONFIG_BIGNUM
  case JS_TAG_BIG_INT: {
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    /* Note: integer zeros are not necessarily positive */
    return p->num.sign && !bf_is_zero(&p->num);
  }
  case JS_TAG_BIG_FLOAT: {
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    return p->num.sign;
  } break;
  case JS_TAG_BIG_DECIMAL: {
    JSBigDecimal *p = JS_VALUE_GET_PTR(val);
    return p->num.sign;
  } break;
#endif
  default:
    return FALSE;
  }
}

#define MAX_SAFE_INTEGER (((int64_t)1 << 53) - 1)

BOOL is_safe_integer(double d) {
  return isfinite(d) && floor(d) == d && fabs(d) <= (double)MAX_SAFE_INTEGER;
}

BOOL tag_is_number(uint32_t tag) {
  return (tag == JS_TAG_INT || tag == JS_TAG_BIG_INT || tag == JS_TAG_FLOAT64 ||
          tag == JS_TAG_BIG_FLOAT || tag == JS_TAG_BIG_DECIMAL);
}