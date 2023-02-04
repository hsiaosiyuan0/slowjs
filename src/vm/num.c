#include "num.h"

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
