#ifndef QUICKJS_NUM_H
#define QUICKJS_NUM_H

#include "def.h"

typedef union JSFloat64Union {
  double d;
  uint64_t u64;
  uint32_t u32[2];
} JSFloat64Union;

#ifdef CONFIG_BIGNUM
/* the same structure is used for big integers and big floats. Big
   integers are never infinite or NaNs */
typedef struct JSBigFloat {
  JSRefCountHeader header; /* must come first, 32-bit */
  bf_t num;
} JSBigFloat;

typedef struct JSBigDecimal {
  JSRefCountHeader header; /* must come first, 32-bit */
  bfdec_t num;
} JSBigDecimal;

JSValue JS_NewBigFloat(JSContext *ctx);
static inline bf_t *JS_GetBigFloat(JSValueConst val) {
  JSBigFloat *p = JS_VALUE_GET_PTR(val);
  return &p->num;
}

JSValue JS_NewBigDecimal(JSContext *ctx);
static inline bfdec_t *JS_GetBigDecimal(JSValueConst val) {
  JSBigDecimal *p = JS_VALUE_GET_PTR(val);
  return &p->num;
}

JSValue JS_NewBigInt(JSContext *ctx);
static inline bf_t *JS_GetBigInt(JSValueConst val) {
  JSBigFloat *p = JS_VALUE_GET_PTR(val);
  return &p->num;
}

JSBigFloat *js_new_bf(JSContext *ctx);
JSValue JS_CompactBigInt1(JSContext *ctx, JSValue val,
                          BOOL convert_to_safe_integer);
JSValue JS_CompactBigInt(JSContext *ctx, JSValue val);
JSValue JS_NewBigInt64_1(JSContext *ctx, int64_t v);
JSValue JS_NewBigInt64(JSContext *ctx, int64_t v);
JSValue JS_NewBigUint64(JSContext *ctx, uint64_t v);

JSValue js_mul_pow10_to_float64(JSContext *ctx, const bf_t *a,
                                int64_t exponent);
no_inline int js_mul_pow10(JSContext *ctx, JSValue *sp);
#else
JSValue JS_ThrowUnsupportedBigint(JSContext *ctx);
JSValue JS_NewBigInt64(JSContext *ctx, int64_t v);
JSValue JS_NewBigUint64(JSContext *ctx, uint64_t v);
#endif /* !CONFIG_BIGNUM */

double js_pow(double a, double b);

/* Note: can return an exception */
int JS_NumberIsInteger(JSContext *ctx, JSValueConst val);
BOOL JS_NumberIsNegativeOrMinusZero(JSContext *ctx, JSValueConst val);

#define MAX_SAFE_INTEGER (((int64_t)1 << 53) - 1)

BOOL is_safe_integer(double d);

BOOL tag_is_number(uint32_t tag);

#endif