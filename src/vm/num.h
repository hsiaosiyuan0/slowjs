#ifndef QUICKJS_NUM_H
#define QUICKJS_NUM_H

#include "def.h"

#ifdef CONFIG_BIGNUM
/* function pointers are used for numeric operations so that it is
   possible to remove some numeric types */
typedef struct {
  JSValue (*to_string)(JSContext *ctx, JSValueConst val);
  JSValue (*from_string)(JSContext *ctx, const char *buf, int radix, int flags,
                         slimb_t *pexponent);
  int (*unary_arith)(JSContext *ctx, JSValue *pres, OPCodeEnum op, JSValue op1);
  int (*binary_arith)(JSContext *ctx, OPCodeEnum op, JSValue *pres, JSValue op1,
                      JSValue op2);
  int (*compare)(JSContext *ctx, OPCodeEnum op, JSValue op1, JSValue op2);
  /* only for bigfloat: */
  JSValue (*mul_pow10_to_float64)(JSContext *ctx, const bf_t *a,
                                  int64_t exponent);
  int (*mul_pow10)(JSContext *ctx, JSValue *sp);
} JSNumericOperations;
#endif

typedef union JSFloat64Union {
  double d;
  uint64_t u64;
  uint32_t u32[2];
} JSFloat64Union;

#ifdef CONFIG_BIGNUM
typedef struct JSFloatEnv {
  limb_t prec;
  bf_flags_t flags;
  unsigned int status;
} JSFloatEnv;

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
#endif

#ifdef CONFIG_BIGNUM

JSValue JS_NewBigInt64_1(JSContext *ctx, int64_t v);
JSValue JS_NewBigInt64(JSContext *ctx, int64_t v);
JSValue JS_NewBigUint64(JSContext *ctx, uint64_t v);

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

#endif