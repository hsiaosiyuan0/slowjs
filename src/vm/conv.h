#ifndef QUICKJS_CONV_H
#define QUICKJS_CONV_H

#include "def.h"

/* -- ToPrimitive ----------------------------------- */

#define HINT_STRING 0
#define HINT_NUMBER 1
#define HINT_NONE 2
/* don't try Symbol.toPrimitive */
#define HINT_FORCE_ORDINARY (1 << 4)
JSValue JS_ToPrimitiveFree(JSContext *ctx, JSValue val, int hint);
JSValue JS_ToPrimitive(JSContext *ctx, JSValueConst val, int hint);

/* -- ToString ----------------------------------- */

JSValue JS_ToStringFree(JSContext *ctx, JSValue val);
JSValue JS_ToLocaleStringFree(JSContext *ctx, JSValue val);
JSValue JS_ToStringCheckObject(JSContext *ctx, JSValueConst val);
JSValue JS_ToQuotedString(JSContext *ctx, JSValueConst val1);

JSValue JS_ToPropertyKey(JSContext *ctx, JSValueConst val);

/* return (NULL, 0) if exception. */
/* return pointer into a JSString with a live ref_count */
/* cesu8 determines if non-BMP1 codepoints are encoded as 1 or 2 utf-8 sequences
 */
const char *JS_ToCStringLen2(JSContext *ctx, size_t *plen, JSValueConst val1,
                             BOOL cesu8);
void JS_FreeCString(JSContext *ctx, const char *ptr);

JSValue js_dtoa(JSContext *ctx, double d, int radix, int n_digits, int flags);

#ifdef CONFIG_BIGNUM
JSValue js_bigint_to_string1(JSContext *ctx, JSValueConst val, int radix);
JSValue js_bigint_to_string(JSContext *ctx, JSValueConst val);
JSValue js_ftoa(JSContext *ctx, JSValueConst val1, int radix, limb_t prec,
                bf_flags_t flags);
JSValue js_bigfloat_to_string(JSContext *ctx, JSValueConst val);
JSValue js_bigdecimal_to_string1(JSContext *ctx, JSValueConst val, limb_t prec,
                                 int flags);
JSValue js_bigdecimal_to_string(JSContext *ctx, JSValueConst val);
#endif

/* -- ToNumber ----------------------------------- */

#define ATOD_INT_ONLY (1 << 0)
/* accept Oo and Ob prefixes in addition to 0x prefix if radix = 0 */
#define ATOD_ACCEPT_BIN_OCT (1 << 2)
/* accept O prefix as octal if radix == 0 and properly formed (Annex B) */
#define ATOD_ACCEPT_LEGACY_OCTAL (1 << 4)
/* accept _ between digits as a digit separator */
#define ATOD_ACCEPT_UNDERSCORES (1 << 5)
/* allow a suffix to override the type */
#define ATOD_ACCEPT_SUFFIX (1 << 6)
/* default type */
#define ATOD_TYPE_MASK (3 << 7)
#define ATOD_TYPE_FLOAT64 (0 << 7)
#define ATOD_TYPE_BIG_INT (1 << 7)
#define ATOD_TYPE_BIG_FLOAT (2 << 7)
#define ATOD_TYPE_BIG_DECIMAL (3 << 7)
/* assume bigint mode: floats are parsed as integers if no decimal
   point nor exponent */
#define ATOD_MODE_BIGINT (1 << 9)
/* accept -0x1 */
#define ATOD_ACCEPT_PREFIX_AFTER_SIGN (1 << 10)

/* return an exception in case of memory error. Return JS_NAN if
   invalid syntax */
#ifdef CONFIG_BIGNUM
JSValue js_atof2(JSContext *ctx, const char *str, const char **pp, int radix,
                 int flags, slimb_t *pexponent);
#endif

JSValue js_atof(JSContext *ctx, const char *str, const char **pp, int radix,
                int flags);

JSValue JS_ToNumber(JSContext *ctx, JSValueConst val);
JSValue JS_ToNumberFree(JSContext *ctx, JSValue val);

/* same as JS_ToNumber() but return 0 in case of NaN/Undefined */
__maybe_unused JSValue JS_ToIntegerFree(JSContext *ctx, JSValue val);

JSValue JS_ToNumeric(JSContext *ctx, JSValueConst val);
JSValue JS_ToNumericFree(JSContext *ctx, JSValue val);

int JS_ToInt32Free(JSContext *ctx, int32_t *pres, JSValue val);
static inline int JS_ToUint32Free(JSContext *ctx, uint32_t *pres, JSValue val) {
  return JS_ToInt32Free(ctx, (int32_t *)pres, val);
}
int JS_ToInt32Clamp(JSContext *ctx, int *pres, JSValueConst val, int min,
                    int max, int min_offset);
int JS_ToInt32Sat(JSContext *ctx, int *pres, JSValueConst val);

int JS_ToUint8ClampFree(JSContext *ctx, int32_t *pres, JSValue val);

__exception int __JS_ToFloat64Free(JSContext *ctx, double *pres, JSValue val);
static inline int JS_ToFloat64Free(JSContext *ctx, double *pres, JSValue val) {
  uint32_t tag;

  tag = JS_VALUE_GET_TAG(val);
  if (tag <= JS_TAG_NULL) {
    *pres = JS_VALUE_GET_INT(val);
    return 0;
  } else if (JS_TAG_IS_FLOAT64(tag)) {
    *pres = JS_VALUE_GET_FLOAT64(val);
    return 0;
  } else {
    return __JS_ToFloat64Free(ctx, pres, val);
  }
}

int JS_ToInt64Sat(JSContext *ctx, int64_t *pres, JSValueConst val);
int JS_ToInt64Clamp(JSContext *ctx, int64_t *pres, JSValueConst val,
                    int64_t min, int64_t max, int64_t neg_offset);
int JS_ToInt64SatFree(JSContext *ctx, int64_t *pres, JSValue val);
int JS_ToIndex(JSContext *ctx, uint64_t *plen, JSValueConst val);
__exception int JS_ToLengthFree(JSContext *ctx, int64_t *plen, JSValue val);
__exception int JS_ToArrayLengthFree(JSContext *ctx, uint32_t *plen,
                                     JSValue val, BOOL is_array_ctor);

/* -- Big number  ----------------------------------- */

int JS_ToBigInt64(JSContext *ctx, int64_t *pres, JSValueConst val);

/* radix != 10 is only supported with flags = JS_DTOA_VAR_FORMAT */
/* use as many digits as necessary */
#define JS_DTOA_VAR_FORMAT (0 << 0)
/* use n_digits significant digits (1 <= n_digits <= 101) */
#define JS_DTOA_FIXED_FORMAT (1 << 0)
/* force fractional format: [-]dd.dd with n_digits fractional digits */
#define JS_DTOA_FRAC_FORMAT (2 << 0)
/* force exponential notation either in fixed or variable format */
#define JS_DTOA_FORCE_EXP (1 << 2)

#ifdef CONFIG_BIGNUM
bf_t *JS_ToBigInt(JSContext *ctx, bf_t *buf, JSValueConst val);
__maybe_unused JSValue JS_ToBigIntValueFree(JSContext *ctx, JSValue val);
/* free the bf_t allocated by JS_ToBigInt */
void JS_FreeBigInt(JSContext *ctx, bf_t *a, bf_t *buf);

/* if the returned bigfloat is allocated it is equal to
   'buf'. Otherwise it is a pointer to the bigfloat in 'val'. Return
   NULL in case of error. */
bf_t *JS_ToBigFloat(JSContext *ctx, bf_t *buf, JSValueConst val);
/* return NULL if invalid type */
bfdec_t *JS_ToBigDecimal(JSContext *ctx, JSValueConst val);
/* return NaN if bad bigint literal */
JSValue JS_StringToBigInt(JSContext *ctx, JSValue val);
JSValue JS_StringToBigIntErr(JSContext *ctx, JSValue val);
/* if the returned bigfloat is allocated it is equal to
   'buf'. Otherwise it is a pointer to the bigfloat in 'val'. */
bf_t *JS_ToBigIntFree(JSContext *ctx, bf_t *buf, JSValue val);

/* XXX: merge with JS_ToInt64Free with a specific flag */
int JS_ToBigInt64Free(JSContext *ctx, int64_t *pres, JSValue val);

JSValue js_string_to_bigint(JSContext *ctx, const char *buf, int radix,
                            int flags, slimb_t *pexponent);
JSValue js_string_to_bigfloat(JSContext *ctx, const char *buf, int radix,
                              int flags, slimb_t *pexponent);
JSValue js_string_to_bigdecimal(JSContext *ctx, const char *buf, int radix,
                                int flags, slimb_t *pexponent);
#endif

/* -- ToBool ----------------------------------- */

int JS_ToBoolFree(JSContext *ctx, JSValue val);

/* -- ToObject ----------------------------------- */

JSValue JS_ToObject(JSContext *ctx, JSValueConst val);
JSValue JS_ToObjectFree(JSContext *ctx, JSValue val);

#endif