#ifndef QUICKJS_OP_H
#define QUICKJS_OP_H

#include "def.h"

typedef enum JSStrictEqModeEnum {
  JS_EQ_STRICT,
  JS_EQ_SAME_VALUE,
  JS_EQ_SAME_VALUE_ZERO,
} JSStrictEqModeEnum;

typedef enum {
  /* binary operators */
  JS_OVOP_ADD,
  JS_OVOP_SUB,
  JS_OVOP_MUL,
  JS_OVOP_DIV,
  JS_OVOP_MOD,
  JS_OVOP_POW,
  JS_OVOP_OR,
  JS_OVOP_AND,
  JS_OVOP_XOR,
  JS_OVOP_SHL,
  JS_OVOP_SAR,
  JS_OVOP_SHR,
  JS_OVOP_EQ,
  JS_OVOP_LESS,

  JS_OVOP_BINARY_COUNT,
  /* unary operators */
  JS_OVOP_POS = JS_OVOP_BINARY_COUNT,
  JS_OVOP_NEG,
  JS_OVOP_INC,
  JS_OVOP_DEC,
  JS_OVOP_NOT,

  JS_OVOP_COUNT,
} JSOverloadableOperatorEnum;

typedef struct {
  uint32_t operator_index;
  JSObject *ops[JS_OVOP_BINARY_COUNT]; /* self operators */
} JSBinaryOperatorDefEntry;

typedef struct {
  int count;
  JSBinaryOperatorDefEntry *tab;
} JSBinaryOperatorDef;

typedef struct {
  uint32_t operator_counter;
  BOOL is_primitive; /* OperatorSet for a primitive type */
  /* NULL if no operator is defined */
  JSObject *self_ops[JS_OVOP_COUNT]; /* self operators */
  JSBinaryOperatorDef left;
  JSBinaryOperatorDef right;
} JSOperatorSetData;

#ifdef CONFIG_BIGNUM
const char js_overloadable_operator_names[JS_OVOP_COUNT][4];
/* return NULL if not present */
JSObject *find_binary_op(JSBinaryOperatorDef *def, uint32_t operator_index,
                         JSOverloadableOperatorEnum op);
/* return -1 if exception, 0 if no operator overloading, 1 if
overloaded operator called */
__exception int js_call_binary_op_fallback(JSContext *ctx, JSValue *pret,
                                           JSValueConst op1, JSValueConst op2,
                                           OPCodeEnum op, BOOL is_numeric,
                                           int hint);
/* try to call the operation on the operatorSet field of 'obj'. Only
   used for "/" and "**" on the BigInt prototype in math mode */
__exception int js_call_binary_op_simple(JSContext *ctx, JSValue *pret,
                                         JSValueConst obj, JSValueConst op1,
                                         JSValueConst op2, OPCodeEnum op);
/* return -1 if exception, 0 if no operator overloading, 1 if
overloaded operator called */
__exception int js_call_unary_op_fallback(JSContext *ctx, JSValue *pret,
                                          JSValueConst op1, OPCodeEnum op);
JSValue throw_bf_exception(JSContext *ctx, int status);
int js_unary_arith_bigint(JSContext *ctx, JSValue *pres, OPCodeEnum op,
                          JSValue op1);
int js_unary_arith_bigfloat(JSContext *ctx, JSValue *pres, OPCodeEnum op,
                            JSValue op1);
int js_unary_arith_bigdecimal(JSContext *ctx, JSValue *pres, OPCodeEnum op,
                              JSValue op1);
no_inline __exception int js_unary_arith_slow(JSContext *ctx, JSValue *sp,
                                              OPCodeEnum op);
__exception int js_post_inc_slow(JSContext *ctx, JSValue *sp, OPCodeEnum op);
no_inline int js_not_slow(JSContext *ctx, JSValue *sp);
int js_binary_arith_bigfloat(JSContext *ctx, OPCodeEnum op, JSValue *pres,
                             JSValue op1, JSValue op2);
int js_binary_arith_bigint(JSContext *ctx, OPCodeEnum op, JSValue *pres,
                           JSValue op1, JSValue op2);
/* b must be a positive integer */
int js_bfdec_pow(bfdec_t *r, const bfdec_t *a, const bfdec_t *b);
int js_binary_arith_bigdecimal(JSContext *ctx, OPCodeEnum op, JSValue *pres,
                               JSValue op1, JSValue op2);
no_inline __exception int js_binary_arith_slow(JSContext *ctx, JSValue *sp,
                                               OPCodeEnum op);
no_inline __exception int js_add_slow(JSContext *ctx, JSValue *sp);
no_inline __exception int js_binary_logic_slow(JSContext *ctx, JSValue *sp,
                                               OPCodeEnum op);
/* Note: also used for bigint */
int js_compare_bigfloat(JSContext *ctx, OPCodeEnum op, JSValue op1,
                        JSValue op2);
int js_compare_bigdecimal(JSContext *ctx, OPCodeEnum op, JSValue op1,
                          JSValue op2);
no_inline int js_relational_slow(JSContext *ctx, JSValue *sp, OPCodeEnum op);
no_inline __exception int js_eq_slow(JSContext *ctx, JSValue *sp, BOOL is_neq);
no_inline int js_shr_slow(JSContext *ctx, JSValue *sp);
#else /* !CONFIG_BIGNUM */
no_inline __exception int js_unary_arith_slow(JSContext *ctx, JSValue *sp,
                                              OPCodeEnum op);

__exception int js_post_inc_slow(JSContext *ctx, JSValue *sp, OPCodeEnum op);
no_inline __exception int js_binary_arith_slow(JSContext *ctx, JSValue *sp,
                                               OPCodeEnum op);
no_inline __exception int js_add_slow(JSContext *ctx, JSValue *sp);
no_inline __exception int js_binary_logic_slow(JSContext *ctx, JSValue *sp,
                                               OPCodeEnum op);
no_inline int js_not_slow(JSContext *ctx, JSValue *sp);
no_inline int js_relational_slow(JSContext *ctx, JSValue *sp, OPCodeEnum op);
no_inline __exception int js_eq_slow(JSContext *ctx, JSValue *sp, BOOL is_neq);
no_inline int js_shr_slow(JSContext *ctx, JSValue *sp);

#endif /* !CONFIG_BIGNUM */

/* XXX: Should take JSValueConst arguments */
BOOL js_strict_eq2(JSContext *ctx, JSValue op1, JSValue op2,
                   JSStrictEqModeEnum eq_mode);
BOOL js_strict_eq(JSContext *ctx, JSValue op1, JSValue op2);
BOOL js_same_value(JSContext *ctx, JSValueConst op1, JSValueConst op2);
BOOL js_same_value_zero(JSContext *ctx, JSValueConst op1, JSValueConst op2);
no_inline int js_strict_eq_slow(JSContext *ctx, JSValue *sp, BOOL is_neq);

__exception int js_operator_in(JSContext *ctx, JSValue *sp);
__exception int js_has_unscopable(JSContext *ctx, JSValueConst obj,
                                  JSAtom atom);
__exception int js_operator_instanceof(JSContext *ctx, JSValue *sp);
__exception int js_operator_typeof(JSContext *ctx, JSValueConst op1);
__exception int js_operator_delete(JSContext *ctx, JSValue *sp);

#define JS_DEFINE_CLASS_HAS_HERITAGE (1 << 0)

int js_op_define_class(JSContext *ctx, JSValue *sp, JSAtom class_name,
                       int class_flags, JSVarRef **cur_var_refs,
                       JSStackFrame *sf, BOOL is_computed_name);
__exception int js_append_enumerate(JSContext *ctx, JSValue *sp);

#endif