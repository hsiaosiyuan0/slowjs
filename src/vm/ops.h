#ifndef QUICKJS_OP_H
#define QUICKJS_OP_H

#include "def.h"

#ifdef CONFIG_BIGNUM

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

typedef enum JSStrictEqModeEnum {
  JS_EQ_STRICT,
  JS_EQ_SAME_VALUE,
  JS_EQ_SAME_VALUE_ZERO,
} JSStrictEqModeEnum;

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