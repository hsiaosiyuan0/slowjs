#ifndef QUICKJS_VM_H
#define QUICKJS_VM_H

#include "def.h"

/* -- JSContext --------------------------------- */

JSContext *JS_NewContextRaw(JSRuntime *rt);
JSContext *JS_NewContext(JSRuntime *rt);
JSContext *JS_DupContext(JSContext *ctx);
void JS_FreeContext(JSContext *ctx);

JSRuntime *JS_GetRuntime(JSContext *ctx);

void JS_SetClassProto(JSContext *ctx, JSClassID class_id, JSValue obj);
JSValue JS_GetClassProto(JSContext *ctx, JSClassID class_id);

/* WARNING: obj is freed */
JSValue JS_Throw(JSContext *ctx, JSValue obj);
/* return the pending exception (cannot be called twice). */
JSValue JS_GetException(JSContext *ctx);

#ifdef CONFIG_BIGNUM
static inline BOOL is_math_mode(JSContext *ctx) {
  JSStackFrame *sf = ctx->rt->current_stack_frame;
  return (sf && (sf->js_mode & JS_MODE_MATH));
}
#endif

/* -- JSRuntime --------------------------------- */

void JS_SetRuntimeInfo(JSRuntime *rt, const char *s);
void JS_FreeRuntime(JSRuntime *rt);

void *JS_GetRuntimeOpaque(JSRuntime *rt);
void JS_SetRuntimeOpaque(JSRuntime *rt, void *opaque);

/* -- Global ----------------------------------- */

#define DEFINE_GLOBAL_LEX_VAR (1 << 7)
#define DEFINE_GLOBAL_FUNC_VAR (1 << 6)

/* flags is 0, DEFINE_GLOBAL_LEX_VAR or DEFINE_GLOBAL_FUNC_VAR */
/* XXX: could support exotic global object. */
int JS_CheckDefineGlobalVar(JSContext *ctx, JSAtom prop, int flags);

/* def_flags is (0, DEFINE_GLOBAL_LEX_VAR) |
   JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE */
/* XXX: could support exotic global object. */
int JS_DefineGlobalVar(JSContext *ctx, JSAtom prop, int def_flags);

/* 'def_flags' is 0 or JS_PROP_CONFIGURABLE. */
/* XXX: could support exotic global object. */
int JS_DefineGlobalFunction(JSContext *ctx, JSAtom prop, JSValueConst func,
                            int def_flags);

/* construct a reference to a global variable */
JSValue JS_GetGlobalVar(JSContext *ctx, JSAtom prop, BOOL throw_ref_error);

/* use for strict variable access: test if the variable exists */
int JS_GetGlobalVarRef(JSContext *ctx, JSAtom prop, JSValue *sp);

/* use for strict variable access: test if the variable exists */
int JS_CheckGlobalVar(JSContext *ctx, JSAtom prop);

/* flag = 0: normal variable write
   flag = 1: initialize lexical variable
   flag = 2: normal variable write, strict check was done before
*/
int JS_SetGlobalVar(JSContext *ctx, JSAtom prop, JSValue val, int flag);

/* -- JSStackFrame ----------------------------------- */

static inline BOOL is_strict_mode(JSContext *ctx) {
  JSStackFrame *sf = ctx->rt->current_stack_frame;
  return (sf && (sf->js_mode & JS_MODE_STRICT));
}

/* only valid inside C functions */
JSValueConst JS_GetActiveFunction(JSContext *ctx);

/* -- Evaluation ----------------------------------- */

/* must be large enough to have a negligible runtime cost and small
   enough to call the interrupt callback often. */
#define JS_INTERRUPT_COUNTER_INIT 10000

no_inline __exception int __js_poll_interrupts(JSContext *ctx);

static inline __exception int js_poll_interrupts(JSContext *ctx) {
  if (unlikely(--ctx->interrupt_counter <= 0)) {
    return __js_poll_interrupts(ctx);
  } else {
    return 0;
  }
}

#define FUNC_RET_AWAIT 0
#define FUNC_RET_YIELD 1
#define FUNC_RET_YIELD_STAR 2

JSValue JS_EvalObject(JSContext *ctx, JSValueConst this_obj, JSValueConst val,
                      int flags, int scope_idx);

/* Run the <eval> function of the module and of all its requested
   modules. */
JSValue js_evaluate_module(JSContext *ctx, JSModuleDef *m);

/* 'input' must be zero terminated i.e. input[input_len] = '\0'. */
JSValue __JS_EvalInternal(JSContext *ctx, JSValueConst this_obj,
                          const char *input, size_t input_len,
                          const char *filename, int flags, int scope_idx);

/* -- Pending job ----------------------------------- */

typedef struct JSJobEntry {
  struct list_head link;
  JSContext *ctx;
  JSJobFunc *job_func;
  int argc;
  JSValue argv[0];
} JSJobEntry;

/* return 0 if OK, < 0 if exception */
int JS_EnqueueJob(JSContext *ctx, JSJobFunc *job_func, int argc,
                  JSValueConst *argv);
BOOL JS_IsJobPending(JSRuntime *rt);
/* return < 0 if exception, 0 if no job pending, 1 if a job was
   executed successfully. the context of the job is stored in '*pctx' */
int JS_ExecutePendingJob(JSRuntime *rt, JSContext **pctx);

#endif