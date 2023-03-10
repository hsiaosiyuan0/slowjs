#ifndef QUICKJS_FUNC_H
#define QUICKJS_FUNC_H

#include "def.h"
#include "gc.h"

typedef struct JSClosureVar {
  uint8_t is_local : 1;
  uint8_t is_arg : 1;
  uint8_t is_const : 1;
  uint8_t is_lexical : 1;
  uint8_t var_kind : 4; /* see JSVarKindEnum */
  /* 8 bits available */
  uint16_t var_idx; /* is_local = TRUE: index to a normal variable of the
                  parent function. otherwise: index to a closure
                  variable of the parent function */
  JSAtom var_name;
} JSClosureVar;

#define ARG_SCOPE_INDEX 1
#define ARG_SCOPE_END (-2)

typedef struct JSVarScope {
  int parent; /* index into fd->scopes of the enclosing scope */
  int first;  /* index into fd->vars of the last variable in this scope */
} JSVarScope;

typedef enum {
  /* XXX: add more variable kinds here instead of using bit fields */
  JS_VAR_NORMAL,
  JS_VAR_FUNCTION_DECL,     /* lexical var with function declaration */
  JS_VAR_NEW_FUNCTION_DECL, /* lexical var with async/generator
                               function declaration */
  JS_VAR_CATCH,
  JS_VAR_FUNCTION_NAME, /* function expression name */
  JS_VAR_PRIVATE_FIELD,
  JS_VAR_PRIVATE_METHOD,
  JS_VAR_PRIVATE_GETTER,
  JS_VAR_PRIVATE_SETTER,        /* must come after JS_VAR_PRIVATE_GETTER */
  JS_VAR_PRIVATE_GETTER_SETTER, /* must come after JS_VAR_PRIVATE_SETTER */
} JSVarKindEnum;

/* XXX: could use a different structure in bytecode functions to save
   memory */
typedef struct JSVarDef {
  JSAtom var_name;
  /* index into fd->scopes of this variable lexical scope */
  int scope_level;
  /* during compilation:
      - if scope_level = 0: scope in which the variable is defined
      - if scope_level != 0: index into fd->vars of the next
        variable in the same or enclosing lexical scope
     in a bytecode function:
     index into fd->vars of the next
     variable in the same or enclosing lexical scope
  */
  int scope_next;
  uint8_t is_const : 1;
  uint8_t is_lexical : 1;
  uint8_t is_captured : 1;
  uint8_t var_kind : 4; /* see JSVarKindEnum */
  /* only used during compilation: function pool index for lexical
     variables with var_kind =
     JS_VAR_FUNCTION_DECL/JS_VAR_NEW_FUNCTION_DECL or scope level of
     the definition of the 'var' variables (they have scope_level =
     0) */
  int func_pool_idx : 24; /* only used during compilation : index in
                             the constant pool for hoisted function
                             definition */
} JSVarDef;

/* -- Function ----------------------------------- */

#define JS_MAX_LOCAL_VARS 65536
#define JS_STACK_SIZE_MAX 65534

/* for the encoding of the pc2line table */
#define PC2LINE_BASE (-1)
#define PC2LINE_RANGE 5
#define PC2LINE_OP_FIRST 1
#define PC2LINE_DIFF_PC_MAX ((255 - PC2LINE_OP_FIRST) / PC2LINE_RANGE)

typedef enum JSFunctionKindEnum {
  JS_FUNC_NORMAL = 0,
  JS_FUNC_GENERATOR = (1 << 0),
  JS_FUNC_ASYNC = (1 << 1),
  JS_FUNC_ASYNC_GENERATOR = (JS_FUNC_GENERATOR | JS_FUNC_ASYNC),
} JSFunctionKindEnum;

typedef struct JSFunctionBytecode {
  JSGCObjectHeader header; /* must come first */
  uint8_t js_mode;
  uint8_t has_prototype : 1; /* true if a prototype field is necessary */
  uint8_t has_simple_parameter_list : 1;
  uint8_t is_derived_class_constructor : 1;
  /* true if home_object needs to be initialized */
  uint8_t need_home_object : 1;
  uint8_t func_kind : 2;
  uint8_t new_target_allowed : 1;
  uint8_t super_call_allowed : 1;
  uint8_t super_allowed : 1;
  uint8_t arguments_allowed : 1;
  uint8_t has_debug : 1;
  uint8_t backtrace_barrier : 1; /* stop backtrace on this function */
  uint8_t read_only_bytecode : 1;
  /* XXX: 4 bits available */
  uint8_t *byte_code_buf; /* (self pointer) */
  int byte_code_len;
  JSAtom func_name;

  /* arguments + local variables (arg_count + var_count) (self pointer) */
  JSVarDef *vardefs;

  /* list of variables in the closure (self pointer) */
  JSClosureVar *closure_var;

  uint16_t arg_count;
  uint16_t var_count;
  uint16_t defined_arg_count; /* for length function property */
  uint16_t stack_size;        /* maximum stack size */
  JSContext *realm;           /* function realm */
  JSValue *cpool;             /* constant pool (self pointer) */
  int cpool_count;
  int closure_var_count;
  struct {
    /* debug info, move to separate structure to save memory? */
    JSAtom filename;
    int line_num;
    int source_len;
    int pc2line_len;
    uint8_t *pc2line_buf;
    char *source;
  } debug;
} JSFunctionBytecode;

typedef struct JSBoundFunction {
  JSValue func_obj;
  JSValue this_val;
  int argc;
  JSValue argv[0];
} JSBoundFunction;

typedef struct JSAsyncFunctionState {
  JSValue this_val; /* 'this' generator argument */
  int argc;         /* number of function arguments */
  BOOL throw_flag;  /* used to throw an exception in JS_CallInternal() */
  JSStackFrame frame;
} JSAsyncFunctionState;

/* XXX: could use an object instead to avoid the
   JS_TAG_ASYNC_FUNCTION tag for the GC */
typedef struct JSAsyncFunctionData {
  JSGCObjectHeader header; /* must come first */
  JSValue resolving_funcs[2];
  BOOL is_active; /* true if the async function state is valid */
  JSAsyncFunctionState func_state;
} JSAsyncFunctionData;

extern const uint16_t func_kind_to_class_id[4];

/* -- Arguments ----------------------------------- */

extern const JSClassExoticMethods js_arguments_exotic_methods;

int js_arguments_define_own_property(JSContext *ctx, JSValueConst this_obj,
                                     JSAtom prop, JSValueConst val,
                                     JSValueConst getter, JSValueConst setter,
                                     int flags);
JSValue js_build_arguments(JSContext *ctx, int argc, JSValueConst *argv);
JSValue js_build_mapped_arguments(JSContext *ctx, int argc, JSValueConst *argv,
                                  JSStackFrame *sf, int arg_count);
JSValue js_build_rest(JSContext *ctx, int first, int argc, JSValueConst *argv);
void free_arg_list(JSContext *ctx, JSValue *tab, uint32_t len);
JSValue *build_arg_list(JSContext *ctx, uint32_t *plen, JSValueConst array_arg);

/* -- Call ----------------------------------- */

#define JS_CALL_FLAG_CONSTRUCTOR (1 << 0)
#define JS_CALL_FLAG_COPY_ARGV (1 << 1)
#define JS_CALL_FLAG_GENERATOR (1 << 2)

JSValue JS_Call(JSContext *ctx, JSValueConst func_obj, JSValueConst this_obj,
                int argc, JSValueConst *argv);
JSValue JS_CallFree(JSContext *ctx, JSValue func_obj, JSValueConst this_obj,
                    int argc, JSValueConst *argv);

JSContext *JS_GetFunctionRealm(JSContext *ctx, JSValueConst func_obj);
JSValue js_create_from_ctor(JSContext *ctx, JSValueConst ctor, int class_id);

JSValue JS_CallConstructorInternal(JSContext *ctx, JSValueConst func_obj,
                                   JSValueConst new_target, int argc,
                                   JSValue *argv, int flags);
JSValue JS_CallConstructor2(JSContext *ctx, JSValueConst func_obj,
                            JSValueConst new_target, int argc,
                            JSValueConst *argv);
JSValue JS_CallConstructor(JSContext *ctx, JSValueConst func_obj, int argc,
                           JSValueConst *argv);
JSValue JS_Invoke(JSContext *ctx, JSValueConst this_val, JSAtom atom, int argc,
                  JSValueConst *argv);
JSValue JS_InvokeFree(JSContext *ctx, JSValue this_val, JSAtom atom, int argc,
                      JSValueConst *argv);

#if !defined(CONFIG_STACK_CHECK)
/* no stack limitation */
static inline uintptr_t js_get_stack_pointer(void) { return 0; }

static inline BOOL js_check_stack_overflow(JSRuntime *rt, size_t alloca_size) {
  return FALSE;
}
#else
/* Note: OS and CPU dependent */
static inline uintptr_t js_get_stack_pointer(void) {
  return (uintptr_t)__builtin_frame_address(0);
}

static inline BOOL js_check_stack_overflow(JSRuntime *rt, size_t alloca_size) {
  uintptr_t sp;
  sp = js_get_stack_pointer() - alloca_size;
  return unlikely(sp < rt->stack_limit);
}
#endif

int check_function(JSContext *ctx, JSValueConst obj);
JSValue js_function_apply(JSContext *ctx, JSValueConst this_val, int argc,
                          JSValueConst *argv, int magic);
JSValue js_call_bound_function(JSContext *ctx, JSValueConst func_obj,
                               JSValueConst this_obj, int argc,
                               JSValueConst *argv, int flags);
JSValue js_call_c_function(JSContext *ctx, JSValueConst func_obj,
                           JSValueConst this_obj, int argc, JSValueConst *argv,
                           int flags);

/* -- Function utils ----------------------------------- */

int find_line_num(JSContext *ctx, JSFunctionBytecode *b, uint32_t pc_value);
const char *get_func_name(JSContext *ctx, JSValueConst func);

/* return NULL without exception if not a function or no bytecode */
JSFunctionBytecode *JS_GetFunctionBytecode(JSValueConst val);

BOOL JS_IsFunction(JSContext *ctx, JSValueConst val);
BOOL JS_IsCFunction(JSContext *ctx, JSValueConst val, JSCFunction *func,
                    int magic);
BOOL JS_IsConstructor(JSContext *ctx, JSValueConst val);
BOOL JS_SetConstructorBit(JSContext *ctx, JSValueConst func_obj, BOOL val);

JSValue js_get_function_name(JSContext *ctx, JSAtom name);
void js_function_set_properties(JSContext *ctx, JSValueConst func_obj,
                                JSAtom name, int len);
void js_method_set_home_object(JSContext *ctx, JSValueConst func_obj,
                               JSValueConst home_obj);
int js_method_set_properties(JSContext *ctx, JSValueConst func_obj, JSAtom name,
                             int flags, JSValueConst home_obj);

/* -- Closure ----------------------------------- */

JSVarRef *get_var_ref(JSContext *ctx, JSStackFrame *sf, int var_idx,
                      BOOL is_arg);

JSValue js_closure2(JSContext *ctx, JSValue func_obj, JSFunctionBytecode *b,
                    JSVarRef **cur_var_refs, JSStackFrame *sf);
JSValue js_closure(JSContext *ctx, JSValue bfunc, JSVarRef **cur_var_refs,
                   JSStackFrame *sf);

void close_var_refs(JSRuntime *rt, JSStackFrame *sf);
void close_lexical_var(JSContext *ctx, JSStackFrame *sf, int idx, int is_arg);

/* -- AsyncFunction ----------------------------------- */

/* JSAsyncFunctionState (used by generator and async functions) */
__exception int async_func_init(JSContext *ctx, JSAsyncFunctionState *s,
                                JSValueConst func_obj, JSValueConst this_obj,
                                int argc, JSValueConst *argv);
void js_async_function_terminate(JSRuntime *rt, JSAsyncFunctionData *s);
void js_async_function_free0(JSRuntime *rt, JSAsyncFunctionData *s);
void js_async_function_free(JSRuntime *rt, JSAsyncFunctionData *s);
void js_async_function_resolve_finalizer(JSRuntime *rt, JSValue val);
void js_async_function_resolve_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func);
int js_async_function_resolve_create(JSContext *ctx, JSAsyncFunctionData *s,
                                     JSValue *resolving_funcs);
void js_async_function_resume(JSContext *ctx, JSAsyncFunctionData *s);
JSValue js_async_function_resolve_call(JSContext *ctx, JSValueConst func_obj,
                                       JSValueConst this_obj, int argc,
                                       JSValueConst *argv, int flags);
JSValue js_async_function_call(JSContext *ctx, JSValueConst func_obj,
                               JSValueConst this_obj, int argc,
                               JSValueConst *argv, int flags);

JSValue async_func_resume(JSContext *ctx, JSAsyncFunctionState *s);
void async_func_free(JSRuntime *rt, JSAsyncFunctionState *s);
void async_func_mark(JSRuntime *rt, JSAsyncFunctionState *s,
                     JS_MarkFunc *mark_func);
void async_func_walk(JSRuntime *rt, JSAsyncFunctionState *s,
                     JS_WalkFunc *walk_func, void *uctx);

#endif