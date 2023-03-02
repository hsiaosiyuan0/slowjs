#ifndef QUICKJS_DEF_H
#define QUICKJS_DEF_H

#include <assert.h>
#include <fenv.h>
#include <inttypes.h>
#include <math.h>
#if defined(__APPLE__)
#include <dispatch/dispatch.h>
#else
#include <semaphore.h>
#endif
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#if defined(__APPLE__)
#include <malloc/malloc.h>
#elif defined(__linux__)
#include <malloc.h>
#elif defined(__FreeBSD__)
#include <malloc_np.h>
#endif

#include "include/quickjs.h"
#include "libs/cutils.h"
#include "libs/libregexp.h"
#include "libs/list.h"
#ifdef CONFIG_BIGNUM
#include "libs/libbf.h"
#endif

#define OPTIMIZE 1
#define SHORT_OPCODES 1
#if defined(EMSCRIPTEN)
#define DIRECT_DISPATCH 0
#else
#define DIRECT_DISPATCH 1
#endif

#if defined(__APPLE__)
#define MALLOC_OVERHEAD 0
#else
#define MALLOC_OVERHEAD 8
#endif

#if !defined(_WIN32)
/* define it if printf uses the RNDN rounding mode instead of RNDNA */
#define CONFIG_PRINTF_RNDN
#endif

/* define to include Atomics.* operations which depend on the OS
   threads */
#if !defined(EMSCRIPTEN)
#define CONFIG_ATOMICS
#endif

#if !defined(EMSCRIPTEN)
/* enable stack limitation */
#define CONFIG_STACK_CHECK
#endif

/* test the GC by forcing it before each object allocation */
//#define FORCE_GC_AT_MALLOC

#ifdef CONFIG_ATOMICS
#include <errno.h>
#include <pthread.h>
#include <stdatomic.h>
#endif

#define __exception __attribute__((warn_unused_result))

typedef struct JSShape JSShape;
typedef struct JSString JSString;
typedef struct JSString JSAtomStruct;

typedef enum JSErrorEnum {
  JS_EVAL_ERROR,
  JS_RANGE_ERROR,
  JS_REFERENCE_ERROR,
  JS_SYNTAX_ERROR,
  JS_TYPE_ERROR,
  JS_URI_ERROR,
  JS_INTERNAL_ERROR,
  JS_AGGREGATE_ERROR,

  JS_NATIVE_ERROR_COUNT, /* number of different NativeError objects */
} JSErrorEnum;

typedef enum {
  JS_GC_PHASE_NONE,
  JS_GC_PHASE_DECREF,
  JS_GC_PHASE_REMOVE_CYCLES,
} JSGCPhaseEnum;

typedef enum OPCodeEnum OPCodeEnum;

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

struct JSRuntime {
  JSMallocFunctions mf;
  JSMallocState malloc_state;
  const char *rt_info;

  int atom_hash_size; /* power of two */
  int atom_count;
  int atom_size;
  int atom_count_resize; /* resize hash table at this count */
  uint32_t *atom_hash;
  JSAtomStruct **atom_array;
  int atom_free_index; /* 0 = none */

  int class_count; /* size of class_array */
  JSClass *class_array;

  struct list_head context_list; /* list of JSContext.link */
  /* list of JSGCObjectHeader.link. List of allocated GC objects (used
     by the garbage collector) */
  struct list_head gc_obj_list;
  /* list of JSGCObjectHeader.link. Used during JS_FreeValueRT() */
  struct list_head gc_zero_ref_count_list;
  struct list_head tmp_obj_list; /* used during GC */
  JSGCPhaseEnum gc_phase : 8;
  size_t malloc_gc_threshold;
#ifdef DUMP_LEAKS
  struct list_head string_list; /* list of JSString.link */
#endif
  /* stack limitation */
  uintptr_t stack_size; /* in bytes, 0 if no limit */
  uintptr_t stack_top;
  uintptr_t stack_limit; /* lower stack limit */

  JSValue current_exception;
  /* true if inside an out of memory error, to avoid recursing */
  BOOL in_out_of_memory : 8;

  struct JSStackFrame *current_stack_frame;

  JSInterruptHandler *interrupt_handler;
  void *interrupt_opaque;

  JSPcInterruptHandler *pc_interrupt_handler;
  void *pc_interrupt_opaque;

  JSHostPromiseRejectionTracker *host_promise_rejection_tracker;
  void *host_promise_rejection_tracker_opaque;

  struct list_head job_list; /* list of JSJobEntry.link */

  JSModuleNormalizeFunc *module_normalize_func;
  JSModuleLoaderFunc *module_loader_func;
  void *module_loader_opaque;

  BOOL can_block : 8; /* TRUE if Atomics.wait can block */
  /* used to allocate, free and clone SharedArrayBuffers */
  JSSharedArrayBufferFunctions sab_funcs;

  /* Shape hash table */
  int shape_hash_bits;
  int shape_hash_size;
  int shape_hash_count; /* number of hashed shapes */
  JSShape **shape_hash;
#ifdef CONFIG_BIGNUM
  bf_context_t bf_ctx;
  JSNumericOperations bigint_ops;
  JSNumericOperations bigfloat_ops;
  JSNumericOperations bigdecimal_ops;
  uint32_t operator_count;
#endif

  BOOL debug;

  void *user_opaque;
};

#define JS_MODE_STRICT (1 << 0)
#define JS_MODE_STRIP (1 << 1)
#define JS_MODE_MATH (1 << 2)

typedef struct JSStackFrame {
  struct JSStackFrame *prev_frame; /* NULL if first stack frame */
  JSValue
      cur_func; /* current function, JS_UNDEFINED if the frame is detached */
  JSValue *arg_buf;              /* arguments */
  JSValue *var_buf;              /* variables */
  struct list_head var_ref_list; /* list of JSVarRef.link */
  const uint8_t *cur_pc;         /* only used in bytecode functions : PC of the
                              instruction after the call */
  int arg_count;
  int js_mode; /* 0 or JS_MODE_MATH for C functions */
  /* only used in generators. Current stack pointer value. NULL if
     the function is running. */
  JSValue *cur_sp;
} JSStackFrame;

typedef enum {
  JS_GC_OBJ_TYPE_JS_OBJECT,
  JS_GC_OBJ_TYPE_FUNCTION_BYTECODE,
  JS_GC_OBJ_TYPE_SHAPE,
  JS_GC_OBJ_TYPE_VAR_REF,
  JS_GC_OBJ_TYPE_ASYNC_FUNCTION,
  JS_GC_OBJ_TYPE_JS_CONTEXT,
} JSGCObjectTypeEnum;

/* header for GC objects. GC objects are C data structures with a
   reference count that can reference other GC objects. JS Objects are
   a particular type of GC object. */
struct JSGCObjectHeader {
  int ref_count; /* must come first, 32-bit */
  JSGCObjectTypeEnum gc_obj_type : 4;
  uint8_t mark : 4; /* used by the GC */
  uint8_t dummy1;   /* not used by the GC */
  uint16_t dummy2;  /* not used by the GC */
  struct list_head link;
};

typedef struct JSVarRef {
  union {
    JSGCObjectHeader header; /* must come first */
    struct {
      int __gc_ref_count; /* corresponds to header.ref_count */
      uint8_t __gc_mark;  /* corresponds to header.mark/gc_obj_type */

      /* 0 : the JSVarRef is on the stack. header.link is an element
         of JSStackFrame.var_ref_list.
         1 : the JSVarRef is detached. header.link has the normal meaning
      */
      uint8_t is_detached : 1;
      uint8_t is_arg : 1;
      uint16_t var_idx; /* index of the corresponding function variable on
                           the stack */
    };
  };
  JSValue *pvalue; /* pointer to the value, either on the stack or
                      to 'value' */
  JSValue value;   /* used when the variable is no longer on the stack */
} JSVarRef;

#ifdef CONFIG_BIGNUM
typedef struct JSFloatEnv {
  limb_t prec;
  bf_flags_t flags;
  unsigned int status;
} JSFloatEnv;
#endif

typedef struct JSBreakpoint {
  struct list_head link;
  JSAtom file;
  int line;
  int col;
} JSBreakpoint;

struct JSContext {
  JSGCObjectHeader header; /* must come first */
  JSRuntime *rt;
  struct list_head link;

  uint16_t binary_object_count;
  int binary_object_size;

  JSShape *array_shape; /* initial shape for Array objects */

  JSValue *class_proto;
  JSValue function_proto;
  JSValue function_ctor;
  JSValue array_ctor;
  JSValue regexp_ctor;
  JSValue promise_ctor;
  JSValue native_error_proto[JS_NATIVE_ERROR_COUNT];
  JSValue iterator_proto;
  JSValue async_iterator_proto;
  JSValue array_proto_values;
  JSValue throw_type_error;
  JSValue eval_obj;

  JSValue global_obj;     /* global object */
  JSValue global_var_obj; /* contains the global let/const definitions */

  uint64_t random_state;
#ifdef CONFIG_BIGNUM
  bf_context_t *bf_ctx; /* points to rt->bf_ctx, shared by all contexts */
  JSFloatEnv fp_env;    /* global FP environment */
  BOOL bignum_ext : 8;  /* enable math mode */
  BOOL allow_operator_overloading : 8;
#endif
  /* when the counter reaches zero, JSRutime.interrupt_handler is called */
  int interrupt_counter;
  BOOL is_error_property_enabled;

  struct list_head loaded_modules; /* list of JSModuleDef.link */

  /* if NULL, RegExp compilation is not supported */
  JSValue (*compile_regexp)(JSContext *ctx, JSValueConst pattern,
                            JSValueConst flags);
  /* if NULL, eval is not supported */
  JSValue (*eval_internal)(JSContext *ctx, JSValueConst this_obj,
                           const char *input, size_t input_len,
                           const char *filename, int flags, int scope_idx);

  struct {
#if defined(__APPLE__)
    dispatch_semaphore_t ready2start;
#else
    sem_t ready2start;
#endif
    pthread_mutex_t bp_mutex;
    pthread_cond_t bp_cond;
    struct list_head bps;
    const char *entry;
    BOOL pause;
  } debug;

  void *user_opaque;
};

#endif