#ifndef QUICKJS_CLASS_H
#define QUICKJS_CLASS_H

#include "def.h"

#include "str.h"

enum {
  /* classid tag        */ /* union usage   | properties */
  JS_CLASS_OBJECT = 1,     /* must be first */
  JS_CLASS_ARRAY,          /* u.array       | length */
  JS_CLASS_ERROR,
  JS_CLASS_NUMBER,           /* u.object_data */
  JS_CLASS_STRING,           /* u.object_data */
  JS_CLASS_BOOLEAN,          /* u.object_data */
  JS_CLASS_SYMBOL,           /* u.object_data */
  JS_CLASS_ARGUMENTS,        /* u.array       | length */
  JS_CLASS_MAPPED_ARGUMENTS, /*               | length */
  JS_CLASS_DATE,             /* u.object_data */
  JS_CLASS_MODULE_NS,
  JS_CLASS_C_FUNCTION,          /* u.cfunc */
  JS_CLASS_BYTECODE_FUNCTION,   /* u.func */
  JS_CLASS_BOUND_FUNCTION,      /* u.bound_function */
  JS_CLASS_C_FUNCTION_DATA,     /* u.c_function_data_record */
  JS_CLASS_GENERATOR_FUNCTION,  /* u.func */
  JS_CLASS_FOR_IN_ITERATOR,     /* u.for_in_iterator */
  JS_CLASS_REGEXP,              /* u.regexp */
  JS_CLASS_ARRAY_BUFFER,        /* u.array_buffer */
  JS_CLASS_SHARED_ARRAY_BUFFER, /* u.array_buffer */
  JS_CLASS_UINT8C_ARRAY,        /* u.array (typed_array) */
  JS_CLASS_INT8_ARRAY,          /* u.array (typed_array) */
  JS_CLASS_UINT8_ARRAY,         /* u.array (typed_array) */
  JS_CLASS_INT16_ARRAY,         /* u.array (typed_array) */
  JS_CLASS_UINT16_ARRAY,        /* u.array (typed_array) */
  JS_CLASS_INT32_ARRAY,         /* u.array (typed_array) */
  JS_CLASS_UINT32_ARRAY,        /* u.array (typed_array) */
#ifdef CONFIG_BIGNUM
  JS_CLASS_BIG_INT64_ARRAY,  /* u.array (typed_array) */
  JS_CLASS_BIG_UINT64_ARRAY, /* u.array (typed_array) */
#endif
  JS_CLASS_FLOAT32_ARRAY, /* u.array (typed_array) */
  JS_CLASS_FLOAT64_ARRAY, /* u.array (typed_array) */
  JS_CLASS_DATAVIEW,      /* u.typed_array */
#ifdef CONFIG_BIGNUM
  JS_CLASS_BIG_INT,      /* u.object_data */
  JS_CLASS_BIG_FLOAT,    /* u.object_data */
  JS_CLASS_FLOAT_ENV,    /* u.float_env */
  JS_CLASS_BIG_DECIMAL,  /* u.object_data */
  JS_CLASS_OPERATOR_SET, /* u.operator_set */
#endif
  JS_CLASS_MAP,                      /* u.map_state */
  JS_CLASS_SET,                      /* u.map_state */
  JS_CLASS_WEAKMAP,                  /* u.map_state */
  JS_CLASS_WEAKSET,                  /* u.map_state */
  JS_CLASS_MAP_ITERATOR,             /* u.map_iterator_data */
  JS_CLASS_SET_ITERATOR,             /* u.map_iterator_data */
  JS_CLASS_ARRAY_ITERATOR,           /* u.array_iterator_data */
  JS_CLASS_STRING_ITERATOR,          /* u.array_iterator_data */
  JS_CLASS_REGEXP_STRING_ITERATOR,   /* u.regexp_string_iterator_data */
  JS_CLASS_GENERATOR,                /* u.generator_data */
  JS_CLASS_PROXY,                    /* u.proxy_data */
  JS_CLASS_PROMISE,                  /* u.promise_data */
  JS_CLASS_PROMISE_RESOLVE_FUNCTION, /* u.promise_function_data */
  JS_CLASS_PROMISE_REJECT_FUNCTION,  /* u.promise_function_data */
  JS_CLASS_ASYNC_FUNCTION,           /* u.func */
  JS_CLASS_ASYNC_FUNCTION_RESOLVE,   /* u.async_function_data */
  JS_CLASS_ASYNC_FUNCTION_REJECT,    /* u.async_function_data */
  JS_CLASS_ASYNC_FROM_SYNC_ITERATOR, /* u.async_from_sync_iterator_data */
  JS_CLASS_ASYNC_GENERATOR_FUNCTION, /* u.func */
  JS_CLASS_ASYNC_GENERATOR,          /* u.async_generator_data */

  JS_CLASS_INIT_COUNT, /* last entry for predefined classes */
};

struct JSClass {
  uint32_t class_id; /* 0 means free entry */
  JSAtom class_name;
  JSClassFinalizer *finalizer;
  JSClassGCMark *gc_mark;
  JSClassGCWalk *gc_walk;
  JSClassCall *call;
  /* pointers for exotic behavior, can be NULL if none are present */
  const JSClassExoticMethods *exotic;
};

typedef struct JSClassShortDef {
  JSAtom class_name;
  JSClassFinalizer *finalizer;
  JSClassGCMark *gc_mark;
  JSClassGCWalk *gc_walk;
} JSClassShortDef;

void js_array_finalizer(JSRuntime *rt, JSValue val);
void js_array_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_array_walk(JSRuntime *rt, JSValueConst val, JS_WalkFunc *walk_func,
                   void *uctx);

void js_object_data_finalizer(JSRuntime *rt, JSValue val);
void js_object_data_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
void js_object_data_walk(JSRuntime *rt, JSValueConst val,
                         JS_WalkFunc *walk_func, void *uctx);

void js_c_function_finalizer(JSRuntime *rt, JSValue val);
void js_c_function_mark(JSRuntime *rt, JSValueConst val,
                        JS_MarkFunc *mark_func);
void js_c_function_walk(JSRuntime *rt, JSValueConst val, JS_WalkFunc *walk_func,
                        void *uctx);

void js_bytecode_function_finalizer(JSRuntime *rt, JSValue val);
void js_bytecode_function_mark(JSRuntime *rt, JSValueConst val,
                               JS_MarkFunc *mark_func);
void js_bytecode_function_walk(JSRuntime *rt, JSValueConst val,
                               JS_WalkFunc *walk_func, void *uctx);

void js_bound_function_finalizer(JSRuntime *rt, JSValue val);
void js_bound_function_mark(JSRuntime *rt, JSValueConst val,
                            JS_MarkFunc *mark_func);
void js_bound_function_walk(JSRuntime *rt, JSValueConst val,
                            JS_WalkFunc *walk_func, void *uctx);

void js_for_in_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_for_in_iterator_mark(JSRuntime *rt, JSValueConst val,
                             JS_MarkFunc *mark_func);
void js_for_in_iterator_walk(JSRuntime *rt, JSValueConst val,
                             JS_WalkFunc *walk_func, void *uctx);

void js_c_function_data_finalizer(JSRuntime *rt, JSValue val);
void js_c_function_data_mark(JSRuntime *rt, JSValueConst val,
                             JS_MarkFunc *mark_func);
void js_c_function_data_walk(JSRuntime *rt, JSValueConst val,
                             JS_WalkFunc *walk_func, void *uctx);

void js_regexp_finalizer(JSRuntime *rt, JSValue val);
void js_array_buffer_finalizer(JSRuntime *rt, JSValue val);
void js_typed_array_finalizer(JSRuntime *rt, JSValue val);
void js_typed_array_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
void js_typed_array_walk(JSRuntime *rt, JSValueConst val,
                         JS_WalkFunc *walk_func, void *uctx);

void js_proxy_finalizer(JSRuntime *rt, JSValue val);
void js_proxy_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_proxy_walk(JSRuntime *rt, JSValueConst val, JS_WalkFunc *walk_func,
                   void *uctx);

void js_map_finalizer(JSRuntime *rt, JSValue val);
void js_map_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_map_walk(JSRuntime *rt, JSValueConst val, JS_WalkFunc *walk_func,
                 void *uctx);

void js_map_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_map_iterator_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func);
void js_map_iterator_walk(JSRuntime *rt, JSValueConst val,
                          JS_WalkFunc *walk_func, void *uctx);

void js_array_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_array_iterator_mark(JSRuntime *rt, JSValueConst val,
                            JS_MarkFunc *mark_func);
void js_array_iterator_walk(JSRuntime *rt, JSValueConst val,
                            JS_WalkFunc *walk_func, void *uctx);

void js_regexp_string_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_regexp_string_iterator_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func);
void js_regexp_string_iterator_walk(JSRuntime *rt, JSValueConst val,
                                    JS_WalkFunc *walk_func, void *uctx);

void js_generator_finalizer(JSRuntime *rt, JSValue obj);
void js_generator_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_generator_walk(JSRuntime *rt, JSValueConst val, JS_WalkFunc *walk_func,
                       void *uctx);

void js_promise_finalizer(JSRuntime *rt, JSValue val);
void js_promise_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_promise_walk(JSRuntime *rt, JSValueConst val, JS_WalkFunc *walk_func,
                     void *uctx);

void js_promise_resolve_function_finalizer(JSRuntime *rt, JSValue val);
void js_promise_resolve_function_mark(JSRuntime *rt, JSValueConst val,
                                      JS_MarkFunc *mark_func);
void js_promise_resolve_function_walk(JSRuntime *rt, JSValueConst val,
                                      JS_WalkFunc *walk_func, void *uctx);
#ifdef CONFIG_BIGNUM
void js_float_env_finalizer(JSRuntime *rt, JSValue val);

void js_operator_set_finalizer(JSRuntime *rt, JSValue val);
void js_operator_set_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func);
void js_operator_set_walk(JSRuntime *rt, JSValueConst val,
                          JS_WalkFunc *walk_func, void *uctx);
#endif

#ifdef CONFIG_BIGNUM
extern JSClassShortDef const js_std_class_def[47];
#else
extern JSClassShortDef const js_std_class_def[40];
#endif

int init_class_range(JSRuntime *rt, JSClassShortDef const *tab, int start,
                     int count);

BOOL js_class_has_bytecode(JSClassID class_id);

#endif