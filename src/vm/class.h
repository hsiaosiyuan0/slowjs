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
  JSClassCall *call;
  /* pointers for exotic behavior, can be NULL if none are present */
  const JSClassExoticMethods *exotic;
};

typedef struct JSClassShortDef {
  JSAtom class_name;
  JSClassFinalizer *finalizer;
  JSClassGCMark *gc_mark;
} JSClassShortDef;

void js_array_finalizer(JSRuntime *rt, JSValue val);
void js_array_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_object_data_finalizer(JSRuntime *rt, JSValue val);
void js_object_data_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
void js_c_function_finalizer(JSRuntime *rt, JSValue val);
void js_c_function_mark(JSRuntime *rt, JSValueConst val,
                        JS_MarkFunc *mark_func);
void js_bytecode_function_finalizer(JSRuntime *rt, JSValue val);
void js_bytecode_function_mark(JSRuntime *rt, JSValueConst val,
                               JS_MarkFunc *mark_func);
void js_bound_function_finalizer(JSRuntime *rt, JSValue val);
void js_bound_function_mark(JSRuntime *rt, JSValueConst val,
                            JS_MarkFunc *mark_func);
void js_for_in_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_for_in_iterator_mark(JSRuntime *rt, JSValueConst val,
                             JS_MarkFunc *mark_func);
void js_c_function_data_finalizer(JSRuntime *rt, JSValue val);
void js_c_function_data_mark(JSRuntime *rt, JSValueConst val,
                             JS_MarkFunc *mark_func);
void js_regexp_finalizer(JSRuntime *rt, JSValue val);
void js_array_buffer_finalizer(JSRuntime *rt, JSValue val);
void js_typed_array_finalizer(JSRuntime *rt, JSValue val);
void js_typed_array_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func);
void js_proxy_finalizer(JSRuntime *rt, JSValue val);
void js_proxy_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_map_finalizer(JSRuntime *rt, JSValue val);
void js_map_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_map_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_map_iterator_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func);
void js_array_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_array_iterator_mark(JSRuntime *rt, JSValueConst val,
                            JS_MarkFunc *mark_func);
void js_regexp_string_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_regexp_string_iterator_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func);
void js_generator_finalizer(JSRuntime *rt, JSValue obj);
void js_generator_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_promise_finalizer(JSRuntime *rt, JSValue val);
void js_promise_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);
void js_promise_resolve_function_finalizer(JSRuntime *rt, JSValue val);
void js_promise_resolve_function_mark(JSRuntime *rt, JSValueConst val,
                                      JS_MarkFunc *mark_func);
#ifdef CONFIG_BIGNUM
void js_operator_set_finalizer(JSRuntime *rt, JSValue val);
void js_operator_set_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func);
#endif

JSClassShortDef const js_std_class_def[] = {
    {JS_ATOM_Object, NULL, NULL},                       /* JS_CLASS_OBJECT */
    {JS_ATOM_Array, js_array_finalizer, js_array_mark}, /* JS_CLASS_ARRAY */
    {JS_ATOM_Error, NULL, NULL},                        /* JS_CLASS_ERROR */
    {JS_ATOM_Number, js_object_data_finalizer,
     js_object_data_mark}, /* JS_CLASS_NUMBER */
    {JS_ATOM_String, js_object_data_finalizer,
     js_object_data_mark}, /* JS_CLASS_STRING */
    {JS_ATOM_Boolean, js_object_data_finalizer,
     js_object_data_mark}, /* JS_CLASS_BOOLEAN */
    {JS_ATOM_Symbol, js_object_data_finalizer,
     js_object_data_mark}, /* JS_CLASS_SYMBOL */
    {JS_ATOM_Arguments, js_array_finalizer,
     js_array_mark},                 /* JS_CLASS_ARGUMENTS */
    {JS_ATOM_Arguments, NULL, NULL}, /* JS_CLASS_MAPPED_ARGUMENTS */
    {JS_ATOM_Date, js_object_data_finalizer,
     js_object_data_mark},        /* JS_CLASS_DATE */
    {JS_ATOM_Object, NULL, NULL}, /* JS_CLASS_MODULE_NS */
    {JS_ATOM_Function, js_c_function_finalizer,
     js_c_function_mark}, /* JS_CLASS_C_FUNCTION */
    {JS_ATOM_Function, js_bytecode_function_finalizer,
     js_bytecode_function_mark}, /* JS_CLASS_BYTECODE_FUNCTION */
    {JS_ATOM_Function, js_bound_function_finalizer,
     js_bound_function_mark}, /* JS_CLASS_BOUND_FUNCTION */
    {JS_ATOM_Function, js_c_function_data_finalizer,
     js_c_function_data_mark}, /* JS_CLASS_C_FUNCTION_DATA */
    {JS_ATOM_GeneratorFunction, js_bytecode_function_finalizer,
     js_bytecode_function_mark}, /* JS_CLASS_GENERATOR_FUNCTION */
    {JS_ATOM_ForInIterator, js_for_in_iterator_finalizer,
     js_for_in_iterator_mark},                   /* JS_CLASS_FOR_IN_ITERATOR */
    {JS_ATOM_RegExp, js_regexp_finalizer, NULL}, /* JS_CLASS_REGEXP */
    {JS_ATOM_ArrayBuffer, js_array_buffer_finalizer,
     NULL}, /* JS_CLASS_ARRAY_BUFFER */
    {JS_ATOM_SharedArrayBuffer, js_array_buffer_finalizer,
     NULL}, /* JS_CLASS_SHARED_ARRAY_BUFFER */
    {JS_ATOM_Uint8ClampedArray, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_UINT8C_ARRAY */
    {JS_ATOM_Int8Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_INT8_ARRAY */
    {JS_ATOM_Uint8Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_UINT8_ARRAY */
    {JS_ATOM_Int16Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_INT16_ARRAY */
    {JS_ATOM_Uint16Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_UINT16_ARRAY */
    {JS_ATOM_Int32Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_INT32_ARRAY */
    {JS_ATOM_Uint32Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_UINT32_ARRAY */
#ifdef CONFIG_BIGNUM
    {JS_ATOM_BigInt64Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_BIG_INT64_ARRAY */
    {JS_ATOM_BigUint64Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_BIG_UINT64_ARRAY */
#endif
    {JS_ATOM_Float32Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_FLOAT32_ARRAY */
    {JS_ATOM_Float64Array, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_FLOAT64_ARRAY */
    {JS_ATOM_DataView, js_typed_array_finalizer,
     js_typed_array_mark}, /* JS_CLASS_DATAVIEW */
#ifdef CONFIG_BIGNUM
    {JS_ATOM_BigInt, js_object_data_finalizer,
     js_object_data_mark}, /* JS_CLASS_BIG_INT */
    {JS_ATOM_BigFloat, js_object_data_finalizer,
     js_object_data_mark}, /* JS_CLASS_BIG_FLOAT */
    {JS_ATOM_BigFloatEnv, js_float_env_finalizer,
     NULL}, /* JS_CLASS_FLOAT_ENV */
    {JS_ATOM_BigDecimal, js_object_data_finalizer,
     js_object_data_mark}, /* JS_CLASS_BIG_DECIMAL */
    {JS_ATOM_OperatorSet, js_operator_set_finalizer,
     js_operator_set_mark}, /* JS_CLASS_OPERATOR_SET */
#endif
    {JS_ATOM_Map, js_map_finalizer, js_map_mark},     /* JS_CLASS_MAP */
    {JS_ATOM_Set, js_map_finalizer, js_map_mark},     /* JS_CLASS_SET */
    {JS_ATOM_WeakMap, js_map_finalizer, js_map_mark}, /* JS_CLASS_WEAKMAP */
    {JS_ATOM_WeakSet, js_map_finalizer, js_map_mark}, /* JS_CLASS_WEAKSET */
    {JS_ATOM_Map_Iterator, js_map_iterator_finalizer,
     js_map_iterator_mark}, /* JS_CLASS_MAP_ITERATOR */
    {JS_ATOM_Set_Iterator, js_map_iterator_finalizer,
     js_map_iterator_mark}, /* JS_CLASS_SET_ITERATOR */
    {JS_ATOM_Array_Iterator, js_array_iterator_finalizer,
     js_array_iterator_mark}, /* JS_CLASS_ARRAY_ITERATOR */
    {JS_ATOM_String_Iterator, js_array_iterator_finalizer,
     js_array_iterator_mark}, /* JS_CLASS_STRING_ITERATOR */
    {JS_ATOM_RegExp_String_Iterator, js_regexp_string_iterator_finalizer,
     js_regexp_string_iterator_mark}, /* JS_CLASS_REGEXP_STRING_ITERATOR */
    {JS_ATOM_Generator, js_generator_finalizer,
     js_generator_mark}, /* JS_CLASS_GENERATOR */
};

int init_class_range(JSRuntime *rt, JSClassShortDef const *tab, int start,
                     int count);

BOOL js_class_has_bytecode(JSClassID class_id);

#endif