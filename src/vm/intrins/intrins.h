#ifndef QUICKJS_INTRINS_H
#define QUICKJS_INTRINS_H

#include "def.h"

#include "vm/class.h"
#include "vm/func.h"
#include "vm/obj.h"

/* -- Registering basic objects registering ---------------*/

/* Minimum amount of objects to be able to compile code and display
   error messages. No JSAtom should be allocated by this function. */
void JS_AddIntrinsicBasicObjects(JSContext *ctx);

/* the following functions are used to select the intrinsic object to save
 * memory */

void JS_AddIntrinsicBaseObjects(JSContext *ctx);
void JS_AddIntrinsicDate(JSContext *ctx);
void JS_AddIntrinsicEval(JSContext *ctx);
void JS_AddIntrinsicStringNormalize(JSContext *ctx);
void JS_AddIntrinsicRegExpCompiler(JSContext *ctx);
void JS_AddIntrinsicRegExp(JSContext *ctx);
void JS_AddIntrinsicJSON(JSContext *ctx);
void JS_AddIntrinsicProxy(JSContext *ctx);
void JS_AddIntrinsicMapSet(JSContext *ctx);
void JS_AddIntrinsicTypedArrays(JSContext *ctx);
void JS_AddIntrinsicPromise(JSContext *ctx);
void JS_AddIntrinsicBigInt(JSContext *ctx);
void JS_AddIntrinsicBigFloat(JSContext *ctx);
void JS_AddIntrinsicBigDecimal(JSContext *ctx);
/* enable operator overloading */
void JS_AddIntrinsicOperators(JSContext *ctx);

/* -- Utils ----------------------------------- */

int check_function(JSContext *ctx, JSValueConst obj);
int check_exception_free(JSContext *ctx, JSValue obj);
JSAtom find_atom(JSContext *ctx, const char *name);

JSValue JS_InstantiateFunctionListItem2(JSContext *ctx, JSObject *p,
                                        JSAtom atom, void *opaque);
int JS_InstantiateFunctionListItem(JSContext *ctx, JSValueConst obj,
                                   JSAtom atom, const JSCFunctionListEntry *e);
void JS_SetPropertyFunctionList(JSContext *ctx, JSValueConst obj,
                                const JSCFunctionListEntry *tab, int len);

/* Note: 'func_obj' is not necessarily a constructor */
void JS_SetConstructor2(JSContext *ctx, JSValueConst func_obj,
                        JSValueConst proto, int proto_flags, int ctor_flags);
void JS_SetConstructor(JSContext *ctx, JSValueConst func_obj,
                       JSValueConst proto);
void JS_NewGlobalCConstructor2(JSContext *ctx, JSValue func_obj,
                               const char *name, JSValueConst proto);
JSValueConst JS_NewGlobalCConstructor(JSContext *ctx, const char *name,
                                      JSCFunction *func, int length,
                                      JSValueConst proto);
JSValueConst JS_NewGlobalCConstructorOnly(JSContext *ctx, const char *name,
                                          JSCFunction *func, int length,
                                          JSValueConst proto);

JSValue js_get_this(JSContext *ctx, JSValueConst this_val);

/* -- Global functions ----------------------------------- */

JSValue js_global_eval(JSContext *ctx, JSValueConst this_val, int argc,
                       JSValueConst *argv);
JSValue js_global_isNaN(JSContext *ctx, JSValueConst this_val, int argc,
                        JSValueConst *argv);
JSValue js_global_isFinite(JSContext *ctx, JSValueConst this_val, int argc,
                           JSValueConst *argv);

/* -- Object ----------------------------------- */

JSValue js_object_constructor(JSContext *ctx, JSValueConst new_target, int argc,
                              JSValueConst *argv);
int js_obj_to_desc(JSContext *ctx, JSPropertyDescriptor *d, JSValueConst desc);
JSValue js_object_seal(JSContext *ctx, JSValueConst this_val, int argc,
                       JSValueConst *argv, int freeze_flag);

JSValue JS_GetOwnPropertyNames2(JSContext *ctx, JSValueConst obj1, int flags,
                                int kind);

JSValue JS_SpeciesConstructor(JSContext *ctx, JSValueConst obj,
                              JSValueConst defaultConstructor);
JSValue js_object_keys(JSContext *ctx, JSValueConst this_val, int argc,
                       JSValueConst *argv, int kind);
JSValue js_object_toString(JSContext *ctx, JSValueConst this_val, int argc,
                           JSValueConst *argv);

/* magic = 1 if called as Reflect.defineProperty */
JSValue js_object_defineProperty(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic);
JSValue js_object_getOwnPropertyDescriptor(JSContext *ctx,
                                           JSValueConst this_val, int argc,
                                           JSValueConst *argv, int magic);
JSValue js_object_getPrototypeOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic);

JSValue js_object_isExtensible(JSContext *ctx, JSValueConst this_val, int argc,
                               JSValueConst *argv, int reflect);
JSValue js_object_preventExtensions(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv, int reflect);

extern const JSCFunctionListEntry js_object_funcs[23];
extern const JSCFunctionListEntry js_object_proto_funcs[11];

/* -- Function ----------------------------------- */

JSValue js_function_proto(JSContext *ctx, JSValueConst this_val, int argc,
                          JSValueConst *argv);
/* XXX: add a specific eval mode so that Function("}), ({") is rejected */
JSValue js_function_constructor(JSContext *ctx, JSValueConst new_target,
                                int argc, JSValueConst *argv, int magic);

JSValue js_function_call(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv);
JSValue js_function_bind(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv);

int js_obj_to_desc(JSContext *ctx, JSPropertyDescriptor *d, JSValueConst desc);
/* XXX: not 100% compatible, but mozilla seems to use a similar
   implementation to ensure that caller in non strict mode does not
   throw (ES5 compatibility) */
JSValue js_function_proto_caller(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv);

extern const JSCFunctionListEntry js_function_proto_funcs[7];

/* -- Error ----------------------------------- */

JSValue js_error_constructor(JSContext *ctx, JSValueConst new_target, int argc,
                             JSValueConst *argv, int magic);
JSValue js_error_toString(JSContext *ctx, JSValueConst this_val, int argc,
                          JSValueConst *argv);

JSValue js_throw_type_error(JSContext *ctx, JSValueConst this_val, int argc,
                            JSValueConst *argv);

extern const JSCFunctionListEntry js_error_proto_funcs[3];
extern const char *const native_error_name[JS_NATIVE_ERROR_COUNT];

/* -- AggregateError ----------------------------------- */

/* used by C code. */
JSValue js_aggregate_error_constructor(JSContext *ctx, JSValueConst errors);

/* -- Array ----------------------------------- */

typedef enum JSIteratorKindEnum {
  JS_ITERATOR_KIND_KEY,
  JS_ITERATOR_KIND_VALUE,
  JS_ITERATOR_KIND_KEY_AND_VALUE,
} JSIteratorKindEnum;

typedef struct JSArrayIteratorData {
  JSValue obj;
  JSIteratorKindEnum kind;
  uint32_t idx;
} JSArrayIteratorData;

extern const JSCFunctionListEntry js_array_funcs[4];

JSValue js_array_constructor(JSContext *ctx, JSValueConst new_target, int argc,
                             JSValueConst *argv);

JSValue js_array_iterator_next(JSContext *ctx, JSValueConst this_val, int argc,
                               JSValueConst *argv, BOOL *pdone, int magic);

JSValue js_create_array_iterator(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic);

JSValue js_array_pop(JSContext *ctx, JSValueConst this_val, int argc,
                     JSValueConst *argv, int shift);
JSValue js_array_includes(JSContext *ctx, JSValueConst this_val, int argc,
                          JSValueConst *argv);
JSValue js_array_push(JSContext *ctx, JSValueConst this_val, int argc,
                      JSValueConst *argv, int unshift);

#define special_every 0
#define special_some 1
#define special_forEach 2
#define special_map 3
#define special_filter 4
#define special_TA 8
JSValue js_array_every(JSContext *ctx, JSValueConst this_val, int argc,
                       JSValueConst *argv, int special);

#define special_reduce 0
#define special_reduceRight 1

JSValue js_array_reduce(JSContext *ctx, JSValueConst this_val, int argc,
                        JSValueConst *argv, int special);

JSValue js_iterator_proto_iterator(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv);

extern const JSCFunctionListEntry js_iterator_proto_funcs[1];
extern const JSCFunctionListEntry js_array_proto_funcs[32];
extern const JSCFunctionListEntry js_array_iterator_proto_funcs[2];

/* -- Number ----------------------------------- */

JSValue js_number_constructor(JSContext *ctx, JSValueConst new_target, int argc,
                              JSValueConst *argv);

extern const JSCFunctionListEntry js_number_proto_funcs[6];
extern const JSCFunctionListEntry js_number_funcs[14];

/* -- Boolean ----------------------------------- */

JSValue js_boolean_constructor(JSContext *ctx, JSValueConst new_target,
                               int argc, JSValueConst *argv);

extern const JSCFunctionListEntry js_boolean_proto_funcs[2];

/* -- String ----------------------------------- */

const JSClassExoticMethods js_string_exotic_methods;

JSValue js_string_constructor(JSContext *ctx, JSValueConst new_target, int argc,
                              JSValueConst *argv);
int64_t string_advance_index(JSString *p, int64_t index, BOOL unicode);
int string_indexof_char(JSString *p, int c, int from);
JSValue js_string___GetSubstitution(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv);

extern const JSCFunctionListEntry js_string_funcs[3];
extern const JSCFunctionListEntry js_string_proto_funcs[49];

extern const JSCFunctionListEntry js_string_iterator_proto_funcs[2];

#ifdef CONFIG_ALL_UNICODE
extern const JSCFunctionListEntry js_string_proto_normalize[1];
#endif

/* -- Math ----------------------------------- */

void js_random_init(JSContext *ctx);

extern const JSCFunctionListEntry js_math_obj[1];

/* -- RegExp ----------------------------------- */

/* create a RegExp object from a string containing the RegExp bytecode
   and the source pattern */
JSValue js_regexp_constructor_internal(JSContext *ctx, JSValueConst ctor,
                                       JSValue pattern, JSValue bc);

/* return < 0 if exception or TRUE/FALSE */
int js_is_regexp(JSContext *ctx, JSValueConst obj);
void js_regexp_finalizer(JSRuntime *rt, JSValue val);
void js_regexp_string_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_regexp_string_iterator_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func);

/* -- JSON ----------------------------------- */

/* -- Reflect ----------------------------------- */

extern const JSCFunctionListEntry js_reflect_obj[1];

/* -- Proxy ----------------------------------- */

typedef struct JSProxyData {
  JSValue target;
  JSValue handler;
  uint8_t is_func;
  uint8_t is_revoked;
} JSProxyData;

JSValue JS_ThrowTypeErrorRevokedProxy(JSContext *ctx);

int js_proxy_isArray(JSContext *ctx, JSValueConst obj);
JSValue js_proxy_getPrototypeOf(JSContext *ctx, JSValueConst obj);
int js_proxy_setPrototypeOf(JSContext *ctx, JSValueConst obj,
                            JSValueConst proto_val, BOOL throw_flag);

int js_proxy_isExtensible(JSContext *ctx, JSValueConst obj);
int js_proxy_preventExtensions(JSContext *ctx, JSValueConst obj);

/* -- Symbol ----------------------------------- */

JSValue js_symbol_constructor(JSContext *ctx, JSValueConst new_target, int argc,
                              JSValueConst *argv);

extern const JSCFunctionListEntry js_symbol_proto_funcs[5];
extern const JSCFunctionListEntry js_symbol_funcs[2];

/* -- Set/Map/WeakSet/WeakMap ----------------------------------- */

typedef struct JSMapRecord {
  int ref_count; /* used during enumeration to avoid freeing the record */
  BOOL empty;    /* TRUE if the record is deleted */
  struct JSMapState *map;
  struct JSMapRecord *next_weak_ref;
  struct list_head link;
  struct list_head hash_link;
  JSValue key;
  JSValue value;
} JSMapRecord;

typedef struct JSMapState {
  BOOL is_weak;             /* TRUE if WeakSet/WeakMap */
  struct list_head records; /* list of JSMapRecord.link */
  uint32_t record_count;
  struct list_head *hash_table;
  uint32_t hash_size;              /* must be a power of two */
  uint32_t record_count_threshold; /* count at which a hash table
                                      resize is needed */
} JSMapState;

#define MAGIC_SET (1 << 0)
#define MAGIC_WEAK (1 << 1)

void js_map_finalizer(JSRuntime *rt, JSValue val);
void js_map_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);

/* Map Iterator */

typedef struct JSMapIteratorData {
  JSValue obj;
  JSIteratorKindEnum kind;
  JSMapRecord *cur_record;
} JSMapIteratorData;

void js_map_iterator_finalizer(JSRuntime *rt, JSValue val);
void js_map_iterator_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func);

void JS_AddIntrinsicMapSet(JSContext *ctx);

/* -- Generator ----------------------------------- */

typedef enum JSGeneratorStateEnum {
  JS_GENERATOR_STATE_SUSPENDED_START,
  JS_GENERATOR_STATE_SUSPENDED_YIELD,
  JS_GENERATOR_STATE_SUSPENDED_YIELD_STAR,
  JS_GENERATOR_STATE_EXECUTING,
  JS_GENERATOR_STATE_COMPLETED,
} JSGeneratorStateEnum;

typedef struct JSGeneratorData {
  JSGeneratorStateEnum state;
  JSAsyncFunctionState func_state;
} JSGeneratorData;

extern const JSCFunctionListEntry js_generator_function_proto_funcs[1];
extern const JSCFunctionListEntry js_generator_proto_funcs[4];

/* XXX: use enum */
#define GEN_MAGIC_NEXT 0
#define GEN_MAGIC_RETURN 1
#define GEN_MAGIC_THROW 2

JSValue js_generator_function_call(JSContext *ctx, JSValueConst func_obj,
                                   JSValueConst this_obj, int argc,
                                   JSValueConst *argv, int flags);
void js_generator_finalizer(JSRuntime *rt, JSValue obj);
void js_generator_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func);

/* -- Promise ----------------------------------- */

typedef enum JSPromiseStateEnum {
  JS_PROMISE_PENDING,
  JS_PROMISE_FULFILLED,
  JS_PROMISE_REJECTED,
} JSPromiseStateEnum;

typedef struct JSPromiseData {
  JSPromiseStateEnum promise_state;
  /* 0=fulfill, 1=reject, list of JSPromiseReactionData.link */
  struct list_head promise_reactions[2];
  BOOL is_handled; /* Note: only useful to debug */
  JSValue promise_result;
} JSPromiseData;

typedef struct JSPromiseFunctionDataResolved {
  int ref_count;
  BOOL already_resolved;
} JSPromiseFunctionDataResolved;

typedef struct JSPromiseFunctionData {
  JSValue promise;
  JSPromiseFunctionDataResolved *presolved;
} JSPromiseFunctionData;

typedef struct JSPromiseReactionData {
  struct list_head link; /* not used in promise_reaction_job */
  JSValue resolving_funcs[2];
  JSValue handler;
} JSPromiseReactionData;

JSValue js_promise_resolve(JSContext *ctx, JSValueConst this_val, int argc,
                           JSValueConst *argv, int magic);
__exception int perform_promise_then(JSContext *ctx, JSValueConst promise,
                                     JSValueConst *resolve_reject,
                                     JSValueConst *cap_resolving_funcs);

/* -- AsyncFunction ----------------------------------- */

extern const JSCFunctionListEntry js_async_function_proto_funcs[1];

/* -- AsyncGenerator ----------------------------------- */

typedef enum JSAsyncGeneratorStateEnum {
  JS_ASYNC_GENERATOR_STATE_SUSPENDED_START,
  JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD,
  JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD_STAR,
  JS_ASYNC_GENERATOR_STATE_EXECUTING,
  JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN,
  JS_ASYNC_GENERATOR_STATE_COMPLETED,
} JSAsyncGeneratorStateEnum;

typedef struct JSAsyncGeneratorRequest {
  struct list_head link;
  /* completion */
  int completion_type; /* GEN_MAGIC_x */
  JSValue result;
  /* promise capability */
  JSValue promise;
  JSValue resolving_funcs[2];
} JSAsyncGeneratorRequest;

typedef struct JSAsyncGeneratorData {
  JSObject *generator; /* back pointer to the object (const) */
  JSAsyncGeneratorStateEnum state;
  JSAsyncFunctionState func_state;
  struct list_head queue; /* list of JSAsyncGeneratorRequest.link */
} JSAsyncGeneratorData;

JSValue js_async_generator_function_call(JSContext *ctx, JSValueConst func_obj,
                                         JSValueConst this_obj, int argc,
                                         JSValueConst *argv, int flags);

/* -- AsyncIteratorPrototype ----------------------------------- */

extern const JSCFunctionListEntry js_async_iterator_proto_funcs[1];

/* -- AsyncFromSyncIteratorPrototype ----------------------------------- */

JSValue JS_CreateAsyncFromSyncIterator(JSContext *ctx, JSValueConst sync_iter);
extern const JSCFunctionListEntry js_async_from_sync_iterator_proto_funcs[3];

/* -- AsyncGeneratorFunction ----------------------------------- */

extern const JSCFunctionListEntry js_async_generator_function_proto_funcs[1];

/* -- AsyncGenerator prototype ----------------------------------- */

const JSCFunctionListEntry js_async_generator_proto_funcs[4];

extern JSClassShortDef const js_async_class_def[9];

/* -- URI handling ----------------------------------- */

JSValue js_global_escape(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv);
JSValue js_global_unescape(JSContext *ctx, JSValueConst this_val, int argc,
                           JSValueConst *argv);

JSValue js_global_decodeURI(JSContext *ctx, JSValueConst this_val, int argc,
                            JSValueConst *argv, int isComponent);
JSValue js_global_encodeURI(JSContext *ctx, JSValueConst this_val, int argc,
                            JSValueConst *argv, int isComponent);

/* -- Date ----------------------------------- */

JSValue js___date_clock(JSContext *ctx, JSValueConst this_val, int argc,
                        JSValueConst *argv);

/* -- Operators ----------------------------------- */

/* -- Typed Arrays ----------------------------------- */

typedef struct JSTypedArray {
  struct list_head link; /* link to arraybuffer */
  JSObject *obj;         /* back pointer to the TypedArray/DataView object */
  JSObject *buffer;      /* based array buffer */
  uint32_t offset;       /* offset in the array buffer */
  uint32_t length;       /* length in the array buffer */
} JSTypedArray;

/* number of typed array types */
#define JS_TYPED_ARRAY_COUNT                                                   \
  (JS_CLASS_FLOAT64_ARRAY - JS_CLASS_UINT8C_ARRAY + 1)
static uint8_t const typed_array_size_log2[JS_TYPED_ARRAY_COUNT];
#define typed_array_size_log2(classid)                                         \
  (typed_array_size_log2[(classid)-JS_CLASS_UINT8C_ARRAY])

JSValue js_typed_array_constructor(JSContext *ctx, JSValueConst new_target,
                                   int argc, JSValueConst *argv, int classid);
JSValue js_array_buffer_constructor3(JSContext *ctx, JSValueConst new_target,
                                     uint64_t len, JSClassID class_id,
                                     uint8_t *buf,
                                     JSFreeArrayBufferDataFunc *free_func,
                                     void *opaque, BOOL alloc_flag);
JSObject *get_typed_array(JSContext *ctx, JSValueConst this_val,
                          int is_dataview);
/* WARNING: 'p' must be a typed array */
BOOL typed_array_is_detached(JSContext *ctx, JSObject *p);
/* WARNING: 'p' must be a typed array. Works even if the array buffer
   is detached */
uint32_t typed_array_get_length(JSContext *ctx, JSObject *p);

/* get an ArrayBuffer or SharedArrayBuffer */
JSArrayBuffer *js_get_array_buffer(JSContext *ctx, JSValueConst obj);

/* return < 0 if exception */
int js_typed_array_get_length_internal(JSContext *ctx, JSValueConst obj);
JSValue js_typed_array___speciesCreate(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv);

/* -- SharedArrayBuffer ----------------------------------- */

JSValue JS_ThrowTypeErrorDetachedArrayBuffer(JSContext *ctx);

/* -- Atomics ----------------------------------- */

#endif