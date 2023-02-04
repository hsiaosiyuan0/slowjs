#ifndef QUICKJS_OBJ_H
#define QUICKJS_OBJ_H

#include "def.h"
#include "shape.h"

/* -- JSProperty ----------------------------------- */

typedef struct JSProperty {
  union {
    JSValue value;      /* JS_PROP_NORMAL */
    struct {            /* JS_PROP_GETSET */
      JSObject *getter; /* NULL if undefined */
      JSObject *setter; /* NULL if undefined */
    } getset;
    JSVarRef *var_ref; /* JS_PROP_VARREF */
    struct {           /* JS_PROP_AUTOINIT */
      /* in order to use only 2 pointers, we compress the realm
         and the init function pointer */
      uintptr_t realm_and_id; /* realm and init_id (JS_AUTOINIT_ID_x)
                                 in the 2 low bits */
      void *opaque;
    } init;
  } u;
} JSProperty;

#define JS_PROP_INITIAL_SIZE 2
#define JS_PROP_INITIAL_HASH_SIZE 4 /* must be a power of two */
#define JS_ARRAY_INITIAL_SIZE 2

/* -- Regexp ----------------------------------- */

typedef struct JSRegExp {
  JSString *pattern;
  JSString *bytecode; /* also contains the flags */
} JSRegExp;

/* -- JSObject ----------------------------------- */

typedef struct JSArrayBuffer {
  int byte_length; /* 0 if detached */
  uint8_t detached;
  uint8_t shared; /* if shared, the array buffer cannot be detached */
  uint8_t *data;  /* NULL if detached */
  struct list_head array_list;
  void *opaque;
  JSFreeArrayBufferDataFunc *free_func;
} JSArrayBuffer;

struct JSObject {
  union {
    JSGCObjectHeader header;
    struct {
      int __gc_ref_count; /* corresponds to header.ref_count */
      uint8_t __gc_mark;  /* corresponds to header.mark/gc_obj_type */

      uint8_t extensible : 1;
      uint8_t free_mark : 1;  /* only used when freeing objects with cycles */
      uint8_t is_exotic : 1;  /* TRUE if object has exotic property handlers */
      uint8_t fast_array : 1; /* TRUE if u.array is used for get/put (for
                                 JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS and typed
                                 arrays) */
      uint8_t is_constructor : 1; /* TRUE if object is a constructor function */
      uint8_t is_uncatchable_error : 1; /* if TRUE, error is not catchable */
      uint8_t tmp_mark : 1;             /* used in JS_WriteObjectRec() */
      uint8_t is_HTMLDDA : 1; /* specific annex B IsHtmlDDA behavior */
      uint16_t class_id;      /* see JS_CLASS_x */
    };
  };
  /* byte offsets: 16/24 */
  JSShape *shape;   /* prototype and property names + flag */
  JSProperty *prop; /* array of properties */
  /* byte offsets: 24/40 */
  struct JSMapRecord
      *first_weak_ref; /* XXX: use a bit and an external hash table? */
  /* byte offsets: 28/48 */
  union {
    void *opaque;
    struct JSBoundFunction *bound_function; /* JS_CLASS_BOUND_FUNCTION */
    struct JSCFunctionDataRecord
        *c_function_data_record;             /* JS_CLASS_C_FUNCTION_DATA */
    struct JSForInIterator *for_in_iterator; /* JS_CLASS_FOR_IN_ITERATOR */
    struct JSArrayBuffer
        *array_buffer; /* JS_CLASS_ARRAY_BUFFER, JS_CLASS_SHARED_ARRAY_BUFFER */
    struct JSTypedArray
        *typed_array; /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_DATAVIEW */
#ifdef CONFIG_BIGNUM
    struct JSFloatEnv *float_env;           /* JS_CLASS_FLOAT_ENV */
    struct JSOperatorSetData *operator_set; /* JS_CLASS_OPERATOR_SET */
#endif
    struct JSMapState *map_state; /* JS_CLASS_MAP..JS_CLASS_WEAKSET */
    struct JSMapIteratorData
        *map_iterator_data; /* JS_CLASS_MAP_ITERATOR, JS_CLASS_SET_ITERATOR */
    struct JSArrayIteratorData
        *array_iterator_data; /* JS_CLASS_ARRAY_ITERATOR,
                                 JS_CLASS_STRING_ITERATOR */
    struct JSRegExpStringIteratorData
        *regexp_string_iterator_data; /* JS_CLASS_REGEXP_STRING_ITERATOR */
    struct JSGeneratorData *generator_data; /* JS_CLASS_GENERATOR */
    struct JSProxyData *proxy_data;         /* JS_CLASS_PROXY */
    struct JSPromiseData *promise_data;     /* JS_CLASS_PROMISE */
    struct JSPromiseFunctionData
        *promise_function_data; /* JS_CLASS_PROMISE_RESOLVE_FUNCTION,
                                   JS_CLASS_PROMISE_REJECT_FUNCTION */
    struct JSAsyncFunctionData
        *async_function_data; /* JS_CLASS_ASYNC_FUNCTION_RESOLVE,
                                 JS_CLASS_ASYNC_FUNCTION_REJECT */
    struct JSAsyncFromSyncIteratorData
        *async_from_sync_iterator_data; /* JS_CLASS_ASYNC_FROM_SYNC_ITERATOR */
    struct JSAsyncGeneratorData
        *async_generator_data; /* JS_CLASS_ASYNC_GENERATOR */
    struct {                   /* JS_CLASS_BYTECODE_FUNCTION: 12/24 bytes */
      /* also used by JS_CLASS_GENERATOR_FUNCTION, JS_CLASS_ASYNC_FUNCTION and
       * JS_CLASS_ASYNC_GENERATOR_FUNCTION */
      struct JSFunctionBytecode *function_bytecode;
      JSVarRef **var_refs;
      JSObject *home_object; /* for 'super' access */
    } func;
    struct { /* JS_CLASS_C_FUNCTION: 12/20 bytes */
      JSContext *realm;
      JSCFunctionType c_function;
      uint8_t length;
      uint8_t cproto;
      int16_t magic;
    } cfunc;
    /* array part for fast arrays and typed arrays */
    struct { /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS,
                JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
      union {
        uint32_t size; /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS */
        struct JSTypedArray
            *typed_array; /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
      } u1;
      union {
        JSValue *values;    /* JS_CLASS_ARRAY, JS_CLASS_ARGUMENTS */
        void *ptr;          /* JS_CLASS_UINT8C_ARRAY..JS_CLASS_FLOAT64_ARRAY */
        int8_t *int8_ptr;   /* JS_CLASS_INT8_ARRAY */
        uint8_t *uint8_ptr; /* JS_CLASS_UINT8_ARRAY, JS_CLASS_UINT8C_ARRAY */
        int16_t *int16_ptr; /* JS_CLASS_INT16_ARRAY */
        uint16_t *uint16_ptr; /* JS_CLASS_UINT16_ARRAY */
        int32_t *int32_ptr;   /* JS_CLASS_INT32_ARRAY */
        uint32_t *uint32_ptr; /* JS_CLASS_UINT32_ARRAY */
        int64_t *int64_ptr;   /* JS_CLASS_INT64_ARRAY */
        uint64_t *uint64_ptr; /* JS_CLASS_UINT64_ARRAY */
        float *float_ptr;     /* JS_CLASS_FLOAT32_ARRAY */
        double *double_ptr;   /* JS_CLASS_FLOAT64_ARRAY */
      } u;
      uint32_t count;    /* <= 2^31-1. 0 for a detached typed array */
    } array;             /* 12/20 bytes */
    JSRegExp regexp;     /* JS_CLASS_REGEXP: 8/16 bytes */
    JSValue object_data; /* for JS_SetObjectData(): 8/16/16 bytes */
  } u;
  /* byte sizes: 40/48/72 */
};

/* -- Prototype ----------------------------------- */

int JS_SetPrototypeInternal(JSContext *ctx, JSValueConst obj,
                            JSValueConst proto_val, BOOL throw_flag);
int JS_SetPrototype(JSContext *ctx, JSValueConst obj, JSValueConst proto_val);
JSValueConst JS_GetPrototypePrimitive(JSContext *ctx, JSValueConst val);
JSValue JS_GetPrototypeFree(JSContext *ctx, JSValue obj);

/* return TRUE, FALSE or (-1) in case of exception */
int JS_OrdinaryIsInstanceOf(JSContext *ctx, JSValueConst val, JSValueConst obj);
/* return TRUE, FALSE or (-1) in case of exception */
int JS_IsInstanceOf(JSContext *ctx, JSValueConst val, JSValueConst obj);

JSObject *get_proto_obj(JSValueConst proto_val);

/* -- Property ----------------------------------- */

JSValue js_instantiate_prototype(JSContext *ctx, JSObject *p, JSAtom atom,
                                 void *opaque);
JSValue js_module_ns_autoinit(JSContext *ctx, JSObject *p, JSAtom atom,
                              void *opaque);
JSValue JS_InstantiateFunctionListItem2(JSContext *ctx, JSObject *p,
                                        JSAtom atom, void *opaque);

int JS_AutoInitProperty(JSContext *ctx, JSObject *p, JSAtom prop,
                        JSProperty *pr, JSShapeProperty *prs);

int JS_DefinePrivateField(JSContext *ctx, JSValueConst obj, JSValueConst name,
                          JSValue val);
JSValue JS_GetPrivateField(JSContext *ctx, JSValueConst obj, JSValueConst name);
int JS_SetPrivateField(JSContext *ctx, JSValueConst obj, JSValueConst name,
                       JSValue val);

int JS_AddBrand(JSContext *ctx, JSValueConst obj, JSValueConst home_obj);
int JS_CheckBrand(JSContext *ctx, JSValueConst obj, JSValueConst func);

void js_free_prop_enum(JSContext *ctx, JSPropertyEnum *tab, uint32_t len);
int __exception JS_GetOwnPropertyNamesInternal(JSContext *ctx,
                                               JSPropertyEnum **ptab,
                                               uint32_t *plen, JSObject *p,
                                               int flags);
int JS_GetOwnPropertyNames(JSContext *ctx, JSPropertyEnum **ptab,
                           uint32_t *plen, JSValueConst obj, int flags);
int JS_GetOwnPropertyInternal(JSContext *ctx, JSPropertyDescriptor *desc,
                              JSObject *p, JSAtom prop);
int JS_GetOwnProperty(JSContext *ctx, JSPropertyDescriptor *desc,
                      JSValueConst obj, JSAtom prop);
int JS_IsExtensible(JSContext *ctx, JSValueConst obj);
int JS_PreventExtensions(JSContext *ctx, JSValueConst obj);
int JS_HasProperty(JSContext *ctx, JSValueConst obj, JSAtom prop);
JSValue JS_GetPropertyValue(JSContext *ctx, JSValueConst this_obj,
                            JSValue prop);
JSValue JS_GetPropertyUint32(JSContext *ctx, JSValueConst this_obj,
                             uint32_t idx);
int JS_TryGetPropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx,
                           JSValue *pval);
JSValue JS_GetPropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx);
JSValue JS_GetPropertyStr(JSContext *ctx, JSValueConst this_obj,
                          const char *prop);

JSProperty *add_property(JSContext *ctx, JSObject *p, JSAtom prop,
                         int prop_flags);
no_inline __exception int convert_fast_array_to_array(JSContext *ctx,
                                                      JSObject *p);
int delete_property(JSContext *ctx, JSObject *p, JSAtom atom);
int call_setter(JSContext *ctx, JSObject *setter, JSValueConst this_obj,
                JSValue val, int flags);

/* set the array length and remove the array elements if necessary. */
int set_array_length(JSContext *ctx, JSObject *p, JSValue val, int flags);

/* return -1 if exception */
int expand_fast_array(JSContext *ctx, JSObject *p, uint32_t new_len);

/* Preconditions: 'p' must be of class JS_CLASS_ARRAY, p->fast_array =
   TRUE and p->extensible = TRUE */
int add_fast_array_element(JSContext *ctx, JSObject *p, JSValue val, int flags);

void js_free_desc(JSContext *ctx, JSPropertyDescriptor *desc);
/* generic (and slower) version of JS_SetProperty() for
 * Reflect.set(). 'obj' must be an object.  */
int JS_SetPropertyGeneric(JSContext *ctx, JSValueConst obj, JSAtom prop,
                          JSValue val, JSValueConst this_obj, int flags);
/* return -1 in case of exception or TRUE or FALSE. Warning: 'val' is
freed by the function. 'flags' is a bitmask of JS_PROP_NO_ADD,
JS_PROP_THROW or JS_PROP_THROW_STRICT. If JS_PROP_NO_ADD is set,
the new property is not added and an error is raised. */
int JS_SetPropertyInternal(JSContext *ctx, JSValueConst this_obj, JSAtom prop,
                           JSValue val, int flags);
/* flags can be JS_PROP_THROW or JS_PROP_THROW_STRICT */
int JS_SetPropertyValue(JSContext *ctx, JSValueConst this_obj, JSValue prop,
                        JSValue val, int flags);
int JS_SetPropertyUint32(JSContext *ctx, JSValueConst this_obj, uint32_t idx,
                         JSValue val);
int JS_SetPropertyInt64(JSContext *ctx, JSValueConst this_obj, int64_t idx,
                        JSValue val);
int JS_SetPropertyStr(JSContext *ctx, JSValueConst this_obj, const char *prop,
                      JSValue val);
int JS_CreateProperty(JSContext *ctx, JSObject *p, JSAtom prop,
                      JSValueConst val, JSValueConst getter,
                      JSValueConst setter, int flags);
/* return -1, FALSE or TRUE. return FALSE if not configurable or
invalid object. return -1 in case of exception.
flags can be 0, JS_PROP_THROW or JS_PROP_THROW_STRICT */
int JS_DeleteProperty(JSContext *ctx, JSValueConst obj, JSAtom prop, int flags);
int JS_DeletePropertyInt64(JSContext *ctx, JSValueConst obj, int64_t idx,
                           int flags);

/* return FALSE if not OK */
BOOL check_define_prop_flags(int prop_flags, int flags);
int js_shape_prepare_update(JSContext *ctx, JSObject *p,
                            JSShapeProperty **pprs);
typedef enum {
  JS_AUTOINIT_ID_PROTOTYPE,
  JS_AUTOINIT_ID_MODULE_NS,
  JS_AUTOINIT_ID_PROP,
} JSAutoInitIDEnum;

int JS_DefineProperty(JSContext *ctx, JSValueConst this_obj, JSAtom prop,
                      JSValueConst val, JSValueConst getter,
                      JSValueConst setter, int flags);

int JS_DefineAutoInitProperty(JSContext *ctx, JSValueConst this_obj,
                              JSAtom prop, JSAutoInitIDEnum id, void *opaque,
                              int flags);
/* shortcut to add or redefine a new property value */
int JS_DefinePropertyValue(JSContext *ctx, JSValueConst this_obj, JSAtom prop,
                           JSValue val, int flags);
int JS_DefinePropertyValueValue(JSContext *ctx, JSValueConst this_obj,
                                JSValue prop, JSValue val, int flags);
int JS_DefinePropertyValueUint32(JSContext *ctx, JSValueConst this_obj,
                                 uint32_t idx, JSValue val, int flags);
int JS_DefinePropertyValueInt64(JSContext *ctx, JSValueConst this_obj,
                                int64_t idx, JSValue val, int flags);
int JS_DefinePropertyValueStr(JSContext *ctx, JSValueConst this_obj,
                              const char *prop, JSValue val, int flags);
/* shortcut to add getter & setter */
int JS_DefinePropertyGetSet(JSContext *ctx, JSValueConst this_obj, JSAtom prop,
                            JSValue getter, JSValue setter, int flags);
int JS_CreateDataPropertyUint32(JSContext *ctx, JSValueConst this_obj,
                                int64_t idx, JSValue val, int flags);

int JS_DefineObjectName(JSContext *ctx, JSValueConst obj, JSAtom name,
                        int flags);
int JS_DefineObjectNameComputed(JSContext *ctx, JSValueConst obj,
                                JSValueConst str, int flags);

JSContext *js_autoinit_get_realm(JSProperty *pr);
void free_property(JSRuntime *rt, JSProperty *pr, int prop_flags);

force_inline JSShapeProperty *find_own_property1(JSObject *p, JSAtom atom) {
  JSShape *sh;
  JSShapeProperty *pr, *prop;
  intptr_t h;
  sh = p->shape;
  h = (uintptr_t)atom & sh->prop_hash_mask;
  h = prop_hash_end(sh)[-h - 1];
  prop = get_shape_prop(sh);
  while (h) {
    pr = &prop[h - 1];
    if (likely(pr->atom == atom)) {
      return pr;
    }
    h = pr->hash_next;
  }
  return NULL;
}

force_inline JSShapeProperty *find_own_property(JSProperty **ppr, JSObject *p,
                                                JSAtom atom) {
  JSShape *sh;
  JSShapeProperty *pr, *prop;
  intptr_t h;
  sh = p->shape;
  h = (uintptr_t)atom & sh->prop_hash_mask;
  h = prop_hash_end(sh)[-h - 1];
  prop = get_shape_prop(sh);
  while (h) {
    pr = &prop[h - 1];
    if (likely(pr->atom == atom)) {
      *ppr = &p->prop[h - 1];
      /* the compiler should be able to assume that pr != NULL here */
      return pr;
    }
    h = pr->hash_next;
  }
  *ppr = NULL;
  return NULL;
}

/* -- Utils ----------------------------------- */

/* WARNING: proto must be an object or JS_NULL */
JSValue JS_NewObjectProtoClass(JSContext *ctx, JSValueConst proto_val,
                               JSClassID class_id);
int JS_SetObjectData(JSContext *ctx, JSValueConst obj, JSValue val);
JSValue JS_NewObjectClass(JSContext *ctx, int class_id);
JSValue JS_NewObjectProto(JSContext *ctx, JSValueConst proto);
JSValue JS_NewArray(JSContext *ctx);
JSValue JS_NewObject(JSContext *ctx);
JSValue js_create_array(JSContext *ctx, int len, JSValueConst *tab);
BOOL js_is_fast_array(JSContext *ctx, JSValueConst obj);
/* Access an Array's internal JSValue array if available */
BOOL js_get_fast_array(JSContext *ctx, JSValueConst obj, JSValue **arrpp,
                       uint32_t *countp);

__exception int JS_CopyDataProperties(JSContext *ctx, JSValueConst target,
                                      JSValueConst source,
                                      JSValueConst excluded, BOOL setprop);

__exception int js_get_length32(JSContext *ctx, uint32_t *pres,
                                JSValueConst obj);
__exception int js_get_length64(JSContext *ctx, int64_t *pres,
                                JSValueConst obj);

/* Note: all the fields are already sealed except length */
int seal_template_obj(JSContext *ctx, JSValueConst obj);

void JS_SetOpaque(JSValue obj, void *opaque);
/* return NULL if not an object of class class_id */
void JS_SetOpaque(JSValue obj, void *opaque);
void *JS_GetOpaque2(JSContext *ctx, JSValueConst obj, JSClassID class_id);

static inline BOOL JS_IsHTMLDDA(JSContext *ctx, JSValueConst obj) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
    return FALSE;
  p = JS_VALUE_GET_OBJ(obj);
  return p->is_HTMLDDA;
}

#endif