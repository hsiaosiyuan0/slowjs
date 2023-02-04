#include "class.h"

#include "str.h"

static JSClassID js_class_id_alloc = JS_CLASS_INIT_COUNT;

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

/* a new class ID is allocated if *pclass_id != 0 */
JSClassID JS_NewClassID(JSClassID *pclass_id) {
  JSClassID class_id;
  /* XXX: make it thread safe */
  class_id = *pclass_id;
  if (class_id == 0) {
    class_id = js_class_id_alloc++;
    *pclass_id = class_id;
  }
  return class_id;
}

BOOL JS_IsRegisteredClass(JSRuntime *rt, JSClassID class_id) {
  return (class_id < rt->class_count &&
          rt->class_array[class_id].class_id != 0);
}

/* create a new object internal class. Return -1 if error, 0 if
   OK. The finalizer can be NULL if none is needed. */
static int JS_NewClass1(JSRuntime *rt, JSClassID class_id,
                        const JSClassDef *class_def, JSAtom name) {
  int new_size, i;
  JSClass *cl, *new_class_array;
  struct list_head *el;

  if (class_id >= (1 << 16))
    return -1;
  if (class_id < rt->class_count && rt->class_array[class_id].class_id != 0)
    return -1;

  if (class_id >= rt->class_count) {
    new_size = max_int(JS_CLASS_INIT_COUNT,
                       max_int(class_id + 1, rt->class_count * 3 / 2));

    /* reallocate the context class prototype array, if any */
    list_for_each(el, &rt->context_list) {
      JSContext *ctx = list_entry(el, JSContext, link);
      JSValue *new_tab;
      new_tab = js_realloc_rt(rt, ctx->class_proto,
                              sizeof(ctx->class_proto[0]) * new_size);
      if (!new_tab)
        return -1;
      for (i = rt->class_count; i < new_size; i++)
        new_tab[i] = JS_NULL;
      ctx->class_proto = new_tab;
    }
    /* reallocate the class array */
    new_class_array =
        js_realloc_rt(rt, rt->class_array, sizeof(JSClass) * new_size);
    if (!new_class_array)
      return -1;
    memset(new_class_array + rt->class_count, 0,
           (new_size - rt->class_count) * sizeof(JSClass));
    rt->class_array = new_class_array;
    rt->class_count = new_size;
  }
  cl = &rt->class_array[class_id];
  cl->class_id = class_id;
  cl->class_name = JS_DupAtomRT(rt, name);
  cl->finalizer = class_def->finalizer;
  cl->gc_mark = class_def->gc_mark;
  cl->call = class_def->call;
  cl->exotic = class_def->exotic;
  return 0;
}

int JS_NewClass(JSRuntime *rt, JSClassID class_id,
                const JSClassDef *class_def) {
  int ret, len;
  JSAtom name;

  len = strlen(class_def->class_name);
  name = __JS_FindAtom(rt, class_def->class_name, len, JS_ATOM_TYPE_STRING);
  if (name == JS_ATOM_NULL) {
    name =
        __JS_NewAtomInit(rt, class_def->class_name, len, JS_ATOM_TYPE_STRING);
    if (name == JS_ATOM_NULL)
      return -1;
  }
  ret = JS_NewClass1(rt, class_id, class_def, name);
  JS_FreeAtomRT(rt, name);
  return ret;
}
int init_class_range(JSRuntime *rt, JSClassShortDef const *tab, int start,
                     int count) {
  JSClassDef cm_s, *cm = &cm_s;
  int i, class_id;

  for (i = 0; i < count; i++) {
    class_id = i + start;
    memset(cm, 0, sizeof(*cm));
    cm->finalizer = tab[i].finalizer;
    cm->gc_mark = tab[i].gc_mark;
    if (JS_NewClass1(rt, class_id, cm, tab[i].class_name) < 0)
      return -1;
  }
  return 0;
}

BOOL js_class_has_bytecode(JSClassID class_id) {
  return (class_id == JS_CLASS_BYTECODE_FUNCTION ||
          class_id == JS_CLASS_GENERATOR_FUNCTION ||
          class_id == JS_CLASS_ASYNC_FUNCTION ||
          class_id == JS_CLASS_ASYNC_GENERATOR_FUNCTION);
}
