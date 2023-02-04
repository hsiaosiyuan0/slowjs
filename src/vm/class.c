#include "class.h"

#include "str.h"

static JSClassID js_class_id_alloc = JS_CLASS_INIT_COUNT;

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
