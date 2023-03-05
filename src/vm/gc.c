#include "gc.h"

#include "cfunc.h"
#include "class.h"
#include "func.h"
#include "instr.h"
#include "intrins/intrins.h"
#include "iter.h"
#include "libs/list.h"
#include "mod.h"
#include "num.h"
#include "obj.h"

/* -- Malloc ----------------------------------- */

size_t js_malloc_usable_size_unknown(const void *ptr) { return 0; }

void *js_malloc_rt(JSRuntime *rt, size_t size) {
  return rt->mf.js_malloc(&rt->malloc_state, size);
}

void js_free_rt(JSRuntime *rt, void *ptr) {
  rt->mf.js_free(&rt->malloc_state, ptr);
}

void *js_realloc_rt(JSRuntime *rt, void *ptr, size_t size) {
  return rt->mf.js_realloc(&rt->malloc_state, ptr, size);
}

size_t js_malloc_usable_size_rt(JSRuntime *rt, const void *ptr) {
  return rt->mf.js_malloc_usable_size(ptr);
}

void *js_mallocz_rt(JSRuntime *rt, size_t size) {
  void *ptr;
  ptr = js_malloc_rt(rt, size);
  if (!ptr)
    return NULL;
  return memset(ptr, 0, size);
}

#ifdef CONFIG_BIGNUM
/* called by libbf */
void *js_bf_realloc(void *opaque, void *ptr, size_t size) {
  JSRuntime *rt = opaque;
  return js_realloc_rt(rt, ptr, size);
}
#endif /* CONFIG_BIGNUM */

/* Throw out of memory in case of error */
void *js_malloc(JSContext *ctx, size_t size) {
  void *ptr;
  ptr = js_malloc_rt(ctx->rt, size);
  if (unlikely(!ptr)) {
    JS_ThrowOutOfMemory(ctx);
    return NULL;
  }
  return ptr;
}

/* Throw out of memory in case of error */
void *js_mallocz(JSContext *ctx, size_t size) {
  void *ptr;
  ptr = js_mallocz_rt(ctx->rt, size);
  if (unlikely(!ptr)) {
    JS_ThrowOutOfMemory(ctx);
    return NULL;
  }
  return ptr;
}

void js_free(JSContext *ctx, void *ptr) { js_free_rt(ctx->rt, ptr); }

/* Throw out of memory in case of error */
void *js_realloc(JSContext *ctx, void *ptr, size_t size) {
  void *ret;
  ret = js_realloc_rt(ctx->rt, ptr, size);
  if (unlikely(!ret && size != 0)) {
    JS_ThrowOutOfMemory(ctx);
    return NULL;
  }
  return ret;
}

/* store extra allocated size in *pslack if successful */
void *js_realloc2(JSContext *ctx, void *ptr, size_t size, size_t *pslack) {
  void *ret;
  ret = js_realloc_rt(ctx->rt, ptr, size);
  if (unlikely(!ret && size != 0)) {
    JS_ThrowOutOfMemory(ctx);
    return NULL;
  }
  if (pslack) {
    size_t new_size = js_malloc_usable_size_rt(ctx->rt, ret);
    *pslack = (new_size > size) ? new_size - size : 0;
  }
  return ret;
}

size_t js_malloc_usable_size(JSContext *ctx, const void *ptr) {
  return js_malloc_usable_size_rt(ctx->rt, ptr);
}

/* Throw out of memory exception in case of error */
char *js_strndup(JSContext *ctx, const char *s, size_t n) {
  char *ptr;
  ptr = js_malloc(ctx, n + 1);
  if (ptr) {
    memcpy(ptr, s, n);
    ptr[n] = '\0';
  }
  return ptr;
}

char *js_strdup(JSContext *ctx, const char *str) {
  return js_strndup(ctx, str, strlen(str));
}

no_inline int js_realloc_array(JSContext *ctx, void **parray, int elem_size,
                               int *psize, int req_size) {
  int new_size;
  size_t slack;
  void *new_array;
  /* XXX: potential arithmetic overflow */
  new_size = max_int(req_size, *psize * 3 / 2);
  new_array = js_realloc2(ctx, *parray, new_size * elem_size, &slack);
  if (!new_array)
    return -1;
  new_size += slack / elem_size;
  *psize = new_size;
  *parray = new_array;
  return 0;
}

/* -- Object mark ----------------------------------- */

/* indicate that the object may be part of a function prototype cycle */
void set_cycle_flag(JSContext *ctx, JSValueConst obj) {}

void free_var_ref(JSRuntime *rt, JSVarRef *var_ref) {
  if (var_ref) {
    assert(var_ref->header.ref_count > 0);
    if (--var_ref->header.ref_count == 0) {
      if (var_ref->is_detached) {
        JS_FreeValueRT(rt, var_ref->value);
        remove_gc_object(&var_ref->header);
      } else {
        list_del(&var_ref->header.link); /* still on the stack */
      }
      js_free_rt(rt, var_ref);
    }
  }
}

void js_array_finalizer(JSRuntime *rt, JSValue val) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  int i;

  for (i = 0; i < p->u.array.count; i++) {
    JS_FreeValueRT(rt, p->u.array.u.values[i]);
  }
  js_free_rt(rt, p->u.array.u.values);
}

void js_array_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  int i;

  for (i = 0; i < p->u.array.count; i++) {
    JS_MarkValue(rt, p->u.array.u.values[i], mark_func);
  }
}

void js_object_data_finalizer(JSRuntime *rt, JSValue val) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JS_FreeValueRT(rt, p->u.object_data);
  p->u.object_data = JS_UNDEFINED;
}

void js_object_data_mark(JSRuntime *rt, JSValueConst val,
                         JS_MarkFunc *mark_func) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JS_MarkValue(rt, p->u.object_data, mark_func);
}

void js_c_function_finalizer(JSRuntime *rt, JSValue val) {
  JSObject *p = JS_VALUE_GET_OBJ(val);

  if (p->u.cfunc.realm)
    JS_FreeContext(p->u.cfunc.realm);
}

void js_c_function_mark(JSRuntime *rt, JSValueConst val,
                        JS_MarkFunc *mark_func) {
  JSObject *p = JS_VALUE_GET_OBJ(val);

  if (p->u.cfunc.realm)
    mark_func(rt, &p->u.cfunc.realm->header);
}

void js_bytecode_function_finalizer(JSRuntime *rt, JSValue val) {
  JSObject *p1, *p = JS_VALUE_GET_OBJ(val);
  JSFunctionBytecode *b;
  JSVarRef **var_refs;
  int i;

  p1 = p->u.func.home_object;
  if (p1) {
    JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, p1));
  }
  b = p->u.func.function_bytecode;
  if (b) {
    var_refs = p->u.func.var_refs;
    if (var_refs) {
      for (i = 0; i < b->closure_var_count; i++)
        free_var_ref(rt, var_refs[i]);
      js_free_rt(rt, var_refs);
    }
    JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_FUNCTION_BYTECODE, b));
  }
}

void js_bytecode_function_mark(JSRuntime *rt, JSValueConst val,
                               JS_MarkFunc *mark_func) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JSVarRef **var_refs = p->u.func.var_refs;
  JSFunctionBytecode *b = p->u.func.function_bytecode;
  int i;

  if (p->u.func.home_object) {
    JS_MarkValue(rt, JS_MKPTR(JS_TAG_OBJECT, p->u.func.home_object), mark_func);
  }
  if (b) {
    if (var_refs) {
      for (i = 0; i < b->closure_var_count; i++) {
        JSVarRef *var_ref = var_refs[i];
        if (var_ref && var_ref->is_detached) {
          mark_func(rt, &var_ref->header);
        }
      }
    }
    /* must mark the function bytecode because template objects may be
       part of a cycle */
    JS_MarkValue(rt, JS_MKPTR(JS_TAG_FUNCTION_BYTECODE, b), mark_func);
  }
}

void js_bound_function_finalizer(JSRuntime *rt, JSValue val) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JSBoundFunction *bf = p->u.bound_function;
  int i;

  JS_FreeValueRT(rt, bf->func_obj);
  JS_FreeValueRT(rt, bf->this_val);
  for (i = 0; i < bf->argc; i++) {
    JS_FreeValueRT(rt, bf->argv[i]);
  }
  js_free_rt(rt, bf);
}

void js_bound_function_mark(JSRuntime *rt, JSValueConst val,
                            JS_MarkFunc *mark_func) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JSBoundFunction *bf = p->u.bound_function;
  int i;

  JS_MarkValue(rt, bf->func_obj, mark_func);
  JS_MarkValue(rt, bf->this_val, mark_func);
  for (i = 0; i < bf->argc; i++)
    JS_MarkValue(rt, bf->argv[i], mark_func);
}

void js_for_in_iterator_finalizer(JSRuntime *rt, JSValue val) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JSForInIterator *it = p->u.for_in_iterator;
  JS_FreeValueRT(rt, it->obj);
  js_free_rt(rt, it);
}

void js_for_in_iterator_mark(JSRuntime *rt, JSValueConst val,
                             JS_MarkFunc *mark_func) {
  JSObject *p = JS_VALUE_GET_OBJ(val);
  JSForInIterator *it = p->u.for_in_iterator;
  JS_MarkValue(rt, it->obj, mark_func);
}

static void js_mark_module_def(JSRuntime *rt, JSModuleDef *m,
                               JS_MarkFunc *mark_func) {
  int i;

  for (i = 0; i < m->export_entries_count; i++) {
    JSExportEntry *me = &m->export_entries[i];
    if (me->export_type == JS_EXPORT_TYPE_LOCAL && me->u.local.var_ref) {
      mark_func(rt, &me->u.local.var_ref->header);
    }
  }

  JS_MarkValue(rt, m->module_ns, mark_func);
  JS_MarkValue(rt, m->func_obj, mark_func);
  JS_MarkValue(rt, m->eval_exception, mark_func);
  JS_MarkValue(rt, m->meta_obj, mark_func);
}

static void js_autoinit_mark(JSRuntime *rt, JSProperty *pr,
                             JS_MarkFunc *mark_func) {
  mark_func(rt, &js_autoinit_get_realm(pr)->header);
}

/* used by the GC */
static void JS_MarkContext(JSRuntime *rt, JSContext *ctx,
                           JS_MarkFunc *mark_func) {
  int i;
  struct list_head *el;

  /* modules are not seen by the GC, so we directly mark the objects
     referenced by each module */
  list_for_each(el, &ctx->loaded_modules) {
    JSModuleDef *m = list_entry(el, JSModuleDef, link);
    js_mark_module_def(rt, m, mark_func);
  }

  JS_MarkValue(rt, ctx->global_obj, mark_func);
  JS_MarkValue(rt, ctx->global_var_obj, mark_func);

  JS_MarkValue(rt, ctx->throw_type_error, mark_func);
  JS_MarkValue(rt, ctx->eval_obj, mark_func);

  JS_MarkValue(rt, ctx->array_proto_values, mark_func);
  for (i = 0; i < JS_NATIVE_ERROR_COUNT; i++) {
    JS_MarkValue(rt, ctx->native_error_proto[i], mark_func);
  }
  for (i = 0; i < rt->class_count; i++) {
    JS_MarkValue(rt, ctx->class_proto[i], mark_func);
  }
  JS_MarkValue(rt, ctx->iterator_proto, mark_func);
  JS_MarkValue(rt, ctx->async_iterator_proto, mark_func);
  JS_MarkValue(rt, ctx->promise_ctor, mark_func);
  JS_MarkValue(rt, ctx->array_ctor, mark_func);
  JS_MarkValue(rt, ctx->regexp_ctor, mark_func);
  JS_MarkValue(rt, ctx->function_ctor, mark_func);
  JS_MarkValue(rt, ctx->function_proto, mark_func);

  if (ctx->array_shape)
    mark_func(rt, &ctx->array_shape->header);
}

/* -- Garbage collection ----------------------------------- */

void add_gc_object(JSRuntime *rt, JSGCObjectHeader *h,
                   JSGCObjectTypeEnum type) {
  h->mark = 0;
  h->gc_obj_type = type;
  list_add_tail(&h->link, &rt->gc_obj_list);
}

void remove_gc_object(JSGCObjectHeader *h) { list_del(&h->link); }

void JS_MarkValue(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func) {
  if (JS_VALUE_HAS_REF_COUNT(val)) {
    switch (JS_VALUE_GET_TAG(val)) {
    case JS_TAG_OBJECT:
    case JS_TAG_FUNCTION_BYTECODE:
      mark_func(rt, JS_VALUE_GET_PTR(val));
      break;
    default:
      break;
    }
  }
}

static void mark_children(JSRuntime *rt, JSGCObjectHeader *gp,
                          JS_MarkFunc *mark_func) {
  switch (gp->gc_obj_type) {
  case JS_GC_OBJ_TYPE_JS_OBJECT: {
    JSObject *p = (JSObject *)gp;
    JSShapeProperty *prs;
    JSShape *sh;
    int i;
    sh = p->shape;
    mark_func(rt, &sh->header);
    /* mark all the fields */
    prs = get_shape_prop(sh);
    for (i = 0; i < sh->prop_count; i++) {
      JSProperty *pr = &p->prop[i];
      if (prs->atom != JS_ATOM_NULL) {
        if (prs->flags & JS_PROP_TMASK) {
          if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
            if (pr->u.getset.getter)
              mark_func(rt, &pr->u.getset.getter->header);
            if (pr->u.getset.setter)
              mark_func(rt, &pr->u.getset.setter->header);
          } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
            if (pr->u.var_ref->is_detached) {
              /* Note: the tag does not matter
                 provided it is a GC object */
              mark_func(rt, &pr->u.var_ref->header);
            }
          } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
            js_autoinit_mark(rt, pr, mark_func);
          }
        } else {
          JS_MarkValue(rt, pr->u.value, mark_func);
        }
      }
      prs++;
    }

    if (p->class_id != JS_CLASS_OBJECT) {
      JSClassGCMark *gc_mark;
      gc_mark = rt->class_array[p->class_id].gc_mark;
      if (gc_mark)
        gc_mark(rt, JS_MKPTR(JS_TAG_OBJECT, p), mark_func);
    }
  } break;
  case JS_GC_OBJ_TYPE_FUNCTION_BYTECODE:
    /* the template objects can be part of a cycle */
    {
      JSFunctionBytecode *b = (JSFunctionBytecode *)gp;
      int i;
      for (i = 0; i < b->cpool_count; i++) {
        JS_MarkValue(rt, b->cpool[i], mark_func);
      }
      if (b->realm)
        mark_func(rt, &b->realm->header);
    }
    break;
  case JS_GC_OBJ_TYPE_VAR_REF: {
    JSVarRef *var_ref = (JSVarRef *)gp;
    /* only detached variable referenced are taken into account */
    assert(var_ref->is_detached);
    JS_MarkValue(rt, *var_ref->pvalue, mark_func);
  } break;
  case JS_GC_OBJ_TYPE_ASYNC_FUNCTION: {
    JSAsyncFunctionData *s = (JSAsyncFunctionData *)gp;
    if (s->is_active)
      async_func_mark(rt, &s->func_state, mark_func);
    JS_MarkValue(rt, s->resolving_funcs[0], mark_func);
    JS_MarkValue(rt, s->resolving_funcs[1], mark_func);
  } break;
  case JS_GC_OBJ_TYPE_SHAPE: {
    JSShape *sh = (JSShape *)gp;
    if (sh->proto != NULL) {
      mark_func(rt, &sh->proto->header);
    }
  } break;
  case JS_GC_OBJ_TYPE_JS_CONTEXT: {
    JSContext *ctx = (JSContext *)gp;
    JS_MarkContext(rt, ctx, mark_func);
  } break;
  default:
    abort();
  }
}

static void gc_decref_child(JSRuntime *rt, JSGCObjectHeader *p) {
  assert(p->ref_count > 0);
  p->ref_count--;
  if (p->ref_count == 0 && p->mark == 1) {
    list_del(&p->link);
    list_add_tail(&p->link, &rt->tmp_obj_list);
  }
}

void gc_decref(JSRuntime *rt) {
  struct list_head *el, *el1;
  JSGCObjectHeader *p;

  init_list_head(&rt->tmp_obj_list);

  /* decrement the refcount of all the children of all the GC
     objects and move the GC objects with zero refcount to
     tmp_obj_list */
  list_for_each_safe(el, el1, &rt->gc_obj_list) {
    p = list_entry(el, JSGCObjectHeader, link);
    assert(p->mark == 0);
    mark_children(rt, p, gc_decref_child);
    p->mark = 1;
    if (p->ref_count == 0) {
      list_del(&p->link);
      list_add_tail(&p->link, &rt->tmp_obj_list);
    }
  }
}

static void gc_scan_incref_child(JSRuntime *rt, JSGCObjectHeader *p) {
  p->ref_count++;
  if (p->ref_count == 1) {
    /* ref_count was 0: remove from tmp_obj_list and add at the
       end of gc_obj_list */
    list_del(&p->link);
    list_add_tail(&p->link, &rt->gc_obj_list);
    p->mark = 0; /* reset the mark for the next GC call */
  }
}

static void gc_scan_incref_child2(JSRuntime *rt, JSGCObjectHeader *p) {
  p->ref_count++;
}

static void gc_scan(JSRuntime *rt) {
  struct list_head *el;
  JSGCObjectHeader *p;

  /* keep the objects with a refcount > 0 and their children. */
  list_for_each(el, &rt->gc_obj_list) {
    p = list_entry(el, JSGCObjectHeader, link);
    assert(p->ref_count > 0);
    p->mark = 0; /* reset the mark for the next GC call */
    mark_children(rt, p, gc_scan_incref_child);
  }

  /* restore the refcount of the objects to be deleted. */
  list_for_each(el, &rt->tmp_obj_list) {
    p = list_entry(el, JSGCObjectHeader, link);
    mark_children(rt, p, gc_scan_incref_child2);
  }
}

static void gc_free_cycles(JSRuntime *rt) {
  struct list_head *el, *el1;
  JSGCObjectHeader *p;
#ifdef DUMP_GC_FREE
  BOOL header_done = FALSE;
#endif

  rt->gc_phase = JS_GC_PHASE_REMOVE_CYCLES;

  for (;;) {
    el = rt->tmp_obj_list.next;
    if (el == &rt->tmp_obj_list)
      break;
    p = list_entry(el, JSGCObjectHeader, link);
    /* Only need to free the GC object associated with JS
       values. The rest will be automatically removed because they
       must be referenced by them. */
    switch (p->gc_obj_type) {
    case JS_GC_OBJ_TYPE_JS_OBJECT:
    case JS_GC_OBJ_TYPE_FUNCTION_BYTECODE:
#ifdef DUMP_GC_FREE
      if (!header_done) {
        printf("Freeing cycles:\n");
        JS_DumpObjectHeader(rt);
        header_done = TRUE;
      }
      JS_DumpGCObject(rt, p);
#endif
      free_gc_object(rt, p);
      break;
    default:
      list_del(&p->link);
      list_add_tail(&p->link, &rt->gc_zero_ref_count_list);
      break;
    }
  }
  rt->gc_phase = JS_GC_PHASE_NONE;

  list_for_each_safe(el, el1, &rt->gc_zero_ref_count_list) {
    p = list_entry(el, JSGCObjectHeader, link);
    assert(p->gc_obj_type == JS_GC_OBJ_TYPE_JS_OBJECT ||
           p->gc_obj_type == JS_GC_OBJ_TYPE_FUNCTION_BYTECODE);
    js_free_rt(rt, p);
  }

  init_list_head(&rt->gc_zero_ref_count_list);
}

void JS_RunGC(JSRuntime *rt) {
  /* decrement the reference of the children of each object. mark =
     1 after this pass. */
  gc_decref(rt);

  /* keep the GC objects with a non zero refcount and their childs */
  gc_scan(rt);

  /* free the GC objects in a cycle */
  gc_free_cycles(rt);
}

void js_trigger_gc(JSRuntime *rt, size_t size) {
  BOOL force_gc;
#ifdef FORCE_GC_AT_MALLOC
  force_gc = TRUE;
#else
  force_gc = ((rt->malloc_state.malloc_size + size) > rt->malloc_gc_threshold);
#endif
  if (force_gc) {
#ifdef DUMP_GC
    printf("GC: size=%" PRIu64 "\n", (uint64_t)rt->malloc_state.malloc_size);
#endif
    JS_RunGC(rt);
    rt->malloc_gc_threshold =
        rt->malloc_state.malloc_size + (rt->malloc_state.malloc_size >> 1);
  }
}

void JS_SetMemoryLimit(JSRuntime *rt, size_t limit) {
  rt->malloc_state.malloc_limit = limit;
}

/* use -1 to disable automatic GC */
void JS_SetGCThreshold(JSRuntime *rt, size_t gc_threshold) {
  rt->malloc_gc_threshold = gc_threshold;
}

#define malloc(s) malloc_is_forbidden(s)
#define free(p) free_is_forbidden(p)
#define realloc(p, s) realloc_is_forbidden(p, s)

/* -- Free ----------------------------------- */

static void reset_weak_ref(JSRuntime *rt, JSObject *p) {
  JSMapRecord *mr, *mr_next;
#ifndef NDEBUG
  JSMapState *s;
#endif

  /* first pass to remove the records from the WeakMap/WeakSet
     lists */
  for (mr = p->first_weak_ref; mr != NULL; mr = mr->next_weak_ref) {
#ifndef NDEBUG
    s = mr->map;
#endif
    assert(s->is_weak);
    assert(!mr->empty); /* no iterator on WeakMap/WeakSet */
    list_del(&mr->hash_link);
    list_del(&mr->link);
  }

  /* second pass to free the values to avoid modifying the weak
     reference list while traversing it. */
  for (mr = p->first_weak_ref; mr != NULL; mr = mr_next) {
    mr_next = mr->next_weak_ref;
    JS_FreeValueRT(rt, mr->value);
    js_free_rt(rt, mr);
  }

  p->first_weak_ref = NULL; /* fail safe */
}

void free_bytecode_atoms(JSRuntime *rt, const uint8_t *bc_buf, int bc_len,
                         BOOL use_short_opcodes) {
  int pos, len, op;
  JSAtom atom;
  const JSOpCode *oi;

  pos = 0;
  while (pos < bc_len) {
    op = bc_buf[pos];
    if (use_short_opcodes)
      oi = &short_opcode_info(op);
    else
      oi = &opcode_info[op];

    len = oi->size;
    switch (oi->fmt) {
    case OP_FMT_atom:
    case OP_FMT_atom_u8:
    case OP_FMT_atom_u16:
    case OP_FMT_atom_label_u8:
    case OP_FMT_atom_label_u16:
      atom = get_u32(bc_buf + pos + 1);
      JS_FreeAtomRT(rt, atom);
      break;
    default:
      break;
    }
    pos += len;
  }
}

static void free_object(JSRuntime *rt, JSObject *p) {
  int i;
  JSClassFinalizer *finalizer;
  JSShape *sh;
  JSShapeProperty *pr;

  p->free_mark = 1; /* used to tell the object is invalid when
                       freeing cycles */
  /* free all the fields */
  sh = p->shape;
  pr = get_shape_prop(sh);
  for (i = 0; i < sh->prop_count; i++) {
    free_property(rt, &p->prop[i], pr->flags);
    pr++;
  }
  js_free_rt(rt, p->prop);
  /* as an optimization we destroy the shape immediately without
     putting it in gc_zero_ref_count_list */
  js_free_shape(rt, sh);

  /* fail safe */
  p->shape = NULL;
  p->prop = NULL;

  if (unlikely(p->first_weak_ref)) {
    reset_weak_ref(rt, p);
  }

  finalizer = rt->class_array[p->class_id].finalizer;
  if (finalizer)
    (*finalizer)(rt, JS_MKPTR(JS_TAG_OBJECT, p));

  /* fail safe */
  p->class_id = 0;
  p->u.opaque = NULL;
  p->u.func.var_refs = NULL;
  p->u.func.home_object = NULL;

  remove_gc_object(&p->header);
  if (rt->gc_phase == JS_GC_PHASE_REMOVE_CYCLES && p->header.ref_count != 0) {
    list_add_tail(&p->header.link, &rt->gc_zero_ref_count_list);
  } else {
    js_free_rt(rt, p);
  }
}

static void free_function_bytecode(JSRuntime *rt, JSFunctionBytecode *b) {
  int i;

#if 0
    {
        char buf[ATOM_GET_STR_BUF_SIZE];
        printf("freeing %s\n",
               JS_AtomGetStrRT(rt, buf, sizeof(buf), b->func_name));
    }
#endif
  free_bytecode_atoms(rt, b->byte_code_buf, b->byte_code_len, TRUE);

  if (b->vardefs) {
    for (i = 0; i < b->arg_count + b->var_count; i++) {
      JS_FreeAtomRT(rt, b->vardefs[i].var_name);
    }
  }
  for (i = 0; i < b->cpool_count; i++)
    JS_FreeValueRT(rt, b->cpool[i]);

  for (i = 0; i < b->closure_var_count; i++) {
    JSClosureVar *cv = &b->closure_var[i];
    JS_FreeAtomRT(rt, cv->var_name);
  }
  if (b->realm)
    JS_FreeContext(b->realm);

  JS_FreeAtomRT(rt, b->func_name);
  if (b->has_debug) {
    JS_FreeAtomRT(rt, b->debug.filename);
    js_free_rt(rt, b->debug.pc2line_buf);
    js_free_rt(rt, b->debug.source);
  }

  remove_gc_object(&b->header);
  if (rt->gc_phase == JS_GC_PHASE_REMOVE_CYCLES && b->header.ref_count != 0) {
    list_add_tail(&b->header.link, &rt->gc_zero_ref_count_list);
  } else {
    js_free_rt(rt, b);
  }
}

void free_gc_object(JSRuntime *rt, JSGCObjectHeader *gp) {
  switch (gp->gc_obj_type) {
  case JS_GC_OBJ_TYPE_JS_OBJECT:
    free_object(rt, (JSObject *)gp);
    break;
  case JS_GC_OBJ_TYPE_FUNCTION_BYTECODE:
    free_function_bytecode(rt, (JSFunctionBytecode *)gp);
    break;
  default:
    abort();
  }
}

static void free_zero_refcount(JSRuntime *rt) {
  struct list_head *el;
  JSGCObjectHeader *p;

  rt->gc_phase = JS_GC_PHASE_DECREF;
  for (;;) {
    el = rt->gc_zero_ref_count_list.next;
    if (el == &rt->gc_zero_ref_count_list)
      break;
    p = list_entry(el, JSGCObjectHeader, link);
    assert(p->ref_count == 0);
    free_gc_object(rt, p);
  }
  rt->gc_phase = JS_GC_PHASE_NONE;
}

/* called with the ref_count of 'v' reaches zero. */
void __JS_FreeValueRT(JSRuntime *rt, JSValue v) {
  uint32_t tag = JS_VALUE_GET_TAG(v);

#ifdef DUMP_FREE
  {
    printf("Freeing ");
    if (tag == JS_TAG_OBJECT) {
      JS_DumpObject(rt, JS_VALUE_GET_OBJ(v));
    } else {
      JS_DumpValueShort(rt, v);
      printf("\n");
    }
  }
#endif

  switch (tag) {
  case JS_TAG_STRING: {
    JSString *p = JS_VALUE_GET_STRING(v);
    if (p->atom_type) {
      JS_FreeAtomStruct(rt, p);
    } else {
#ifdef DUMP_LEAKS
      list_del(&p->link);
#endif
      js_free_rt(rt, p);
    }
  } break;
  case JS_TAG_OBJECT:
  case JS_TAG_FUNCTION_BYTECODE: {
    JSGCObjectHeader *p = JS_VALUE_GET_PTR(v);
    if (rt->gc_phase != JS_GC_PHASE_REMOVE_CYCLES) {
      list_del(&p->link);
      list_add(&p->link, &rt->gc_zero_ref_count_list);
      if (rt->gc_phase == JS_GC_PHASE_NONE) {
        free_zero_refcount(rt);
      }
    }
  } break;
  case JS_TAG_MODULE:
    abort(); /* never freed here */
    break;
#ifdef CONFIG_BIGNUM
  case JS_TAG_BIG_INT:
  case JS_TAG_BIG_FLOAT: {
    JSBigFloat *bf = JS_VALUE_GET_PTR(v);
    bf_delete(&bf->num);
    js_free_rt(rt, bf);
  } break;
  case JS_TAG_BIG_DECIMAL: {
    JSBigDecimal *bf = JS_VALUE_GET_PTR(v);
    bfdec_delete(&bf->num);
    js_free_rt(rt, bf);
  } break;
#endif
  case JS_TAG_SYMBOL: {
    JSAtomStruct *p = JS_VALUE_GET_PTR(v);
    JS_FreeAtomStruct(rt, p);
  } break;
  default:
    printf("__JS_FreeValue: unknown tag=%d\n", tag);
    abort();
  }
}

void __JS_FreeValue(JSContext *ctx, JSValue v) { __JS_FreeValueRT(ctx->rt, v); }

/* Return false if not an object or if the object has already been
   freed (zombie objects are visible in finalizers when freeing
   cycles). */
BOOL JS_IsLiveObject(JSRuntime *rt, JSValueConst obj) {
  JSObject *p;
  if (!JS_IsObject(obj))
    return FALSE;
  p = JS_VALUE_GET_OBJ(obj);
  return !p->free_mark;
}

/* -- MemoryUsage ----------------------------------- */

/* Compute memory used by various object types */
/* XXX: poor man's approach to handling multiply referenced objects */
typedef struct JSMemoryUsage_helper {
  double memory_used_count;
  double str_count;
  double str_size;
  int64_t js_func_count;
  double js_func_size;
  int64_t js_func_code_size;
  int64_t js_func_pc2line_count;
  int64_t js_func_pc2line_size;
} JSMemoryUsage_helper;

static void compute_value_size(JSValueConst val, JSMemoryUsage_helper *hp);

static void compute_jsstring_size(JSString *str, JSMemoryUsage_helper *hp) {
  if (!str->atom_type) { /* atoms are handled separately */
    double s_ref_count = str->header.ref_count;
    hp->str_count += 1 / s_ref_count;
    hp->str_size += ((sizeof(*str) + (str->len << str->is_wide_char) + 1 -
                      str->is_wide_char) /
                     s_ref_count);
  }
}

static void compute_bytecode_size(JSFunctionBytecode *b,
                                  JSMemoryUsage_helper *hp) {
  int memory_used_count, js_func_size, i;

  memory_used_count = 0;
  js_func_size = offsetof(JSFunctionBytecode, debug);
  if (b->vardefs) {
    js_func_size += (b->arg_count + b->var_count) * sizeof(*b->vardefs);
  }
  if (b->cpool) {
    js_func_size += b->cpool_count * sizeof(*b->cpool);
    for (i = 0; i < b->cpool_count; i++) {
      JSValueConst val = b->cpool[i];
      compute_value_size(val, hp);
    }
  }
  if (b->closure_var) {
    js_func_size += b->closure_var_count * sizeof(*b->closure_var);
  }
  if (!b->read_only_bytecode && b->byte_code_buf) {
    hp->js_func_code_size += b->byte_code_len;
  }
  if (b->has_debug) {
    js_func_size += sizeof(*b) - offsetof(JSFunctionBytecode, debug);
    if (b->debug.source) {
      memory_used_count++;
      js_func_size += b->debug.source_len + 1;
    }
    if (b->debug.pc2line_len) {
      memory_used_count++;
      hp->js_func_pc2line_count += 1;
      hp->js_func_pc2line_size += b->debug.pc2line_len;
    }
  }
  hp->js_func_size += js_func_size;
  hp->js_func_count += 1;
  hp->memory_used_count += memory_used_count;
}

static void compute_value_size(JSValueConst val, JSMemoryUsage_helper *hp) {
  switch (JS_VALUE_GET_TAG(val)) {
  case JS_TAG_STRING:
    compute_jsstring_size(JS_VALUE_GET_STRING(val), hp);
    break;
#ifdef CONFIG_BIGNUM
  case JS_TAG_BIG_INT:
  case JS_TAG_BIG_FLOAT:
  case JS_TAG_BIG_DECIMAL:
    /* should track JSBigFloat usage */
    break;
#endif
  }
}

void JS_ComputeMemoryUsage(JSRuntime *rt, JSMemoryUsage *s) {
  struct list_head *el, *el1;
  int i;
  JSMemoryUsage_helper mem = {0}, *hp = &mem;

  memset(s, 0, sizeof(*s));
  s->malloc_count = rt->malloc_state.malloc_count;
  s->malloc_size = rt->malloc_state.malloc_size;
  s->malloc_limit = rt->malloc_state.malloc_limit;

  s->memory_used_count = 2; /* rt + rt->class_array */
  s->memory_used_size = sizeof(JSRuntime) + sizeof(JSValue) * rt->class_count;

  list_for_each(el, &rt->context_list) {
    JSContext *ctx = list_entry(el, JSContext, link);
    JSShape *sh = ctx->array_shape;
    s->memory_used_count += 2; /* ctx + ctx->class_proto */
    s->memory_used_size +=
        sizeof(JSContext) + sizeof(JSValue) * rt->class_count;
    s->binary_object_count += ctx->binary_object_count;
    s->binary_object_size += ctx->binary_object_size;

    /* the hashed shapes are counted separately */
    if (sh && !sh->is_hashed) {
      int hash_size = sh->prop_hash_mask + 1;
      s->shape_count++;
      s->shape_size += get_shape_size(hash_size, sh->prop_size);
    }
    list_for_each(el1, &ctx->loaded_modules) {
      JSModuleDef *m = list_entry(el1, JSModuleDef, link);
      s->memory_used_count += 1;
      s->memory_used_size += sizeof(*m);
      if (m->req_module_entries) {
        s->memory_used_count += 1;
        s->memory_used_size +=
            m->req_module_entries_count * sizeof(*m->req_module_entries);
      }
      if (m->export_entries) {
        s->memory_used_count += 1;
        s->memory_used_size +=
            m->export_entries_count * sizeof(*m->export_entries);
        for (i = 0; i < m->export_entries_count; i++) {
          JSExportEntry *me = &m->export_entries[i];
          if (me->export_type == JS_EXPORT_TYPE_LOCAL && me->u.local.var_ref) {
            /* potential multiple count */
            s->memory_used_count += 1;
            compute_value_size(me->u.local.var_ref->value, hp);
          }
        }
      }
      if (m->star_export_entries) {
        s->memory_used_count += 1;
        s->memory_used_size +=
            m->star_export_entries_count * sizeof(*m->star_export_entries);
      }
      if (m->import_entries) {
        s->memory_used_count += 1;
        s->memory_used_size +=
            m->import_entries_count * sizeof(*m->import_entries);
      }
      compute_value_size(m->module_ns, hp);
      compute_value_size(m->func_obj, hp);
    }
  }

  list_for_each(el, &rt->gc_obj_list) {
    JSGCObjectHeader *gp = list_entry(el, JSGCObjectHeader, link);
    JSObject *p;
    JSShape *sh;
    JSShapeProperty *prs;

    /* XXX: could count the other GC object types too */
    if (gp->gc_obj_type == JS_GC_OBJ_TYPE_FUNCTION_BYTECODE) {
      compute_bytecode_size((JSFunctionBytecode *)gp, hp);
      continue;
    } else if (gp->gc_obj_type != JS_GC_OBJ_TYPE_JS_OBJECT) {
      continue;
    }
    p = (JSObject *)gp;
    sh = p->shape;
    s->obj_count++;
    if (p->prop) {
      s->memory_used_count++;
      s->prop_size += sh->prop_size * sizeof(*p->prop);
      s->prop_count += sh->prop_count;
      prs = get_shape_prop(sh);
      for (i = 0; i < sh->prop_count; i++) {
        JSProperty *pr = &p->prop[i];
        if (prs->atom != JS_ATOM_NULL && !(prs->flags & JS_PROP_TMASK)) {
          compute_value_size(pr->u.value, hp);
        }
        prs++;
      }
    }
    /* the hashed shapes are counted separately */
    if (!sh->is_hashed) {
      int hash_size = sh->prop_hash_mask + 1;
      s->shape_count++;
      s->shape_size += get_shape_size(hash_size, sh->prop_size);
    }

    switch (p->class_id) {
    case JS_CLASS_ARRAY:     /* u.array | length */
    case JS_CLASS_ARGUMENTS: /* u.array | length */
      s->array_count++;
      if (p->fast_array) {
        s->fast_array_count++;
        if (p->u.array.u.values) {
          s->memory_used_count++;
          s->memory_used_size +=
              p->u.array.count * sizeof(*p->u.array.u.values);
          s->fast_array_elements += p->u.array.count;
          for (i = 0; i < p->u.array.count; i++) {
            compute_value_size(p->u.array.u.values[i], hp);
          }
        }
      }
      break;
    case JS_CLASS_NUMBER:  /* u.object_data */
    case JS_CLASS_STRING:  /* u.object_data */
    case JS_CLASS_BOOLEAN: /* u.object_data */
    case JS_CLASS_SYMBOL:  /* u.object_data */
    case JS_CLASS_DATE:    /* u.object_data */
#ifdef CONFIG_BIGNUM
    case JS_CLASS_BIG_INT:     /* u.object_data */
    case JS_CLASS_BIG_FLOAT:   /* u.object_data */
    case JS_CLASS_BIG_DECIMAL: /* u.object_data */
#endif
      compute_value_size(p->u.object_data, hp);
      break;
    case JS_CLASS_C_FUNCTION: /* u.cfunc */
      s->c_func_count++;
      break;
    case JS_CLASS_BYTECODE_FUNCTION: /* u.func */
    {
      JSFunctionBytecode *b = p->u.func.function_bytecode;
      JSVarRef **var_refs = p->u.func.var_refs;
      /* home_object: object will be accounted for in list scan */
      if (var_refs) {
        s->memory_used_count++;
        s->js_func_size += b->closure_var_count * sizeof(*var_refs);
        for (i = 0; i < b->closure_var_count; i++) {
          if (var_refs[i]) {
            double ref_count = var_refs[i]->header.ref_count;
            s->memory_used_count += 1 / ref_count;
            s->js_func_size += sizeof(*var_refs[i]) / ref_count;
            /* handle non object closed values */
            if (var_refs[i]->pvalue == &var_refs[i]->value) {
              /* potential multiple count */
              compute_value_size(var_refs[i]->value, hp);
            }
          }
        }
      }
    } break;
    case JS_CLASS_BOUND_FUNCTION: /* u.bound_function */
    {
      JSBoundFunction *bf = p->u.bound_function;
      /* func_obj and this_val are objects */
      for (i = 0; i < bf->argc; i++) {
        compute_value_size(bf->argv[i], hp);
      }
      s->memory_used_count += 1;
      s->memory_used_size += sizeof(*bf) + bf->argc * sizeof(*bf->argv);
    } break;
    case JS_CLASS_C_FUNCTION_DATA: /* u.c_function_data_record */
    {
      JSCFunctionDataRecord *fd = p->u.c_function_data_record;
      if (fd) {
        for (i = 0; i < fd->data_len; i++) {
          compute_value_size(fd->data[i], hp);
        }
        s->memory_used_count += 1;
        s->memory_used_size += sizeof(*fd) + fd->data_len * sizeof(*fd->data);
      }
    } break;
    case JS_CLASS_REGEXP: /* u.regexp */
      compute_jsstring_size(p->u.regexp.pattern, hp);
      compute_jsstring_size(p->u.regexp.bytecode, hp);
      break;

    case JS_CLASS_FOR_IN_ITERATOR: /* u.for_in_iterator */
    {
      JSForInIterator *it = p->u.for_in_iterator;
      if (it) {
        compute_value_size(it->obj, hp);
        s->memory_used_count += 1;
        s->memory_used_size += sizeof(*it);
      }
    } break;
    case JS_CLASS_ARRAY_BUFFER:        /* u.array_buffer */
    case JS_CLASS_SHARED_ARRAY_BUFFER: /* u.array_buffer */
    {
      JSArrayBuffer *abuf = p->u.array_buffer;
      if (abuf) {
        s->memory_used_count += 1;
        s->memory_used_size += sizeof(*abuf);
        if (abuf->data) {
          s->memory_used_count += 1;
          s->memory_used_size += abuf->byte_length;
        }
      }
    } break;
    case JS_CLASS_GENERATOR:    /* u.generator_data */
    case JS_CLASS_UINT8C_ARRAY: /* u.typed_array / u.array */
    case JS_CLASS_INT8_ARRAY:   /* u.typed_array / u.array */
    case JS_CLASS_UINT8_ARRAY:  /* u.typed_array / u.array */
    case JS_CLASS_INT16_ARRAY:  /* u.typed_array / u.array */
    case JS_CLASS_UINT16_ARRAY: /* u.typed_array / u.array */
    case JS_CLASS_INT32_ARRAY:  /* u.typed_array / u.array */
    case JS_CLASS_UINT32_ARRAY: /* u.typed_array / u.array */
#ifdef CONFIG_BIGNUM
    case JS_CLASS_BIG_INT64_ARRAY:  /* u.typed_array / u.array */
    case JS_CLASS_BIG_UINT64_ARRAY: /* u.typed_array / u.array */
#endif
    case JS_CLASS_FLOAT32_ARRAY: /* u.typed_array / u.array */
    case JS_CLASS_FLOAT64_ARRAY: /* u.typed_array / u.array */
    case JS_CLASS_DATAVIEW:      /* u.typed_array */
#ifdef CONFIG_BIGNUM
    case JS_CLASS_FLOAT_ENV: /* u.float_env */
#endif
    case JS_CLASS_MAP:                      /* u.map_state */
    case JS_CLASS_SET:                      /* u.map_state */
    case JS_CLASS_WEAKMAP:                  /* u.map_state */
    case JS_CLASS_WEAKSET:                  /* u.map_state */
    case JS_CLASS_MAP_ITERATOR:             /* u.map_iterator_data */
    case JS_CLASS_SET_ITERATOR:             /* u.map_iterator_data */
    case JS_CLASS_ARRAY_ITERATOR:           /* u.array_iterator_data */
    case JS_CLASS_STRING_ITERATOR:          /* u.array_iterator_data */
    case JS_CLASS_PROXY:                    /* u.proxy_data */
    case JS_CLASS_PROMISE:                  /* u.promise_data */
    case JS_CLASS_PROMISE_RESOLVE_FUNCTION: /* u.promise_function_data */
    case JS_CLASS_PROMISE_REJECT_FUNCTION:  /* u.promise_function_data */
    case JS_CLASS_ASYNC_FUNCTION_RESOLVE:   /* u.async_function_data */
    case JS_CLASS_ASYNC_FUNCTION_REJECT:    /* u.async_function_data */
    case JS_CLASS_ASYNC_FROM_SYNC_ITERATOR: /* u.async_from_sync_iterator_data
                                             */
    case JS_CLASS_ASYNC_GENERATOR:          /* u.async_generator_data */
                                            /* TODO */
    default:
      /* XXX: class definition should have an opaque block size */
      if (p->u.opaque) {
        s->memory_used_count += 1;
      }
      break;
    }
  }
  s->obj_size += s->obj_count * sizeof(JSObject);

  /* hashed shapes */
  s->memory_used_count++; /* rt->shape_hash */
  s->memory_used_size += sizeof(rt->shape_hash[0]) * rt->shape_hash_size;
  for (i = 0; i < rt->shape_hash_size; i++) {
    JSShape *sh;
    for (sh = rt->shape_hash[i]; sh != NULL; sh = sh->shape_hash_next) {
      int hash_size = sh->prop_hash_mask + 1;
      s->shape_count++;
      s->shape_size += get_shape_size(hash_size, sh->prop_size);
    }
  }

  /* atoms */
  s->memory_used_count += 2; /* rt->atom_array, rt->atom_hash */
  s->atom_count = rt->atom_count;
  s->atom_size = sizeof(rt->atom_array[0]) * rt->atom_size +
                 sizeof(rt->atom_hash[0]) * rt->atom_hash_size;
  for (i = 0; i < rt->atom_size; i++) {
    JSAtomStruct *p = rt->atom_array[i];
    if (!atom_is_free(p)) {
      s->atom_size +=
          (sizeof(*p) + (p->len << p->is_wide_char) + 1 - p->is_wide_char);
    }
  }
  s->str_count = round(mem.str_count);
  s->str_size = round(mem.str_size);
  s->js_func_count = mem.js_func_count;
  s->js_func_size = round(mem.js_func_size);
  s->js_func_code_size = mem.js_func_code_size;
  s->js_func_pc2line_count = mem.js_func_pc2line_count;
  s->js_func_pc2line_size = mem.js_func_pc2line_size;
  s->memory_used_count += round(mem.memory_used_count) + s->atom_count +
                          s->str_count + s->obj_count + s->shape_count +
                          s->js_func_count + s->js_func_pc2line_count;
  s->memory_used_size += s->atom_size + s->str_size + s->obj_size +
                         s->prop_size + s->shape_size + s->js_func_size +
                         s->js_func_code_size + s->js_func_pc2line_size;
}

#ifndef CONFIG_VERSION
#define CONFIG_VERSION "Unknown"
#endif

void JS_DumpMemoryUsage(FILE *fp, const JSMemoryUsage *s, JSRuntime *rt) {
  fprintf(fp,
          "QuickJS memory usage -- "
#ifdef CONFIG_BIGNUM
          "BigNum "
#endif
          CONFIG_VERSION " version, %d-bit, malloc limit: %" PRId64 "\n\n",
          (int)sizeof(void *) * 8, (int64_t)(ssize_t)s->malloc_limit);
#if 1
  if (rt) {
    static const struct {
      const char *name;
      size_t size;
    } object_types[] = {
        {"JSRuntime", sizeof(JSRuntime)},
        {"JSContext", sizeof(JSContext)},
        {"JSObject", sizeof(JSObject)},
        {"JSString", sizeof(JSString)},
        {"JSFunctionBytecode", sizeof(JSFunctionBytecode)},
    };
    int i, usage_size_ok = 0;
    for (i = 0; i < countof(object_types); i++) {
      unsigned int size = object_types[i].size;
      void *p = js_malloc_rt(rt, size);
      if (p) {
        unsigned int size1 = js_malloc_usable_size_rt(rt, p);
        if (size1 >= size) {
          usage_size_ok = 1;
          fprintf(fp, "  %3u + %-2u  %s\n", size, size1 - size,
                  object_types[i].name);
        }
        js_free_rt(rt, p);
      }
    }
    if (!usage_size_ok) {
      fprintf(fp, "  malloc_usable_size unavailable\n");
    }
    {
      int obj_classes[JS_CLASS_INIT_COUNT + 1] = {0};
      int class_id;
      struct list_head *el;
      list_for_each(el, &rt->gc_obj_list) {
        JSGCObjectHeader *gp = list_entry(el, JSGCObjectHeader, link);
        JSObject *p;
        if (gp->gc_obj_type == JS_GC_OBJ_TYPE_JS_OBJECT) {
          p = (JSObject *)gp;
          obj_classes[min_uint32(p->class_id, JS_CLASS_INIT_COUNT)]++;
        }
      }
      fprintf(fp, "\n"
                  "JSObject classes\n");
      if (obj_classes[0])
        fprintf(fp, "  %5d  %2.0d %s\n", obj_classes[0], 0, "none");
      for (class_id = 1; class_id < JS_CLASS_INIT_COUNT; class_id++) {
        if (obj_classes[class_id]) {
          char buf[ATOM_GET_STR_BUF_SIZE];
          fprintf(fp, "  %5d  %2.0d %s\n", obj_classes[class_id], class_id,
                  JS_AtomGetStrRT(rt, buf, sizeof(buf),
                                  js_std_class_def[class_id - 1].class_name));
        }
      }
      if (obj_classes[JS_CLASS_INIT_COUNT])
        fprintf(fp, "  %5d  %2.0d %s\n", obj_classes[JS_CLASS_INIT_COUNT], 0,
                "other");
    }
    fprintf(fp, "\n");
  }
#endif
  fprintf(fp, "%-20s %8s %8s\n", "NAME", "COUNT", "SIZE");

  if (s->malloc_count) {
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per block)\n",
            "memory allocated", s->malloc_count, s->malloc_size,
            (double)s->malloc_size / s->malloc_count);
    fprintf(fp,
            "%-20s %8" PRId64 " %8" PRId64
            "  (%d overhead, %0.1f average slack)\n",
            "memory used", s->memory_used_count, s->memory_used_size,
            MALLOC_OVERHEAD,
            ((double)(s->malloc_size - s->memory_used_size) /
             s->memory_used_count));
  }
  if (s->atom_count) {
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per atom)\n", "atoms",
            s->atom_count, s->atom_size, (double)s->atom_size / s->atom_count);
  }
  if (s->str_count) {
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per string)\n",
            "strings", s->str_count, s->str_size,
            (double)s->str_size / s->str_count);
  }
  if (s->obj_count) {
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per object)\n",
            "objects", s->obj_count, s->obj_size,
            (double)s->obj_size / s->obj_count);
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per object)\n",
            "  properties", s->prop_count, s->prop_size,
            (double)s->prop_count / s->obj_count);
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per shape)\n",
            "  shapes", s->shape_count, s->shape_size,
            (double)s->shape_size / s->shape_count);
  }
  if (s->js_func_count) {
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "\n", "bytecode functions",
            s->js_func_count, s->js_func_size);
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per function)\n",
            "  bytecode", s->js_func_count, s->js_func_code_size,
            (double)s->js_func_code_size / s->js_func_count);
    if (s->js_func_pc2line_count) {
      fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per function)\n",
              "  pc2line", s->js_func_pc2line_count, s->js_func_pc2line_size,
              (double)s->js_func_pc2line_size / s->js_func_pc2line_count);
    }
  }
  if (s->c_func_count) {
    fprintf(fp, "%-20s %8" PRId64 "\n", "C functions", s->c_func_count);
  }
  if (s->array_count) {
    fprintf(fp, "%-20s %8" PRId64 "\n", "arrays", s->array_count);
    if (s->fast_array_count) {
      fprintf(fp, "%-20s %8" PRId64 "\n", "  fast arrays", s->fast_array_count);
      fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "  (%0.1f per fast array)\n",
              "  elements", s->fast_array_elements,
              s->fast_array_elements * (int)sizeof(JSValue),
              (double)s->fast_array_elements / s->fast_array_count);
    }
  }
  if (s->binary_object_count) {
    fprintf(fp, "%-20s %8" PRId64 " %8" PRId64 "\n", "binary objects",
            s->binary_object_count, s->binary_object_size);
  }
}
