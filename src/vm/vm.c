#include "vm.h"

#include "cfunc.h"
#include "class.h"
#include "error.h"
#include "func.h"
#include "intrins/intrins.h"
#include "mod.h"
#include "obj.h"
#include "parse/parse.h"

#ifdef DUMP_LEAKS
#include "dump.h"
#endif

/* -- JSContext --------------------------------- */

JSContext *JS_NewContextRaw(JSRuntime *rt) {
  JSContext *ctx;
  int i;

  ctx = js_mallocz_rt(rt, sizeof(JSContext));
  if (!ctx)
    return NULL;
  ctx->header.ref_count = 1;
  add_gc_object(rt, &ctx->header, JS_GC_OBJ_TYPE_JS_CONTEXT);

  ctx->class_proto =
      js_malloc_rt(rt, sizeof(ctx->class_proto[0]) * rt->class_count);
  if (!ctx->class_proto) {
    js_free_rt(rt, ctx);
    return NULL;
  }
  ctx->rt = rt;
  list_add_tail(&ctx->link, &rt->context_list);
#ifdef CONFIG_BIGNUM
  ctx->bf_ctx = &rt->bf_ctx;
  ctx->fp_env.prec = 113;
  ctx->fp_env.flags = bf_set_exp_bits(15) | BF_RNDN | BF_FLAG_SUBNORMAL;
#endif
  for (i = 0; i < rt->class_count; i++)
    ctx->class_proto[i] = JS_NULL;
  ctx->array_ctor = JS_NULL;
  ctx->regexp_ctor = JS_NULL;
  ctx->promise_ctor = JS_NULL;
  init_list_head(&ctx->loaded_modules);

  JS_AddIntrinsicBasicObjects(ctx);
  return ctx;
}

JSContext *JS_NewContext(JSRuntime *rt) {
  JSContext *ctx;

  ctx = JS_NewContextRaw(rt);
  if (!ctx)
    return NULL;

  JS_AddIntrinsicBaseObjects(ctx);
  JS_AddIntrinsicDate(ctx);
  JS_AddIntrinsicEval(ctx);
  JS_AddIntrinsicStringNormalize(ctx);
  JS_AddIntrinsicRegExp(ctx);
  JS_AddIntrinsicJSON(ctx);
  JS_AddIntrinsicProxy(ctx);
  JS_AddIntrinsicMapSet(ctx);
  JS_AddIntrinsicTypedArrays(ctx);
  JS_AddIntrinsicPromise(ctx);
#ifdef CONFIG_BIGNUM
  JS_AddIntrinsicBigInt(ctx);
#endif
  return ctx;
}

void *JS_GetContextOpaque(JSContext *ctx) { return ctx->user_opaque; }

void JS_SetContextOpaque(JSContext *ctx, void *opaque) {
  ctx->user_opaque = opaque;
}

JSContext *JS_DupContext(JSContext *ctx) {
  ctx->header.ref_count++;
  return ctx;
}

/* WARNING: obj is freed */
JSValue JS_Throw(JSContext *ctx, JSValue obj) {
  JSRuntime *rt = ctx->rt;
  JS_FreeValue(ctx, rt->current_exception);
  rt->current_exception = obj;
  return JS_EXCEPTION;
}

/* return the pending exception (cannot be called twice). */
JSValue JS_GetException(JSContext *ctx) {
  JSValue val;
  JSRuntime *rt = ctx->rt;
  val = rt->current_exception;
  rt->current_exception = JS_NULL;
  return val;
}

void JS_FreeContext(JSContext *ctx) {
  JSRuntime *rt = ctx->rt;
  int i;

  if (--ctx->header.ref_count > 0)
    return;
  assert(ctx->header.ref_count == 0);

#ifdef DUMP_ATOMS
  JS_DumpAtoms(ctx->rt);
#endif
#ifdef DUMP_SHAPES
  JS_DumpShapes(ctx->rt);
#endif
#ifdef DUMP_OBJECTS
  {
    struct list_head *el;
    JSGCObjectHeader *p;
    printf("JSObjects: {\n");
    JS_DumpObjectHeader(ctx->rt);
    list_for_each(el, &rt->gc_obj_list) {
      p = list_entry(el, JSGCObjectHeader, link);
      JS_DumpGCObject(rt, p);
    }
    printf("}\n");
  }
#endif
#ifdef DUMP_MEM
  {
    JSMemoryUsage stats;
    JS_ComputeMemoryUsage(rt, &stats);
    JS_DumpMemoryUsage(stdout, &stats, rt);
  }
#endif

  js_free_modules(ctx, JS_FREE_MODULE_ALL);

  JS_FreeValue(ctx, ctx->global_obj);
  JS_FreeValue(ctx, ctx->global_var_obj);

  JS_FreeValue(ctx, ctx->throw_type_error);
  JS_FreeValue(ctx, ctx->eval_obj);

  JS_FreeValue(ctx, ctx->array_proto_values);
  for (i = 0; i < JS_NATIVE_ERROR_COUNT; i++) {
    JS_FreeValue(ctx, ctx->native_error_proto[i]);
  }
  for (i = 0; i < rt->class_count; i++) {
    JS_FreeValue(ctx, ctx->class_proto[i]);
  }
  js_free_rt(rt, ctx->class_proto);
  JS_FreeValue(ctx, ctx->iterator_proto);
  JS_FreeValue(ctx, ctx->async_iterator_proto);
  JS_FreeValue(ctx, ctx->promise_ctor);
  JS_FreeValue(ctx, ctx->array_ctor);
  JS_FreeValue(ctx, ctx->regexp_ctor);
  JS_FreeValue(ctx, ctx->function_ctor);
  JS_FreeValue(ctx, ctx->function_proto);

  js_free_shape_null(ctx->rt, ctx->array_shape);

  list_del(&ctx->link);
  remove_gc_object(&ctx->header);
  js_free_rt(ctx->rt, ctx);
}

JSRuntime *JS_GetRuntime(JSContext *ctx) { return ctx->rt; }

void JS_SetClassProto(JSContext *ctx, JSClassID class_id, JSValue obj) {
#ifndef NDEBUG
  JSRuntime *rt = ctx->rt;
#endif
  assert(class_id < rt->class_count);
  set_value(ctx, &ctx->class_proto[class_id], obj);
}

JSValue JS_GetClassProto(JSContext *ctx, JSClassID class_id) {
#ifndef NDEBUG
  JSRuntime *rt = ctx->rt;
#endif
  assert(class_id < rt->class_count);
  return JS_DupValue(ctx, ctx->class_proto[class_id]);
}

JSValue JS_GetGlobalObject(JSContext *ctx) {
  return JS_DupValue(ctx, ctx->global_obj);
}

/* -- JSRuntime --------------------------------- */

/* default memory allocation functions with memory limitation */
static inline size_t js_def_malloc_usable_size(void *ptr) {
#if defined(__APPLE__)
  return malloc_size(ptr);
#elif defined(_WIN32)
  return _msize(ptr);
#elif defined(EMSCRIPTEN)
  return 0;
#elif defined(__linux__)
  return malloc_usable_size(ptr);
#else
  /* change this to `return 0;` if compilation fails */
  return malloc_usable_size(ptr);
#endif
}

static void *js_def_malloc(JSMallocState *s, size_t size) {
  void *ptr;

  /* Do not allocate zero bytes: behavior is platform dependent */
  assert(size != 0);

  if (unlikely(s->malloc_size + size > s->malloc_limit))
    return NULL;

  ptr = malloc(size);
  if (!ptr)
    return NULL;

  s->malloc_count++;
  s->malloc_size += js_def_malloc_usable_size(ptr) + MALLOC_OVERHEAD;
  return ptr;
}

static void js_def_free(JSMallocState *s, void *ptr) {
  if (!ptr)
    return;

  s->malloc_count--;
  s->malloc_size -= js_def_malloc_usable_size(ptr) + MALLOC_OVERHEAD;
  free(ptr);
}

static void *js_def_realloc(JSMallocState *s, void *ptr, size_t size) {
  size_t old_size;

  if (!ptr) {
    if (size == 0)
      return NULL;
    return js_def_malloc(s, size);
  }
  old_size = js_def_malloc_usable_size(ptr);
  if (size == 0) {
    s->malloc_count--;
    s->malloc_size -= old_size + MALLOC_OVERHEAD;
    free(ptr);
    return NULL;
  }
  if (s->malloc_size + size - old_size > s->malloc_limit)
    return NULL;

  ptr = realloc(ptr, size);
  if (!ptr)
    return NULL;

  s->malloc_size += js_def_malloc_usable_size(ptr) - old_size;
  return ptr;
}

static const JSMallocFunctions def_malloc_funcs = {
    js_def_malloc,
    js_def_free,
    js_def_realloc,
#if defined(__APPLE__)
    malloc_size,
#elif defined(_WIN32)
    (size_t(*)(const void *))_msize,
#elif defined(EMSCRIPTEN)
    NULL,
#elif defined(__linux__)
    (size_t(*)(const void *))malloc_usable_size,
#else
    /* change this to `NULL,` if compilation fails */
    malloc_usable_size,
#endif
};

#ifdef CONFIG_BIGNUM
static JSValue JS_ThrowUnsupportedOperation(JSContext *ctx) {
  return JS_ThrowTypeError(ctx, "unsupported operation");
}

static JSValue invalid_to_string(JSContext *ctx, JSValueConst val) {
  return JS_ThrowUnsupportedOperation(ctx);
}

static JSValue invalid_from_string(JSContext *ctx, const char *buf, int radix,
                                   int flags, slimb_t *pexponent) {
  return JS_NAN;
}

static int invalid_unary_arith(JSContext *ctx, JSValue *pres, OPCodeEnum op,
                               JSValue op1) {
  JS_FreeValue(ctx, op1);
  JS_ThrowUnsupportedOperation(ctx);
  return -1;
}

static int invalid_binary_arith(JSContext *ctx, OPCodeEnum op, JSValue *pres,
                                JSValue op1, JSValue op2) {
  JS_FreeValue(ctx, op1);
  JS_FreeValue(ctx, op2);
  JS_ThrowUnsupportedOperation(ctx);
  return -1;
}

static JSValue invalid_mul_pow10_to_float64(JSContext *ctx, const bf_t *a,
                                            int64_t exponent) {
  return JS_ThrowUnsupportedOperation(ctx);
}

static int invalid_mul_pow10(JSContext *ctx, JSValue *sp) {
  JS_ThrowUnsupportedOperation(ctx);
  return -1;
}

static void set_dummy_numeric_ops(JSNumericOperations *ops) {
  ops->to_string = invalid_to_string;
  ops->from_string = invalid_from_string;
  ops->unary_arith = invalid_unary_arith;
  ops->binary_arith = invalid_binary_arith;
  ops->mul_pow10_to_float64 = invalid_mul_pow10_to_float64;
  ops->mul_pow10 = invalid_mul_pow10;
}

#endif /* CONFIG_BIGNUM */

JSRuntime *JS_NewRuntime2(const JSMallocFunctions *mf, void *opaque) {
  JSRuntime *rt;
  JSMallocState ms;

  memset(&ms, 0, sizeof(ms));
  ms.opaque = opaque;
  ms.malloc_limit = -1;

  rt = mf->js_malloc(&ms, sizeof(JSRuntime));
  if (!rt)
    return NULL;
  memset(rt, 0, sizeof(*rt));
  rt->mf = *mf;
  if (!rt->mf.js_malloc_usable_size) {
    /* use dummy function if none provided */
    rt->mf.js_malloc_usable_size = js_malloc_usable_size_unknown;
  }
  rt->malloc_state = ms;
  rt->malloc_gc_threshold = 256 * 1024;

#ifdef CONFIG_BIGNUM
  bf_context_init(&rt->bf_ctx, js_bf_realloc, rt);
  set_dummy_numeric_ops(&rt->bigint_ops);
  set_dummy_numeric_ops(&rt->bigfloat_ops);
  set_dummy_numeric_ops(&rt->bigdecimal_ops);
#endif

  init_list_head(&rt->context_list);
  init_list_head(&rt->gc_obj_list);
  init_list_head(&rt->gc_zero_ref_count_list);
  rt->gc_phase = JS_GC_PHASE_NONE;

#ifdef DUMP_LEAKS
  init_list_head(&rt->string_list);
#endif
  init_list_head(&rt->job_list);

  if (JS_InitAtoms(rt))
    goto fail;

  /* create the object, array and function classes */
  if (init_class_range(rt, js_std_class_def, JS_CLASS_OBJECT,
                       countof(js_std_class_def)) < 0)
    goto fail;
  rt->class_array[JS_CLASS_ARGUMENTS].exotic = &js_arguments_exotic_methods;
  rt->class_array[JS_CLASS_STRING].exotic = &js_string_exotic_methods;
  rt->class_array[JS_CLASS_MODULE_NS].exotic = &js_module_ns_exotic_methods;

  rt->class_array[JS_CLASS_C_FUNCTION].call = js_call_c_function;
  rt->class_array[JS_CLASS_C_FUNCTION_DATA].call = js_c_function_data_call;
  rt->class_array[JS_CLASS_BOUND_FUNCTION].call = js_call_bound_function;
  rt->class_array[JS_CLASS_GENERATOR_FUNCTION].call =
      js_generator_function_call;
  if (init_shape_hash(rt))
    goto fail;

  rt->stack_size = JS_DEFAULT_STACK_SIZE;
  JS_UpdateStackTop(rt);

  rt->current_exception = JS_NULL;

  return rt;
fail:
  JS_FreeRuntime(rt);
  return NULL;
}

JSRuntime *JS_NewRuntime(void) {
  return JS_NewRuntime2(&def_malloc_funcs, NULL);
}

void JS_SetRuntimeInfo(JSRuntime *rt, const char *s) {
  if (rt)
    rt->rt_info = s;
}

void JS_FreeRuntime(JSRuntime *rt) {
  struct list_head *el, *el1;
  int i;

  JS_FreeValueRT(rt, rt->current_exception);

  list_for_each_safe(el, el1, &rt->job_list) {
    JSJobEntry *e = list_entry(el, JSJobEntry, link);
    for (i = 0; i < e->argc; i++)
      JS_FreeValueRT(rt, e->argv[i]);
    js_free_rt(rt, e);
  }
  init_list_head(&rt->job_list);

  JS_RunGC(rt);

#ifdef DUMP_LEAKS
  /* leaking objects */
  {
    BOOL header_done;
    JSGCObjectHeader *p;
    int count;

    /* remove the internal refcounts to display only the object
       referenced externally */
    list_for_each(el, &rt->gc_obj_list) {
      p = list_entry(el, JSGCObjectHeader, link);
      p->mark = 0;
    }
    gc_decref(rt);

    header_done = FALSE;
    list_for_each(el, &rt->gc_obj_list) {
      p = list_entry(el, JSGCObjectHeader, link);
      if (p->ref_count != 0) {
        if (!header_done) {
          printf("Object leaks:\n");
          JS_DumpObjectHeader(rt);
          header_done = TRUE;
        }
        JS_DumpGCObject(rt, p);
      }
    }

    count = 0;
    list_for_each(el, &rt->gc_obj_list) {
      p = list_entry(el, JSGCObjectHeader, link);
      if (p->ref_count == 0) {
        count++;
      }
    }
    if (count != 0)
      printf("Secondary object leaks: %d\n", count);
  }
#endif
  assert(list_empty(&rt->gc_obj_list));

  /* free the classes */
  for (i = 0; i < rt->class_count; i++) {
    JSClass *cl = &rt->class_array[i];
    if (cl->class_id != 0) {
      JS_FreeAtomRT(rt, cl->class_name);
    }
  }
  js_free_rt(rt, rt->class_array);

#ifdef CONFIG_BIGNUM
  bf_context_end(&rt->bf_ctx);
#endif

#ifdef DUMP_LEAKS
  /* only the atoms defined in JS_InitAtoms() should be left */
  {
    BOOL header_done = FALSE;

    for (i = 0; i < rt->atom_size; i++) {
      JSAtomStruct *p = rt->atom_array[i];
      if (!atom_is_free(p) /* && p->str*/) {
        if (i >= JS_ATOM_END || p->header.ref_count != 1) {
          if (!header_done) {
            header_done = TRUE;
            if (rt->rt_info) {
              printf("%s:1: atom leakage:", rt->rt_info);
            } else {
              printf("Atom leaks:\n"
                     "    %6s %6s %s\n",
                     "ID", "REFCNT", "NAME");
            }
          }
          if (rt->rt_info) {
            printf(" ");
          } else {
            printf("    %6u %6u ", i, p->header.ref_count);
          }
          switch (p->atom_type) {
          case JS_ATOM_TYPE_STRING:
            JS_DumpString(rt, p);
            break;
          case JS_ATOM_TYPE_GLOBAL_SYMBOL:
            printf("Symbol.for(");
            JS_DumpString(rt, p);
            printf(")");
            break;
          case JS_ATOM_TYPE_SYMBOL:
            if (p->hash == JS_ATOM_HASH_SYMBOL) {
              printf("Symbol(");
              JS_DumpString(rt, p);
              printf(")");
            } else {
              printf("Private(");
              JS_DumpString(rt, p);
              printf(")");
            }
            break;
          }
          if (rt->rt_info) {
            printf(":%u", p->header.ref_count);
          } else {
            printf("\n");
          }
        }
      }
    }
    if (rt->rt_info && header_done)
      printf("\n");
  }
#endif

  /* free the atoms */
  for (i = 0; i < rt->atom_size; i++) {
    JSAtomStruct *p = rt->atom_array[i];
    if (!atom_is_free(p)) {
#ifdef DUMP_LEAKS
      list_del(&p->link);
#endif
      js_free_rt(rt, p);
    }
  }
  js_free_rt(rt, rt->atom_array);
  js_free_rt(rt, rt->atom_hash);
  js_free_rt(rt, rt->shape_hash);
#ifdef DUMP_LEAKS
  if (!list_empty(&rt->string_list)) {
    if (rt->rt_info) {
      printf("%s:1: string leakage:", rt->rt_info);
    } else {
      printf("String leaks:\n"
             "    %6s %s\n",
             "REFCNT", "VALUE");
    }
    list_for_each_safe(el, el1, &rt->string_list) {
      JSString *str = list_entry(el, JSString, link);
      if (rt->rt_info) {
        printf(" ");
      } else {
        printf("    %6u ", str->header.ref_count);
      }
      JS_DumpString(rt, str);
      if (rt->rt_info) {
        printf(":%u", str->header.ref_count);
      } else {
        printf("\n");
      }
      list_del(&str->link);
      js_free_rt(rt, str);
    }
    if (rt->rt_info)
      printf("\n");
  }
  {
    JSMallocState *s = &rt->malloc_state;
    if (s->malloc_count > 1) {
      if (rt->rt_info)
        printf("%s:1: ", rt->rt_info);
      printf("Memory leak: %" PRIu64 " bytes lost in %" PRIu64 " block%s\n",
             (uint64_t)(s->malloc_size - sizeof(JSRuntime)),
             (uint64_t)(s->malloc_count - 1), &"s"[s->malloc_count == 2]);
    }
  }
#endif

  {
    JSMallocState ms = rt->malloc_state;
    rt->mf.js_free(&ms, rt);
  }
}

void *JS_GetRuntimeOpaque(JSRuntime *rt) { return rt->user_opaque; }

void JS_SetRuntimeOpaque(JSRuntime *rt, void *opaque) {
  rt->user_opaque = opaque;
}

static void update_stack_limit(JSRuntime *rt) {
  if (rt->stack_size == 0) {
    rt->stack_limit = 0; /* no limit */
  } else {
    rt->stack_limit = rt->stack_top - rt->stack_size;
  }
}

void JS_SetMaxStackSize(JSRuntime *rt, size_t stack_size) {
  rt->stack_size = stack_size;
  update_stack_limit(rt);
}

void JS_UpdateStackTop(JSRuntime *rt) {
  rt->stack_top = js_get_stack_pointer();
  update_stack_limit(rt);
}

void JS_SetInterruptHandler(JSRuntime *rt, JSInterruptHandler *cb,
                            void *opaque) {
  rt->interrupt_handler = cb;
  rt->interrupt_opaque = opaque;
}

void JS_SetPCInterruptHandler(JSRuntime *rt, JSPcInterruptHandler *cb,
                              void *opaque) {
  rt->pc_interrupt_handler = cb;
  rt->pc_interrupt_opaque = opaque;
}

void JS_SetCanBlock(JSRuntime *rt, BOOL can_block) {
  rt->can_block = can_block;
}

void JS_SetSharedArrayBufferFunctions(JSRuntime *rt,
                                      const JSSharedArrayBufferFunctions *sf) {
  rt->sab_funcs = *sf;
}

/* -- Global variable ----------------------------------- */

/* flags is 0, DEFINE_GLOBAL_LEX_VAR or DEFINE_GLOBAL_FUNC_VAR */
/* XXX: could support exotic global object. */
int JS_CheckDefineGlobalVar(JSContext *ctx, JSAtom prop, int flags) {
  JSObject *p;
  JSShapeProperty *prs;

  p = JS_VALUE_GET_OBJ(ctx->global_obj);
  prs = find_own_property1(p, prop);
  /* XXX: should handle JS_PROP_AUTOINIT */
  if (flags & DEFINE_GLOBAL_LEX_VAR) {
    if (prs && !(prs->flags & JS_PROP_CONFIGURABLE))
      goto fail_redeclaration;
  } else {
    if (!prs && !p->extensible)
      goto define_error;
    if (flags & DEFINE_GLOBAL_FUNC_VAR) {
      if (prs) {
        if (!(prs->flags & JS_PROP_CONFIGURABLE) &&
            ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET ||
             ((prs->flags & (JS_PROP_WRITABLE | JS_PROP_ENUMERABLE)) !=
              (JS_PROP_WRITABLE | JS_PROP_ENUMERABLE)))) {
        define_error:
          JS_ThrowTypeErrorAtom(ctx, "cannot define variable '%s'", prop);
          return -1;
        }
      }
    }
  }
  /* check if there already is a lexical declaration */
  p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
  prs = find_own_property1(p, prop);
  if (prs) {
  fail_redeclaration:
    JS_ThrowSyntaxErrorVarRedeclaration(ctx, prop);
    return -1;
  }
  return 0;
}

/* def_flags is (0, DEFINE_GLOBAL_LEX_VAR) |
   JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE */
/* XXX: could support exotic global object. */
int JS_DefineGlobalVar(JSContext *ctx, JSAtom prop, int def_flags) {
  JSObject *p;
  JSShapeProperty *prs;
  JSProperty *pr;
  JSValue val;
  int flags;

  if (def_flags & DEFINE_GLOBAL_LEX_VAR) {
    p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
    flags = JS_PROP_ENUMERABLE | (def_flags & JS_PROP_WRITABLE) |
            JS_PROP_CONFIGURABLE;
    val = JS_UNINITIALIZED;
  } else {
    p = JS_VALUE_GET_OBJ(ctx->global_obj);
    flags = JS_PROP_ENUMERABLE | JS_PROP_WRITABLE |
            (def_flags & JS_PROP_CONFIGURABLE);
    val = JS_UNDEFINED;
  }
  prs = find_own_property1(p, prop);
  if (prs)
    return 0;
  if (!p->extensible)
    return 0;
  pr = add_property(ctx, p, prop, flags);
  if (unlikely(!pr))
    return -1;
  pr->u.value = val;
  return 0;
}

/* 'def_flags' is 0 or JS_PROP_CONFIGURABLE. */
/* XXX: could support exotic global object. */
int JS_DefineGlobalFunction(JSContext *ctx, JSAtom prop, JSValueConst func,
                            int def_flags) {

  JSObject *p;
  JSShapeProperty *prs;
  int flags;

  p = JS_VALUE_GET_OBJ(ctx->global_obj);
  prs = find_own_property1(p, prop);
  flags = JS_PROP_HAS_VALUE | JS_PROP_THROW;
  if (!prs || (prs->flags & JS_PROP_CONFIGURABLE)) {
    flags |= JS_PROP_ENUMERABLE | JS_PROP_WRITABLE | def_flags |
             JS_PROP_HAS_CONFIGURABLE | JS_PROP_HAS_WRITABLE |
             JS_PROP_HAS_ENUMERABLE;
  }
  if (JS_DefineProperty(ctx, ctx->global_obj, prop, func, JS_UNDEFINED,
                        JS_UNDEFINED, flags) < 0)
    return -1;
  return 0;
}

JSValue JS_GetGlobalVar(JSContext *ctx, JSAtom prop, BOOL throw_ref_error) {
  JSObject *p;
  JSShapeProperty *prs;
  JSProperty *pr;

  /* no exotic behavior is possible in global_var_obj */
  p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
  prs = find_own_property(&pr, p, prop);
  if (prs) {
    /* XXX: should handle JS_PROP_TMASK properties */
    if (unlikely(JS_IsUninitialized(pr->u.value)))
      return JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
    return JS_DupValue(ctx, pr->u.value);
  }
  return JS_GetPropertyInternal(ctx, ctx->global_obj, prop, ctx->global_obj,
                                throw_ref_error);
}

/* construct a reference to a global variable */
int JS_GetGlobalVarRef(JSContext *ctx, JSAtom prop, JSValue *sp) {
  JSObject *p;
  JSShapeProperty *prs;
  JSProperty *pr;

  /* no exotic behavior is possible in global_var_obj */
  p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
  prs = find_own_property(&pr, p, prop);
  if (prs) {
    /* XXX: should handle JS_PROP_AUTOINIT properties? */
    /* XXX: conformance: do these tests in
       OP_put_var_ref/OP_get_var_ref ? */
    if (unlikely(JS_IsUninitialized(pr->u.value))) {
      JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
      return -1;
    }
    if (unlikely(!(prs->flags & JS_PROP_WRITABLE))) {
      return JS_ThrowTypeErrorReadOnly(ctx, JS_PROP_THROW, prop);
    }
    sp[0] = JS_DupValue(ctx, ctx->global_var_obj);
  } else {
    int ret;
    ret = JS_HasProperty(ctx, ctx->global_obj, prop);
    if (ret < 0)
      return -1;
    if (ret) {
      sp[0] = JS_DupValue(ctx, ctx->global_obj);
    } else {
      sp[0] = JS_UNDEFINED;
    }
  }
  sp[1] = JS_AtomToValue(ctx, prop);
  return 0;
}

/* use for strict variable access: test if the variable exists */
int JS_CheckGlobalVar(JSContext *ctx, JSAtom prop) {
  JSObject *p;
  JSShapeProperty *prs;
  int ret;

  /* no exotic behavior is possible in global_var_obj */
  p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
  prs = find_own_property1(p, prop);
  if (prs) {
    ret = TRUE;
  } else {
    ret = JS_HasProperty(ctx, ctx->global_obj, prop);
    if (ret < 0)
      return -1;
  }
  return ret;
}

/* flag = 0: normal variable write
   flag = 1: initialize lexical variable
   flag = 2: normal variable write, strict check was done before
*/
int JS_SetGlobalVar(JSContext *ctx, JSAtom prop, JSValue val, int flag) {
  JSObject *p;
  JSShapeProperty *prs;
  JSProperty *pr;
  int flags;

  /* no exotic behavior is possible in global_var_obj */
  p = JS_VALUE_GET_OBJ(ctx->global_var_obj);
  prs = find_own_property(&pr, p, prop);
  if (prs) {
    /* XXX: should handle JS_PROP_AUTOINIT properties? */
    if (flag != 1) {
      if (unlikely(JS_IsUninitialized(pr->u.value))) {
        JS_FreeValue(ctx, val);
        JS_ThrowReferenceErrorUninitialized(ctx, prs->atom);
        return -1;
      }
      if (unlikely(!(prs->flags & JS_PROP_WRITABLE))) {
        JS_FreeValue(ctx, val);
        return JS_ThrowTypeErrorReadOnly(ctx, JS_PROP_THROW, prop);
      }
    }
    set_value(ctx, &pr->u.value, val);
    return 0;
  }
  flags = JS_PROP_THROW_STRICT;
  if (is_strict_mode(ctx))
    flags |= JS_PROP_NO_ADD;
  return JS_SetPropertyInternal(ctx, ctx->global_obj, prop, val, flags);
}

/* -- JSStackFrame ----------------------------------- */

/* only valid inside C functions */
JSValueConst JS_GetActiveFunction(JSContext *ctx) {
  return ctx->rt->current_stack_frame->cur_func;
}

/* -- Eval ----------------------------------- */

no_inline __exception int __js_poll_interrupts(JSContext *ctx) {
  JSRuntime *rt = ctx->rt;
  ctx->interrupt_counter = JS_INTERRUPT_COUNTER_INIT;
  if (rt->interrupt_handler) {
    if (rt->interrupt_handler(rt, rt->interrupt_opaque)) {
      /* XXX: should set a specific flag to avoid catching */
      JS_ThrowInternalError(ctx, "interrupted");
      JS_SetUncatchableError(ctx, ctx->rt->current_exception, TRUE);
      return -1;
    }
  }
  return 0;
}

static JSValue JS_EvalFunctionInternal(JSContext *ctx, JSValue fun_obj,
                                       JSValueConst this_obj,
                                       JSVarRef **var_refs, JSStackFrame *sf) {
  JSValue ret_val;
  uint32_t tag;

  tag = JS_VALUE_GET_TAG(fun_obj);
  if (tag == JS_TAG_FUNCTION_BYTECODE) {
    fun_obj = js_closure(ctx, fun_obj, var_refs, sf);
    ret_val = JS_CallFree(ctx, fun_obj, this_obj, 0, NULL);
  } else if (tag == JS_TAG_MODULE) {
    JSModuleDef *m;
    m = JS_VALUE_GET_PTR(fun_obj);
    /* the module refcount should be >= 2 */
    JS_FreeValue(ctx, fun_obj);
    if (js_create_module_function(ctx, m) < 0)
      goto fail;
    if (js_link_module(ctx, m) < 0)
      goto fail;
    ret_val = js_evaluate_module(ctx, m);
    if (JS_IsException(ret_val)) {
    fail:
      js_free_modules(ctx, JS_FREE_MODULE_NOT_EVALUATED);
      return JS_EXCEPTION;
    }
  } else {
    JS_FreeValue(ctx, fun_obj);
    ret_val = JS_ThrowTypeError(ctx, "bytecode function expected");
  }
  return ret_val;
}

JSValue JS_EvalFunction(JSContext *ctx, JSValue fun_obj) {
  return JS_EvalFunctionInternal(ctx, fun_obj, ctx->global_obj, NULL, NULL);
}

/* 'input' must be zero terminated i.e. input[input_len] = '\0'. */
JSValue __JS_EvalInternal(JSContext *ctx, JSValueConst this_obj,
                          const char *input, size_t input_len,
                          const char *filename, int flags, int scope_idx) {
  JSParseState s1, *s = &s1;
  int err, js_mode, eval_type;
  JSValue fun_obj, ret_val;
  JSStackFrame *sf;
  JSVarRef **var_refs;
  JSFunctionBytecode *b;
  JSFunctionDef *fd;
  JSModuleDef *m;

  js_parse_init(ctx, s, input, input_len, filename);
  skip_shebang(s);

  eval_type = flags & JS_EVAL_TYPE_MASK;
  m = NULL;
  if (eval_type == JS_EVAL_TYPE_DIRECT) {
    JSObject *p;
    sf = ctx->rt->current_stack_frame;
    assert(sf != NULL);
    assert(JS_VALUE_GET_TAG(sf->cur_func) == JS_TAG_OBJECT);
    p = JS_VALUE_GET_OBJ(sf->cur_func);
    assert(js_class_has_bytecode(p->class_id));
    b = p->u.func.function_bytecode;
    var_refs = p->u.func.var_refs;
    js_mode = b->js_mode;
  } else {
    sf = NULL;
    b = NULL;
    var_refs = NULL;
    js_mode = 0;
    if (flags & JS_EVAL_FLAG_STRICT)
      js_mode |= JS_MODE_STRICT;
    if (flags & JS_EVAL_FLAG_STRIP)
      js_mode |= JS_MODE_STRIP;
    if (eval_type == JS_EVAL_TYPE_MODULE) {
      JSAtom module_name = JS_NewAtom(ctx, filename);
      if (module_name == JS_ATOM_NULL)
        return JS_EXCEPTION;
      m = js_new_module_def(ctx, module_name);
      if (!m)
        return JS_EXCEPTION;
      js_mode |= JS_MODE_STRICT;
    }
  }
  fd = js_new_function_def(ctx, NULL, TRUE, FALSE, filename, 1);
  if (!fd)
    goto fail1;
  s->cur_func = fd;
  fd->eval_type = eval_type;
  fd->has_this_binding = (eval_type != JS_EVAL_TYPE_DIRECT);
  fd->backtrace_barrier = ((flags & JS_EVAL_FLAG_BACKTRACE_BARRIER) != 0);
  if (eval_type == JS_EVAL_TYPE_DIRECT) {
    fd->new_target_allowed = b->new_target_allowed;
    fd->super_call_allowed = b->super_call_allowed;
    fd->super_allowed = b->super_allowed;
    fd->arguments_allowed = b->arguments_allowed;
  } else {
    fd->new_target_allowed = FALSE;
    fd->super_call_allowed = FALSE;
    fd->super_allowed = FALSE;
    fd->arguments_allowed = TRUE;
  }
  fd->js_mode = js_mode;
  fd->func_name = JS_DupAtom(ctx, JS_ATOM__eval_);
  if (b) {
    if (add_closure_variables(ctx, fd, b, scope_idx))
      goto fail;
  }
  fd->module = m;
  s->is_module = (m != NULL);
  s->allow_html_comments = !s->is_module;

  push_scope(s); /* body scope */
  fd->body_scope = fd->scope_level;

  err = js_parse_program(s);
  if (err) {
  fail:
    free_token(s, &s->token);
    js_free_function_def(ctx, fd);
    goto fail1;
  }

  /* create the function object and all the enclosed functions */
  fun_obj = js_create_function(ctx, fd);
  if (JS_IsException(fun_obj))
    goto fail1;
  /* Could add a flag to avoid resolution if necessary */
  if (m) {
    m->func_obj = fun_obj;
    if (js_resolve_module(ctx, m) < 0)
      goto fail1;
    fun_obj = JS_DupValue(ctx, JS_MKPTR(JS_TAG_MODULE, m));
  }
  if (flags & JS_EVAL_FLAG_COMPILE_ONLY) {
    ret_val = fun_obj;
  } else {
    ret_val = JS_EvalFunctionInternal(ctx, fun_obj, this_obj, var_refs, sf);
  }
  return ret_val;
fail1:
  /* XXX: should free all the unresolved dependencies */
  if (m)
    js_free_module_def(ctx, m);
  return JS_EXCEPTION;
}

/* the indirection is needed to make 'eval' optional */
static JSValue JS_EvalInternal(JSContext *ctx, JSValueConst this_obj,
                               const char *input, size_t input_len,
                               const char *filename, int flags, int scope_idx) {
  if (unlikely(!ctx->eval_internal)) {
    return JS_ThrowTypeError(ctx, "eval is not supported");
  }
  return ctx->eval_internal(ctx, this_obj, input, input_len, filename, flags,
                            scope_idx);
}

JSValue JS_EvalObject(JSContext *ctx, JSValueConst this_obj, JSValueConst val,
                      int flags, int scope_idx) {
  JSValue ret;
  const char *str;
  size_t len;

  if (!JS_IsString(val))
    return JS_DupValue(ctx, val);
  str = JS_ToCStringLen(ctx, &len, val);
  if (!str)
    return JS_EXCEPTION;
  ret = JS_EvalInternal(ctx, this_obj, str, len, "<input>", flags, scope_idx);
  JS_FreeCString(ctx, str);
  return ret;
}

JSValue JS_EvalThis(JSContext *ctx, JSValueConst this_obj, const char *input,
                    size_t input_len, const char *filename, int eval_flags) {
#ifndef NDEBUG
  int eval_type = eval_flags & JS_EVAL_TYPE_MASK;
#endif
  JSValue ret;

  assert(eval_type == JS_EVAL_TYPE_GLOBAL || eval_type == JS_EVAL_TYPE_MODULE);
  ret = JS_EvalInternal(ctx, this_obj, input, input_len, filename, eval_flags,
                        -1);
  return ret;
}

JSValue JS_Eval(JSContext *ctx, const char *input, size_t input_len,
                const char *filename, int eval_flags) {
  return JS_EvalThis(ctx, ctx->global_obj, input, input_len, filename,
                     eval_flags);
}

/* -- Pending job ----------------------------------- */

/* return 0 if OK, < 0 if exception */
int JS_EnqueueJob(JSContext *ctx, JSJobFunc *job_func, int argc,
                  JSValueConst *argv) {
  JSRuntime *rt = ctx->rt;
  JSJobEntry *e;
  int i;

  e = js_malloc(ctx, sizeof(*e) + argc * sizeof(JSValue));
  if (!e)
    return -1;
  e->ctx = ctx;
  e->job_func = job_func;
  e->argc = argc;
  for (i = 0; i < argc; i++) {
    e->argv[i] = JS_DupValue(ctx, argv[i]);
  }
  list_add_tail(&e->link, &rt->job_list);
  return 0;
}

BOOL JS_IsJobPending(JSRuntime *rt) { return !list_empty(&rt->job_list); }

/* return < 0 if exception, 0 if no job pending, 1 if a job was
   executed successfully. the context of the job is stored in '*pctx' */
int JS_ExecutePendingJob(JSRuntime *rt, JSContext **pctx) {
  JSContext *ctx;
  JSJobEntry *e;
  JSValue res;
  int i, ret;

  if (list_empty(&rt->job_list)) {
    *pctx = NULL;
    return 0;
  }

  /* get the first pending job and execute it */
  e = list_entry(rt->job_list.next, JSJobEntry, link);
  list_del(&e->link);
  ctx = e->ctx;
  res = e->job_func(e->ctx, e->argc, (JSValueConst *)e->argv);
  for (i = 0; i < e->argc; i++)
    JS_FreeValue(ctx, e->argv[i]);
  if (JS_IsException(res))
    ret = -1;
  else
    ret = 1;
  JS_FreeValue(ctx, res);
  js_free(ctx, e);
  *pctx = ctx;
  return ret;
}