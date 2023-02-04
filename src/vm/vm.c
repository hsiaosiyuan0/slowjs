#include "vm.h"

#include "class.h"
#include "error.h"
#include "func.h"
#include "mod.h"
#include "obj.h"
#include "parse/parse.h"

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
  int eval_type = eval_flags & JS_EVAL_TYPE_MASK;
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

/* Run the <eval> function of the module and of all its requested
   modules. */
JSValue js_evaluate_module(JSContext *ctx, JSModuleDef *m) {
  JSModuleDef *m1;
  int i;
  JSValue ret_val;

  if (m->eval_mark)
    return JS_UNDEFINED; /* avoid cycles */

  if (m->evaluated) {
    /* if the module was already evaluated, rethrow the exception
       it raised */
    if (m->eval_has_exception) {
      return JS_Throw(ctx, JS_DupValue(ctx, m->eval_exception));
    } else {
      return JS_UNDEFINED;
    }
  }

  m->eval_mark = TRUE;

  for (i = 0; i < m->req_module_entries_count; i++) {
    JSReqModuleEntry *rme = &m->req_module_entries[i];
    m1 = rme->module;
    if (!m1->eval_mark) {
      ret_val = js_evaluate_module(ctx, m1);
      if (JS_IsException(ret_val)) {
        m->eval_mark = FALSE;
        return ret_val;
      }
      JS_FreeValue(ctx, ret_val);
    }
  }

  if (m->init_func) {
    /* C module init */
    if (m->init_func(ctx, m) < 0)
      ret_val = JS_EXCEPTION;
    else
      ret_val = JS_UNDEFINED;
  } else {
    ret_val = JS_CallFree(ctx, m->func_obj, JS_UNDEFINED, 0, NULL);
    m->func_obj = JS_UNDEFINED;
  }
  if (JS_IsException(ret_val)) {
    /* save the thrown exception value */
    m->eval_has_exception = TRUE;
    m->eval_exception = JS_DupValue(ctx, ctx->rt->current_exception);
  }
  m->eval_mark = FALSE;
  m->evaluated = TRUE;
  return ret_val;
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