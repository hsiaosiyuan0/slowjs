#include "intrins.h"

#include "vm/func.h"
#include "vm/iter.h"
#include "vm/vm.h"

/* -- AsyncFunction ----------------------------------- */

const JSCFunctionListEntry js_async_function_proto_funcs[] = {
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "AsyncFunction",
                       JS_PROP_CONFIGURABLE),
};

static JSValue js_async_from_sync_iterator_unwrap(JSContext *ctx,
                                                  JSValueConst this_val,
                                                  int argc, JSValueConst *argv,
                                                  int magic,
                                                  JSValue *func_data) {
  return js_create_iterator_result(ctx, JS_DupValue(ctx, argv[0]),
                                   JS_ToBool(ctx, func_data[0]));
}

static JSValue js_async_from_sync_iterator_unwrap_func_create(JSContext *ctx,
                                                              BOOL done) {
  JSValueConst func_data[1];

  func_data[0] = (JSValueConst)JS_NewBool(ctx, done);
  return JS_NewCFunctionData(ctx, js_async_from_sync_iterator_unwrap, 1, 0, 1,
                             func_data);
}

/* -- AsyncGenerator ----------------------------------- */

static void js_async_generator_free(JSRuntime *rt, JSAsyncGeneratorData *s) {
  struct list_head *el, *el1;
  JSAsyncGeneratorRequest *req;

  list_for_each_safe(el, el1, &s->queue) {
    req = list_entry(el, JSAsyncGeneratorRequest, link);
    JS_FreeValueRT(rt, req->result);
    JS_FreeValueRT(rt, req->promise);
    JS_FreeValueRT(rt, req->resolving_funcs[0]);
    JS_FreeValueRT(rt, req->resolving_funcs[1]);
    js_free_rt(rt, req);
  }
  if (s->state != JS_ASYNC_GENERATOR_STATE_COMPLETED &&
      s->state != JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN) {
    async_func_free(rt, &s->func_state);
  }
  js_free_rt(rt, s);
}

static void js_async_generator_finalizer(JSRuntime *rt, JSValue obj) {
  JSAsyncGeneratorData *s = JS_GetOpaque(obj, JS_CLASS_ASYNC_GENERATOR);

  if (s) {
    js_async_generator_free(rt, s);
  }
}

static void js_async_generator_mark(JSRuntime *rt, JSValueConst val,
                                    JS_MarkFunc *mark_func) {
  JSAsyncGeneratorData *s = JS_GetOpaque(val, JS_CLASS_ASYNC_GENERATOR);
  struct list_head *el;
  JSAsyncGeneratorRequest *req;
  if (s) {
    list_for_each(el, &s->queue) {
      req = list_entry(el, JSAsyncGeneratorRequest, link);
      JS_MarkValue(rt, req->result, mark_func);
      JS_MarkValue(rt, req->promise, mark_func);
      JS_MarkValue(rt, req->resolving_funcs[0], mark_func);
      JS_MarkValue(rt, req->resolving_funcs[1], mark_func);
    }
    if (s->state != JS_ASYNC_GENERATOR_STATE_COMPLETED &&
        s->state != JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN) {
      async_func_mark(rt, &s->func_state, mark_func);
    }
  }
}

static JSValue js_async_generator_resolve_function(JSContext *ctx,
                                                   JSValueConst this_obj,
                                                   int argc, JSValueConst *argv,
                                                   int magic,
                                                   JSValue *func_data);

static int js_async_generator_resolve_function_create(JSContext *ctx,
                                                      JSValueConst generator,
                                                      JSValue *resolving_funcs,
                                                      BOOL is_resume_next) {
  int i;
  JSValue func;

  for (i = 0; i < 2; i++) {
    func = JS_NewCFunctionData(ctx, js_async_generator_resolve_function, 1,
                               i + is_resume_next * 2, 1, &generator);
    if (JS_IsException(func)) {
      if (i == 1)
        JS_FreeValue(ctx, resolving_funcs[0]);
      return -1;
    }
    resolving_funcs[i] = func;
  }
  return 0;
}

static int js_async_generator_await(JSContext *ctx, JSAsyncGeneratorData *s,
                                    JSValueConst value) {
  JSValue promise, resolving_funcs[2], resolving_funcs1[2];
  int i, res;

  promise = js_promise_resolve(ctx, ctx->promise_ctor, 1, &value, 0);
  if (JS_IsException(promise))
    goto fail;

  if (js_async_generator_resolve_function_create(
          ctx, JS_MKPTR(JS_TAG_OBJECT, s->generator), resolving_funcs, FALSE)) {
    JS_FreeValue(ctx, promise);
    goto fail;
  }

  /* Note: no need to create 'thrownawayCapability' as in
     the spec */
  for (i = 0; i < 2; i++)
    resolving_funcs1[i] = JS_UNDEFINED;
  res = perform_promise_then(ctx, promise, (JSValueConst *)resolving_funcs,
                             (JSValueConst *)resolving_funcs1);
  JS_FreeValue(ctx, promise);
  for (i = 0; i < 2; i++)
    JS_FreeValue(ctx, resolving_funcs[i]);
  if (res)
    goto fail;
  return 0;
fail:
  return -1;
}

static void js_async_generator_resolve_or_reject(JSContext *ctx,
                                                 JSAsyncGeneratorData *s,
                                                 JSValueConst result,
                                                 int is_reject) {
  JSAsyncGeneratorRequest *next;
  JSValue ret;

  next = list_entry(s->queue.next, JSAsyncGeneratorRequest, link);
  list_del(&next->link);
  ret =
      JS_Call(ctx, next->resolving_funcs[is_reject], JS_UNDEFINED, 1, &result);
  JS_FreeValue(ctx, ret);
  JS_FreeValue(ctx, next->result);
  JS_FreeValue(ctx, next->promise);
  JS_FreeValue(ctx, next->resolving_funcs[0]);
  JS_FreeValue(ctx, next->resolving_funcs[1]);
  js_free(ctx, next);
}

static void js_async_generator_resolve(JSContext *ctx, JSAsyncGeneratorData *s,
                                       JSValueConst value, BOOL done) {
  JSValue result;
  result = js_create_iterator_result(ctx, JS_DupValue(ctx, value), done);
  /* XXX: better exception handling ? */
  js_async_generator_resolve_or_reject(ctx, s, result, 0);
  JS_FreeValue(ctx, result);
}

static void js_async_generator_reject(JSContext *ctx, JSAsyncGeneratorData *s,
                                      JSValueConst exception) {
  js_async_generator_resolve_or_reject(ctx, s, exception, 1);
}

static void js_async_generator_complete(JSContext *ctx,
                                        JSAsyncGeneratorData *s) {
  if (s->state != JS_ASYNC_GENERATOR_STATE_COMPLETED) {
    s->state = JS_ASYNC_GENERATOR_STATE_COMPLETED;
    async_func_free(ctx->rt, &s->func_state);
  }
}

static int js_async_generator_completed_return(JSContext *ctx,
                                               JSAsyncGeneratorData *s,
                                               JSValueConst value) {
  JSValue promise, resolving_funcs[2], resolving_funcs1[2];
  int res;

  promise =
      js_promise_resolve(ctx, ctx->promise_ctor, 1, (JSValueConst *)&value, 0);
  if (JS_IsException(promise))
    return -1;
  if (js_async_generator_resolve_function_create(
          ctx, JS_MKPTR(JS_TAG_OBJECT, s->generator), resolving_funcs1, TRUE)) {
    JS_FreeValue(ctx, promise);
    return -1;
  }
  resolving_funcs[0] = JS_UNDEFINED;
  resolving_funcs[1] = JS_UNDEFINED;
  res = perform_promise_then(ctx, promise, (JSValueConst *)resolving_funcs1,
                             (JSValueConst *)resolving_funcs);
  JS_FreeValue(ctx, resolving_funcs1[0]);
  JS_FreeValue(ctx, resolving_funcs1[1]);
  JS_FreeValue(ctx, promise);
  return res;
}

static void js_async_generator_resume_next(JSContext *ctx,
                                           JSAsyncGeneratorData *s) {
  JSAsyncGeneratorRequest *next;
  JSValue func_ret, value;

  for (;;) {
    if (list_empty(&s->queue))
      break;
    next = list_entry(s->queue.next, JSAsyncGeneratorRequest, link);
    switch (s->state) {
    case JS_ASYNC_GENERATOR_STATE_EXECUTING:
      /* only happens when restarting execution after await() */
      goto resume_exec;
    case JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN:
      goto done;
    case JS_ASYNC_GENERATOR_STATE_SUSPENDED_START:
      if (next->completion_type == GEN_MAGIC_NEXT) {
        goto exec_no_arg;
      } else {
        js_async_generator_complete(ctx, s);
      }
      break;
    case JS_ASYNC_GENERATOR_STATE_COMPLETED:
      if (next->completion_type == GEN_MAGIC_NEXT) {
        js_async_generator_resolve(ctx, s, JS_UNDEFINED, TRUE);
      } else if (next->completion_type == GEN_MAGIC_RETURN) {
        s->state = JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN;
        js_async_generator_completed_return(ctx, s, next->result);
        goto done;
      } else {
        js_async_generator_reject(ctx, s, next->result);
      }
      goto done;
    case JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD:
    case JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD_STAR:
      value = JS_DupValue(ctx, next->result);
      if (next->completion_type == GEN_MAGIC_THROW &&
          s->state == JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD) {
        JS_Throw(ctx, value);
        s->func_state.throw_flag = TRUE;
      } else {
        /* 'yield' returns a value. 'yield *' also returns a value
           in case the 'throw' method is called */
        s->func_state.frame.cur_sp[-1] = value;
        s->func_state.frame.cur_sp[0] = JS_NewInt32(ctx, next->completion_type);
        s->func_state.frame.cur_sp++;
      exec_no_arg:
        s->func_state.throw_flag = FALSE;
      }
      s->state = JS_ASYNC_GENERATOR_STATE_EXECUTING;
    resume_exec:
      func_ret = async_func_resume(ctx, &s->func_state);
      if (JS_IsException(func_ret)) {
        value = JS_GetException(ctx);
        js_async_generator_complete(ctx, s);
        js_async_generator_reject(ctx, s, value);
        JS_FreeValue(ctx, value);
      } else if (JS_VALUE_GET_TAG(func_ret) == JS_TAG_INT) {
        int func_ret_code;
        value = s->func_state.frame.cur_sp[-1];
        s->func_state.frame.cur_sp[-1] = JS_UNDEFINED;
        func_ret_code = JS_VALUE_GET_INT(func_ret);
        switch (func_ret_code) {
        case FUNC_RET_YIELD:
        case FUNC_RET_YIELD_STAR:
          if (func_ret_code == FUNC_RET_YIELD_STAR)
            s->state = JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD_STAR;
          else
            s->state = JS_ASYNC_GENERATOR_STATE_SUSPENDED_YIELD;
          js_async_generator_resolve(ctx, s, value, FALSE);
          JS_FreeValue(ctx, value);
          break;
        case FUNC_RET_AWAIT:
          js_async_generator_await(ctx, s, value);
          JS_FreeValue(ctx, value);
          goto done;
        default:
          abort();
        }
      } else {
        assert(JS_IsUndefined(func_ret));
        /* end of function */
        value = s->func_state.frame.cur_sp[-1];
        s->func_state.frame.cur_sp[-1] = JS_UNDEFINED;
        js_async_generator_complete(ctx, s);
        js_async_generator_resolve(ctx, s, value, TRUE);
        JS_FreeValue(ctx, value);
      }
      break;
    default:
      abort();
    }
  }
done:;
}

static JSValue js_async_generator_resolve_function(JSContext *ctx,
                                                   JSValueConst this_obj,
                                                   int argc, JSValueConst *argv,
                                                   int magic,
                                                   JSValue *func_data) {
  BOOL is_reject = magic & 1;
  JSAsyncGeneratorData *s =
      JS_GetOpaque(func_data[0], JS_CLASS_ASYNC_GENERATOR);
  JSValueConst arg = argv[0];

  /* XXX: what if s == NULL */

  if (magic >= 2) {
    /* resume next case in AWAITING_RETURN state */
    assert(s->state == JS_ASYNC_GENERATOR_STATE_AWAITING_RETURN ||
           s->state == JS_ASYNC_GENERATOR_STATE_COMPLETED);
    s->state = JS_ASYNC_GENERATOR_STATE_COMPLETED;
    if (is_reject) {
      js_async_generator_reject(ctx, s, arg);
    } else {
      js_async_generator_resolve(ctx, s, arg, TRUE);
    }
  } else {
    /* restart function execution after await() */
    assert(s->state == JS_ASYNC_GENERATOR_STATE_EXECUTING);
    s->func_state.throw_flag = is_reject;
    if (is_reject) {
      JS_Throw(ctx, JS_DupValue(ctx, arg));
    } else {
      /* return value of await */
      s->func_state.frame.cur_sp[-1] = JS_DupValue(ctx, arg);
    }
    js_async_generator_resume_next(ctx, s);
  }
  return JS_UNDEFINED;
}

/* magic = GEN_MAGIC_x */
static JSValue js_async_generator_next(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv,
                                       int magic) {
  JSAsyncGeneratorData *s = JS_GetOpaque(this_val, JS_CLASS_ASYNC_GENERATOR);
  JSValue promise, resolving_funcs[2];
  JSAsyncGeneratorRequest *req;

  promise = JS_NewPromiseCapability(ctx, resolving_funcs);
  if (JS_IsException(promise))
    return JS_EXCEPTION;
  if (!s) {
    JSValue err, res2;
    JS_ThrowTypeError(ctx, "not an AsyncGenerator object");
    err = JS_GetException(ctx);
    res2 =
        JS_Call(ctx, resolving_funcs[1], JS_UNDEFINED, 1, (JSValueConst *)&err);
    JS_FreeValue(ctx, err);
    JS_FreeValue(ctx, res2);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    return promise;
  }
  req = js_mallocz(ctx, sizeof(*req));
  if (!req)
    goto fail;
  req->completion_type = magic;
  req->result = JS_DupValue(ctx, argv[0]);
  req->promise = JS_DupValue(ctx, promise);
  req->resolving_funcs[0] = resolving_funcs[0];
  req->resolving_funcs[1] = resolving_funcs[1];
  list_add_tail(&req->link, &s->queue);
  if (s->state != JS_ASYNC_GENERATOR_STATE_EXECUTING) {
    js_async_generator_resume_next(ctx, s);
  }
  return promise;
fail:
  JS_FreeValue(ctx, resolving_funcs[0]);
  JS_FreeValue(ctx, resolving_funcs[1]);
  JS_FreeValue(ctx, promise);
  return JS_EXCEPTION;
}

JSValue js_async_generator_function_call(JSContext *ctx, JSValueConst func_obj,
                                         JSValueConst this_obj, int argc,
                                         JSValueConst *argv, int flags) {
  JSValue obj, func_ret;
  JSAsyncGeneratorData *s;

  s = js_mallocz(ctx, sizeof(*s));
  if (!s)
    return JS_EXCEPTION;
  s->state = JS_ASYNC_GENERATOR_STATE_SUSPENDED_START;
  init_list_head(&s->queue);
  if (async_func_init(ctx, &s->func_state, func_obj, this_obj, argc, argv)) {
    s->state = JS_ASYNC_GENERATOR_STATE_COMPLETED;
    goto fail;
  }

  /* execute the function up to 'OP_initial_yield' (no yield nor
     await are possible) */
  func_ret = async_func_resume(ctx, &s->func_state);
  if (JS_IsException(func_ret))
    goto fail;
  JS_FreeValue(ctx, func_ret);

  obj = js_create_from_ctor(ctx, func_obj, JS_CLASS_ASYNC_GENERATOR);
  if (JS_IsException(obj))
    goto fail;
  s->generator = JS_VALUE_GET_OBJ(obj);
  JS_SetOpaque(obj, s);
  return obj;
fail:
  js_async_generator_free(ctx->rt, s);
  return JS_EXCEPTION;
}

/* -- AsyncIteratorPrototype ----------------------------------- */

const JSCFunctionListEntry js_async_iterator_proto_funcs[] = {
    JS_CFUNC_DEF("[Symbol.asyncIterator]", 0, js_iterator_proto_iterator),
};

/* -- AsyncFromSyncIteratorPrototype ----------------------------------- */

static void js_async_from_sync_iterator_finalizer(JSRuntime *rt, JSValue val) {
  JSAsyncFromSyncIteratorData *s =
      JS_GetOpaque(val, JS_CLASS_ASYNC_FROM_SYNC_ITERATOR);
  if (s) {
    JS_FreeValueRT(rt, s->sync_iter);
    JS_FreeValueRT(rt, s->next_method);
    js_free_rt(rt, s);
  }
}

static void js_async_from_sync_iterator_mark(JSRuntime *rt, JSValueConst val,
                                             JS_MarkFunc *mark_func) {
  JSAsyncFromSyncIteratorData *s =
      JS_GetOpaque(val, JS_CLASS_ASYNC_FROM_SYNC_ITERATOR);
  if (s) {
    JS_MarkValue(rt, s->sync_iter, mark_func);
    JS_MarkValue(rt, s->next_method, mark_func);
  }
}

JSValue JS_CreateAsyncFromSyncIterator(JSContext *ctx, JSValueConst sync_iter) {
  JSValue async_iter, next_method;
  JSAsyncFromSyncIteratorData *s;

  next_method = JS_GetProperty(ctx, sync_iter, JS_ATOM_next);
  if (JS_IsException(next_method))
    return JS_EXCEPTION;
  async_iter = JS_NewObjectClass(ctx, JS_CLASS_ASYNC_FROM_SYNC_ITERATOR);
  if (JS_IsException(async_iter)) {
    JS_FreeValue(ctx, next_method);
    return async_iter;
  }
  s = js_mallocz(ctx, sizeof(*s));
  if (!s) {
    JS_FreeValue(ctx, async_iter);
    JS_FreeValue(ctx, next_method);
    return JS_EXCEPTION;
  }
  s->sync_iter = JS_DupValue(ctx, sync_iter);
  s->next_method = next_method;
  JS_SetOpaque(async_iter, s);
  return async_iter;
}

static JSValue js_async_from_sync_iterator_next(JSContext *ctx,
                                                JSValueConst this_val, int argc,
                                                JSValueConst *argv, int magic) {
  JSValue promise, resolving_funcs[2], value, err, method;
  JSAsyncFromSyncIteratorData *s;
  int done;
  int is_reject;

  promise = JS_NewPromiseCapability(ctx, resolving_funcs);
  if (JS_IsException(promise))
    return JS_EXCEPTION;
  s = JS_GetOpaque(this_val, JS_CLASS_ASYNC_FROM_SYNC_ITERATOR);
  if (!s) {
    JS_ThrowTypeError(ctx, "not an Async-from-Sync Iterator");
    goto reject;
  }

  if (magic == GEN_MAGIC_NEXT) {
    method = JS_DupValue(ctx, s->next_method);
  } else {
    method = JS_GetProperty(ctx, s->sync_iter,
                            magic == GEN_MAGIC_RETURN ? JS_ATOM_return
                                                      : JS_ATOM_throw);
    if (JS_IsException(method))
      goto reject;
    if (JS_IsUndefined(method) || JS_IsNull(method)) {
      if (magic == GEN_MAGIC_RETURN) {
        err = js_create_iterator_result(ctx, JS_DupValue(ctx, argv[0]), TRUE);
        is_reject = 0;
      } else {
        err = JS_DupValue(ctx, argv[0]);
        is_reject = 1;
      }
      goto done_resolve;
    }
  }
  value = JS_IteratorNext2(ctx, s->sync_iter, method, argc >= 1 ? 1 : 0, argv,
                           &done);
  JS_FreeValue(ctx, method);
  if (JS_IsException(value))
    goto reject;
  if (done == 2) {
    JSValue obj = value;
    value = JS_IteratorGetCompleteValue(ctx, obj, &done);
    JS_FreeValue(ctx, obj);
    if (JS_IsException(value))
      goto reject;
  }

  if (JS_IsException(value)) {
    JSValue res2;
  reject:
    err = JS_GetException(ctx);
    is_reject = 1;
  done_resolve:
    res2 = JS_Call(ctx, resolving_funcs[is_reject], JS_UNDEFINED, 1,
                   (JSValueConst *)&err);
    JS_FreeValue(ctx, err);
    JS_FreeValue(ctx, res2);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    return promise;
  }
  {
    JSValue value_wrapper_promise, resolve_reject[2];
    int res;

    value_wrapper_promise = js_promise_resolve(ctx, ctx->promise_ctor, 1,
                                               (JSValueConst *)&value, 0);
    if (JS_IsException(value_wrapper_promise)) {
      JS_FreeValue(ctx, value);
      goto reject;
    }

    resolve_reject[0] =
        js_async_from_sync_iterator_unwrap_func_create(ctx, done);
    if (JS_IsException(resolve_reject[0])) {
      JS_FreeValue(ctx, value_wrapper_promise);
      goto fail;
    }
    JS_FreeValue(ctx, value);
    resolve_reject[1] = JS_UNDEFINED;

    res = perform_promise_then(ctx, value_wrapper_promise,
                               (JSValueConst *)resolve_reject,
                               (JSValueConst *)resolving_funcs);
    JS_FreeValue(ctx, resolve_reject[0]);
    JS_FreeValue(ctx, value_wrapper_promise);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    if (res) {
      JS_FreeValue(ctx, promise);
      return JS_EXCEPTION;
    }
  }
  return promise;
fail:
  JS_FreeValue(ctx, value);
  JS_FreeValue(ctx, resolving_funcs[0]);
  JS_FreeValue(ctx, resolving_funcs[1]);
  JS_FreeValue(ctx, promise);
  return JS_EXCEPTION;
}

const JSCFunctionListEntry js_async_from_sync_iterator_proto_funcs[] = {
    JS_CFUNC_MAGIC_DEF("next", 1, js_async_from_sync_iterator_next,
                       GEN_MAGIC_NEXT),
    JS_CFUNC_MAGIC_DEF("return", 1, js_async_from_sync_iterator_next,
                       GEN_MAGIC_RETURN),
    JS_CFUNC_MAGIC_DEF("throw", 1, js_async_from_sync_iterator_next,
                       GEN_MAGIC_THROW),
};

/* -- AsyncGeneratorFunction ----------------------------------- */

const JSCFunctionListEntry js_async_generator_function_proto_funcs[] = {
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "AsyncGeneratorFunction",
                       JS_PROP_CONFIGURABLE),
};

/* -- AsyncGenerator prototype ----------------------------------- */

const JSCFunctionListEntry js_async_generator_proto_funcs[] = {
    JS_CFUNC_MAGIC_DEF("next", 1, js_async_generator_next, GEN_MAGIC_NEXT),
    JS_CFUNC_MAGIC_DEF("return", 1, js_async_generator_next, GEN_MAGIC_RETURN),
    JS_CFUNC_MAGIC_DEF("throw", 1, js_async_generator_next, GEN_MAGIC_THROW),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "AsyncGenerator",
                       JS_PROP_CONFIGURABLE),
};

JSClassShortDef const js_async_class_def[] = {
    {JS_ATOM_Promise, js_promise_finalizer,
     js_promise_mark}, /* JS_CLASS_PROMISE */
    {JS_ATOM_PromiseResolveFunction, js_promise_resolve_function_finalizer,
     js_promise_resolve_function_mark}, /* JS_CLASS_PROMISE_RESOLVE_FUNCTION */
    {JS_ATOM_PromiseRejectFunction, js_promise_resolve_function_finalizer,
     js_promise_resolve_function_mark}, /* JS_CLASS_PROMISE_REJECT_FUNCTION */
    {JS_ATOM_AsyncFunction, js_bytecode_function_finalizer,
     js_bytecode_function_mark}, /* JS_CLASS_ASYNC_FUNCTION */
    {JS_ATOM_AsyncFunctionResolve, js_async_function_resolve_finalizer,
     js_async_function_resolve_mark}, /* JS_CLASS_ASYNC_FUNCTION_RESOLVE */
    {JS_ATOM_AsyncFunctionReject, js_async_function_resolve_finalizer,
     js_async_function_resolve_mark}, /* JS_CLASS_ASYNC_FUNCTION_REJECT */
    {JS_ATOM_empty_string, js_async_from_sync_iterator_finalizer,
     js_async_from_sync_iterator_mark}, /* JS_CLASS_ASYNC_FROM_SYNC_ITERATOR */
    {JS_ATOM_AsyncGeneratorFunction, js_bytecode_function_finalizer,
     js_bytecode_function_mark}, /* JS_CLASS_ASYNC_GENERATOR_FUNCTION */
    {JS_ATOM_AsyncGenerator, js_async_generator_finalizer,
     js_async_generator_mark}, /* JS_CLASS_ASYNC_GENERATOR */
};