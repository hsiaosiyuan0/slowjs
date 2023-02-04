#include "iter.h"

#include "class.h"
#include "conv.h"
#include "error.h"
#include "func.h"
#include "obj.h"
#include "vm.h"

JSValue build_for_in_iterator(JSContext *ctx, JSValue obj) {
  JSObject *p;
  JSPropertyEnum *tab_atom;
  int i;
  JSValue enum_obj, obj1;
  JSForInIterator *it;
  uint32_t tag, tab_atom_count;

  tag = JS_VALUE_GET_TAG(obj);
  if (tag != JS_TAG_OBJECT && tag != JS_TAG_NULL && tag != JS_TAG_UNDEFINED) {
    obj = JS_ToObjectFree(ctx, obj);
  }

  it = js_malloc(ctx, sizeof(*it));
  if (!it) {
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
  }
  enum_obj = JS_NewObjectProtoClass(ctx, JS_NULL, JS_CLASS_FOR_IN_ITERATOR);
  if (JS_IsException(enum_obj)) {
    js_free(ctx, it);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
  }
  it->is_array = FALSE;
  it->obj = obj;
  it->idx = 0;
  p = JS_VALUE_GET_OBJ(enum_obj);
  p->u.for_in_iterator = it;

  if (tag == JS_TAG_NULL || tag == JS_TAG_UNDEFINED)
    return enum_obj;

  /* fast path: assume no enumerable properties in the prototype chain */
  obj1 = JS_DupValue(ctx, obj);
  for (;;) {
    obj1 = JS_GetPrototypeFree(ctx, obj1);
    if (JS_IsNull(obj1))
      break;
    if (JS_IsException(obj1))
      goto fail;
    if (JS_GetOwnPropertyNamesInternal(ctx, &tab_atom, &tab_atom_count,
                                       JS_VALUE_GET_OBJ(obj1),
                                       JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY)) {
      JS_FreeValue(ctx, obj1);
      goto fail;
    }
    js_free_prop_enum(ctx, tab_atom, tab_atom_count);
    if (tab_atom_count != 0) {
      JS_FreeValue(ctx, obj1);
      goto slow_path;
    }
    /* must check for timeout to avoid infinite loop */
    if (js_poll_interrupts(ctx)) {
      JS_FreeValue(ctx, obj1);
      goto fail;
    }
  }

  p = JS_VALUE_GET_OBJ(obj);

  if (p->fast_array) {
    JSShape *sh;
    JSShapeProperty *prs;
    /* check that there are no enumerable normal fields */
    sh = p->shape;
    for (i = 0, prs = get_shape_prop(sh); i < sh->prop_count; i++, prs++) {
      if (prs->flags & JS_PROP_ENUMERABLE)
        goto normal_case;
    }
    /* for fast arrays, we only store the number of elements */
    it->is_array = TRUE;
    it->array_length = p->u.array.count;
  } else {
  normal_case:
    if (JS_GetOwnPropertyNamesInternal(ctx, &tab_atom, &tab_atom_count, p,
                                       JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY))
      goto fail;
    for (i = 0; i < tab_atom_count; i++) {
      JS_SetPropertyInternal(ctx, enum_obj, tab_atom[i].atom, JS_NULL, 0);
    }
    js_free_prop_enum(ctx, tab_atom, tab_atom_count);
  }
  return enum_obj;

slow_path:
  /* non enumerable properties hide the enumerable ones in the
     prototype chain */
  obj1 = JS_DupValue(ctx, obj);
  for (;;) {
    if (JS_GetOwnPropertyNamesInternal(ctx, &tab_atom, &tab_atom_count,
                                       JS_VALUE_GET_OBJ(obj1),
                                       JS_GPN_STRING_MASK | JS_GPN_SET_ENUM)) {
      JS_FreeValue(ctx, obj1);
      goto fail;
    }
    for (i = 0; i < tab_atom_count; i++) {
      JS_DefinePropertyValue(
          ctx, enum_obj, tab_atom[i].atom, JS_NULL,
          (tab_atom[i].is_enumerable ? JS_PROP_ENUMERABLE : 0));
    }
    js_free_prop_enum(ctx, tab_atom, tab_atom_count);
    obj1 = JS_GetPrototypeFree(ctx, obj1);
    if (JS_IsNull(obj1))
      break;
    if (JS_IsException(obj1))
      goto fail;
    /* must check for timeout to avoid infinite loop */
    if (js_poll_interrupts(ctx)) {
      JS_FreeValue(ctx, obj1);
      goto fail;
    }
  }
  return enum_obj;

fail:
  JS_FreeValue(ctx, enum_obj);
  return JS_EXCEPTION;
}

/* obj -> enum_obj */
__exception int js_for_in_start(JSContext *ctx, JSValue *sp) {
  sp[-1] = build_for_in_iterator(ctx, sp[-1]);
  if (JS_IsException(sp[-1]))
    return -1;
  return 0;
}

/* enum_obj -> enum_obj value done */
__exception int js_for_in_next(JSContext *ctx, JSValue *sp) {
  JSValueConst enum_obj;
  JSObject *p;
  JSAtom prop;
  JSForInIterator *it;
  int ret;

  enum_obj = sp[-1];
  /* fail safe */
  if (JS_VALUE_GET_TAG(enum_obj) != JS_TAG_OBJECT)
    goto done;
  p = JS_VALUE_GET_OBJ(enum_obj);
  if (p->class_id != JS_CLASS_FOR_IN_ITERATOR)
    goto done;
  it = p->u.for_in_iterator;

  for (;;) {
    if (it->is_array) {
      if (it->idx >= it->array_length)
        goto done;
      prop = __JS_AtomFromUInt32(it->idx);
      it->idx++;
    } else {
      JSShape *sh = p->shape;
      JSShapeProperty *prs;
      if (it->idx >= sh->prop_count)
        goto done;
      prs = get_shape_prop(sh) + it->idx;
      prop = prs->atom;
      it->idx++;
      if (prop == JS_ATOM_NULL || !(prs->flags & JS_PROP_ENUMERABLE))
        continue;
    }
    /* check if the property was deleted */
    ret = JS_HasProperty(ctx, it->obj, prop);
    if (ret < 0)
      return ret;
    if (ret)
      break;
  }
  /* return the property */
  sp[0] = JS_AtomToValue(ctx, prop);
  sp[1] = JS_FALSE;
  return 0;
done:
  /* return the end */
  sp[0] = JS_UNDEFINED;
  sp[1] = JS_TRUE;
  return 0;
}

JSValue JS_GetIterator2(JSContext *ctx, JSValueConst obj, JSValueConst method) {
  JSValue enum_obj;

  enum_obj = JS_Call(ctx, method, obj, 0, NULL);
  if (JS_IsException(enum_obj))
    return enum_obj;
  if (!JS_IsObject(enum_obj)) {
    JS_FreeValue(ctx, enum_obj);
    return JS_ThrowTypeErrorNotAnObject(ctx);
  }
  return enum_obj;
}

JSValue JS_CreateAsyncFromSyncIterator(JSContext *ctx, JSValueConst sync_iter);

JSValue JS_GetIterator(JSContext *ctx, JSValueConst obj, BOOL is_async) {
  JSValue method, ret, sync_iter;

  if (is_async) {
    method = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_asyncIterator);
    if (JS_IsException(method))
      return method;
    if (JS_IsUndefined(method) || JS_IsNull(method)) {
      method = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_iterator);
      if (JS_IsException(method))
        return method;
      sync_iter = JS_GetIterator2(ctx, obj, method);
      JS_FreeValue(ctx, method);
      if (JS_IsException(sync_iter))
        return sync_iter;
      ret = JS_CreateAsyncFromSyncIterator(ctx, sync_iter);
      JS_FreeValue(ctx, sync_iter);
      return ret;
    }
  } else {
    method = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_iterator);
    if (JS_IsException(method))
      return method;
  }
  if (!JS_IsFunction(ctx, method)) {
    JS_FreeValue(ctx, method);
    return JS_ThrowTypeError(ctx, "value is not iterable");
  }
  ret = JS_GetIterator2(ctx, obj, method);
  JS_FreeValue(ctx, method);
  return ret;
}

/* return *pdone = 2 if the iterator object is not parsed */
JSValue JS_IteratorNext2(JSContext *ctx, JSValueConst enum_obj,
                         JSValueConst method, int argc, JSValueConst *argv,
                         int *pdone) {
  JSValue obj;

  /* fast path for the built-in iterators (avoid creating the
     intermediate result object) */
  if (JS_IsObject(method)) {
    JSObject *p = JS_VALUE_GET_OBJ(method);
    if (p->class_id == JS_CLASS_C_FUNCTION &&
        p->u.cfunc.cproto == JS_CFUNC_iterator_next) {
      JSCFunctionType func;
      JSValueConst args[1];

      /* in case the function expects one argument */
      if (argc == 0) {
        args[0] = JS_UNDEFINED;
        argv = args;
      }
      func = p->u.cfunc.c_function;
      return func.iterator_next(ctx, enum_obj, argc, argv, pdone,
                                p->u.cfunc.magic);
    }
  }
  obj = JS_Call(ctx, method, enum_obj, argc, argv);
  if (JS_IsException(obj))
    goto fail;
  if (!JS_IsObject(obj)) {
    JS_FreeValue(ctx, obj);
    JS_ThrowTypeError(ctx, "iterator must return an object");
    goto fail;
  }
  *pdone = 2;
  return obj;
fail:
  *pdone = FALSE;
  return JS_EXCEPTION;
}

JSValue JS_IteratorNext(JSContext *ctx, JSValueConst enum_obj,
                        JSValueConst method, int argc, JSValueConst *argv,
                        BOOL *pdone) {
  JSValue obj, value, done_val;
  int done;

  obj = JS_IteratorNext2(ctx, enum_obj, method, argc, argv, &done);
  if (JS_IsException(obj))
    goto fail;
  if (done != 2) {
    *pdone = done;
    return obj;
  } else {
    done_val = JS_GetProperty(ctx, obj, JS_ATOM_done);
    if (JS_IsException(done_val))
      goto fail;
    *pdone = JS_ToBoolFree(ctx, done_val);
    value = JS_UNDEFINED;
    if (!*pdone) {
      value = JS_GetProperty(ctx, obj, JS_ATOM_value);
    }
    JS_FreeValue(ctx, obj);
    return value;
  }
fail:
  JS_FreeValue(ctx, obj);
  *pdone = FALSE;
  return JS_EXCEPTION;
}

/* return < 0 in case of exception */
int JS_IteratorClose(JSContext *ctx, JSValueConst enum_obj,
                     BOOL is_exception_pending) {
  JSValue method, ret, ex_obj;
  int res;

  if (is_exception_pending) {
    ex_obj = ctx->rt->current_exception;
    ctx->rt->current_exception = JS_NULL;
    res = -1;
  } else {
    ex_obj = JS_UNDEFINED;
    res = 0;
  }
  method = JS_GetProperty(ctx, enum_obj, JS_ATOM_return);
  if (JS_IsException(method)) {
    res = -1;
    goto done;
  }
  if (JS_IsUndefined(method) || JS_IsNull(method)) {
    goto done;
  }
  ret = JS_CallFree(ctx, method, enum_obj, 0, NULL);
  if (!is_exception_pending) {
    if (JS_IsException(ret)) {
      res = -1;
    } else if (!JS_IsObject(ret)) {
      JS_ThrowTypeErrorNotAnObject(ctx);
      res = -1;
    }
  }
  JS_FreeValue(ctx, ret);
done:
  if (is_exception_pending) {
    JS_Throw(ctx, ex_obj);
  }
  return res;
}

/* obj -> enum_rec (3 slots) */
__exception int js_for_of_start(JSContext *ctx, JSValue *sp, BOOL is_async) {
  JSValue op1, obj, method;
  op1 = sp[-1];
  obj = JS_GetIterator(ctx, op1, is_async);
  if (JS_IsException(obj))
    return -1;
  JS_FreeValue(ctx, op1);
  sp[-1] = obj;
  method = JS_GetProperty(ctx, obj, JS_ATOM_next);
  if (JS_IsException(method))
    return -1;
  sp[0] = method;
  return 0;
}

/* enum_rec [objs] -> enum_rec [objs] value done. There are 'offset'
   objs. If 'done' is true or in case of exception, 'enum_rec' is set
   to undefined. If 'done' is true, 'value' is always set to
   undefined. */
__exception int js_for_of_next(JSContext *ctx, JSValue *sp, int offset) {
  JSValue value = JS_UNDEFINED;
  int done = 1;

  if (likely(!JS_IsUndefined(sp[offset]))) {
    value = JS_IteratorNext(ctx, sp[offset], sp[offset + 1], 0, NULL, &done);
    if (JS_IsException(value))
      done = -1;
    if (done) {
      /* value is JS_UNDEFINED or JS_EXCEPTION */
      /* replace the iteration object with undefined */
      JS_FreeValue(ctx, sp[offset]);
      sp[offset] = JS_UNDEFINED;
      if (done < 0) {
        return -1;
      } else {
        JS_FreeValue(ctx, value);
        value = JS_UNDEFINED;
      }
    }
  }
  sp[0] = value;
  sp[1] = JS_NewBool(ctx, done);
  return 0;
}

JSValue JS_IteratorGetCompleteValue(JSContext *ctx, JSValueConst obj,
                                    BOOL *pdone) {
  JSValue done_val, value;
  BOOL done;
  done_val = JS_GetProperty(ctx, obj, JS_ATOM_done);
  if (JS_IsException(done_val))
    goto fail;
  done = JS_ToBoolFree(ctx, done_val);
  value = JS_GetProperty(ctx, obj, JS_ATOM_value);
  if (JS_IsException(value))
    goto fail;
  *pdone = done;
  return value;
fail:
  *pdone = FALSE;
  return JS_EXCEPTION;
}

__exception int js_iterator_get_value_done(JSContext *ctx, JSValue *sp) {
  JSValue obj, value;
  BOOL done;
  obj = sp[-1];
  if (!JS_IsObject(obj)) {
    JS_ThrowTypeError(ctx, "iterator must return an object");
    return -1;
  }
  value = JS_IteratorGetCompleteValue(ctx, obj, &done);
  if (JS_IsException(value))
    return -1;
  JS_FreeValue(ctx, obj);
  sp[-1] = value;
  sp[0] = JS_NewBool(ctx, done);
  return 0;
}

JSValue js_create_iterator_result(JSContext *ctx, JSValue val, BOOL done) {
  JSValue obj;
  obj = JS_NewObject(ctx);
  if (JS_IsException(obj)) {
    JS_FreeValue(ctx, val);
    return obj;
  }
  if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_value, val, JS_PROP_C_W_E) < 0) {
    goto fail;
  }
  if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_done, JS_NewBool(ctx, done),
                             JS_PROP_C_W_E) < 0) {
  fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
  }
  return obj;
}
