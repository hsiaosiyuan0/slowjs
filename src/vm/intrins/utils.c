#include "intrins.h"

#include "vm/gc.h"
#include "vm/obj.h"
#include "vm/str.h"

int check_function(JSContext *ctx, JSValueConst obj) {
  if (likely(JS_IsFunction(ctx, obj)))
    return 0;
  JS_ThrowTypeError(ctx, "not a function");
  return -1;
}

int check_exception_free(JSContext *ctx, JSValue obj) {
  JS_FreeValue(ctx, obj);
  return JS_IsException(obj);
}

JSAtom find_atom(JSContext *ctx, const char *name) {
  JSAtom atom;
  int len;

  if (*name == '[') {
    name++;
    len = strlen(name) - 1;
    /* We assume 8 bit non null strings, which is the case for these
       symbols */
    for (atom = JS_ATOM_Symbol_toPrimitive; atom < JS_ATOM_END; atom++) {
      JSAtomStruct *p = ctx->rt->atom_array[atom];
      JSString *str = p;
      if (str->len == len && !memcmp(str->u.str8, name, len))
        return JS_DupAtom(ctx, atom);
    }
    abort();
  } else {
    atom = JS_NewAtom(ctx, name);
  }
  return atom;
}

JSValue JS_InstantiateFunctionListItem2(JSContext *ctx, JSObject *p,
                                        JSAtom atom, void *opaque) {
  const JSCFunctionListEntry *e = opaque;
  JSValue val;

  switch (e->def_type) {
  case JS_DEF_CFUNC:
    val = JS_NewCFunction2(ctx, e->u.func.cfunc.generic, e->name,
                           e->u.func.length, e->u.func.cproto, e->magic);
    break;
  case JS_DEF_PROP_STRING:
    val = JS_NewAtomString(ctx, e->u.str);
    break;
  case JS_DEF_OBJECT:

  {
    val = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, val, e->u.prop_list.tab,
                               e->u.prop_list.len);
  } break;
  default:
    abort();
  }
  return val;
}

int JS_InstantiateFunctionListItem(JSContext *ctx, JSValueConst obj,
                                   JSAtom atom, const JSCFunctionListEntry *e) {
  JSValue val;
  int prop_flags = e->prop_flags;

  switch (e->def_type) {
  case JS_DEF_ALIAS: /* using autoinit for aliases is not safe */
  {
    JSAtom atom1 = find_atom(ctx, e->u.alias.name);
    switch (e->u.alias.base) {
    case -1:
      val = JS_GetProperty(ctx, obj, atom1);
      break;
    case 0:
      val = JS_GetProperty(ctx, ctx->global_obj, atom1);
      break;
    case 1:
      val = JS_GetProperty(ctx, ctx->class_proto[JS_CLASS_ARRAY], atom1);
      break;
    default:
      abort();
    }
    JS_FreeAtom(ctx, atom1);
    if (atom == JS_ATOM_Symbol_toPrimitive) {
      /* Symbol.toPrimitive functions are not writable */
      prop_flags = JS_PROP_CONFIGURABLE;
    } else if (atom == JS_ATOM_Symbol_hasInstance) {
      /* Function.prototype[Symbol.hasInstance] is not writable nor configurable
       */
      prop_flags = 0;
    }
  } break;
  case JS_DEF_CFUNC:
    if (atom == JS_ATOM_Symbol_toPrimitive) {
      /* Symbol.toPrimitive functions are not writable */
      prop_flags = JS_PROP_CONFIGURABLE;
    } else if (atom == JS_ATOM_Symbol_hasInstance) {
      /* Function.prototype[Symbol.hasInstance] is not writable nor configurable
       */
      prop_flags = 0;
    }
    JS_DefineAutoInitProperty(ctx, obj, atom, JS_AUTOINIT_ID_PROP, (void *)e,
                              prop_flags);
    return 0;
  case JS_DEF_CGETSET: /* XXX: use autoinit again ? */
  case JS_DEF_CGETSET_MAGIC: {
    JSValue getter, setter;
    char buf[64];

    getter = JS_UNDEFINED;
    if (e->u.getset.get.generic) {
      snprintf(buf, sizeof(buf), "get %s", e->name);
      getter = JS_NewCFunction2(ctx, e->u.getset.get.generic, buf, 0,
                                e->def_type == JS_DEF_CGETSET_MAGIC
                                    ? JS_CFUNC_getter_magic
                                    : JS_CFUNC_getter,
                                e->magic);
    }
    setter = JS_UNDEFINED;
    if (e->u.getset.set.generic) {
      snprintf(buf, sizeof(buf), "set %s", e->name);
      setter = JS_NewCFunction2(ctx, e->u.getset.set.generic, buf, 1,
                                e->def_type == JS_DEF_CGETSET_MAGIC
                                    ? JS_CFUNC_setter_magic
                                    : JS_CFUNC_setter,
                                e->magic);
    }
    JS_DefinePropertyGetSet(ctx, obj, atom, getter, setter, prop_flags);
    return 0;
  } break;
  case JS_DEF_PROP_INT32:
    val = JS_NewInt32(ctx, e->u.i32);
    break;
  case JS_DEF_PROP_INT64:
    val = JS_NewInt64(ctx, e->u.i64);
    break;
  case JS_DEF_PROP_DOUBLE:
    val = __JS_NewFloat64(ctx, e->u.f64);
    break;
  case JS_DEF_PROP_UNDEFINED:
    val = JS_UNDEFINED;
    break;
  case JS_DEF_PROP_STRING:
  case JS_DEF_OBJECT:
    JS_DefineAutoInitProperty(ctx, obj, atom, JS_AUTOINIT_ID_PROP, (void *)e,
                              prop_flags);
    return 0;
  default:
    abort();
  }
  JS_DefinePropertyValue(ctx, obj, atom, val, prop_flags);
  return 0;
}

void JS_SetPropertyFunctionList(JSContext *ctx, JSValueConst obj,
                                const JSCFunctionListEntry *tab, int len) {
  int i;

  for (i = 0; i < len; i++) {
    const JSCFunctionListEntry *e = &tab[i];
    JSAtom atom = find_atom(ctx, e->name);
    JS_InstantiateFunctionListItem(ctx, obj, atom, e);
    JS_FreeAtom(ctx, atom);
  }
}

/* Note: 'func_obj' is not necessarily a constructor */
void JS_SetConstructor2(JSContext *ctx, JSValueConst func_obj,
                        JSValueConst proto, int proto_flags, int ctor_flags) {
  JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_prototype,
                         JS_DupValue(ctx, proto), proto_flags);
  JS_DefinePropertyValue(ctx, proto, JS_ATOM_constructor,
                         JS_DupValue(ctx, func_obj), ctor_flags);
  set_cycle_flag(ctx, func_obj);
  set_cycle_flag(ctx, proto);
}

void JS_SetConstructor(JSContext *ctx, JSValueConst func_obj,
                       JSValueConst proto) {
  JS_SetConstructor2(ctx, func_obj, proto, 0,
                     JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
}

void JS_NewGlobalCConstructor2(JSContext *ctx, JSValue func_obj,
                               const char *name, JSValueConst proto) {
  JS_DefinePropertyValueStr(ctx, ctx->global_obj, name,
                            JS_DupValue(ctx, func_obj),
                            JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  JS_SetConstructor(ctx, func_obj, proto);
  JS_FreeValue(ctx, func_obj);
}

JSValueConst JS_NewGlobalCConstructor(JSContext *ctx, const char *name,
                                      JSCFunction *func, int length,
                                      JSValueConst proto) {
  JSValue func_obj;
  func_obj = JS_NewCFunction2(ctx, func, name, length,
                              JS_CFUNC_constructor_or_func, 0);
  JS_NewGlobalCConstructor2(ctx, func_obj, name, proto);
  return func_obj;
}

JSValueConst JS_NewGlobalCConstructorOnly(JSContext *ctx, const char *name,
                                          JSCFunction *func, int length,
                                          JSValueConst proto) {
  JSValue func_obj;
  func_obj = JS_NewCFunction2(ctx, func, name, length, JS_CFUNC_constructor, 0);
  JS_NewGlobalCConstructor2(ctx, func_obj, name, proto);
  return func_obj;
}

JSValue js_get_this(JSContext *ctx, JSValueConst this_val) {
  return JS_DupValue(ctx, this_val);
}