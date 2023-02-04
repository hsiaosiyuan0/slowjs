#include "error.h"

#include "class.h"
#include "func.h"
#include "obj.h"
#include "utils/dbuf.h"
#include "vm.h"

/* if filename != NULL, an additional level is added with the filename
   and line number information (used for parse error). */
void build_backtrace(JSContext *ctx, JSValueConst error_obj,
                     const char *filename, int line_num, int backtrace_flags) {
  JSStackFrame *sf;
  JSValue str;
  DynBuf dbuf;
  const char *func_name_str;
  const char *str1;
  JSObject *p;
  BOOL backtrace_barrier;

  js_dbuf_init(ctx, &dbuf);
  if (filename) {
    dbuf_printf(&dbuf, "    at %s", filename);
    if (line_num != -1)
      dbuf_printf(&dbuf, ":%d", line_num);
    dbuf_putc(&dbuf, '\n');
    str = JS_NewString(ctx, filename);
    JS_DefinePropertyValue(ctx, error_obj, JS_ATOM_fileName, str,
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    JS_DefinePropertyValue(ctx, error_obj, JS_ATOM_lineNumber,
                           JS_NewInt32(ctx, line_num),
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    if (backtrace_flags & JS_BACKTRACE_FLAG_SINGLE_LEVEL)
      goto done;
  }
  for (sf = ctx->rt->current_stack_frame; sf != NULL; sf = sf->prev_frame) {
    if (backtrace_flags & JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL) {
      backtrace_flags &= ~JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL;
      continue;
    }
    func_name_str = get_func_name(ctx, sf->cur_func);
    if (!func_name_str || func_name_str[0] == '\0')
      str1 = "<anonymous>";
    else
      str1 = func_name_str;
    dbuf_printf(&dbuf, "    at %s", str1);
    JS_FreeCString(ctx, func_name_str);

    p = JS_VALUE_GET_OBJ(sf->cur_func);
    backtrace_barrier = FALSE;
    if (js_class_has_bytecode(p->class_id)) {
      JSFunctionBytecode *b;
      const char *atom_str;
      int line_num1;

      b = p->u.func.function_bytecode;
      backtrace_barrier = b->backtrace_barrier;
      if (b->has_debug) {
        line_num1 = find_line_num(ctx, b, sf->cur_pc - b->byte_code_buf - 1);
        atom_str = JS_AtomToCString(ctx, b->debug.filename);
        dbuf_printf(&dbuf, " (%s", atom_str ? atom_str : "<null>");
        JS_FreeCString(ctx, atom_str);
        if (line_num1 != -1)
          dbuf_printf(&dbuf, ":%d", line_num1);
        dbuf_putc(&dbuf, ')');
      }
    } else {
      dbuf_printf(&dbuf, " (native)");
    }
    dbuf_putc(&dbuf, '\n');
    /* stop backtrace if JS_EVAL_FLAG_BACKTRACE_BARRIER was used */
    if (backtrace_barrier)
      break;
  }
done:
  dbuf_putc(&dbuf, '\0');
  if (dbuf_error(&dbuf))
    str = JS_NULL;
  else
    str = JS_NewString(ctx, (char *)dbuf.buf);
  dbuf_free(&dbuf);
  JS_DefinePropertyValue(ctx, error_obj, JS_ATOM_stack, str,
                         JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
}

/* Note: it is important that no exception is returned by this function */
BOOL is_backtrace_needed(JSContext *ctx, JSValueConst obj) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
    return FALSE;
  p = JS_VALUE_GET_OBJ(obj);
  if (p->class_id != JS_CLASS_ERROR)
    return FALSE;
  if (find_own_property1(p, JS_ATOM_stack))
    return FALSE;
  return TRUE;
}

JSValue JS_NewError(JSContext *ctx) {
  return JS_NewObjectClass(ctx, JS_CLASS_ERROR);
}

JSValue JS_ThrowError2(JSContext *ctx, JSErrorEnum error_num, const char *fmt,
                       va_list ap, BOOL add_backtrace) {
  char buf[256];
  JSValue obj, ret;

  vsnprintf(buf, sizeof(buf), fmt, ap);
  obj = JS_NewObjectProtoClass(ctx, ctx->native_error_proto[error_num],
                               JS_CLASS_ERROR);
  if (unlikely(JS_IsException(obj))) {
    /* out of memory: throw JS_NULL to avoid recursing */
    obj = JS_NULL;
  } else {
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_message, JS_NewString(ctx, buf),
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
  }
  if (add_backtrace) {
    build_backtrace(ctx, obj, NULL, 0, 0);
  }
  ret = JS_Throw(ctx, obj);
  return ret;
}

JSValue JS_ThrowError(JSContext *ctx, JSErrorEnum error_num, const char *fmt,
                      va_list ap) {
  JSRuntime *rt = ctx->rt;
  JSStackFrame *sf;
  BOOL add_backtrace;

  /* the backtrace is added later if called from a bytecode function */
  sf = rt->current_stack_frame;
  add_backtrace = !rt->in_out_of_memory &&
                  (!sf || (JS_GetFunctionBytecode(sf->cur_func) == NULL));
  return JS_ThrowError2(ctx, error_num, fmt, ap, add_backtrace);
}

JSValue __attribute__((format(printf, 2, 3)))
JS_ThrowSyntaxError(JSContext *ctx, const char *fmt, ...) {
  JSValue val;
  va_list ap;

  va_start(ap, fmt);
  val = JS_ThrowError(ctx, JS_SYNTAX_ERROR, fmt, ap);
  va_end(ap);
  return val;
}

JSValue __attribute__((format(printf, 2, 3)))
JS_ThrowTypeError(JSContext *ctx, const char *fmt, ...) {
  JSValue val;
  va_list ap;

  va_start(ap, fmt);
  val = JS_ThrowError(ctx, JS_TYPE_ERROR, fmt, ap);
  va_end(ap);
  return val;
}

int __attribute__((format(printf, 3, 4)))
JS_ThrowTypeErrorOrFalse(JSContext *ctx, int flags, const char *fmt, ...) {
  va_list ap;

  if ((flags & JS_PROP_THROW) ||
      ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
    va_start(ap, fmt);
    JS_ThrowError(ctx, JS_TYPE_ERROR, fmt, ap);
    va_end(ap);
    return -1;
  } else {
    return FALSE;
  }
}

/* never use it directly */
JSValue __attribute__((format(printf, 3, 4)))
__JS_ThrowTypeErrorAtom(JSContext *ctx, JSAtom atom, const char *fmt, ...) {
  char buf[ATOM_GET_STR_BUF_SIZE];
  return JS_ThrowTypeError(ctx, fmt,
                           JS_AtomGetStr(ctx, buf, sizeof(buf), atom));
}

/* never use it directly */
JSValue __attribute__((format(printf, 3, 4)))
__JS_ThrowSyntaxErrorAtom(JSContext *ctx, JSAtom atom, const char *fmt, ...) {
  char buf[ATOM_GET_STR_BUF_SIZE];
  return JS_ThrowSyntaxError(ctx, fmt,
                             JS_AtomGetStr(ctx, buf, sizeof(buf), atom));
}

int JS_ThrowTypeErrorReadOnly(JSContext *ctx, int flags, JSAtom atom) {
  if ((flags & JS_PROP_THROW) ||
      ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
    JS_ThrowTypeErrorAtom(ctx, "'%s' is read-only", atom);
    return -1;
  } else {
    return FALSE;
  }
}

JSValue __attribute__((format(printf, 2, 3)))
JS_ThrowReferenceError(JSContext *ctx, const char *fmt, ...) {
  JSValue val;
  va_list ap;

  va_start(ap, fmt);
  val = JS_ThrowError(ctx, JS_REFERENCE_ERROR, fmt, ap);
  va_end(ap);
  return val;
}

JSValue __attribute__((format(printf, 2, 3)))
JS_ThrowRangeError(JSContext *ctx, const char *fmt, ...) {
  JSValue val;
  va_list ap;

  va_start(ap, fmt);
  val = JS_ThrowError(ctx, JS_RANGE_ERROR, fmt, ap);
  va_end(ap);
  return val;
}

JSValue __attribute__((format(printf, 2, 3)))
JS_ThrowInternalError(JSContext *ctx, const char *fmt, ...) {
  JSValue val;
  va_list ap;

  va_start(ap, fmt);
  val = JS_ThrowError(ctx, JS_INTERNAL_ERROR, fmt, ap);
  va_end(ap);
  return val;
}

JSValue JS_ThrowOutOfMemory(JSContext *ctx) {
  JSRuntime *rt = ctx->rt;
  if (!rt->in_out_of_memory) {
    rt->in_out_of_memory = TRUE;
    JS_ThrowInternalError(ctx, "out of memory");
    rt->in_out_of_memory = FALSE;
  }
  return JS_EXCEPTION;
}

JSValue JS_ThrowStackOverflow(JSContext *ctx) {
  return JS_ThrowInternalError(ctx, "stack overflow");
}

JSValue JS_ThrowTypeErrorNotAnObject(JSContext *ctx) {
  return JS_ThrowTypeError(ctx, "not an object");
}

JSValue JS_ThrowTypeErrorNotASymbol(JSContext *ctx) {
  return JS_ThrowTypeError(ctx, "not a symbol");
}

JSValue JS_ThrowReferenceErrorNotDefined(JSContext *ctx, JSAtom name) {
  char buf[ATOM_GET_STR_BUF_SIZE];
  return JS_ThrowReferenceError(ctx, "'%s' is not defined",
                                JS_AtomGetStr(ctx, buf, sizeof(buf), name));
}

JSValue JS_ThrowReferenceErrorUninitialized(JSContext *ctx, JSAtom name) {
  char buf[ATOM_GET_STR_BUF_SIZE];
  return JS_ThrowReferenceError(
      ctx, "%s is not initialized",
      name == JS_ATOM_NULL ? "lexical variable"
                           : JS_AtomGetStr(ctx, buf, sizeof(buf), name));
}

JSValue JS_ThrowReferenceErrorUninitialized2(JSContext *ctx,
                                             JSFunctionBytecode *b, int idx,
                                             BOOL is_ref) {
  JSAtom atom = JS_ATOM_NULL;
  if (is_ref) {
    atom = b->closure_var[idx].var_name;
  } else {
    /* not present if the function is stripped and contains no eval() */
    if (b->vardefs)
      atom = b->vardefs[b->arg_count + idx].var_name;
  }
  return JS_ThrowReferenceErrorUninitialized(ctx, atom);
}

JSValue JS_ThrowTypeErrorInvalidClass(JSContext *ctx, int class_id) {
  JSRuntime *rt = ctx->rt;
  JSAtom name;
  name = rt->class_array[class_id].class_name;
  return JS_ThrowTypeErrorAtom(ctx, "%s object expected", name);
}

JSValue JS_ThrowSyntaxErrorVarRedeclaration(JSContext *ctx, JSAtom prop) {
  return JS_ThrowSyntaxErrorAtom(ctx, "redeclaration of '%s'", prop);
}

BOOL JS_IsError(JSContext *ctx, JSValueConst val) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
    return FALSE;
  p = JS_VALUE_GET_OBJ(val);
  return (p->class_id == JS_CLASS_ERROR);
}

/* used to avoid catching interrupt exceptions */
BOOL JS_IsUncatchableError(JSContext *ctx, JSValueConst val) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
    return FALSE;
  p = JS_VALUE_GET_OBJ(val);
  return p->class_id == JS_CLASS_ERROR && p->is_uncatchable_error;
}

void JS_SetUncatchableError(JSContext *ctx, JSValueConst val, BOOL flag) {
  JSObject *p;
  if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT)
    return;
  p = JS_VALUE_GET_OBJ(val);
  if (p->class_id == JS_CLASS_ERROR)
    p->is_uncatchable_error = flag;
}

void JS_ResetUncatchableError(JSContext *ctx) {
  JS_SetUncatchableError(ctx, ctx->rt->current_exception, FALSE);
}
