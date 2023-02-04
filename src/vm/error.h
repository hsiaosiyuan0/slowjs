#ifndef QUICKJS_THROW_H
#define QUICKJS_THROW_H

#include "def.h"

typedef struct JSFunctionBytecode JSFunctionBytecode;

BOOL is_backtrace_needed(JSContext *ctx, JSValueConst obj);

#define JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL (1 << 0)
/* only taken into account if filename is provided */
#define JS_BACKTRACE_FLAG_SINGLE_LEVEL (1 << 1)

/* if filename != NULL, an additional level is added with the filename
   and line number information (used for parse error). */
void build_backtrace(JSContext *ctx, JSValueConst error_obj,
                     const char *filename, int line_num, int backtrace_flags);

int __attribute__((format(printf, 3, 4)))
JS_ThrowTypeErrorOrFalse(JSContext *ctx, int flags, const char *fmt, ...);

/* never use it directly */
JSValue __attribute__((format(printf, 3, 4)))
__JS_ThrowTypeErrorAtom(JSContext *ctx, JSAtom atom, const char *fmt, ...);
/* never use it directly */
JSValue __attribute__((format(printf, 3, 4)))
__JS_ThrowSyntaxErrorAtom(JSContext *ctx, JSAtom atom, const char *fmt, ...);

/* %s is replaced by 'atom'. The macro is used so that gcc can check
    the format string. */
#define JS_ThrowTypeErrorAtom(ctx, fmt, atom)                                  \
  __JS_ThrowTypeErrorAtom(ctx, atom, fmt, "")
#define JS_ThrowSyntaxErrorAtom(ctx, fmt, atom)                                \
  __JS_ThrowSyntaxErrorAtom(ctx, atom, fmt, "")

JSValue JS_ThrowError2(JSContext *ctx, JSErrorEnum error_num, const char *fmt,
                       va_list ap, BOOL add_backtrace);
JSValue JS_ThrowError(JSContext *ctx, JSErrorEnum error_num, const char *fmt,
                      va_list ap);

int JS_ThrowTypeErrorReadOnly(JSContext *ctx, int flags, JSAtom atom);
JSValue JS_ThrowOutOfMemory(JSContext *ctx);
JSValue JS_ThrowStackOverflow(JSContext *ctx);
JSValue JS_ThrowTypeErrorNotAnObject(JSContext *ctx);
JSValue JS_ThrowTypeErrorNotASymbol(JSContext *ctx);
JSValue JS_ThrowReferenceErrorNotDefined(JSContext *ctx, JSAtom name);
JSValue JS_ThrowReferenceErrorUninitialized(JSContext *ctx, JSAtom name);
JSValue JS_ThrowReferenceErrorUninitialized2(JSContext *ctx,
                                             JSFunctionBytecode *b, int idx,
                                             BOOL is_ref);
JSValue JS_ThrowTypeErrorInvalidClass(JSContext *ctx, int class_id);
JSValue JS_ThrowSyntaxErrorVarRedeclaration(JSContext *ctx, JSAtom prop);

BOOL JS_IsError(JSContext *ctx, JSValueConst val);
BOOL JS_IsUncatchableError(JSContext *ctx, JSValueConst val);
void JS_SetUncatchableError(JSContext *ctx, JSValueConst val, BOOL flag);
void JS_ResetUncatchableError(JSContext *ctx);

#endif