#ifndef QUICKJS_DUMP_H
#define QUICKJS_DUMP_H

#include "def.h"

__maybe_unused void JS_DumpString(JSRuntime *rt, const JSString *p);
__maybe_unused void JS_DumpAtoms(JSRuntime *rt);

__maybe_unused void JS_DumpObjectHeader(JSRuntime *rt);
/* for debug only: dump an object without side effect */
__maybe_unused void JS_DumpObject(JSRuntime *rt, JSObject *p);
__maybe_unused void JS_DumpGCObject(JSRuntime *rt, JSGCObjectHeader *p);
__maybe_unused void JS_DumpValueShort(JSRuntime *rt, JSValueConst val);
__maybe_unused void JS_DumpValue(JSContext *ctx, JSValueConst val);
__maybe_unused void JS_PrintValue(JSContext *ctx, const char *str,
                                  JSValueConst val);

#ifdef DUMP_BYTECODE

#include "func.h"
#include "parse/parse.h"

const char *skip_lines(const char *p, int n);
void print_lines(const char *source, int line, int line1);
void dump_byte_code(JSContext *ctx, int pass, const uint8_t *tab, int len,
                    const JSVarDef *args, int arg_count, const JSVarDef *vars,
                    int var_count, const JSClosureVar *closure_var,
                    int closure_var_count, const JSValue *cpool,
                    uint32_t cpool_count, const char *source, int line_num,
                    const LabelSlot *label_slots, JSFunctionBytecode *b);
__maybe_unused void dump_pc2line(JSContext *ctx, const uint8_t *buf, int len,
                                 int line_num);
__maybe_unused void js_dump_function_bytecode(JSContext *ctx,
                                              JSFunctionBytecode *b);
#endif

#endif