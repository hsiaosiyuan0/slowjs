#ifndef QUICKJS_DEBUG_H
#define QUICKJS_DEBUG_H

#include "def.h"

JSValue js_debug_pc2line(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv);

int js_debug_init(JSContext *ctx);
void js_debug_wait_ready2start(JSContext *ctx);
void js_debug_ready2start(JSContext *ctx);

void js_debug_on(JSContext *ctx);
void js_debug_off(JSContext *ctx);

int js_debug_set_breakpoint(JSContext *ctx, const char *file, int line,
                            int col);
int js_debug_del_breakpoint(JSContext *ctx, const char *file, int line,
                            int col);
void js_debug_continue(JSContext *ctx);

// list all the available breakpoints of currently paused function
//
// only use this method when vm is paused by breakpoint hit
JSValue js_debug_list_breakpoints(JSContext *ctx);

JSValue js_debug_list_stackframes(JSContext *ctx);

// list the args, vars and var_refs in the stackframe specified by the
// parameter `int i` which is the number offset from the topmost stackframe
//
// only use this method when vm is paused by breakpoint hit
JSValue js_debug_dump_stackframe(JSContext *ctx, int i);

#endif