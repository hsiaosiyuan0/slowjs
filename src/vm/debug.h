#ifndef QUICKJS_DEBUG_H
#define QUICKJS_DEBUG_H

#include "def.h"

JSValue js_debug_pc2line(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv);

void js_debug_init(JSContext *ctx);
void js_debug_pause(JSContext *ctx);
void js_debug_continue(JSContext *ctx);

#endif