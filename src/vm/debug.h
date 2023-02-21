#ifndef QUICKJS_DEBUG_H
#define QUICKJS_DEBUG_H

#include "def.h"

JSValue js_debug_pc2line(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv);

#endif