#ifndef QUICKJS_EXEC_H
#define QUICKJS_EXEC_H

#include "def.h"

JSValue JS_CallInternal(JSContext *caller_ctx, JSValueConst func_obj,
                        JSValueConst this_obj, JSValueConst new_target,
                        int argc, JSValue *argv, int flags);

#endif