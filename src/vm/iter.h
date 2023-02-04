#ifndef QUICKJS_ITER_H
#define QUICKJS_ITER_H

#include "def.h"

typedef struct JSForInIterator {
  JSValue obj;
  BOOL is_array;
  uint32_t array_length;
  uint32_t idx;
} JSForInIterator;

typedef struct JSAsyncFromSyncIteratorData {
  JSValue sync_iter;
  JSValue next_method;
} JSAsyncFromSyncIteratorData;

JSValue JS_GetIterator(JSContext *ctx, JSValueConst obj, BOOL is_async);
JSValue JS_IteratorNext(JSContext *ctx, JSValueConst enum_obj,
                        JSValueConst method, int argc, JSValueConst *argv,
                        BOOL *pdone);
JSValue JS_GetIterator2(JSContext *ctx, JSValueConst obj, JSValueConst method);

/* return *pdone = 2 if the iterator object is not parsed */
JSValue JS_IteratorNext2(JSContext *ctx, JSValueConst enum_obj,
                         JSValueConst method, int argc, JSValueConst *argv,
                         int *pdone);
int JS_IteratorClose(JSContext *ctx, JSValueConst enum_obj,
                     BOOL is_exception_pending);
JSValue js_create_iterator_result(JSContext *ctx, JSValue val, BOOL done);
JSValue JS_IteratorGetCompleteValue(JSContext *ctx, JSValueConst obj,
                                    BOOL *pdone);

__exception int js_iterator_get_value_done(JSContext *ctx, JSValue *sp);
JSValue js_create_iterator_result(JSContext *ctx, JSValue val, BOOL done);
JSValue build_for_in_iterator(JSContext *ctx, JSValue obj);

__exception int js_for_in_start(JSContext *ctx, JSValue *sp);
__exception int js_for_in_next(JSContext *ctx, JSValue *sp);
__exception int js_for_of_start(JSContext *ctx, JSValue *sp, BOOL is_async);
__exception int js_for_of_next(JSContext *ctx, JSValue *sp, int offset);

#endif