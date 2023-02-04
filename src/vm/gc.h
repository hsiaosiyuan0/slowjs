#ifndef QUICKJS_GC_H
#define QUICKJS_GC_H

#include "def.h"

/* -- Malloc ----------------------------------- */

size_t js_malloc_usable_size_unknown(const void *ptr);

no_inline int js_realloc_array(JSContext *ctx, void **parray, int elem_size,
                               int *psize, int req_size);

/* resize the array and update its size if req_size > *psize */
static inline int js_resize_array(JSContext *ctx, void **parray, int elem_size,
                                  int *psize, int req_size) {
  if (unlikely(req_size > *psize))
    return js_realloc_array(ctx, parray, elem_size, psize, req_size);
  else
    return 0;
}

/* -- Garbage collection ----------------------------------- */

static inline void set_value(JSContext *ctx, JSValue *pval, JSValue new_val) {
  JSValue old_val;
  old_val = *pval;
  *pval = new_val;
  JS_FreeValue(ctx, old_val);
}

void JS_RunGC(JSRuntime *rt);
void js_trigger_gc(JSRuntime *rt, size_t size);
void set_cycle_flag(JSContext *ctx, JSValueConst obj);

void add_gc_object(JSRuntime *rt, JSGCObjectHeader *h, JSGCObjectTypeEnum type);
void remove_gc_object(JSGCObjectHeader *h);
void free_gc_object(JSRuntime *rt, JSGCObjectHeader *gp);

void free_var_ref(JSRuntime *rt, JSVarRef *var_ref);
void free_bytecode_atoms(JSRuntime *rt, const uint8_t *bc_buf, int bc_len,
                         BOOL use_short_opcodes);

#endif
