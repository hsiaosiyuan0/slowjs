#ifndef QUICKJS_SHAPE_H
#define QUICKJS_SHAPE_H

#include "def.h"
#include "gc.h"

#define JS_PROP_INITIAL_SIZE 2
#define JS_PROP_INITIAL_HASH_SIZE 4 /* must be a power of two */

typedef struct JSShapeProperty {
  uint32_t hash_next : 26; /* 0 if last in list */
  uint32_t flags : 6;      /* JS_PROP_XXX */
  JSAtom atom;             /* JS_ATOM_NULL = free property entry */
} JSShapeProperty;

struct JSShape {
  /* hash table of size hash_mask + 1 before the start of the
     structure (see prop_hash_end()). */
  JSGCObjectHeader header;
  /* true if the shape is inserted in the shape hash table. If not,
     JSShape.hash is not valid */
  uint8_t is_hashed;
  /* If true, the shape may have small array index properties 'n' with 0
     <= n <= 2^31-1. If false, the shape is guaranteed not to have
     small array index properties */
  uint8_t has_small_array_index;
  uint32_t hash; /* current hash value */
  uint32_t prop_hash_mask;
  int prop_size;  /* allocated properties */
  int prop_count; /* include deleted properties */
  int deleted_prop_count;
  JSShape *shape_hash_next; /* in JSRuntime.shape_hash[h] list */
  JSObject *proto;
  JSShapeProperty prop[0]; /* prop_size elements */
};

static inline size_t get_shape_size(size_t hash_size, size_t prop_size) {
  return hash_size * sizeof(uint32_t) + sizeof(JSShape) +
         prop_size * sizeof(JSShapeProperty);
}

static inline JSShape *get_shape_from_alloc(void *sh_alloc, size_t hash_size) {
  return (JSShape *)(void *)((uint32_t *)sh_alloc + hash_size);
}

static inline uint32_t *prop_hash_end(JSShape *sh) { return (uint32_t *)sh; }

static inline void *get_alloc_from_shape(JSShape *sh) {
  return prop_hash_end(sh) - ((intptr_t)sh->prop_hash_mask + 1);
}

static inline JSShapeProperty *get_shape_prop(JSShape *sh) { return sh->prop; }

int init_shape_hash(JSRuntime *rt);
uint32_t shape_hash(uint32_t h, uint32_t val);
uint32_t get_shape_hash(uint32_t h, int hash_bits);
uint32_t shape_initial_hash(JSObject *proto);
int resize_shape_hash(JSRuntime *rt, int new_shape_hash_bits);
void js_shape_hash_link(JSRuntime *rt, JSShape *sh);
void js_shape_hash_unlink(JSRuntime *rt, JSShape *sh);

/* create a new empty shape with prototype 'proto' */
no_inline JSShape *js_new_shape2(JSContext *ctx, JSObject *proto, int hash_size,
                                 int prop_size);
JSShape *js_new_shape(JSContext *ctx, JSObject *proto);
JSShape *js_clone_shape(JSContext *ctx, JSShape *sh1);
JSShape *js_dup_shape(JSShape *sh);
void js_free_shape0(JSRuntime *rt, JSShape *sh);
void js_free_shape(JSRuntime *rt, JSShape *sh);
void js_free_shape_null(JSRuntime *rt, JSShape *sh);

/* make space to hold at least 'count' properties */
no_inline int resize_properties(JSContext *ctx, JSShape **psh, JSObject *p,
                                uint32_t count);
int compact_properties(JSContext *ctx, JSObject *p);
int add_shape_property(JSContext *ctx, JSShape **psh, JSObject *p, JSAtom atom,
                       int prop_flags);

/* find a hashed empty shape matching the prototype. Return NULL if
not found */
JSShape *find_hashed_shape_proto(JSRuntime *rt, JSObject *proto);
/* find a hashed shape matching sh + (prop, prop_flags). Return NULL if
   not found */
JSShape *find_hashed_shape_prop(JSRuntime *rt, JSShape *sh, JSAtom atom,
                                int prop_flags);

__maybe_unused void JS_DumpShape(JSRuntime *rt, int i, JSShape *sh);
__maybe_unused void JS_DumpShapes(JSRuntime *rt);

#endif
