#ifndef QUICKJS_OBJBIN_H
#define QUICKJS_OBJBIN_H

#include "def.h"

typedef struct {
  JSObject *obj;
  uint32_t hash_next; /* -1 if no next entry */
} JSObjectListEntry;

/* XXX: reuse it to optimize weak references */
typedef struct {
  JSObjectListEntry *object_tab;
  int object_count;
  int object_size;
  uint32_t *hash_table;
  uint32_t hash_size;
} JSObjectList;

typedef enum BCTagEnum {
  BC_TAG_NULL = 1,
  BC_TAG_UNDEFINED,
  BC_TAG_BOOL_FALSE,
  BC_TAG_BOOL_TRUE,
  BC_TAG_INT32,
  BC_TAG_FLOAT64,
  BC_TAG_STRING,
  BC_TAG_OBJECT,
  BC_TAG_ARRAY,
  BC_TAG_BIG_INT,
  BC_TAG_BIG_FLOAT,
  BC_TAG_BIG_DECIMAL,
  BC_TAG_TEMPLATE_OBJECT,
  BC_TAG_FUNCTION_BYTECODE,
  BC_TAG_MODULE,
  BC_TAG_TYPED_ARRAY,
  BC_TAG_ARRAY_BUFFER,
  BC_TAG_SHARED_ARRAY_BUFFER,
  BC_TAG_DATE,
  BC_TAG_OBJECT_VALUE,
  BC_TAG_OBJECT_REFERENCE,
} BCTagEnum;

#ifdef CONFIG_BIGNUM
#define BC_BASE_VERSION 2
#else
#define BC_BASE_VERSION 1
#endif
#define BC_BE_VERSION 0x40
#ifdef WORDS_BIGENDIAN
#define BC_VERSION (BC_BASE_VERSION | BC_BE_VERSION)
#else
#define BC_VERSION BC_BASE_VERSION
#endif

typedef struct BCWriterState {
  JSContext *ctx;
  DynBuf dbuf;
  BOOL byte_swap : 8;
  BOOL allow_bytecode : 8;
  BOOL allow_sab : 8;
  BOOL allow_reference : 8;
  uint32_t first_atom;
  uint32_t *atom_to_idx;
  int atom_to_idx_size;
  JSAtom *idx_to_atom;
  int idx_to_atom_count;
  int idx_to_atom_size;
  uint8_t **sab_tab;
  int sab_tab_len;
  int sab_tab_size;
  /* list of referenced objects (used if allow_reference = TRUE) */
  JSObjectList object_list;
} BCWriterState;

typedef struct BCReaderState {
  JSContext *ctx;
  const uint8_t *buf_start, *ptr, *buf_end;
  uint32_t first_atom;
  uint32_t idx_to_atom_count;
  JSAtom *idx_to_atom;
  int error_state;
  BOOL allow_sab : 8;
  BOOL allow_bytecode : 8;
  BOOL is_rom_data : 8;
  BOOL allow_reference : 8;
  /* object references */
  JSObject **objects;
  int objects_count;
  int objects_size;

#ifdef DUMP_READ_OBJECT
  const uint8_t *ptr_last;
  int level;
#endif
} BCReaderState;

#endif