#ifndef QUICKJS_OPCODE_H
#define QUICKJS_OPCODE_H

#include "def.h"

typedef enum OPCodeFormat {
#define FMT(f) OP_FMT_##f,
#define DEF(id, size, n_pop, n_push, f)
#include "instrdef.h"
#undef DEF
#undef FMT
} OPCodeFormat;

enum OPCodeEnum {
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f) OP_##id,
#define def(id, size, n_pop, n_push, f)
#include "instrdef.h"
#undef def
#undef DEF
#undef FMT
  OP_COUNT, /* excluding temporary opcodes */
  /* temporary opcodes : overlap with the short opcodes */
  OP_TEMP_START = OP_nop + 1,
  OP___dummy = OP_TEMP_START - 1,
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f)
#define def(id, size, n_pop, n_push, f) OP_##id,
#include "instrdef.h"
#undef def
#undef DEF
#undef FMT
  OP_TEMP_END,
};

typedef struct JSOpCode {
#ifdef DUMP_BYTECODE
  const char *name;
#endif
  uint8_t size; /* in bytes */
  /* the opcodes remove n_pop items from the top of the stack, then
     pushes n_push items */
  uint8_t n_pop;
  uint8_t n_push;
  uint8_t fmt;
} JSOpCode;

static const JSOpCode opcode_info[OP_COUNT + (OP_TEMP_END - OP_TEMP_START)] = {
#define FMT(f)
#ifdef DUMP_BYTECODE
#define DEF(id, size, n_pop, n_push, f) {#id, size, n_pop, n_push, OP_FMT_##f},
#else
#define DEF(id, size, n_pop, n_push, f) {size, n_pop, n_push, OP_FMT_##f},
#endif
#include "instrdef.h"
#undef DEF
#undef FMT
};

/* argument of OP_special_object */
typedef enum {
  OP_SPECIAL_OBJECT_ARGUMENTS,
  OP_SPECIAL_OBJECT_MAPPED_ARGUMENTS,
  OP_SPECIAL_OBJECT_THIS_FUNC,
  OP_SPECIAL_OBJECT_NEW_TARGET,
  OP_SPECIAL_OBJECT_HOME_OBJECT,
  OP_SPECIAL_OBJECT_VAR_OBJECT,
  OP_SPECIAL_OBJECT_IMPORT_META,
} OPSpecialObjectEnum;

#if SHORT_OPCODES
/* After the final compilation pass, short opcodes are used. Their
   opcodes overlap with the temporary opcodes which cannot appear in
   the final bytecode. Their description is after the temporary
   opcodes in opcode_info[]. */
#define short_opcode_info(op)                                                  \
  opcode_info[(op) >= OP_TEMP_START ? (op) + (OP_TEMP_END - OP_TEMP_START)     \
                                    : (op)]
#else
#define short_opcode_info(op) opcode_info[op]
#endif

#define JS_THROW_VAR_RO 0
#define JS_THROW_VAR_REDECL 1
#define JS_THROW_VAR_UNINITIALIZED 2
#define JS_THROW_ERROR_DELETE_SUPER 3
#define JS_THROW_ERROR_ITERATOR_THROW 4

#define OP_DEFINE_METHOD_METHOD 0
#define OP_DEFINE_METHOD_GETTER 1
#define OP_DEFINE_METHOD_SETTER 2
#define OP_DEFINE_METHOD_ENUMERABLE 4

#endif
