#ifndef QUICKJS_STR_H
#define QUICKJS_STR_H

#include "def.h"

#include "utils/char.h"

/* -- JSString ----------------------------------- */

#define JS_STRING_LEN_MAX ((1 << 30) - 1)

typedef struct JSString {
  JSRefCountHeader header; /* must come first, 32-bit */
  uint32_t len : 31;
  uint8_t is_wide_char : 1; /* 0 = 8 bits, 1 = 16 bits characters */
  /* for JS_ATOM_TYPE_SYMBOL: hash = 0, atom_type = 3,
     for JS_ATOM_TYPE_PRIVATE: hash = 1, atom_type = 3
     XXX: could change encoding to have one more bit in hash */
  uint32_t hash : 30;
  uint8_t atom_type : 2; /* != 0 if atom, JS_ATOM_TYPE_x */
  uint32_t hash_next;    /* atom_index for JS_ATOM_TYPE_SYMBOL */
#ifdef DUMP_LEAKS
  struct list_head link; /* string list */
#endif
  union {
    uint8_t str8[0]; /* 8 bit strings will get an extra null terminator */
    uint16_t str16[0];
  } u;
} JSString;

/* Note: the string contents are uninitialized */
JSString *js_alloc_string_rt(JSRuntime *rt, int max_len, int is_wide_char);
JSString *js_alloc_string(JSContext *ctx, int max_len, int is_wide_char);
void JS_FreeAtomStruct(JSRuntime *rt, JSAtomStruct *p);
/* same as JS_FreeValueRT() but faster */
static inline void js_free_string(JSRuntime *rt, JSString *str) {
  if (--str->header.ref_count <= 0) {
    if (str->atom_type) {
      JS_FreeAtomStruct(rt, str);
    } else {
#ifdef DUMP_LEAKS
      list_del(&str->link);
#endif
      js_free_rt(rt, str);
    }
  }
}

JSValue js_new_string8(JSContext *ctx, const uint8_t *buf, int len);
JSValue js_new_string16(JSContext *ctx, const uint16_t *buf, int len);
JSValue js_new_string_char(JSContext *ctx, uint16_t c);
int string_get(const JSString *p, int idx);

JSValue JS_ConcatString(JSContext *ctx, JSValue op1, JSValue op2);
JSValue JS_ConcatString3(JSContext *ctx, const char *str1, JSValue str2,
                         const char *str3);

uint32_t hash_string(const JSString *str, uint32_t h);
int js_string_compare(JSContext *ctx, const JSString *p1, const JSString *p2);
int string_getc(const JSString *p, int *pidx);
JSValue js_sub_string(JSContext *ctx, JSString *p, int start, int end);

static inline BOOL is_num_string(uint32_t *pval, const JSString *p) {
  uint32_t n;
  uint64_t n64;
  int c, i, len;

  len = p->len;
  if (len == 0 || len > 10)
    return FALSE;
  if (p->is_wide_char)
    c = p->u.str16[0];
  else
    c = p->u.str8[0];
  if (is_num(c)) {
    if (c == '0') {
      if (len != 1)
        return FALSE;
      n = 0;
    } else {
      n = c - '0';
      for (i = 1; i < len; i++) {
        if (p->is_wide_char)
          c = p->u.str16[i];
        else
          c = p->u.str8[i];
        if (!is_num(c))
          return FALSE;
        n64 = (uint64_t)n * 10 + (c - '0');
        if ((n64 >> 32) != 0)
          return FALSE;
        n = n64;
      }
    }
    *pval = n;
    return TRUE;
  } else {
    return FALSE;
  }
}

static inline BOOL JS_IsEmptyString(JSValueConst v) {
  return JS_VALUE_GET_TAG(v) == JS_TAG_STRING &&
         JS_VALUE_GET_STRING(v)->len == 0;
}

/* -- StringBuffer ----------------------------------- */

typedef struct StringBuffer {
  JSContext *ctx;
  JSString *str;
  int len;
  int size;
  int is_wide_char;
  int error_status;
} StringBuffer;

int string_buffer_init2(JSContext *ctx, StringBuffer *s, int size, int is_wide);
static inline int string_buffer_init(JSContext *ctx, StringBuffer *s,
                                     int size) {
  return string_buffer_init2(ctx, s, size, 0);
}

void string_buffer_free(StringBuffer *s);
int string_buffer_set_error(StringBuffer *s);
no_inline int string_buffer_widen(StringBuffer *s, int size);
no_inline int string_buffer_realloc(StringBuffer *s, int new_len, int c);
no_inline int string_buffer_putc_slow(StringBuffer *s, uint32_t c);
int string_buffer_putc16(StringBuffer *s, uint32_t c);
int string_buffer_putc(StringBuffer *s, uint32_t c);
int string_buffer_putc8(StringBuffer *s, uint32_t c);
int string_buffer_write8(StringBuffer *s, const uint8_t *p, int len);
int string_buffer_write16(StringBuffer *s, const uint16_t *p, int len);
int string_buffer_puts8(StringBuffer *s, const char *str);
int string_buffer_concat(StringBuffer *s, const JSString *p, uint32_t from,
                         uint32_t to);
int string_buffer_concat_value(StringBuffer *s, JSValueConst v);
int string_buffer_concat_value_free(StringBuffer *s, JSValue v);
int string_buffer_fill(StringBuffer *s, int c, int count);
JSValue string_buffer_end(StringBuffer *s);

/* -- JSAtom ----------------------------------- */

enum {
  JS_ATOM_TYPE_STRING = 1,
  JS_ATOM_TYPE_GLOBAL_SYMBOL,
  JS_ATOM_TYPE_SYMBOL,
  JS_ATOM_TYPE_PRIVATE,
};

enum {
  JS_ATOM_HASH_SYMBOL,
  JS_ATOM_HASH_PRIVATE,
};

typedef enum {
  JS_ATOM_KIND_STRING,
  JS_ATOM_KIND_SYMBOL,
  JS_ATOM_KIND_PRIVATE,
} JSAtomKindEnum;

#define JS_ATOM_HASH_MASK ((1 << 30) - 1)

enum {
  __JS_ATOM_NULL = JS_ATOM_NULL,
#define DEF(name, str) JS_ATOM_##name,
#include "stratom.h"
#undef DEF
  JS_ATOM_END,
};
#define JS_ATOM_LAST_KEYWORD JS_ATOM_super
#define JS_ATOM_LAST_STRICT_KEYWORD JS_ATOM_yield

// clang-format off
static const char js_atom_init[] =
#define DEF(name, str) str "\0"
#include "stratom.h"
#undef DEF
;
// clang-format on

#define ATOM_GET_STR_BUF_SIZE 64

#define JS_ATOM_TAG_INT (1U << 31)
#define JS_ATOM_MAX_INT (JS_ATOM_TAG_INT - 1)
#define JS_ATOM_MAX ((1U << 30) - 1)

/* return the max count from the hash size */
#define JS_ATOM_COUNT_RESIZE(n) ((n)*2)

JSAtom __JS_NewAtom(JSRuntime *rt, JSString *str, int atom_type);
JSAtom JS_NewAtomStr(JSContext *ctx, JSString *p);
const char *JS_AtomGetStr(JSContext *ctx, char *buf, int buf_size, JSAtom atom);
const char *JS_AtomGetStrRT(JSRuntime *rt, char *buf, int buf_size,
                            JSAtom atom);
BOOL JS_AtomIsArrayIndex(JSContext *ctx, uint32_t *pval, JSAtom atom);
JSValue JS_AtomIsNumericIndex1(JSContext *ctx, JSAtom atom);
int JS_AtomIsNumericIndex(JSContext *ctx, JSAtom atom);
JSAtomKindEnum JS_AtomGetKind(JSContext *ctx, JSAtom v);
JSAtom JS_NewAtomInt64(JSContext *ctx, int64_t n);
uint32_t js_string_obj_get_length(JSContext *ctx, JSValueConst obj);

static inline BOOL __JS_AtomIsConst(JSAtom v) {
#if defined(DUMP_LEAKS) && DUMP_LEAKS > 1
  return (int32_t)v <= 0;
#else
  return (int32_t)v < JS_ATOM_END;
#endif
}

BOOL JS_AtomIsString(JSContext *ctx, JSAtom v);

static inline BOOL __JS_AtomIsTaggedInt(JSAtom v) {
  return (v & JS_ATOM_TAG_INT) != 0;
}

static inline JSAtom __JS_AtomFromUInt32(uint32_t v) {
  return v | JS_ATOM_TAG_INT;
}

static inline uint32_t __JS_AtomToUInt32(JSAtom atom) {
  return atom & ~JS_ATOM_TAG_INT;
}

__maybe_unused void print_atom(JSContext *ctx, JSAtom atom);

JSAtom js_atom_concat_str(JSContext *ctx, JSAtom name, const char *str1);
JSAtom js_atom_concat_num(JSContext *ctx, JSAtom name, uint32_t n);

typedef struct JSString JSAtomStruct;

void JS_FreeAtomStruct(JSRuntime *rt, JSAtomStruct *p);

static inline uint32_t atom_get_free(const JSAtomStruct *p) {
  return (uintptr_t)p >> 1;
}

static inline BOOL atom_is_free(const JSAtomStruct *p) {
  return (uintptr_t)p & 1;
}

static inline JSAtomStruct *atom_set_free(uint32_t v) {
  return (JSAtomStruct *)(((uintptr_t)v << 1) | 1);
}

JSAtom js_get_atom_index(JSRuntime *rt, JSAtomStruct *p);

/* only works with zero terminated 8 bit strings */
JSAtom __JS_NewAtomInit(JSRuntime *rt, const char *str, int len, int atom_type);
JSAtom JS_DupAtomRT(JSRuntime *rt, JSAtom v);
JSAtom __JS_FindAtom(JSRuntime *rt, const char *str, size_t len, int atom_type);

/* -- Symbol ----------------------------------- */

BOOL JS_AtomSymbolHasDescription(JSContext *ctx, JSAtom v);
JSAtom js_symbol_to_atom(JSContext *ctx, JSValue val);
JSValue JS_NewSymbol(JSContext *ctx, JSString *p, int atom_type);
JSValue JS_NewSymbolFromAtom(JSContext *ctx, JSAtom descr, int atom_type);

#endif
