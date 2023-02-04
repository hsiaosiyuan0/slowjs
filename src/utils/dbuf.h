#ifndef QUICKJS_DBUF_H
#define QUICKJS_DBUF_H

#include "def.h"

static inline void js_dbuf_init(JSContext *ctx, DynBuf *s) {
  dbuf_init2(s, ctx->rt, (DynBufReallocFunc *)js_realloc_rt);
}

void dbuf_put_leb128(DynBuf *s, uint32_t v);
void dbuf_put_sleb128(DynBuf *s, int32_t v1);
int get_leb128(uint32_t *pval, const uint8_t *buf, const uint8_t *buf_end);
int get_sleb128(int32_t *pval, const uint8_t *buf, const uint8_t *buf_end);

#endif