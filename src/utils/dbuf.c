#include "dbuf.h"

#include "libs/cutils.h"

void dbuf_put_leb128(DynBuf *s, uint32_t v) {
  uint32_t a;
  for (;;) {
    a = v & 0x7f;
    v >>= 7;
    if (v != 0) {
      dbuf_putc(s, a | 0x80);
    } else {
      dbuf_putc(s, a);
      break;
    }
  }
}

void dbuf_put_sleb128(DynBuf *s, int32_t v1) {
  uint32_t v = v1;
  dbuf_put_leb128(s, (2 * v) ^ -(v >> 31));
}

int get_leb128(uint32_t *pval, const uint8_t *buf, const uint8_t *buf_end) {
  const uint8_t *ptr = buf;
  uint32_t v, a, i;
  v = 0;
  for (i = 0; i < 5; i++) {
    if (unlikely(ptr >= buf_end))
      break;
    a = *ptr++;
    v |= (a & 0x7f) << (i * 7);
    if (!(a & 0x80)) {
      *pval = v;
      return ptr - buf;
    }
  }
  *pval = 0;
  return -1;
}

int get_sleb128(int32_t *pval, const uint8_t *buf, const uint8_t *buf_end) {
  int ret;
  uint32_t val;
  ret = get_leb128(&val, buf, buf_end);
  if (ret < 0) {
    *pval = 0;
    return -1;
  }
  *pval = (val >> 1) ^ -(val & 1);
  return ret;
}