#include "char.h"

int skip_spaces(const char *pc) {
  const uint8_t *p, *p_next, *p_start;
  uint32_t c;

  p = p_start = (const uint8_t *)pc;
  for (;;) {
    c = *p;
    if (c < 128) {
      if (!((c >= 0x09 && c <= 0x0d) || (c == 0x20)))
        break;
      p++;
    } else {
      c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p_next);
      if (!lre_is_space(c))
        break;
      p = p_next;
    }
  }
  return p - p_start;
}