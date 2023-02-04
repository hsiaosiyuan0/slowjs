#ifndef QUICKJS_CHAR_H
#define QUICKJS_CHAR_H

#include "def.h"

int skip_spaces(const char *pc);

static inline int to_digit(int c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'A' && c <= 'Z')
    return c - 'A' + 10;
  else if (c >= 'a' && c <= 'z')
    return c - 'a' + 10;
  else
    return 36;
}

static inline int is_digit(int c) { return c >= '0' && c <= '9'; }
static inline int is_num(int c) { return c >= '0' && c <= '9'; };

#endif