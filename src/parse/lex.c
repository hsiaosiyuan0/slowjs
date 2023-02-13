#include "libs/cutils.h"
#include "parse.h"

#include "vm/conv.h"
#include "vm/obj.h"

void skip_shebang(JSParseState *s) {
  const uint8_t *p = s->buf_ptr;
  int c;

  if (p[0] == '#' && p[1] == '!') {
    p += 2;
    while (p < s->buf_end) {
      if (*p == '\n' || *p == '\r') {
        break;
      } else if (*p >= 0x80) {
        c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
        if (c == CP_LS || c == CP_PS) {
          break;
        } else if (c == -1) {
          p++; /* skip invalid UTF-8 */
        }
      } else {
        p++;
      }
    }
    s->buf_ptr = p;
  }
}

__exception int next_token(JSParseState *s) {
  const uint8_t *p, *tp;
  int c;
  BOOL ident_has_escape;
  JSAtom atom;

  if (js_check_stack_overflow(s->ctx->rt, 0)) {
    return js_parse_error(s, "stack overflow");
  }

  free_token(s, &s->token);

  p = s->last_ptr = s->buf_ptr;
  s->got_lf = FALSE;
  s->last_line_num = s->token.line_num;
redo:
  s->token.line_num = s->line_num;
  s->token.col_num = s->col_num;
  s->token.ptr = p;
  c = *p;
  switch (c) {
  case 0:
    if (p >= s->buf_end) {
      s->token.val = TOK_EOF;
    } else {
      goto def_token;
    }
    break;
  case '`':
    if (js_parse_template_part(s, p + 1))
      goto fail;
    p = s->buf_ptr;
    break;
  case '\'':
  case '\"':
    if (js_parse_string(s, c, TRUE, p + 1, &s->token, &p))
      goto fail;
    break;
  case '\r': /* accept DOS and MAC newline sequences */
    if (p[1] == '\n') {
      p++;
    }
    /* fall thru */
  case '\n':
    p++;
  line_terminator:
    s->got_lf = TRUE;
    s->line_num++;
    s->col_num = 1;
    goto redo;
  case '\f':
  case '\v':
  case ' ':
  case '\t':
    p++;
    s->col_num += 1;
    goto redo;
  case '/':
    if (p[1] == '*') {
      /* comment */
      p += 2;
      for (;;) {
        if (*p == '\0' && p >= s->buf_end) {
          js_parse_error(s, "unexpected end of comment");
          goto fail;
        }
        if (p[0] == '*' && p[1] == '/') {
          p += 2;
          break;
        }
        if (*p == '\n') {
          s->line_num++;
          s->col_num = 1;
          s->got_lf = TRUE; /* considered as LF for ASI */
          p++;
        } else if (*p == '\r') {
          s->got_lf = TRUE; /* considered as LF for ASI */
          p++;
        } else if (*p >= 0x80) {
          c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
          if (c == CP_LS || c == CP_PS) {
            s->got_lf = TRUE; /* considered as LF for ASI */
          } else if (c == -1) {
            p++; /* skip invalid UTF-8 */
          }
        } else {
          p++;
        }
      }

#ifdef CONFIG_DEBUGGER
      tp = s->token.ptr;
      while (tp < p) {
        unicode_from_utf8(tp, UTF8_CHAR_LEN_MAX, &tp);
        s->col_num += 1;
      }
#endif

      goto redo;
    } else if (p[1] == '/') {
      /* line comment */
      p += 2;
    skip_line_comment:
      for (;;) {
        if (*p == '\0' && p >= s->buf_end)
          break;
        if (*p == '\r' || *p == '\n')
          break;
        if (*p >= 0x80) {
          c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
          /* LS or PS are considered as line terminator */
          if (c == CP_LS || c == CP_PS) {
            break;
          } else if (c == -1) {
            p++; /* skip invalid UTF-8 */
          }
        } else {
          p++;
        }
      }

#ifdef CONFIG_DEBUGGER
      tp = s->token.ptr;
      while (tp < p) {
        unicode_from_utf8(tp, UTF8_CHAR_LEN_MAX, &tp);
        s->col_num += 1;
      }
#endif

      goto redo;
    } else if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_DIV_ASSIGN;
    } else {
      p++;
      s->token.val = c;
    }
    break;
  case '\\':
    if (p[1] == 'u') {
      const uint8_t *p1 = p + 1;
      int c1 = lre_parse_escape(&p1, TRUE);
      if (c1 >= 0 && lre_js_is_ident_first(c1)) {
        c = c1;
        p = p1;
        ident_has_escape = TRUE;
        goto has_ident;
      } else {
        /* XXX: syntax error? */
      }
    }
    goto def_token;
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':
  case 'g':
  case 'h':
  case 'i':
  case 'j':
  case 'k':
  case 'l':
  case 'm':
  case 'n':
  case 'o':
  case 'p':
  case 'q':
  case 'r':
  case 's':
  case 't':
  case 'u':
  case 'v':
  case 'w':
  case 'x':
  case 'y':
  case 'z':
  case 'A':
  case 'B':
  case 'C':
  case 'D':
  case 'E':
  case 'F':
  case 'G':
  case 'H':
  case 'I':
  case 'J':
  case 'K':
  case 'L':
  case 'M':
  case 'N':
  case 'O':
  case 'P':
  case 'Q':
  case 'R':
  case 'S':
  case 'T':
  case 'U':
  case 'V':
  case 'W':
  case 'X':
  case 'Y':
  case 'Z':
  case '_':
  case '$':
    /* identifier */
    p++;
    ident_has_escape = FALSE;
  has_ident:
    atom = parse_ident(s, &p, &ident_has_escape, c, FALSE);
    if (atom == JS_ATOM_NULL)
      goto fail;
    s->token.u.ident.atom = atom;
    s->token.u.ident.has_escape = ident_has_escape;
    s->token.u.ident.is_reserved = FALSE;
    if (s->token.u.ident.atom <= JS_ATOM_LAST_KEYWORD ||
        (s->token.u.ident.atom <= JS_ATOM_LAST_STRICT_KEYWORD &&
         (s->cur_func->js_mode & JS_MODE_STRICT)) ||
        (s->token.u.ident.atom == JS_ATOM_yield &&
         ((s->cur_func->func_kind & JS_FUNC_GENERATOR) ||
          (s->cur_func->func_type == JS_PARSE_FUNC_ARROW &&
           !s->cur_func->in_function_body && s->cur_func->parent &&
           (s->cur_func->parent->func_kind & JS_FUNC_GENERATOR)))) ||
        (s->token.u.ident.atom == JS_ATOM_await &&
         (s->is_module ||
          (((s->cur_func->func_kind & JS_FUNC_ASYNC) ||
            (s->cur_func->func_type == JS_PARSE_FUNC_ARROW &&
             !s->cur_func->in_function_body && s->cur_func->parent &&
             (s->cur_func->parent->func_kind & JS_FUNC_ASYNC))))))) {
      if (ident_has_escape) {
        s->token.u.ident.is_reserved = TRUE;
        s->token.val = TOK_IDENT;
      } else {
        /* The keywords atoms are pre allocated */
        s->token.val = s->token.u.ident.atom - 1 + TOK_FIRST_KEYWORD;
      }
    } else {
      s->token.val = TOK_IDENT;
    }
    break;
  case '#':
    /* private name */
    {
      const uint8_t *p1;
      p++;
      p1 = p;
      c = *p1++;
      if (c == '\\' && *p1 == 'u') {
        c = lre_parse_escape(&p1, TRUE);
      } else if (c >= 128) {
        c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p1);
      }
      if (!lre_js_is_ident_first(c)) {
        js_parse_error(s, "invalid first character of private name");
        goto fail;
      }
      p = p1;
      ident_has_escape = FALSE; /* not used */
      atom = parse_ident(s, &p, &ident_has_escape, c, TRUE);
      if (atom == JS_ATOM_NULL)
        goto fail;
      s->token.u.ident.atom = atom;
      s->token.val = TOK_PRIVATE_NAME;
    }
    break;
  case '.':
    if (p[1] == '.' && p[2] == '.') {
      p += 3;
      s->token.val = TOK_ELLIPSIS;
      break;
    }
    if (p[1] >= '0' && p[1] <= '9') {
      goto parse_number;
    } else {
      goto def_token;
    }
    break;
  case '0':
    /* in strict mode, octal literals are not accepted */
    if (is_digit(p[1]) && (s->cur_func->js_mode & JS_MODE_STRICT)) {
      js_parse_error(s, "octal literals are deprecated in strict mode");
      goto fail;
    }
    goto parse_number;
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    /* number */
  parse_number : {
    JSValue ret;
    const uint8_t *p1;
    int flags, radix;
    flags = ATOD_ACCEPT_BIN_OCT | ATOD_ACCEPT_LEGACY_OCTAL |
            ATOD_ACCEPT_UNDERSCORES;
#ifdef CONFIG_BIGNUM
    flags |= ATOD_ACCEPT_SUFFIX;
    if (s->cur_func->js_mode & JS_MODE_MATH) {
      flags |= ATOD_MODE_BIGINT;
      if (s->cur_func->js_mode & JS_MODE_MATH)
        flags |= ATOD_TYPE_BIG_FLOAT;
    }
#endif
    radix = 0;
#ifdef CONFIG_BIGNUM
    s->token.u.num.exponent = 0;
    ret = js_atof2(s->ctx, (const char *)p, (const char **)&p, radix, flags,
                   &s->token.u.num.exponent);
#else
    ret = js_atof(s->ctx, (const char *)p, (const char **)&p, radix, flags);
#endif
    if (JS_IsException(ret))
      goto fail;
    /* reject `10instanceof Number` */
    if (JS_VALUE_IS_NAN(ret) ||
        lre_js_is_ident_next(unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p1))) {
      JS_FreeValue(s->ctx, ret);
      js_parse_error(s, "invalid number literal");
      goto fail;
    }
    s->token.val = TOK_NUMBER;
    s->token.u.num.val = ret;
  } break;
  case '*':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_MUL_ASSIGN;
    } else if (p[1] == '*') {
      if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_POW_ASSIGN;
      } else {
        p += 2;
        s->token.val = TOK_POW;
      }
    } else {
      goto def_token;
    }
    break;
  case '%':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_MOD_ASSIGN;
    } else {
      goto def_token;
    }
    break;
  case '+':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_PLUS_ASSIGN;
    } else if (p[1] == '+') {
      p += 2;
      s->token.val = TOK_INC;
    } else {
      goto def_token;
    }
    break;
  case '-':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_MINUS_ASSIGN;
    } else if (p[1] == '-') {
      if (s->allow_html_comments && p[2] == '>' &&
          s->last_line_num != s->line_num) {
        /* Annex B: `-->` at beginning of line is an html comment end.
           It extends to the end of the line.
         */
        goto skip_line_comment;
      }
      p += 2;
      s->token.val = TOK_DEC;
    } else {
      goto def_token;
    }
    break;
  case '<':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_LTE;
    } else if (p[1] == '<') {
      if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_SHL_ASSIGN;
      } else {
        p += 2;
        s->token.val = TOK_SHL;
      }
    } else if (s->allow_html_comments && p[1] == '!' && p[2] == '-' &&
               p[3] == '-') {
      /* Annex B: handle `<!--` single line html comments */
      goto skip_line_comment;
    } else {
      goto def_token;
    }
    break;
  case '>':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_GTE;
    } else if (p[1] == '>') {
      if (p[2] == '>') {
        if (p[3] == '=') {
          p += 4;
          s->token.val = TOK_SHR_ASSIGN;
        } else {
          p += 3;
          s->token.val = TOK_SHR;
        }
      } else if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_SAR_ASSIGN;
      } else {
        p += 2;
        s->token.val = TOK_SAR;
      }
    } else {
      goto def_token;
    }
    break;
  case '=':
    if (p[1] == '=') {
      if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_STRICT_EQ;
      } else {
        p += 2;
        s->token.val = TOK_EQ;
      }
    } else if (p[1] == '>') {
      p += 2;
      s->token.val = TOK_ARROW;
    } else {
      goto def_token;
    }
    break;
  case '!':
    if (p[1] == '=') {
      if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_STRICT_NEQ;
      } else {
        p += 2;
        s->token.val = TOK_NEQ;
      }
    } else {
      goto def_token;
    }
    break;
  case '&':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_AND_ASSIGN;
    } else if (p[1] == '&') {
      if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_LAND_ASSIGN;
      } else {
        p += 2;
        s->token.val = TOK_LAND;
      }
    } else {
      goto def_token;
    }
    break;
#ifdef CONFIG_BIGNUM
    /* in math mode, '^' is the power operator. '^^' is always the
       xor operator and '**' is always the power operator */
  case '^':
    if (p[1] == '=') {
      p += 2;
      if (s->cur_func->js_mode & JS_MODE_MATH)
        s->token.val = TOK_MATH_POW_ASSIGN;
      else
        s->token.val = TOK_XOR_ASSIGN;
    } else if (p[1] == '^') {
      if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_XOR_ASSIGN;
      } else {
        p += 2;
        s->token.val = '^';
      }
    } else {
      p++;
      if (s->cur_func->js_mode & JS_MODE_MATH)
        s->token.val = TOK_MATH_POW;
      else
        s->token.val = '^';
    }
    break;
#else
  case '^':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_XOR_ASSIGN;
    } else {
      goto def_token;
    }
    break;
#endif
  case '|':
    if (p[1] == '=') {
      p += 2;
      s->token.val = TOK_OR_ASSIGN;
    } else if (p[1] == '|') {
      if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_LOR_ASSIGN;
      } else {
        p += 2;
        s->token.val = TOK_LOR;
      }
    } else {
      goto def_token;
    }
    break;
  case '?':
    if (p[1] == '?') {
      if (p[2] == '=') {
        p += 3;
        s->token.val = TOK_DOUBLE_QUESTION_MARK_ASSIGN;
      } else {
        p += 2;
        s->token.val = TOK_DOUBLE_QUESTION_MARK;
      }
    } else if (p[1] == '.' && !(p[2] >= '0' && p[2] <= '9')) {
      p += 2;
      s->token.val = TOK_QUESTION_MARK_DOT;
    } else {
      goto def_token;
    }
    break;
  default:
    if (c >= 128) {
      /* unicode value */
      c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
      switch (c) {
      case CP_PS:
      case CP_LS:
        /* XXX: should avoid incrementing line_number, but
           needed to handle HTML comments */
        goto line_terminator;
      default:

#ifdef CONFIG_DEBUGGER
        tp = s->token.ptr;
        while (tp < p) {
          unicode_from_utf8(tp, UTF8_CHAR_LEN_MAX, &tp);
          s->col_num += 1;
        }
#endif
        if (lre_is_space(c)) {
          goto redo;
        } else if (lre_js_is_ident_first(c)) {
          ident_has_escape = FALSE;
          goto has_ident;
        } else {
          js_parse_error(s, "unexpected character");
          goto fail;
        }
      }
    }
  def_token:
    s->token.val = c;
    p++;
    break;
  }

#ifdef CONFIG_DEBUGGER
  tp = s->token.ptr;
  while (tp < p) {
    unicode_from_utf8(tp, UTF8_CHAR_LEN_MAX, &tp);
    s->col_num += 1;
  }
#endif

  s->buf_ptr = p;

#ifdef DUMP_TOKEN
  dump_token(s, &s->token);
#endif

  return 0;

fail:
  s->token.val = TOK_ERROR;
  return -1;
}

/* only used for ':' and '=>', 'let' or 'function' look-ahead. *pp is
   only set if TOK_IMPORT is returned */
/* XXX: handle all unicode cases */
int simple_next_token(const uint8_t **pp, BOOL no_line_terminator) {
  const uint8_t *p;
  uint32_t c;

  /* skip spaces and comments */
  p = *pp;
  for (;;) {
    switch (c = *p++) {
    case '\r':
    case '\n':
      if (no_line_terminator)
        return '\n';
      continue;
    case ' ':
    case '\t':
    case '\v':
    case '\f':
      continue;
    case '/':
      if (*p == '/') {
        if (no_line_terminator)
          return '\n';
        while (*p && *p != '\r' && *p != '\n')
          p++;
        continue;
      }
      if (*p == '*') {
        while (*++p) {
          if ((*p == '\r' || *p == '\n') && no_line_terminator)
            return '\n';
          if (*p == '*' && p[1] == '/') {
            p += 2;
            break;
          }
        }
        continue;
      }
      break;
    case '=':
      if (*p == '>')
        return TOK_ARROW;
      break;
    default:
      if (lre_js_is_ident_first(c)) {
        if (c == 'i') {
          if (p[0] == 'n' && !lre_js_is_ident_next(p[1])) {
            return TOK_IN;
          }
          if (p[0] == 'm' && p[1] == 'p' && p[2] == 'o' && p[3] == 'r' &&
              p[4] == 't' && !lre_js_is_ident_next(p[5])) {
            *pp = p + 5;
            return TOK_IMPORT;
          }
        } else if (c == 'o' && *p == 'f' && !lre_js_is_ident_next(p[1])) {
          return TOK_OF;
        } else if (c == 'e' && p[0] == 'x' && p[1] == 'p' && p[2] == 'o' &&
                   p[3] == 'r' && p[4] == 't' && !lre_js_is_ident_next(p[5])) {
          *pp = p + 5;
          return TOK_EXPORT;
        } else if (c == 'f' && p[0] == 'u' && p[1] == 'n' && p[2] == 'c' &&
                   p[3] == 't' && p[4] == 'i' && p[5] == 'o' && p[6] == 'n' &&
                   !lre_js_is_ident_next(p[7])) {
          return TOK_FUNCTION;
        }
        return TOK_IDENT;
      }
      break;
    }
    return c;
  }
}

int peek_token(JSParseState *s, BOOL no_line_terminator) {
  const uint8_t *p = s->buf_ptr;
  return simple_next_token(&p, no_line_terminator);
}

void free_token(JSParseState *s, JSToken *token) {
  switch (token->val) {
#ifdef CONFIG_BIGNUM
  case TOK_NUMBER:
    JS_FreeValue(s->ctx, token->u.num.val);
    break;
#endif
  case TOK_STRING:
  case TOK_TEMPLATE:
    JS_FreeValue(s->ctx, token->u.str.str);
    break;
  case TOK_REGEXP:
    JS_FreeValue(s->ctx, token->u.regexp.body);
    JS_FreeValue(s->ctx, token->u.regexp.flags);
    break;
  case TOK_IDENT:
  case TOK_PRIVATE_NAME:
    JS_FreeAtom(s->ctx, token->u.ident.atom);
    break;
  default:
    if (token->val >= TOK_FIRST_KEYWORD && token->val <= TOK_LAST_KEYWORD) {
      JS_FreeAtom(s->ctx, token->u.ident.atom);
    }
    break;
  }
}

int js_unsupported_keyword(JSParseState *s, JSAtom atom) {
  char buf[ATOM_GET_STR_BUF_SIZE];
  return js_parse_error(s, "unsupported keyword: %s",
                        JS_AtomGetStr(s->ctx, buf, sizeof(buf), atom));
}

__exception int js_parse_seek_token(JSParseState *s, const JSParsePos *sp) {
  s->token.line_num = sp->last_line_num;
  s->line_num = sp->line_num;
  s->col_num = sp->col_num;
  s->buf_ptr = sp->ptr;
  s->got_lf = sp->got_lf;
  return next_token(s);
}

void __attribute((unused)) dump_token(JSParseState *s, const JSToken *token) {
  int line, col;

  line = token->line_num;
  col = token->col_num;
  switch (token->val) {
  case TOK_NUMBER: {
    double d;
    JS_ToFloat64(s->ctx, &d, token->u.num.val); /* no exception possible */
    printf("number: %.14g %d:%d\n", d, line, col);
  } break;
  case TOK_IDENT:
  dump_atom : {
    char buf[ATOM_GET_STR_BUF_SIZE];
    printf("ident: '%s' %d:%d\n",
           JS_AtomGetStr(s->ctx, buf, sizeof(buf), token->u.ident.atom), line,
           col);
  } break;
  case TOK_STRING: {
    const char *str;
    /* XXX: quote the string */
    str = JS_ToCString(s->ctx, token->u.str.str);
    printf("string: '%s' %d:%d\n", str, line, col);
    JS_FreeCString(s->ctx, str);
  } break;
  case TOK_TEMPLATE: {
    const char *str;
    str = JS_ToCString(s->ctx, token->u.str.str);
    printf("template: `%s` %d:%d\n", str, line, col);
    JS_FreeCString(s->ctx, str);
  } break;
  case TOK_REGEXP: {
    const char *str, *str2;
    str = JS_ToCString(s->ctx, token->u.regexp.body);
    str2 = JS_ToCString(s->ctx, token->u.regexp.flags);
    printf("regexp: '%s' '%s' %d:%d\n", str, str2, line, col);
    JS_FreeCString(s->ctx, str);
    JS_FreeCString(s->ctx, str2);
  } break;
  case TOK_EOF:
    printf("eof\n");
    break;
  default:
    if (s->token.val >= TOK_NULL && s->token.val <= TOK_LAST_KEYWORD) {
      goto dump_atom;
    } else if (s->token.val >= 256) {
      printf("token: %d %d:%d\n", token->val, line, col);
    } else {
      printf("token: '%c' %d:%d\n", token->val, line, col);
    }
    break;
  }
}

__exception int js_parse_template_part(JSParseState *s, const uint8_t *p) {
  uint32_t c;
  StringBuffer b_s, *b = &b_s;

  /* p points to the first byte of the template part */
  if (string_buffer_init(s->ctx, b, 32))
    goto fail;
  for (;;) {
    if (p >= s->buf_end)
      goto unexpected_eof;
    c = *p++;
    if (c == '`') {
      /* template end part */
      break;
    }
    if (c == '$' && *p == '{') {
      /* template start or middle part */
      p++;
      break;
    }
    if (c == '\\') {
      if (string_buffer_putc8(b, c))
        goto fail;
      if (p >= s->buf_end)
        goto unexpected_eof;
      c = *p++;
    }
    /* newline sequences are normalized as single '\n' bytes */
    if (c == '\r') {
      if (*p == '\n')
        p++;
      c = '\n';
    }
    if (c == '\n') {
      s->line_num++;
      s->col_num = 1;
    } else if (c >= 0x80) {
      const uint8_t *p_next;
      c = unicode_from_utf8(p - 1, UTF8_CHAR_LEN_MAX, &p_next);
      if (c > 0x10FFFF) {
        js_parse_error(s, "invalid UTF-8 sequence");
        goto fail;
      }
      p = p_next;
    }
    if (string_buffer_putc(b, c))
      goto fail;
  }
  s->token.val = TOK_TEMPLATE;
  s->token.u.str.sep = c;
  s->token.u.str.str = string_buffer_end(b);
  s->buf_ptr = p;
  return 0;

unexpected_eof:
  js_parse_error(s, "unexpected end of string");
fail:
  string_buffer_free(b);
  return -1;
}

__exception int js_parse_template(JSParseState *s, int call, int *argc) {
  JSContext *ctx = s->ctx;
  JSValue raw_array, template_object;
  JSToken cooked;
  int depth, ret;

  raw_array = JS_UNDEFINED;       /* avoid warning */
  template_object = JS_UNDEFINED; /* avoid warning */
  if (call) {
    /* Create a template object: an array of cooked strings */
    /* Create an array of raw strings and store it to the raw property */
    template_object = JS_NewArray(ctx);
    if (JS_IsException(template_object))
      return -1;
    //        pool_idx = s->cur_func->cpool_count;
    ret = emit_push_const(s, template_object, 0);
    JS_FreeValue(ctx, template_object);
    if (ret)
      return -1;
    raw_array = JS_NewArray(ctx);
    if (JS_IsException(raw_array))
      return -1;
    if (JS_DefinePropertyValue(ctx, template_object, JS_ATOM_raw, raw_array,
                               JS_PROP_THROW) < 0) {
      return -1;
    }
  }

  depth = 0;
  while (s->token.val == TOK_TEMPLATE) {
    const uint8_t *p = s->token.ptr + 1;
    cooked = s->token;
    if (call) {
      if (JS_DefinePropertyValueUint32(
              ctx, raw_array, depth, JS_DupValue(ctx, s->token.u.str.str),
              JS_PROP_ENUMERABLE | JS_PROP_THROW) < 0) {
        return -1;
      }
      /* re-parse the string with escape sequences but do not throw a
         syntax error if it contains invalid sequences
       */
      if (js_parse_string(s, '`', FALSE, p, &cooked, &p)) {
        cooked.u.str.str = JS_UNDEFINED;
      }
      if (JS_DefinePropertyValueUint32(
              ctx, template_object, depth, cooked.u.str.str,
              JS_PROP_ENUMERABLE | JS_PROP_THROW) < 0) {
        return -1;
      }
    } else {
      JSString *str;
      /* re-parse the string with escape sequences and throw a
         syntax error if it contains invalid sequences
       */
      JS_FreeValue(ctx, s->token.u.str.str);
      s->token.u.str.str = JS_UNDEFINED;
      if (js_parse_string(s, '`', TRUE, p, &cooked, &p))
        return -1;
      str = JS_VALUE_GET_STRING(cooked.u.str.str);
      if (str->len != 0 || depth == 0) {
        ret = emit_push_const(s, cooked.u.str.str, 1);
        JS_FreeValue(s->ctx, cooked.u.str.str);
        if (ret)
          return -1;
        if (depth == 0) {
          if (s->token.u.str.sep == '`')
            goto done1;
          emit_op(s, OP_get_field2);
          emit_atom(s, JS_ATOM_concat);
        }
        depth++;
      } else {
        JS_FreeValue(s->ctx, cooked.u.str.str);
      }
    }
    if (s->token.u.str.sep == '`')
      goto done;
    if (next_token(s))
      return -1;
    if (js_parse_expr(s))
      return -1;
    depth++;
    if (s->token.val != '}') {
      return js_parse_error(s, "expected '}' after template expression");
    }
    /* XXX: should convert to string at this stage? */
    free_token(s, &s->token);
    /* Resume TOK_TEMPLATE parsing (s->token.line_num and
     * s->token.ptr are OK) */
    s->got_lf = FALSE;
    s->last_line_num = s->token.line_num;
    if (js_parse_template_part(s, s->buf_ptr))
      return -1;
  }
  return js_parse_expect(s, TOK_TEMPLATE);

done:
  if (call) {
    /* Seal the objects */
    seal_template_obj(ctx, raw_array);
    seal_template_obj(ctx, template_object);
    *argc = depth + 1;
  } else {
    emit_op(s, OP_call_method);
    emit_u16(s, depth - 1);
  }
done1:
  return next_token(s);
}

__exception int js_parse_string(JSParseState *s, int sep, BOOL do_throw,
                                const uint8_t *p, JSToken *token,
                                const uint8_t **pp) {
  int ret;
  uint32_t c;
  StringBuffer b_s, *b = &b_s;

  /* string */
  if (string_buffer_init(s->ctx, b, 32))
    goto fail;
  for (;;) {
    if (p >= s->buf_end)
      goto invalid_char;
    c = *p;
    if (c < 0x20) {
      if (!s->cur_func) {
        if (do_throw)
          js_parse_error(s, "invalid character in a JSON string");
        goto fail;
      }
      if (sep == '`') {
        if (c == '\r') {
          if (p[1] == '\n')
            p++;
          c = '\n';
        }
        /* do not update s->line_num */
      } else if (c == '\n' || c == '\r')
        goto invalid_char;
    }
    p++;
    if (c == sep)
      break;
    if (c == '$' && *p == '{' && sep == '`') {
      /* template start or middle part */
      p++;
      break;
    }
    if (c == '\\') {
      c = *p;
      /* XXX: need a specific JSON case to avoid
         accepting invalid escapes */
      switch (c) {
      case '\0':
        if (p >= s->buf_end)
          goto invalid_char;
        p++;
        break;
      case '\'':
      case '\"':
      case '\\':
        p++;
        break;
      case '\r': /* accept DOS and MAC newline sequences */
        if (p[1] == '\n') {
          p++;
        }
        /* fall thru */
      case '\n':
        /* ignore escaped newline sequence */
        p++;
        if (sep != '`') {
          s->line_num++;
          s->col_num = 1;
        }
        continue;
      default:
        if (c >= '0' && c <= '9') {
          if (!s->cur_func)
            goto invalid_escape; /* JSON case */
          if (!(s->cur_func->js_mode & JS_MODE_STRICT) && sep != '`')
            goto parse_escape;
          if (c == '0' && !(p[1] >= '0' && p[1] <= '9')) {
            p++;
            c = '\0';
          } else {
            if (c >= '8' || sep == '`') {
              /* Note: according to ES2021, \8 and \9 are not
                 accepted in strict mode or in templates. */
              goto invalid_escape;
            } else {
              if (do_throw)
                js_parse_error(
                    s, "octal escape sequences are not allowed in strict mode");
            }
            goto fail;
          }
        } else if (c >= 0x80) {
          const uint8_t *p_next;
          c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p_next);
          if (c > 0x10FFFF) {
            goto invalid_utf8;
          }
          p = p_next;
          /* LS or PS are skipped */
          if (c == CP_LS || c == CP_PS)
            continue;
        } else {
        parse_escape:
          ret = lre_parse_escape(&p, TRUE);
          if (ret == -1) {
          invalid_escape:
            if (do_throw)
              js_parse_error(s, "malformed escape sequence in string literal");
            goto fail;
          } else if (ret < 0) {
            /* ignore the '\' (could output a warning) */
            p++;
          } else {
            c = ret;
          }
        }
        break;
      }
    } else if (c >= 0x80) {
      const uint8_t *p_next;
      c = unicode_from_utf8(p - 1, UTF8_CHAR_LEN_MAX, &p_next);
      if (c > 0x10FFFF)
        goto invalid_utf8;
      p = p_next;
    }
    if (string_buffer_putc(b, c))
      goto fail;
  }
  token->val = TOK_STRING;
  token->u.str.sep = c;
  token->u.str.str = string_buffer_end(b);
  *pp = p;
  return 0;

invalid_utf8:
  if (do_throw)
    js_parse_error(s, "invalid UTF-8 sequence");
  goto fail;
invalid_char:
  if (do_throw)
    js_parse_error(s, "unexpected end of string");
fail:
  string_buffer_free(b);
  return -1;
}

__exception int js_parse_regexp(JSParseState *s) {
  const uint8_t *p;
  BOOL in_class;
  StringBuffer b_s, *b = &b_s;
  StringBuffer b2_s, *b2 = &b2_s;
  uint32_t c;

  p = s->buf_ptr;
  p++;
  in_class = FALSE;
  if (string_buffer_init(s->ctx, b, 32))
    return -1;
  if (string_buffer_init(s->ctx, b2, 1))
    goto fail;
  for (;;) {
    if (p >= s->buf_end) {
    eof_error:
      js_parse_error(s, "unexpected end of regexp");
      goto fail;
    }
    c = *p++;
    if (c == '\n' || c == '\r') {
      goto eol_error;
    } else if (c == '/') {
      if (!in_class)
        break;
    } else if (c == '[') {
      in_class = TRUE;
    } else if (c == ']') {
      /* XXX: incorrect as the first character in a class */
      in_class = FALSE;
    } else if (c == '\\') {
      if (string_buffer_putc8(b, c))
        goto fail;
      c = *p++;
      if (c == '\n' || c == '\r')
        goto eol_error;
      else if (c == '\0' && p >= s->buf_end)
        goto eof_error;
      else if (c >= 0x80) {
        const uint8_t *p_next;
        c = unicode_from_utf8(p - 1, UTF8_CHAR_LEN_MAX, &p_next);
        if (c > 0x10FFFF) {
          goto invalid_utf8;
        }
        p = p_next;
        if (c == CP_LS || c == CP_PS)
          goto eol_error;
      }
    } else if (c >= 0x80) {
      const uint8_t *p_next;
      c = unicode_from_utf8(p - 1, UTF8_CHAR_LEN_MAX, &p_next);
      if (c > 0x10FFFF) {
      invalid_utf8:
        js_parse_error(s, "invalid UTF-8 sequence");
        goto fail;
      }
      p = p_next;
      /* LS or PS are considered as line terminator */
      if (c == CP_LS || c == CP_PS) {
      eol_error:
        js_parse_error(s, "unexpected line terminator in regexp");
        goto fail;
      }
    }
    if (string_buffer_putc(b, c))
      goto fail;
  }

  /* flags */
  for (;;) {
    const uint8_t *p_next = p;
    c = *p_next++;
    if (c >= 0x80) {
      c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p_next);
      if (c > 0x10FFFF) {
        goto invalid_utf8;
      }
    }
    if (!lre_js_is_ident_next(c))
      break;
    if (string_buffer_putc(b2, c))
      goto fail;
    p = p_next;
  }

  s->token.val = TOK_REGEXP;
  s->token.u.regexp.body = string_buffer_end(b);
  s->token.u.regexp.flags = string_buffer_end(b2);
  s->buf_ptr = p;
  return 0;
fail:
  string_buffer_free(b);
  string_buffer_free(b2);
  return -1;
}

__exception int ident_realloc(JSContext *ctx, char **pbuf, size_t *psize,
                              char *static_buf) {
  char *buf, *new_buf;
  size_t size, new_size;

  buf = *pbuf;
  size = *psize;
  if (size >= (SIZE_MAX / 3) * 2)
    new_size = SIZE_MAX;
  else
    new_size = size + (size >> 1);
  if (buf == static_buf) {
    new_buf = js_malloc(ctx, new_size);
    if (!new_buf)
      return -1;
    memcpy(new_buf, buf, size);
  } else {
    new_buf = js_realloc(ctx, buf, new_size);
    if (!new_buf)
      return -1;
  }
  *pbuf = new_buf;
  *psize = new_size;
  return 0;
}

/* 'c' is the first character. Return JS_ATOM_NULL in case of error */
JSAtom parse_ident(JSParseState *s, const uint8_t **pp, BOOL *pident_has_escape,
                   int c, BOOL is_private) {
  const uint8_t *p, *p1;
  char ident_buf[128], *buf;
  size_t ident_size, ident_pos;
  JSAtom atom;

  p = *pp;
  buf = ident_buf;
  ident_size = sizeof(ident_buf);
  ident_pos = 0;
  if (is_private)
    buf[ident_pos++] = '#';
  for (;;) {
    p1 = p;

    if (c < 128) {
      buf[ident_pos++] = c;
    } else {
      ident_pos += unicode_to_utf8((uint8_t *)buf + ident_pos, c);
    }
    c = *p1++;
    if (c == '\\' && *p1 == 'u') {
      c = lre_parse_escape(&p1, TRUE);
      *pident_has_escape = TRUE;
    } else if (c >= 128) {
      c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p1);
    }
    if (!lre_js_is_ident_next(c))
      break;
    p = p1;
    if (unlikely(ident_pos >= ident_size - UTF8_CHAR_LEN_MAX)) {
      if (ident_realloc(s->ctx, &buf, &ident_size, ident_buf)) {
        atom = JS_ATOM_NULL;
        goto done;
      }
    }
  }
  atom = JS_NewAtomLen(s->ctx, buf, ident_pos);
done:
  if (unlikely(buf != ident_buf))
    js_free(s->ctx, buf);
  *pp = p;
  return atom;
}

BOOL token_is_ident(int tok) {
  /* Accept keywords and reserved words as property names */
  return (tok == TOK_IDENT ||
          (tok >= TOK_FIRST_KEYWORD && tok <= TOK_LAST_KEYWORD));
}