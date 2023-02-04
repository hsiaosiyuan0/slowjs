#include "parse.h"

#include "vm/conv.h"

/* 'c' is the first character. Return JS_ATOM_NULL in case of error */
JSAtom json_parse_ident(JSParseState *s, const uint8_t **pp, int c) {
  const uint8_t *p;
  char ident_buf[128], *buf;
  size_t ident_size, ident_pos;
  JSAtom atom;

  p = *pp;
  buf = ident_buf;
  ident_size = sizeof(ident_buf);
  ident_pos = 0;
  for (;;) {
    buf[ident_pos++] = c;
    c = *p;
    if (c >= 128 || !((lre_id_continue_table_ascii[c >> 5] >> (c & 31)) & 1))
      break;
    p++;
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

__exception int json_next_token(JSParseState *s) {
  const uint8_t *p;
  int c;
  JSAtom atom;

  if (js_check_stack_overflow(s->ctx->rt, 0)) {
    return js_parse_error(s, "stack overflow");
  }

  free_token(s, &s->token);

  p = s->last_ptr = s->buf_ptr;
  s->last_line_num = s->token.line_num;
redo:
  s->token.line_num = s->line_num;
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
  case '\'':
    if (!s->ext_json) {
      /* JSON does not accept single quoted strings */
      goto def_token;
    }
    /* fall through */
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
    s->line_num++;
    goto redo;
  case '\f':
  case '\v':
    if (!s->ext_json) {
      /* JSONWhitespace does not match <VT>, nor <FF> */
      goto def_token;
    }
    /* fall through */
  case ' ':
  case '\t':
    p++;
    goto redo;
  case '/':
    if (!s->ext_json) {
      /* JSON does not accept comments */
      goto def_token;
    }
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
          p++;
        } else if (*p == '\r') {
          p++;
        } else if (*p >= 0x80) {
          c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
          if (c == -1) {
            p++; /* skip invalid UTF-8 */
          }
        } else {
          p++;
        }
      }
      goto redo;
    } else if (p[1] == '/') {
      /* line comment */
      p += 2;
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
      goto redo;
    } else {
      goto def_token;
    }
    break;
    // clang-format off
    case 'a': case 'b': case 'c': case 'd':
    case 'e': case 'f': case 'g': case 'h':
    case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p':
    case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z': 
    case 'A': case 'B': case 'C': case 'D':
    case 'E': case 'F': case 'G': case 'H':
    case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z': 
    case '_':
    case '$':
    // clang-format on  
        /* identifier : only pure ascii characters are accepted */
        p++;
        atom = json_parse_ident(s, &p, c);
        if (atom == JS_ATOM_NULL)
            goto fail;
        s->token.u.ident.atom = atom;
        s->token.u.ident.has_escape = FALSE;
        s->token.u.ident.is_reserved = FALSE;
        s->token.val = TOK_IDENT;
        break;
    case '+':
        if (!s->ext_json || !is_digit(p[1]))
            goto def_token;
        goto parse_number;
    case '0':
        if (is_digit(p[1]))
            goto def_token;
        goto parse_number;
    case '-':
        if (!is_digit(p[1]))
            goto def_token;
        goto parse_number;
    case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8':
    case '9': 
        /* number */
    parse_number:
        {
            JSValue ret;
            int flags, radix;
            if (!s->ext_json) {
                flags = 0;
                radix = 10;
            } else {
                flags = ATOD_ACCEPT_BIN_OCT;
                radix = 0;
            }
            ret = js_atof(s->ctx, (const char *)p, (const char **)&p, radix,
                          flags);
            if (JS_IsException(ret))
                goto fail;
            s->token.val = TOK_NUMBER;
            s->token.u.num.val = ret;
        }
        break;
    default:
        if (c >= 128) {
            js_parse_error(s, "unexpected character");
            goto fail;
        }
    def_token:
        s->token.val = c;
        p++;
        break;
    }
    s->buf_ptr = p;

    //    dump_token(s, &s->token);
    return 0;

 fail:
    s->token.val = TOK_ERROR;
    return -1;
}

  int json_parse_expect(JSParseState *s, int tok) {
  if (s->token.val != tok) {
    /* XXX: dump token correctly in all cases */
    return js_parse_error(s, "expecting '%c'", tok);
  }
  return json_next_token(s);
}

  JSValue json_parse_value(JSParseState *s) {
  JSContext *ctx = s->ctx;
  JSValue val = JS_NULL;
  int ret;

  switch (s->token.val) {
  case '{': {
    JSValue prop_val;
    JSAtom prop_name;

    if (json_next_token(s))
      goto fail;
    val = JS_NewObject(ctx);
    if (JS_IsException(val))
      goto fail;
    if (s->token.val != '}') {
      for (;;) {
        if (s->token.val == TOK_STRING) {
          prop_name = JS_ValueToAtom(ctx, s->token.u.str.str);
          if (prop_name == JS_ATOM_NULL)
            goto fail;
        } else if (s->ext_json && s->token.val == TOK_IDENT) {
          prop_name = JS_DupAtom(ctx, s->token.u.ident.atom);
        } else {
          js_parse_error(s, "expecting property name");
          goto fail;
        }
        if (json_next_token(s))
          goto fail1;
        if (json_parse_expect(s, ':'))
          goto fail1;
        prop_val = json_parse_value(s);
        if (JS_IsException(prop_val)) {
        fail1:
          JS_FreeAtom(ctx, prop_name);
          goto fail;
        }
        ret = JS_DefinePropertyValue(ctx, val, prop_name, prop_val,
                                     JS_PROP_C_W_E);
        JS_FreeAtom(ctx, prop_name);
        if (ret < 0)
          goto fail;

        if (s->token.val != ',')
          break;
        if (json_next_token(s))
          goto fail;
        if (s->ext_json && s->token.val == '}')
          break;
      }
    }
    if (json_parse_expect(s, '}'))
      goto fail;
  } break;
  case '[': {
    JSValue el;
    uint32_t idx;

    if (json_next_token(s))
      goto fail;
    val = JS_NewArray(ctx);
    if (JS_IsException(val))
      goto fail;
    if (s->token.val != ']') {
      idx = 0;
      for (;;) {
        el = json_parse_value(s);
        if (JS_IsException(el))
          goto fail;
        ret = JS_DefinePropertyValueUint32(ctx, val, idx, el, JS_PROP_C_W_E);
        if (ret < 0)
          goto fail;
        if (s->token.val != ',')
          break;
        if (json_next_token(s))
          goto fail;
        idx++;
        if (s->ext_json && s->token.val == ']')
          break;
      }
    }
    if (json_parse_expect(s, ']'))
      goto fail;
  } break;
  case TOK_STRING:
    val = JS_DupValue(ctx, s->token.u.str.str);
    if (json_next_token(s))
      goto fail;
    break;
  case TOK_NUMBER:
    val = s->token.u.num.val;
    if (json_next_token(s))
      goto fail;
    break;
  case TOK_IDENT:
    if (s->token.u.ident.atom == JS_ATOM_false ||
        s->token.u.ident.atom == JS_ATOM_true) {
      val = JS_NewBool(ctx, (s->token.u.ident.atom == JS_ATOM_true));
    } else if (s->token.u.ident.atom == JS_ATOM_null) {
      val = JS_NULL;
    } else {
      goto def_token;
    }
    if (json_next_token(s))
      goto fail;
    break;
  default:
  def_token:
    if (s->token.val == TOK_EOF) {
      js_parse_error(s, "unexpected end of input");
    } else {
      js_parse_error(s, "unexpected token: '%.*s'",
                     (int)(s->buf_ptr - s->token.ptr), s->token.ptr);
    }
    goto fail;
  }
  return val;
fail:
  JS_FreeValue(ctx, val);
  return JS_EXCEPTION;
}
