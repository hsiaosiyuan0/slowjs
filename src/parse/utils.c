#include "parse.h"

#include "vm/error.h"

int __attribute__((format(printf, 2, 3)))
js_parse_error(JSParseState *s, const char *fmt, ...) {
  JSContext *ctx = s->ctx;
  va_list ap;
  int backtrace_flags;

  va_start(ap, fmt);
  JS_ThrowError2(ctx, JS_SYNTAX_ERROR, fmt, ap, FALSE);
  va_end(ap);
  backtrace_flags = 0;
  if (s->cur_func && s->cur_func->backtrace_barrier)
    backtrace_flags = JS_BACKTRACE_FLAG_SINGLE_LEVEL;
  build_backtrace(ctx, ctx->rt->current_exception, s->filename, s->line_num,
                  backtrace_flags);
  return -1;
}

int js_parse_expect(JSParseState *s, int tok) {
  if (s->token.val != tok) {
    /* XXX: dump token correctly in all cases */
    return js_parse_error(s, "expecting '%c'", tok);
  }
  return next_token(s);
}

int js_parse_expect_semi(JSParseState *s) {
  if (s->token.val != ';') {
    /* automatic insertion of ';' */
    if (s->token.val == TOK_EOF || s->token.val == '}' || s->got_lf) {
      return 0;
    }
    return js_parse_error(s, "expecting '%c'", ';');
  }
  return next_token(s);
}

int js_parse_error_reserved_identifier(JSParseState *s) {
  char buf1[ATOM_GET_STR_BUF_SIZE];
  return js_parse_error(
      s, "'%s' is a reserved identifier",
      JS_AtomGetStr(s->ctx, buf1, sizeof(buf1), s->token.u.ident.atom));
}

int js_parse_get_pos(JSParseState *s, JSParsePos *sp) {
  sp->last_line_num = s->last_line_num;
  sp->line_num = s->token.line_num;
	sp->col_num = s->token.col_num;
  sp->ptr = s->token.ptr;
  sp->got_lf = s->got_lf;
  return 0;
}

/* return TRUE if a regexp literal is allowed after this token */
BOOL is_regexp_allowed(int tok) {
  switch (tok) {
  case TOK_NUMBER:
  case TOK_STRING:
  case TOK_REGEXP:
  case TOK_DEC:
  case TOK_INC:
  case TOK_NULL:
  case TOK_FALSE:
  case TOK_TRUE:
  case TOK_THIS:
  case ')':
  case ']':
  case '}': /* XXX: regexp may occur after */
  case TOK_IDENT:
    return FALSE;
  default:
    return TRUE;
  }
}

/* test if the current token is a label. Use simplistic look-ahead scanner */
BOOL is_label(JSParseState *s) {
  return (s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved &&
          peek_token(s, FALSE) == ':');
}

/* test if the current token is a let keyword. Use simplistic look-ahead scanner
 */
int is_let(JSParseState *s, int decl_mask) {
  int res = FALSE;

  if (token_is_pseudo_keyword(s, JS_ATOM_let)) {
#if 1
    JSParsePos pos;
    js_parse_get_pos(s, &pos);
    for (;;) {
      if (next_token(s)) {
        res = -1;
        break;
      }
      if (s->token.val == '[') {
        /* let [ is a syntax restriction:
           it never introduces an ExpressionStatement */
        res = TRUE;
        break;
      }
      if (s->token.val == '{' ||
          (s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved) ||
          s->token.val == TOK_LET || s->token.val == TOK_YIELD ||
          s->token.val == TOK_AWAIT) {
        /* Check for possible ASI if not scanning for Declaration */
        /* XXX: should also check that `{` introduces a BindingPattern,
           but Firefox does not and rejects eval("let=1;let\n{if(1)2;}") */
        if (s->last_line_num == s->token.line_num ||
            (decl_mask & DECL_MASK_OTHER)) {
          res = TRUE;
          break;
        }
        break;
      }
      break;
    }
    if (js_parse_seek_token(s, &pos)) {
      res = -1;
    }
#else
    int tok = peek_token(s, TRUE);
    if (tok == '{' || tok == TOK_IDENT || peek_token(s, FALSE) == '[') {
      res = TRUE;
    }
#endif
  }
  return res;
}
