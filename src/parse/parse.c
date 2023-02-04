#include "parse.h"

void js_parse_init(JSContext *ctx, JSParseState *s, const char *input,
                   size_t input_len, const char *filename) {
  memset(s, 0, sizeof(*s));
  s->ctx = ctx;
  s->filename = filename;
  s->line_num = 1;
  s->buf_ptr = (const uint8_t *)input;
  s->buf_end = s->buf_ptr + input_len;
  s->token.val = ' ';
  s->token.line_num = 1;
}

__exception int js_parse_program(JSParseState *s) {
  JSFunctionDef *fd = s->cur_func;
  int idx;

  if (next_token(s))
    return -1;

  if (js_parse_directives(s))
    return -1;

  fd->is_global_var = (fd->eval_type == JS_EVAL_TYPE_GLOBAL) ||
                      (fd->eval_type == JS_EVAL_TYPE_MODULE) ||
                      !(fd->js_mode & JS_MODE_STRICT);

  if (!s->is_module) {
    /* hidden variable for the return value */
    fd->eval_ret_idx = idx = add_var(s->ctx, fd, JS_ATOM__ret_);
    if (idx < 0)
      return -1;
  }

  while (s->token.val != TOK_EOF) {
    if (js_parse_source_element(s))
      return -1;
  }

  if (!s->is_module) {
    /* return the value of the hidden variable eval_ret_idx  */
    emit_op(s, OP_get_loc);
    emit_u16(s, fd->eval_ret_idx);

    emit_op(s, OP_return);
  } else {
    emit_op(s, OP_return_undef);
  }

  return 0;
}

/* return true if 'input' contains the source of a module
   (heuristic). 'input' must be a zero terminated.

   Heuristic: skip comments and expect 'import' keyword not followed
   by '(' or '.' or export keyword.
*/
BOOL JS_DetectModule(const char *input, size_t input_len) {
  const uint8_t *p = (const uint8_t *)input;
  int tok;
  switch (simple_next_token(&p, FALSE)) {
  case TOK_IMPORT:
    tok = simple_next_token(&p, FALSE);
    return (tok != '.' && tok != '(');
  case TOK_EXPORT:
    return TRUE;
  default:
    return FALSE;
  }
}