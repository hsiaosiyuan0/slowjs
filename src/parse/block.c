#include "parse.h"

#include "vm/error.h"

void push_break_entry(JSFunctionDef *fd, BlockEnv *be, JSAtom label_name,
                      int label_break, int label_cont, int drop_count) {
  be->prev = fd->top_break;
  fd->top_break = be;
  be->label_name = label_name;
  be->label_break = label_break;
  be->label_cont = label_cont;
  be->drop_count = drop_count;
  be->label_finally = -1;
  be->scope_level = fd->scope_level;
  be->has_iterator = FALSE;
}

void pop_break_entry(JSFunctionDef *fd) {
  BlockEnv *be;
  be = fd->top_break;
  fd->top_break = be->prev;
}

__exception int emit_break(JSParseState *s, JSAtom name, int is_cont) {
  BlockEnv *top;
  int i, scope_level;

  scope_level = s->cur_func->scope_level;
  top = s->cur_func->top_break;
  while (top != NULL) {
    close_scopes(s, scope_level, top->scope_level);
    scope_level = top->scope_level;
    if (is_cont && top->label_cont != -1 &&
        (name == JS_ATOM_NULL || top->label_name == name)) {
      /* continue stays inside the same block */
      emit_goto(s, OP_goto, top->label_cont);
      return 0;
    }
    if (!is_cont && top->label_break != -1 &&
        (name == JS_ATOM_NULL || top->label_name == name)) {
      emit_goto(s, OP_goto, top->label_break);
      return 0;
    }
    i = 0;
    if (top->has_iterator) {
      emit_op(s, OP_iterator_close);
      i += 3;
    }
    for (; i < top->drop_count; i++)
      emit_op(s, OP_drop);
    if (top->label_finally != -1) {
      /* must push dummy value to keep same stack depth */
      emit_op(s, OP_undefined);
      emit_goto(s, OP_gosub, top->label_finally);
      emit_op(s, OP_drop);
    }
    top = top->prev;
  }
  if (name == JS_ATOM_NULL) {
    if (is_cont)
      return js_parse_error(s, "continue must be inside loop");
    else
      return js_parse_error(s, "break must be inside loop or switch");
  } else {
    return js_parse_error(s, "break/continue label not found");
  }
}

__exception int js_parse_block(JSParseState *s) {
  if (js_parse_expect(s, '{'))
    return -1;
  if (s->token.val != '}') {
    push_scope(s);
    for (;;) {
      if (js_parse_statement_or_decl(s, DECL_MASK_ALL))
        return -1;
      if (s->token.val == '}')
        break;
    }
    pop_scope(s);
  }
  if (next_token(s))
    return -1;
  return 0;
}
