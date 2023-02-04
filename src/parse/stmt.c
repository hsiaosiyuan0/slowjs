#include "parse.h"

__exception int js_parse_statement_or_decl(JSParseState *s, int decl_mask);

__exception int js_parse_statement(JSParseState *s) {
  return js_parse_statement_or_decl(s, 0);
}

/* XXX: handle IteratorClose when exiting the loop before the
   enumeration is done */
static __exception int js_parse_for_in_of(JSParseState *s, int label_name,
                                          BOOL is_async) {
  JSContext *ctx = s->ctx;
  JSFunctionDef *fd = s->cur_func;
  JSAtom var_name;
  BOOL has_initializer, is_for_of, has_destructuring;
  int tok, tok1, opcode, scope, block_scope_level;
  int label_next, label_expr, label_cont, label_body, label_break;
  int pos_next, pos_expr;
  BlockEnv break_entry;

  has_initializer = FALSE;
  has_destructuring = FALSE;
  is_for_of = FALSE;
  block_scope_level = fd->scope_level;
  label_cont = new_label(s);
  label_body = new_label(s);
  label_break = new_label(s);
  label_next = new_label(s);

  /* create scope for the lexical variables declared in the enumeration
     expressions. XXX: Not completely correct because of weird capturing
     semantics in `for (i of o) a.push(function(){return i})` */
  push_scope(s);

  /* local for_in scope starts here so individual elements
     can be closed in statement. */
  push_break_entry(s->cur_func, &break_entry, label_name, label_break,
                   label_cont, 1);
  break_entry.scope_level = block_scope_level;

  label_expr = emit_goto(s, OP_goto, -1);

  pos_next = s->cur_func->byte_code.size;
  emit_label(s, label_next);

  tok = s->token.val;
  switch (is_let(s, DECL_MASK_OTHER)) {
  case TRUE:
    tok = TOK_LET;
    break;
  case FALSE:
    break;
  default:
    return -1;
  }
  if (tok == TOK_VAR || tok == TOK_LET || tok == TOK_CONST) {
    if (next_token(s))
      return -1;

    if (!(s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved)) {
      if (s->token.val == '[' || s->token.val == '{') {
        if (js_parse_destructuring_element(s, tok, 0, TRUE, -1, FALSE) < 0)
          return -1;
        has_destructuring = TRUE;
      } else {
        return js_parse_error(s, "variable name expected");
      }
      var_name = JS_ATOM_NULL;
    } else {
      var_name = JS_DupAtom(ctx, s->token.u.ident.atom);
      if (next_token(s)) {
        JS_FreeAtom(s->ctx, var_name);
        return -1;
      }
      if (js_define_var(s, var_name, tok)) {
        JS_FreeAtom(s->ctx, var_name);
        return -1;
      }
      emit_op(s, (tok == TOK_CONST || tok == TOK_LET) ? OP_scope_put_var_init
                                                      : OP_scope_put_var);
      emit_atom(s, var_name);
      emit_u16(s, fd->scope_level);
    }
  } else {
    int skip_bits;
    if ((s->token.val == '[' || s->token.val == '{') &&
        ((tok1 = js_parse_skip_parens_token(s, &skip_bits, FALSE)) == TOK_IN ||
         tok1 == TOK_OF)) {
      if (js_parse_destructuring_element(
              s, 0, 0, TRUE, skip_bits & SKIP_HAS_ELLIPSIS, TRUE) < 0)
        return -1;
    } else {
      int lvalue_label;
      if (js_parse_left_hand_side_expr(s))
        return -1;
      if (get_lvalue(s, &opcode, &scope, &var_name, &lvalue_label, NULL, FALSE,
                     TOK_FOR))
        return -1;
      put_lvalue(s, opcode, scope, var_name, lvalue_label,
                 PUT_LVALUE_NOKEEP_BOTTOM, FALSE);
    }
    var_name = JS_ATOM_NULL;
  }
  emit_goto(s, OP_goto, label_body);

  pos_expr = s->cur_func->byte_code.size;
  emit_label(s, label_expr);
  if (s->token.val == '=') {
    /* XXX: potential scoping issue if inside `with` statement */
    has_initializer = TRUE;
    /* parse and evaluate initializer prior to evaluating the
       object (only used with "for in" with a non lexical variable
       in non strict mode */
    if (next_token(s) || js_parse_assign_expr2(s, 0)) {
      JS_FreeAtom(ctx, var_name);
      return -1;
    }
    if (var_name != JS_ATOM_NULL) {
      emit_op(s, OP_scope_put_var);
      emit_atom(s, var_name);
      emit_u16(s, fd->scope_level);
    }
  }
  JS_FreeAtom(ctx, var_name);

  if (token_is_pseudo_keyword(s, JS_ATOM_of)) {
    break_entry.has_iterator = is_for_of = TRUE;
    break_entry.drop_count += 2;
    if (has_initializer)
      goto initializer_error;
  } else if (s->token.val == TOK_IN) {
    if (is_async)
      return js_parse_error(s, "'for await' loop should be used with 'of'");
    if (has_initializer && (tok != TOK_VAR || (fd->js_mode & JS_MODE_STRICT) ||
                            has_destructuring)) {
    initializer_error:
      return js_parse_error(s,
                            "a declaration in the head of a for-%s loop can't "
                            "have an initializer",
                            is_for_of ? "of" : "in");
    }
  } else {
    return js_parse_error(s, "expected 'of' or 'in' in for control expression");
  }
  if (next_token(s))
    return -1;
  if (is_for_of) {
    if (js_parse_assign_expr(s))
      return -1;
  } else {
    if (js_parse_expr(s))
      return -1;
  }
  /* close the scope after having evaluated the expression so that
     the TDZ values are in the closures */
  close_scopes(s, s->cur_func->scope_level, block_scope_level);
  if (is_for_of) {
    if (is_async)
      emit_op(s, OP_for_await_of_start);
    else
      emit_op(s, OP_for_of_start);
    /* on stack: enum_rec */
  } else {
    emit_op(s, OP_for_in_start);
    /* on stack: enum_obj */
  }
  emit_goto(s, OP_goto, label_cont);

  if (js_parse_expect(s, ')'))
    return -1;

  if (OPTIMIZE) {
    /* move the `next` code here */
    DynBuf *bc = &s->cur_func->byte_code;
    int chunk_size = pos_expr - pos_next;
    int offset = bc->size - pos_next;
    int i;
    dbuf_realloc(bc, bc->size + chunk_size);
    dbuf_put(bc, bc->buf + pos_next, chunk_size);
    memset(bc->buf + pos_next, OP_nop, chunk_size);
    /* `next` part ends with a goto */
    s->cur_func->last_opcode_pos = bc->size - 5;
    /* relocate labels */
    for (i = label_cont; i < s->cur_func->label_count; i++) {
      LabelSlot *ls = &s->cur_func->label_slots[i];
      if (ls->pos >= pos_next && ls->pos < pos_expr)
        ls->pos += offset;
    }
  }

  emit_label(s, label_body);
  if (js_parse_statement(s))
    return -1;

  close_scopes(s, s->cur_func->scope_level, block_scope_level);

  emit_label(s, label_cont);
  if (is_for_of) {
    if (is_async) {
      /* call the next method */
      /* stack: iter_obj next catch_offset */
      emit_op(s, OP_dup3);
      emit_op(s, OP_drop);
      emit_op(s, OP_call_method);
      emit_u16(s, 0);
      /* get the result of the promise */
      emit_op(s, OP_await);
      /* unwrap the value and done values */
      emit_op(s, OP_iterator_get_value_done);
    } else {
      emit_op(s, OP_for_of_next);
      emit_u8(s, 0);
    }
  } else {
    emit_op(s, OP_for_in_next);
  }
  /* on stack: enum_rec / enum_obj value bool */
  emit_goto(s, OP_if_false, label_next);
  /* drop the undefined value from for_xx_next */
  emit_op(s, OP_drop);

  emit_label(s, label_break);
  if (is_for_of) {
    /* close and drop enum_rec */
    emit_op(s, OP_iterator_close);
  } else {
    emit_op(s, OP_drop);
  }
  pop_break_entry(s->cur_func);
  pop_scope(s);
  return 0;
}

static void set_eval_ret_undefined(JSParseState *s) {
  if (s->cur_func->eval_ret_idx >= 0) {
    emit_op(s, OP_undefined);
    emit_op(s, OP_put_loc);
    emit_u16(s, s->cur_func->eval_ret_idx);
  }
}

__exception int js_parse_statement_or_decl(JSParseState *s, int decl_mask) {
  JSContext *ctx = s->ctx;
  JSAtom label_name;
  int tok;

  /* specific label handling */
  /* XXX: support multiple labels on loop statements */
  label_name = JS_ATOM_NULL;
  if (is_label(s)) {
    BlockEnv *be;

    label_name = JS_DupAtom(ctx, s->token.u.ident.atom);

    for (be = s->cur_func->top_break; be; be = be->prev) {
      if (be->label_name == label_name) {
        js_parse_error(s, "duplicate label name");
        goto fail;
      }
    }

    if (next_token(s))
      goto fail;
    if (js_parse_expect(s, ':'))
      goto fail;
    if (s->token.val != TOK_FOR && s->token.val != TOK_DO &&
        s->token.val != TOK_WHILE) {
      /* labelled regular statement */
      int label_break, mask;
      BlockEnv break_entry;

      label_break = new_label(s);
      push_break_entry(s->cur_func, &break_entry, label_name, label_break, -1,
                       0);
      if (!(s->cur_func->js_mode & JS_MODE_STRICT) &&
          (decl_mask & DECL_MASK_FUNC_WITH_LABEL)) {
        mask = DECL_MASK_FUNC | DECL_MASK_FUNC_WITH_LABEL;
      } else {
        mask = 0;
      }
      if (js_parse_statement_or_decl(s, mask))
        goto fail;
      emit_label(s, label_break);
      pop_break_entry(s->cur_func);
      goto done;
    }
  }

  switch (tok = s->token.val) {
  case '{':
    if (js_parse_block(s))
      goto fail;
    break;
  case TOK_RETURN:
    if (s->cur_func->is_eval) {
      js_parse_error(s, "return not in a function");
      goto fail;
    }
    if (next_token(s))
      goto fail;
    if (s->token.val != ';' && s->token.val != '}' && !s->got_lf) {
      if (js_parse_expr(s))
        goto fail;
      emit_return(s, TRUE);
    } else {
      emit_return(s, FALSE);
    }
    if (js_parse_expect_semi(s))
      goto fail;
    break;
  case TOK_THROW:
    if (next_token(s))
      goto fail;
    if (s->got_lf) {
      js_parse_error(s, "line terminator not allowed after throw");
      goto fail;
    }
    if (js_parse_expr(s))
      goto fail;
    emit_op(s, OP_throw);
    if (js_parse_expect_semi(s))
      goto fail;
    break;
  case TOK_LET:
  case TOK_CONST:
  haslet:
    if (!(decl_mask & DECL_MASK_OTHER)) {
      js_parse_error(
          s, "lexical declarations can't appear in single-statement context");
      goto fail;
    }
    /* fall thru */
  case TOK_VAR:
    if (next_token(s))
      goto fail;
    if (js_parse_var(s, TRUE, tok, FALSE))
      goto fail;
    if (js_parse_expect_semi(s))
      goto fail;
    break;
  case TOK_IF: {
    int label1, label2, mask;
    if (next_token(s))
      goto fail;
    /* create a new scope for `let f;if(1) function f(){}` */
    push_scope(s);
    set_eval_ret_undefined(s);
    if (js_parse_expr_paren(s))
      goto fail;
    label1 = emit_goto(s, OP_if_false, -1);
    if (s->cur_func->js_mode & JS_MODE_STRICT)
      mask = 0;
    else
      mask = DECL_MASK_FUNC; /* Annex B.3.4 */

    if (js_parse_statement_or_decl(s, mask))
      goto fail;

    if (s->token.val == TOK_ELSE) {
      label2 = emit_goto(s, OP_goto, -1);
      if (next_token(s))
        goto fail;

      emit_label(s, label1);
      if (js_parse_statement_or_decl(s, mask))
        goto fail;

      label1 = label2;
    }
    emit_label(s, label1);
    pop_scope(s);
  } break;
  case TOK_WHILE: {
    int label_cont, label_break;
    BlockEnv break_entry;

    label_cont = new_label(s);
    label_break = new_label(s);

    push_break_entry(s->cur_func, &break_entry, label_name, label_break,
                     label_cont, 0);

    if (next_token(s))
      goto fail;

    set_eval_ret_undefined(s);

    emit_label(s, label_cont);
    if (js_parse_expr_paren(s))
      goto fail;
    emit_goto(s, OP_if_false, label_break);

    if (js_parse_statement(s))
      goto fail;
    emit_goto(s, OP_goto, label_cont);

    emit_label(s, label_break);

    pop_break_entry(s->cur_func);
  } break;
  case TOK_DO: {
    int label_cont, label_break, label1;
    BlockEnv break_entry;

    label_cont = new_label(s);
    label_break = new_label(s);
    label1 = new_label(s);

    push_break_entry(s->cur_func, &break_entry, label_name, label_break,
                     label_cont, 0);

    if (next_token(s))
      goto fail;

    emit_label(s, label1);

    set_eval_ret_undefined(s);

    if (js_parse_statement(s))
      goto fail;

    emit_label(s, label_cont);
    if (js_parse_expect(s, TOK_WHILE))
      goto fail;
    if (js_parse_expr_paren(s))
      goto fail;
    /* Insert semicolon if missing */
    if (s->token.val == ';') {
      if (next_token(s))
        goto fail;
    }
    emit_goto(s, OP_if_true, label1);

    emit_label(s, label_break);

    pop_break_entry(s->cur_func);
  } break;
  case TOK_FOR: {
    int label_cont, label_break, label_body, label_test;
    int pos_cont, pos_body, block_scope_level;
    BlockEnv break_entry;
    int tok, bits;
    BOOL is_async;

    if (next_token(s))
      goto fail;

    set_eval_ret_undefined(s);
    bits = 0;
    is_async = FALSE;
    if (s->token.val == '(') {
      js_parse_skip_parens_token(s, &bits, FALSE);
    } else if (s->token.val == TOK_AWAIT) {
      if (!(s->cur_func->func_kind & JS_FUNC_ASYNC)) {
        js_parse_error(s, "for await is only valid in asynchronous functions");
        goto fail;
      }
      is_async = TRUE;
      if (next_token(s))
        goto fail;
    }
    if (js_parse_expect(s, '('))
      goto fail;

    if (!(bits & SKIP_HAS_SEMI)) {
      /* parse for/in or for/of */
      if (js_parse_for_in_of(s, label_name, is_async))
        goto fail;
      break;
    }
    block_scope_level = s->cur_func->scope_level;

    /* create scope for the lexical variables declared in the initial,
       test and increment expressions */
    push_scope(s);
    /* initial expression */
    tok = s->token.val;
    if (tok != ';') {
      switch (is_let(s, DECL_MASK_OTHER)) {
      case TRUE:
        tok = TOK_LET;
        break;
      case FALSE:
        break;
      default:
        goto fail;
      }
      if (tok == TOK_VAR || tok == TOK_LET || tok == TOK_CONST) {
        if (next_token(s))
          goto fail;
        if (js_parse_var(s, FALSE, tok, FALSE))
          goto fail;
      } else {
        if (js_parse_expr2(s, FALSE))
          goto fail;
        emit_op(s, OP_drop);
      }

      /* close the closures before the first iteration */
      close_scopes(s, s->cur_func->scope_level, block_scope_level);
    }
    if (js_parse_expect(s, ';'))
      goto fail;

    label_test = new_label(s);
    label_cont = new_label(s);
    label_body = new_label(s);
    label_break = new_label(s);

    push_break_entry(s->cur_func, &break_entry, label_name, label_break,
                     label_cont, 0);

    /* test expression */
    if (s->token.val == ';') {
      /* no test expression */
      label_test = label_body;
    } else {
      emit_label(s, label_test);
      if (js_parse_expr(s))
        goto fail;
      emit_goto(s, OP_if_false, label_break);
    }
    if (js_parse_expect(s, ';'))
      goto fail;

    if (s->token.val == ')') {
      /* no end expression */
      break_entry.label_cont = label_cont = label_test;
      pos_cont = 0; /* avoid warning */
    } else {
      /* skip the end expression */
      emit_goto(s, OP_goto, label_body);

      pos_cont = s->cur_func->byte_code.size;
      emit_label(s, label_cont);
      if (js_parse_expr(s))
        goto fail;
      emit_op(s, OP_drop);
      if (label_test != label_body)
        emit_goto(s, OP_goto, label_test);
    }
    if (js_parse_expect(s, ')'))
      goto fail;

    pos_body = s->cur_func->byte_code.size;
    emit_label(s, label_body);
    if (js_parse_statement(s))
      goto fail;

    /* close the closures before the next iteration */
    /* XXX: check continue case */
    close_scopes(s, s->cur_func->scope_level, block_scope_level);

    if (OPTIMIZE && label_test != label_body && label_cont != label_test) {
      /* move the increment code here */
      DynBuf *bc = &s->cur_func->byte_code;
      int chunk_size = pos_body - pos_cont;
      int offset = bc->size - pos_cont;
      int i;
      dbuf_realloc(bc, bc->size + chunk_size);
      dbuf_put(bc, bc->buf + pos_cont, chunk_size);
      memset(bc->buf + pos_cont, OP_nop, chunk_size);
      /* increment part ends with a goto */
      s->cur_func->last_opcode_pos = bc->size - 5;
      /* relocate labels */
      for (i = label_cont; i < s->cur_func->label_count; i++) {
        LabelSlot *ls = &s->cur_func->label_slots[i];
        if (ls->pos >= pos_cont && ls->pos < pos_body)
          ls->pos += offset;
      }
    } else {
      emit_goto(s, OP_goto, label_cont);
    }

    emit_label(s, label_break);

    pop_break_entry(s->cur_func);
    pop_scope(s);
  } break;
  case TOK_BREAK:
  case TOK_CONTINUE: {
    int is_cont = s->token.val - TOK_BREAK;
    int label;

    if (next_token(s))
      goto fail;
    if (!s->got_lf && s->token.val == TOK_IDENT &&
        !s->token.u.ident.is_reserved)
      label = s->token.u.ident.atom;
    else
      label = JS_ATOM_NULL;
    if (emit_break(s, label, is_cont))
      goto fail;
    if (label != JS_ATOM_NULL) {
      if (next_token(s))
        goto fail;
    }
    if (js_parse_expect_semi(s))
      goto fail;
  } break;
  case TOK_SWITCH: {
    int label_case, label_break, label1;
    int default_label_pos;
    BlockEnv break_entry;

    if (next_token(s))
      goto fail;

    set_eval_ret_undefined(s);
    if (js_parse_expr_paren(s))
      goto fail;

    push_scope(s);
    label_break = new_label(s);
    push_break_entry(s->cur_func, &break_entry, label_name, label_break, -1, 1);

    if (js_parse_expect(s, '{'))
      goto fail;

    default_label_pos = -1;
    label_case = -1;
    while (s->token.val != '}') {
      if (s->token.val == TOK_CASE) {
        label1 = -1;
        if (label_case >= 0) {
          /* skip the case if needed */
          label1 = emit_goto(s, OP_goto, -1);
        }
        emit_label(s, label_case);
        label_case = -1;
        for (;;) {
          /* parse a sequence of case clauses */
          if (next_token(s))
            goto fail;
          emit_op(s, OP_dup);
          if (js_parse_expr(s))
            goto fail;
          if (js_parse_expect(s, ':'))
            goto fail;
          emit_op(s, OP_strict_eq);
          if (s->token.val == TOK_CASE) {
            label1 = emit_goto(s, OP_if_true, label1);
          } else {
            label_case = emit_goto(s, OP_if_false, -1);
            emit_label(s, label1);
            break;
          }
        }
      } else if (s->token.val == TOK_DEFAULT) {
        if (next_token(s))
          goto fail;
        if (js_parse_expect(s, ':'))
          goto fail;
        if (default_label_pos >= 0) {
          js_parse_error(s, "duplicate default");
          goto fail;
        }
        if (label_case < 0) {
          /* falling thru direct from switch expression */
          label_case = emit_goto(s, OP_goto, -1);
        }
        /* Emit a dummy label opcode. Label will be patched after
           the end of the switch body. Do not use emit_label(s, 0)
           because it would clobber label 0 address, preventing
           proper optimizer operation.
         */
        emit_op(s, OP_label);
        emit_u32(s, 0);
        default_label_pos = s->cur_func->byte_code.size - 4;
      } else {
        if (label_case < 0) {
          /* falling thru direct from switch expression */
          js_parse_error(s, "invalid switch statement");
          goto fail;
        }
        if (js_parse_statement_or_decl(s, DECL_MASK_ALL))
          goto fail;
      }
    }
    if (js_parse_expect(s, '}'))
      goto fail;
    if (default_label_pos >= 0) {
      /* Ugly patch for the the `default` label, shameful and risky */
      put_u32(s->cur_func->byte_code.buf + default_label_pos, label_case);
      s->cur_func->label_slots[label_case].pos = default_label_pos + 4;
    } else {
      emit_label(s, label_case);
    }
    emit_label(s, label_break);
    emit_op(s, OP_drop); /* drop the switch expression */

    pop_break_entry(s->cur_func);
    pop_scope(s);
  } break;
  case TOK_TRY: {
    int label_catch, label_catch2, label_finally, label_end;
    JSAtom name;
    BlockEnv block_env;

    set_eval_ret_undefined(s);
    if (next_token(s))
      goto fail;
    label_catch = new_label(s);
    label_catch2 = new_label(s);
    label_finally = new_label(s);
    label_end = new_label(s);

    emit_goto(s, OP_catch, label_catch);

    push_break_entry(s->cur_func, &block_env, JS_ATOM_NULL, -1, -1, 1);
    block_env.label_finally = label_finally;

    if (js_parse_block(s))
      goto fail;

    pop_break_entry(s->cur_func);

    if (js_is_live_code(s)) {
      /* drop the catch offset */
      emit_op(s, OP_drop);
      /* must push dummy value to keep same stack size */
      emit_op(s, OP_undefined);
      emit_goto(s, OP_gosub, label_finally);
      emit_op(s, OP_drop);

      emit_goto(s, OP_goto, label_end);
    }

    if (s->token.val == TOK_CATCH) {
      if (next_token(s))
        goto fail;

      push_scope(s); /* catch variable */
      emit_label(s, label_catch);

      if (s->token.val == '{') {
        /* support optional-catch-binding feature */
        emit_op(s, OP_drop); /* pop the exception object */
      } else {
        if (js_parse_expect(s, '('))
          goto fail;
        if (!(s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved)) {
          if (s->token.val == '[' || s->token.val == '{') {
            /* XXX: TOK_LET is not completely correct */
            if (js_parse_destructuring_element(s, TOK_LET, 0, TRUE, -1, TRUE) <
                0)
              goto fail;
          } else {
            js_parse_error(s, "identifier expected");
            goto fail;
          }
        } else {
          name = JS_DupAtom(ctx, s->token.u.ident.atom);
          if (next_token(s) || js_define_var(s, name, TOK_CATCH) < 0) {
            JS_FreeAtom(ctx, name);
            goto fail;
          }
          /* store the exception value in the catch variable */
          emit_op(s, OP_scope_put_var);
          emit_u32(s, name);
          emit_u16(s, s->cur_func->scope_level);
        }
        if (js_parse_expect(s, ')'))
          goto fail;
      }
      /* XXX: should keep the address to nop it out if there is no finally block
       */
      emit_goto(s, OP_catch, label_catch2);

      push_scope(s); /* catch block */
      push_break_entry(s->cur_func, &block_env, JS_ATOM_NULL, -1, -1, 1);
      block_env.label_finally = label_finally;

      if (js_parse_block(s))
        goto fail;

      pop_break_entry(s->cur_func);
      pop_scope(s); /* catch block */
      pop_scope(s); /* catch variable */

      if (js_is_live_code(s)) {
        /* drop the catch2 offset */
        emit_op(s, OP_drop);
        /* XXX: should keep the address to nop it out if there is no finally
         * block */
        /* must push dummy value to keep same stack size */
        emit_op(s, OP_undefined);
        emit_goto(s, OP_gosub, label_finally);
        emit_op(s, OP_drop);
        emit_goto(s, OP_goto, label_end);
      }
      /* catch exceptions thrown in the catch block to execute the
       * finally clause and rethrow the exception */
      emit_label(s, label_catch2);
      /* catch value is at TOS, no need to push undefined */
      emit_goto(s, OP_gosub, label_finally);
      emit_op(s, OP_throw);

    } else if (s->token.val == TOK_FINALLY) {
      /* finally without catch : execute the finally clause
       * and rethrow the exception */
      emit_label(s, label_catch);
      /* catch value is at TOS, no need to push undefined */
      emit_goto(s, OP_gosub, label_finally);
      emit_op(s, OP_throw);
    } else {
      js_parse_error(s, "expecting catch or finally");
      goto fail;
    }
    emit_label(s, label_finally);
    if (s->token.val == TOK_FINALLY) {
      int saved_eval_ret_idx = 0; /* avoid warning */

      if (next_token(s))
        goto fail;
      /* on the stack: ret_value gosub_ret_value */
      push_break_entry(s->cur_func, &block_env, JS_ATOM_NULL, -1, -1, 2);

      if (s->cur_func->eval_ret_idx >= 0) {
        /* 'finally' updates eval_ret only if not a normal
           termination */
        saved_eval_ret_idx = add_var(s->ctx, s->cur_func, JS_ATOM__ret_);
        if (saved_eval_ret_idx < 0)
          goto fail;
        emit_op(s, OP_get_loc);
        emit_u16(s, s->cur_func->eval_ret_idx);
        emit_op(s, OP_put_loc);
        emit_u16(s, saved_eval_ret_idx);
        set_eval_ret_undefined(s);
      }

      if (js_parse_block(s))
        goto fail;

      if (s->cur_func->eval_ret_idx >= 0) {
        emit_op(s, OP_get_loc);
        emit_u16(s, saved_eval_ret_idx);
        emit_op(s, OP_put_loc);
        emit_u16(s, s->cur_func->eval_ret_idx);
      }
      pop_break_entry(s->cur_func);
    }
    emit_op(s, OP_ret);
    emit_label(s, label_end);
  } break;
  case ';':
    /* empty statement */
    if (next_token(s))
      goto fail;
    break;
  case TOK_WITH:
    if (s->cur_func->js_mode & JS_MODE_STRICT) {
      js_parse_error(s, "invalid keyword: with");
      goto fail;
    } else {
      int with_idx;

      if (next_token(s))
        goto fail;

      if (js_parse_expr_paren(s))
        goto fail;

      push_scope(s);
      with_idx = define_var(s, s->cur_func, JS_ATOM__with_, JS_VAR_DEF_WITH);
      if (with_idx < 0)
        goto fail;
      emit_op(s, OP_to_object);
      emit_op(s, OP_put_loc);
      emit_u16(s, with_idx);

      set_eval_ret_undefined(s);
      if (js_parse_statement(s))
        goto fail;

      /* Popping scope drops lexical context for the with object variable */
      pop_scope(s);
    }
    break;
  case TOK_FUNCTION:
    /* ES6 Annex B.3.2 and B.3.3 semantics */
    if (!(decl_mask & DECL_MASK_FUNC))
      goto func_decl_error;
    if (!(decl_mask & DECL_MASK_OTHER) && peek_token(s, FALSE) == '*')
      goto func_decl_error;
    goto parse_func_var;
  case TOK_IDENT:
    if (s->token.u.ident.is_reserved) {
      js_parse_error_reserved_identifier(s);
      goto fail;
    }
    /* Determine if `let` introduces a Declaration or an ExpressionStatement */
    switch (is_let(s, decl_mask)) {
    case TRUE:
      tok = TOK_LET;
      goto haslet;
    case FALSE:
      break;
    default:
      goto fail;
    }
    if (token_is_pseudo_keyword(s, JS_ATOM_async) &&
        peek_token(s, TRUE) == TOK_FUNCTION) {
      if (!(decl_mask & DECL_MASK_OTHER)) {
      func_decl_error:
        js_parse_error(
            s,
            "function declarations can't appear in single-statement context");
        goto fail;
      }
    parse_func_var:
      if (js_parse_function_decl(s, JS_PARSE_FUNC_VAR, JS_FUNC_NORMAL,
                                 JS_ATOM_NULL, s->token.ptr, s->token.line_num))
        goto fail;
      break;
    }
    goto hasexpr;

  case TOK_CLASS:
    if (!(decl_mask & DECL_MASK_OTHER)) {
      js_parse_error(
          s, "class declarations can't appear in single-statement context");
      goto fail;
    }
    if (js_parse_class(s, FALSE, JS_PARSE_EXPORT_NONE))
      return -1;
    break;

  case TOK_DEBUGGER:
    /* currently no debugger, so just skip the keyword */
    if (next_token(s))
      goto fail;
    if (js_parse_expect_semi(s))
      goto fail;
    break;

  case TOK_ENUM:
  case TOK_EXPORT:
  case TOK_EXTENDS:
    js_unsupported_keyword(s, s->token.u.ident.atom);
    goto fail;

  default:
  hasexpr:
    if (js_parse_expr(s))
      goto fail;
    if (s->cur_func->eval_ret_idx >= 0) {
      /* store the expression value so that it can be returned
         by eval() */
      emit_op(s, OP_put_loc);
      emit_u16(s, s->cur_func->eval_ret_idx);
    } else {
      emit_op(s, OP_drop); /* drop the result */
    }
    if (js_parse_expect_semi(s))
      goto fail;
    break;
  }
done:
  JS_FreeAtom(ctx, label_name);
  return 0;
fail:
  JS_FreeAtom(ctx, label_name);
  return -1;
}
