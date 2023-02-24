
#include "parse.h"

#include "vm/error.h"
#include "vm/num.h"

/* allowed parse_flags: PF_IN_ACCEPTED */
__exception int js_parse_expr2(JSParseState *s, int parse_flags) {
  BOOL comma = FALSE;
  for (;;) {
    if (js_parse_assign_expr2(s, parse_flags))
      return -1;
    if (comma) {
      /* prevent get_lvalue from using the last expression
         as an lvalue. This also prevents the conversion of
         of get_var to get_ref for method lookup in function
         call inside `with` statement.
       */
      s->cur_func->last_opcode_pos = -1;
    }
    if (s->token.val != ',')
      break;
    comma = TRUE;
    if (next_token(s))
      return -1;
    emit_op(s, OP_drop);
  }
  return 0;
}

__exception int js_parse_expr(JSParseState *s) {
  return js_parse_expr2(s, PF_IN_ACCEPTED);
}

/* allowed parse_flags: PF_IN_ACCEPTED */
__exception int js_parse_assign_expr2(JSParseState *s, int parse_flags) {
  int opcode, op, scope;
  JSAtom name0 = JS_ATOM_NULL;
  JSAtom name;
  uint64_t loc = LOC(s->token.line_num, s->token.col_num);

  if (s->token.val == TOK_YIELD) {
    BOOL is_star = FALSE, is_async;

    if (!(s->cur_func->func_kind & JS_FUNC_GENERATOR))
      return js_parse_error(s, "unexpected 'yield' keyword");
    if (!s->cur_func->in_function_body)
      return js_parse_error(s, "yield in default expression");
    if (next_token(s))
      return -1;
    /* XXX: is there a better method to detect 'yield' without
       parameters ? */
    if (s->token.val != ';' && s->token.val != ')' && s->token.val != ']' &&
        s->token.val != '}' && s->token.val != ',' && s->token.val != ':' &&
        !s->got_lf) {
      if (s->token.val == '*') {
        is_star = TRUE;
        if (next_token(s))
          return -1;
      }
      if (js_parse_assign_expr2(s, parse_flags))
        return -1;
    } else {
      emit_op(s, OP_undefined);
    }
    is_async = (s->cur_func->func_kind == JS_FUNC_ASYNC_GENERATOR);

    if (is_star) {
      int label_loop, label_return, label_next;
      int label_return1, label_yield, label_throw, label_throw1;
      int label_throw2;

      label_loop = new_label(s);
      label_yield = new_label(s);

      emit_op(s, is_async ? OP_for_await_of_start : OP_for_of_start);

      /* remove the catch offset (XXX: could avoid pushing back
         undefined) */
      emit_op(s, OP_drop);
      emit_op(s, OP_undefined);

      emit_op(s, OP_undefined); /* initial value */

      emit_label(s, label_loop);
      emit_op(s, OP_iterator_next);
      if (is_async)
        emit_op(s, OP_await);
      emit_op(s, OP_iterator_check_object);
      emit_op(s, OP_get_field2);
      emit_atom(s, JS_ATOM_done);
      label_next = emit_goto(s, OP_if_true, -1); /* end of loop */
      emit_label(s, label_yield);
      if (is_async) {
        /* OP_async_yield_star takes the value as parameter */
        emit_op(s, OP_get_field);
        emit_atom(s, JS_ATOM_value);
        emit_op(s, OP_await);
        emit_op(s, OP_async_yield_star);
      } else {
        /* OP_yield_star takes (value, done) as parameter */
        emit_op(s, OP_yield_star);
      }
      emit_op(s, OP_dup);
      label_return = emit_goto(s, OP_if_true, -1);
      emit_op(s, OP_drop);
      emit_goto(s, OP_goto, label_loop);

      emit_label(s, label_return);
      emit_op(s, OP_push_i32);
      emit_u32(s, 2);
      emit_op(s, OP_strict_eq);
      label_throw = emit_goto(s, OP_if_true, -1);

      /* return handling */
      if (is_async)
        emit_op(s, OP_await);
      emit_op(s, OP_iterator_call);
      emit_u8(s, 0);
      label_return1 = emit_goto(s, OP_if_true, -1);
      if (is_async)
        emit_op(s, OP_await);
      emit_op(s, OP_iterator_check_object);
      emit_op(s, OP_get_field2);
      emit_atom(s, JS_ATOM_done);
      emit_goto(s, OP_if_false, label_yield);

      emit_op(s, OP_get_field);
      emit_atom(s, JS_ATOM_value);

      emit_label(s, label_return1);
      emit_op(s, OP_nip);
      emit_op(s, OP_nip);
      emit_op(s, OP_nip);
      emit_return(s, TRUE);

      /* throw handling */
      emit_label(s, label_throw);
      emit_op(s, OP_iterator_call);
      emit_u8(s, 1);
      label_throw1 = emit_goto(s, OP_if_true, -1);
      if (is_async)
        emit_op(s, OP_await);
      emit_op(s, OP_iterator_check_object);
      emit_op(s, OP_get_field2);
      emit_atom(s, JS_ATOM_done);
      emit_goto(s, OP_if_false, label_yield);
      emit_goto(s, OP_goto, label_next);
      /* close the iterator and throw a type error exception */
      emit_label(s, label_throw1);
      emit_op(s, OP_iterator_call);
      emit_u8(s, 2);
      label_throw2 = emit_goto(s, OP_if_true, -1);
      if (is_async)
        emit_op(s, OP_await);
      emit_label(s, label_throw2);

      emit_op(s, OP_throw_error);
      emit_atom(s, JS_ATOM_NULL);
      emit_u8(s, JS_THROW_ERROR_ITERATOR_THROW);

      emit_label(s, label_next);
      emit_op(s, OP_get_field);
      emit_atom(s, JS_ATOM_value);
      emit_op(s, OP_nip); /* keep the value associated with
                             done = true */
      emit_op(s, OP_nip);
      emit_op(s, OP_nip);
    } else {
      int label_next;

      if (is_async)
        emit_op(s, OP_await);
      emit_op(s, OP_yield);
      label_next = emit_goto(s, OP_if_false, -1);
      emit_return(s, TRUE);
      emit_label(s, label_next);
    }
    return 0;
  }
  if (s->token.val == TOK_IDENT) {
    /* name0 is used to check for OP_set_name pattern, not duplicated */
    name0 = s->token.u.ident.atom;
  }
  if (js_parse_cond_expr(s, parse_flags | PF_ARROW_FUNC))
    return -1;

  op = s->token.val;
  if (op == '=' || (op >= TOK_MUL_ASSIGN && op <= TOK_POW_ASSIGN)) {
    int label;
    if (next_token(s))
      return -1;

    // col_num of token after `=`
    loc = LOC(s->token.line_num, s->token.col_num);
    if (get_lvalue(s, &opcode, &scope, &name, &label, NULL, (op != '='), op) <
        0)
      return -1;

    if (js_parse_assign_expr2(s, parse_flags)) {
      JS_FreeAtom(s->ctx, name);
      return -1;
    }

    if (op == '=') {
      if (opcode == OP_get_ref_value && name == name0) {
        set_object_name(s, name);
      }
    } else {
      static const uint8_t assign_opcodes[] = {
          OP_mul, OP_div, OP_mod, OP_add, OP_sub, OP_shl,
          OP_sar, OP_shr, OP_and, OP_xor, OP_or,
#ifdef CONFIG_BIGNUM
          OP_pow,
#endif
          OP_pow,
      };
      op = assign_opcodes[op - TOK_MUL_ASSIGN];
#ifdef CONFIG_BIGNUM
      if (s->cur_func->js_mode & JS_MODE_MATH) {
        if (op == OP_mod)
          op = OP_math_mod;
      }
#endif
      emit_op(s, op);
    }
    s->loc = loc;
    put_lvalue(s, opcode, scope, name, label, PUT_LVALUE_KEEP_TOP, FALSE);
  } else if (op >= TOK_LAND_ASSIGN && op <= TOK_DOUBLE_QUESTION_MARK_ASSIGN) {
    int label, label1, depth_lvalue, label2;

    if (next_token(s))
      return -1;
    if (get_lvalue(s, &opcode, &scope, &name, &label, &depth_lvalue, TRUE, op) <
        0)
      return -1;

    emit_op(s, OP_dup);
    if (op == TOK_DOUBLE_QUESTION_MARK_ASSIGN)
      emit_op(s, OP_is_undefined_or_null);
    label1 = emit_goto(s, op == TOK_LOR_ASSIGN ? OP_if_true : OP_if_false, -1);
    emit_op(s, OP_drop);

    if (js_parse_assign_expr2(s, parse_flags)) {
      JS_FreeAtom(s->ctx, name);
      return -1;
    }

    if (opcode == OP_get_ref_value && name == name0) {
      set_object_name(s, name);
    }

    switch (depth_lvalue) {
    case 1:
      emit_op(s, OP_insert2);
      break;
    case 2:
      emit_op(s, OP_insert3);
      break;
    case 3:
      emit_op(s, OP_insert4);
      break;
    default:
      abort();
    }

    /* XXX: we disable the OP_put_ref_value optimization by not
       using put_lvalue() otherwise depth_lvalue is not correct */
    put_lvalue(s, opcode, scope, name, label, PUT_LVALUE_NOKEEP_DEPTH, FALSE);
    label2 = emit_goto(s, OP_goto, -1);

    emit_label(s, label1);

    /* remove the lvalue stack entries */
    while (depth_lvalue != 0) {
      emit_op(s, OP_nip);
      depth_lvalue--;
    }

    emit_label(s, label2);
  }
  return 0;
}

__exception int js_parse_assign_expr(JSParseState *s) {
  return js_parse_assign_expr2(s, PF_IN_ACCEPTED);
}

/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
__exception int js_parse_cond_expr(JSParseState *s, int parse_flags) {
  int label1, label2;

  if (js_parse_coalesce_expr(s, parse_flags))
    return -1;
  if (s->token.val == '?') {
    if (next_token(s))
      return -1;
    label1 = emit_goto(s, OP_if_false, -1);

    if (js_parse_assign_expr(s))
      return -1;
    if (js_parse_expect(s, ':'))
      return -1;

    label2 = emit_goto(s, OP_goto, -1);

    emit_label(s, label1);

    if (js_parse_assign_expr2(s, parse_flags & PF_IN_ACCEPTED))
      return -1;

    emit_label(s, label2);
  }
  return 0;
}

__exception int js_parse_coalesce_expr(JSParseState *s, int parse_flags) {
  int label1;

  if (js_parse_logical_and_or(s, TOK_LOR, parse_flags))
    return -1;
  if (s->token.val == TOK_DOUBLE_QUESTION_MARK) {
    label1 = new_label(s);
    for (;;) {
      if (next_token(s))
        return -1;

      emit_op(s, OP_dup);
      emit_op(s, OP_is_undefined_or_null);
      emit_goto(s, OP_if_false, label1);
      emit_op(s, OP_drop);

      if (js_parse_expr_binary(s, 8, parse_flags & ~PF_ARROW_FUNC))
        return -1;
      if (s->token.val != TOK_DOUBLE_QUESTION_MARK)
        break;
    }
    emit_label(s, label1);
  }
  return 0;
}

/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
__exception int js_parse_logical_and_or(JSParseState *s, int op,
                                        int parse_flags) {
  int label1;

  if (op == TOK_LAND) {
    if (js_parse_expr_binary(s, 8, parse_flags))
      return -1;
  } else {
    if (js_parse_logical_and_or(s, TOK_LAND, parse_flags))
      return -1;
  }
  if (s->token.val == op) {
    label1 = new_label(s);

    for (;;) {
      if (next_token(s))
        return -1;
      emit_op(s, OP_dup);
      emit_goto(s, op == TOK_LAND ? OP_if_false : OP_if_true, label1);
      emit_op(s, OP_drop);

      if (op == TOK_LAND) {
        if (js_parse_expr_binary(s, 8, parse_flags & ~PF_ARROW_FUNC))
          return -1;
      } else {
        if (js_parse_logical_and_or(s, TOK_LAND, parse_flags & ~PF_ARROW_FUNC))
          return -1;
      }
      if (s->token.val != op) {
        if (s->token.val == TOK_DOUBLE_QUESTION_MARK)
          return js_parse_error(s, "cannot mix ?? with && or ||");
        break;
      }
    }

    emit_label(s, label1);
  }
  return 0;
}

/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
__exception int js_parse_expr_binary(JSParseState *s, int level,
                                     int parse_flags) {
  int op, opcode;

  if (level == 0) {
    return js_parse_unary(s, (parse_flags & PF_ARROW_FUNC) | PF_POW_ALLOWED);
  }
  if (js_parse_expr_binary(s, level - 1, parse_flags))
    return -1;
  for (;;) {
    op = s->token.val;
    switch (level) {
    case 1:
      switch (op) {
      case '*':
        opcode = OP_mul;
        break;
      case '/':
        opcode = OP_div;
        break;
      case '%':
#ifdef CONFIG_BIGNUM
        if (s->cur_func->js_mode & JS_MODE_MATH)
          opcode = OP_math_mod;
        else
#endif
          opcode = OP_mod;
        break;
      default:
        return 0;
      }
      break;
    case 2:
      switch (op) {
      case '+':
        opcode = OP_add;
        break;
      case '-':
        opcode = OP_sub;
        break;
      default:
        return 0;
      }
      break;
    case 3:
      switch (op) {
      case TOK_SHL:
        opcode = OP_shl;
        break;
      case TOK_SAR:
        opcode = OP_sar;
        break;
      case TOK_SHR:
        opcode = OP_shr;
        break;
      default:
        return 0;
      }
      break;
    case 4:
      switch (op) {
      case '<':
        opcode = OP_lt;
        break;
      case '>':
        opcode = OP_gt;
        break;
      case TOK_LTE:
        opcode = OP_lte;
        break;
      case TOK_GTE:
        opcode = OP_gte;
        break;
      case TOK_INSTANCEOF:
        opcode = OP_instanceof;
        break;
      case TOK_IN:
        if (parse_flags & PF_IN_ACCEPTED) {
          opcode = OP_in;
        } else {
          return 0;
        }
        break;
      default:
        return 0;
      }
      break;
    case 5:
      switch (op) {
      case TOK_EQ:
        opcode = OP_eq;
        break;
      case TOK_NEQ:
        opcode = OP_neq;
        break;
      case TOK_STRICT_EQ:
        opcode = OP_strict_eq;
        break;
      case TOK_STRICT_NEQ:
        opcode = OP_strict_neq;
        break;
      default:
        return 0;
      }
      break;
    case 6:
      switch (op) {
      case '&':
        opcode = OP_and;
        break;
      default:
        return 0;
      }
      break;
    case 7:
      switch (op) {
      case '^':
        opcode = OP_xor;
        break;
      default:
        return 0;
      }
      break;
    case 8:
      switch (op) {
      case '|':
        opcode = OP_or;
        break;
      default:
        return 0;
      }
      break;
    default:
      abort();
    }
    if (next_token(s))
      return -1;
    if (js_parse_expr_binary(s, level - 1, parse_flags & ~PF_ARROW_FUNC))
      return -1;
    emit_op(s, opcode);
  }
  return 0;
}

typedef enum FuncCallType {
  FUNC_CALL_NORMAL,
  FUNC_CALL_NEW,
  FUNC_CALL_SUPER_CTOR,
  FUNC_CALL_TEMPLATE,
} FuncCallType;

static void optional_chain_test(JSParseState *s, int *poptional_chaining_label,
                                int drop_count) {
  int label_next, i;
  if (*poptional_chaining_label < 0)
    *poptional_chaining_label = new_label(s);
  /* XXX: could be more efficient with a specific opcode */
  emit_op(s, OP_dup);
  emit_op(s, OP_is_undefined_or_null);
  label_next = emit_goto(s, OP_if_false, -1);
  for (i = 0; i < drop_count; i++)
    emit_op(s, OP_drop);
  emit_op(s, OP_undefined);
  emit_goto(s, OP_goto, *poptional_chaining_label);
  emit_label(s, label_next);
}

/* allowed parse_flags: PF_POSTFIX_CALL, PF_ARROW_FUNC */
__exception int js_parse_postfix_expr(JSParseState *s, int parse_flags) {
  FuncCallType call_type;
  int optional_chaining_label;
  BOOL accept_lparen = (parse_flags & PF_POSTFIX_CALL) != 0;
  uint64_t loc = LOC(s->token.line_num, s->token.col_num);

  call_type = FUNC_CALL_NORMAL;
  switch (s->token.val) {
  case TOK_NUMBER: {
    JSValue val;
    val = s->token.u.num.val;

    if (JS_VALUE_GET_TAG(val) == JS_TAG_INT) {
      emit_op(s, OP_push_i32);
      emit_u32(s, JS_VALUE_GET_INT(val));
    } else
#ifdef CONFIG_BIGNUM
        if (JS_VALUE_GET_TAG(val) == JS_TAG_BIG_FLOAT) {
      slimb_t e;
      int ret;

      /* need a runtime conversion */
      /* XXX: could add a cache and/or do it once at
         the start of the function */
      if (emit_push_const(s, val, 0) < 0)
        return -1;
      e = s->token.u.num.exponent;
      if (e == (int32_t)e) {
        emit_op(s, OP_push_i32);
        emit_u32(s, e);
      } else {
        val = JS_NewBigInt64_1(s->ctx, e);
        if (JS_IsException(val))
          return -1;
        ret = emit_push_const(s, val, 0);
        JS_FreeValue(s->ctx, val);
        if (ret < 0)
          return -1;
      }
      emit_op(s, OP_mul_pow10);
    } else
#endif
    {
      if (emit_push_const(s, val, 0) < 0)
        return -1;
    }
  }
    if (next_token(s))
      return -1;
    break;
  case TOK_TEMPLATE:
    if (js_parse_template(s, 0, NULL))
      return -1;
    break;
  case TOK_STRING:
    if (emit_push_const(s, s->token.u.str.str, 1))
      return -1;
    if (next_token(s))
      return -1;
    break;

  case TOK_DIV_ASSIGN:
    s->buf_ptr -= 2;
    goto parse_regexp;
  case '/':
    s->buf_ptr--;
  parse_regexp : {
    JSValue str;
    int ret, backtrace_flags;
    if (!s->ctx->compile_regexp)
      return js_parse_error(s, "RegExp are not supported");
    /* the previous token is '/' or '/=', so no need to free */
    if (js_parse_regexp(s))
      return -1;
    ret = emit_push_const(s, s->token.u.regexp.body, 0);
    str = s->ctx->compile_regexp(s->ctx, s->token.u.regexp.body,
                                 s->token.u.regexp.flags);
    if (JS_IsException(str)) {
      /* add the line number info */
      backtrace_flags = 0;
      if (s->cur_func && s->cur_func->backtrace_barrier)
        backtrace_flags = JS_BACKTRACE_FLAG_SINGLE_LEVEL;
      build_backtrace(s->ctx, s->ctx->rt->current_exception, s->filename,
                      s->token.line_num, backtrace_flags);
      return -1;
    }
    ret = emit_push_const(s, str, 0);
    JS_FreeValue(s->ctx, str);
    if (ret)
      return -1;
    /* we use a specific opcode to be sure the correct
       function is called (otherwise the bytecode would have
       to be verified by the RegExp constructor) */
    emit_op(s, OP_regexp);
    if (next_token(s))
      return -1;
  } break;
  case '(':
    if ((parse_flags & PF_ARROW_FUNC) &&
        js_parse_skip_parens_token(s, NULL, TRUE) == TOK_ARROW) {
      if (js_parse_function_decl(s, JS_PARSE_FUNC_ARROW, JS_FUNC_NORMAL,
                                 JS_ATOM_NULL, s->token.ptr, s->token.line_num))
        return -1;
    } else {
      if (js_parse_expr_paren(s))
        return -1;
    }
    break;
  case TOK_FUNCTION:
    if (js_parse_function_decl(s, JS_PARSE_FUNC_EXPR, JS_FUNC_NORMAL,
                               JS_ATOM_NULL, s->token.ptr, s->token.line_num))
      return -1;
    break;
  case TOK_CLASS:
    if (js_parse_class(s, TRUE, JS_PARSE_EXPORT_NONE))
      return -1;
    break;
  case TOK_NULL:
    if (next_token(s))
      return -1;
    emit_op(s, OP_null);
    break;
  case TOK_THIS:
    if (next_token(s))
      return -1;
    emit_op(s, OP_scope_get_var);
    emit_atom(s, JS_ATOM_this);
    emit_u16(s, 0);
    break;
  case TOK_FALSE:
    if (next_token(s))
      return -1;
    emit_op(s, OP_push_false);
    break;
  case TOK_TRUE:
    if (next_token(s))
      return -1;
    emit_op(s, OP_push_true);
    break;
  case TOK_IDENT: {
    JSAtom name;
    if (s->token.u.ident.is_reserved) {
      return js_parse_error_reserved_identifier(s);
    }
    if ((parse_flags & PF_ARROW_FUNC) && peek_token(s, TRUE) == TOK_ARROW) {
      if (js_parse_function_decl(s, JS_PARSE_FUNC_ARROW, JS_FUNC_NORMAL,
                                 JS_ATOM_NULL, s->token.ptr, s->token.line_num))
        return -1;
    } else if (token_is_pseudo_keyword(s, JS_ATOM_async) &&
               peek_token(s, TRUE) != '\n') {
      const uint8_t *source_ptr;
      int source_line_num;

      source_ptr = s->token.ptr;
      source_line_num = s->token.line_num;
      if (next_token(s))
        return -1;
      if (s->token.val == TOK_FUNCTION) {
        if (js_parse_function_decl(s, JS_PARSE_FUNC_EXPR, JS_FUNC_ASYNC,
                                   JS_ATOM_NULL, source_ptr, source_line_num))
          return -1;
      } else if ((parse_flags & PF_ARROW_FUNC) &&
                 ((s->token.val == '(' &&
                   js_parse_skip_parens_token(s, NULL, TRUE) == TOK_ARROW) ||
                  (s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved &&
                   peek_token(s, TRUE) == TOK_ARROW))) {
        if (js_parse_function_decl(s, JS_PARSE_FUNC_ARROW, JS_FUNC_ASYNC,
                                   JS_ATOM_NULL, source_ptr, source_line_num))
          return -1;
      } else {
        name = JS_DupAtom(s->ctx, JS_ATOM_async);
        goto do_get_var;
      }
    } else {
      if (s->token.u.ident.atom == JS_ATOM_arguments &&
          !s->cur_func->arguments_allowed) {
        js_parse_error(
            s,
            "'arguments' identifier is not allowed in class field initializer");
        return -1;
      }
      name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
      if (next_token(s)) /* update line number before emitting code */
        return -1;
    do_get_var:
      emit_op(s, OP_scope_get_var);
      emit_u32(s, name);
      emit_u16(s, s->cur_func->scope_level);
    }
  } break;
  case '{':
  case '[': {
    int skip_bits;
    if (js_parse_skip_parens_token(s, &skip_bits, FALSE) == '=') {
      if (js_parse_destructuring_element(
              s, 0, 0, FALSE, skip_bits & SKIP_HAS_ELLIPSIS, TRUE) < 0)
        return -1;
    } else {
      if (s->token.val == '{') {
        if (js_parse_object_literal(s))
          return -1;
      } else {
        if (js_parse_array_literal(s))
          return -1;
      }
    }
  } break;
  case TOK_NEW:
    if (next_token(s))
      return -1;
    if (s->token.val == '.') {
      if (next_token(s))
        return -1;
      if (!token_is_pseudo_keyword(s, JS_ATOM_target))
        return js_parse_error(s, "expecting target");
      if (!s->cur_func->new_target_allowed)
        return js_parse_error(s, "new.target only allowed within functions");
      if (next_token(s))
        return -1;
      emit_op(s, OP_scope_get_var);
      emit_atom(s, JS_ATOM_new_target);
      emit_u16(s, 0);
    } else {
      if (js_parse_postfix_expr(s, 0))
        return -1;
      accept_lparen = TRUE;
      if (s->token.val != '(') {
        /* new operator on an object */
        emit_op(s, OP_dup);
        emit_op(s, OP_call_constructor);
        emit_u16(s, 0);
      } else {
        call_type = FUNC_CALL_NEW;
      }
    }
    break;
  case TOK_SUPER:
    if (next_token(s))
      return -1;
    if (s->token.val == '(') {
      if (!s->cur_func->super_call_allowed)
        return js_parse_error(
            s, "super() is only valid in a derived class constructor");
      call_type = FUNC_CALL_SUPER_CTOR;
    } else if (s->token.val == '.' || s->token.val == '[') {
      if (!s->cur_func->super_allowed)
        return js_parse_error(s, "'super' is only valid in a method");
      emit_op(s, OP_scope_get_var);
      emit_atom(s, JS_ATOM_this);
      emit_u16(s, 0);
      emit_op(s, OP_scope_get_var);
      emit_atom(s, JS_ATOM_home_object);
      emit_u16(s, 0);
      emit_op(s, OP_get_super);
    } else {
      return js_parse_error(s, "invalid use of 'super'");
    }
    break;
  case TOK_IMPORT:
    if (next_token(s))
      return -1;
    if (s->token.val == '.') {
      if (next_token(s))
        return -1;
      if (!token_is_pseudo_keyword(s, JS_ATOM_meta))
        return js_parse_error(s, "meta expected");
      if (!s->is_module)
        return js_parse_error(s, "import.meta only valid in module code");
      if (next_token(s))
        return -1;
      emit_op(s, OP_special_object);
      emit_u8(s, OP_SPECIAL_OBJECT_IMPORT_META);
    } else {
      if (js_parse_expect(s, '('))
        return -1;
      if (!accept_lparen)
        return js_parse_error(s, "invalid use of 'import()'");
      if (js_parse_assign_expr(s))
        return -1;
      if (js_parse_expect(s, ')'))
        return -1;
      emit_op(s, OP_import);
    }
    break;
  default:
    return js_parse_error(s, "unexpected token in expression: '%.*s'",
                          (int)(s->buf_ptr - s->token.ptr), s->token.ptr);
  }

  optional_chaining_label = -1;
  for (;;) {
    JSFunctionDef *fd = s->cur_func;
    BOOL has_optional_chain = FALSE;

    if (s->token.val == TOK_QUESTION_MARK_DOT) {
      /* optional chaining */
      if (next_token(s))
        return -1;
      has_optional_chain = TRUE;
      if (s->token.val == '(' && accept_lparen) {
        goto parse_func_call;
      } else if (s->token.val == '[') {
        goto parse_array_access;
      } else {
        goto parse_property;
      }
    } else if (s->token.val == TOK_TEMPLATE && call_type == FUNC_CALL_NORMAL) {
      if (optional_chaining_label >= 0) {
        return js_parse_error(
            s, "template literal cannot appear in an optional chain");
      }
      call_type = FUNC_CALL_TEMPLATE;
      goto parse_func_call2;
    } else if (s->token.val == '(' && accept_lparen) {
      int opcode, arg_count, drop_count;
      loc = LOC(s->token.line_num, s->token.col_num);

      /* function call */
    parse_func_call:
      if (next_token(s)) // consume `(`
        return -1;

      if (call_type == FUNC_CALL_NORMAL) {
      parse_func_call2:
        switch (opcode = get_prev_opcode(fd)) {
        case OP_get_field:
          /* keep the object on the stack */
          fd->byte_code.buf[fd->last_opcode_pos] = OP_get_field2;
          drop_count = 2;
          break;
        case OP_scope_get_private_field:
          /* keep the object on the stack */
          fd->byte_code.buf[fd->last_opcode_pos] = OP_scope_get_private_field2;
          drop_count = 2;
          break;
        case OP_get_array_el:
          /* keep the object on the stack */
          fd->byte_code.buf[fd->last_opcode_pos] = OP_get_array_el2;
          drop_count = 2;
          break;
        case OP_scope_get_var: {
          JSAtom name;
          int scope;
          name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
          scope = get_u16(fd->byte_code.buf + fd->last_opcode_pos + 5);
          if (name == JS_ATOM_eval && call_type == FUNC_CALL_NORMAL &&
              !has_optional_chain) {
            /* direct 'eval' */
            opcode = OP_eval;
          } else {
            /* verify if function name resolves to a simple
               get_loc/get_arg: a function call inside a `with`
               statement can resolve to a method call of the
               `with` context object
             */
            /* XXX: always generate the OP_scope_get_ref
               and remove it in variable resolution
               pass ? */
            if (has_with_scope(fd, scope)) {
              opcode = OP_scope_get_ref;
              fd->byte_code.buf[fd->last_opcode_pos] = opcode;
            }
          }
          drop_count = 1;
        } break;
        case OP_get_super_value:
          fd->byte_code.buf[fd->last_opcode_pos] = OP_get_array_el;
          /* on stack: this func_obj */
          opcode = OP_get_array_el;
          drop_count = 2;
          break;
        default:
          opcode = OP_invalid;
          drop_count = 1;
          break;
        }
        if (has_optional_chain) {
          optional_chain_test(s, &optional_chaining_label, drop_count);
        }
      } else {
        opcode = OP_invalid;
      }

      if (call_type == FUNC_CALL_TEMPLATE) {
        if (js_parse_template(s, 1, &arg_count))
          return -1;
        goto emit_func_call;
      } else if (call_type == FUNC_CALL_SUPER_CTOR) {
        emit_op(s, OP_scope_get_var);
        emit_atom(s, JS_ATOM_this_active_func);
        emit_u16(s, 0);

        emit_op(s, OP_get_super);

        emit_op(s, OP_scope_get_var);
        emit_atom(s, JS_ATOM_new_target);
        emit_u16(s, 0);
      } else if (call_type == FUNC_CALL_NEW) {
        emit_op(s, OP_dup); /* new.target = function */
      }

      /* parse arguments */
      arg_count = 0;
      while (s->token.val != ')') {
        if (arg_count >= 65535) {
          return js_parse_error(s, "Too many call arguments");
        }
        if (s->token.val == TOK_ELLIPSIS)
          break;
        if (js_parse_assign_expr(s))
          return -1;
        arg_count++;
        if (s->token.val == ')')
          break;
        /* accept a trailing comma before the ')' */
        if (js_parse_expect(s, ','))
          return -1;
      }
      if (s->token.val == TOK_ELLIPSIS) {
        emit_op(s, OP_array_from);
        emit_u16(s, arg_count);
        emit_op(s, OP_push_i32);
        emit_u32(s, arg_count);

        /* on stack: array idx */
        while (s->token.val != ')') {
          if (s->token.val == TOK_ELLIPSIS) {
            if (next_token(s))
              return -1;
            if (js_parse_assign_expr(s))
              return -1;
#if 1
            /* XXX: could pass is_last indicator? */
            emit_op(s, OP_append);
#else
            int label_next, label_done;
            label_next = new_label(s);
            label_done = new_label(s);
            /* push enumerate object below array/idx pair */
            emit_op(s, OP_for_of_start);
            emit_op(s, OP_rot5l);
            emit_op(s, OP_rot5l);
            emit_label(s, label_next);
            /* on stack: enum_rec array idx */
            emit_op(s, OP_for_of_next);
            emit_u8(s, 2);
            emit_goto(s, OP_if_true, label_done);
            /* append element */
            /* enum_rec array idx val -> enum_rec array new_idx */
            emit_op(s, OP_define_array_el);
            emit_op(s, OP_inc);
            emit_goto(s, OP_goto, label_next);
            emit_label(s, label_done);
            /* close enumeration, drop enum_rec and idx */
            emit_op(s, OP_drop); /* drop undef */
            emit_op(s, OP_nip1); /* drop enum_rec */
            emit_op(s, OP_nip1);
            emit_op(s, OP_nip1);
#endif
          } else {
            if (js_parse_assign_expr(s))
              return -1;
            /* array idx val */
            emit_op(s, OP_define_array_el);
            emit_op(s, OP_inc);
          }
          if (s->token.val == ')')
            break;
          /* accept a trailing comma before the ')' */
          if (js_parse_expect(s, ','))
            return -1;
        }
        if (next_token(s))
          return -1;
        /* drop the index */
        emit_op(s, OP_drop);

        /* apply function call */
        switch (opcode) {
        case OP_get_field:
        case OP_scope_get_private_field:
        case OP_get_array_el:
        case OP_scope_get_ref:
          /* obj func array -> func obj array */
          emit_op(s, OP_perm3);
          emit_op(s, OP_apply);
          emit_u16(s, call_type == FUNC_CALL_NEW);
          break;
        case OP_eval:
          emit_op(s, OP_apply_eval);
          emit_u16(s, fd->scope_level);
          fd->has_eval_call = TRUE;
          break;
        default:
          if (call_type == FUNC_CALL_SUPER_CTOR) {
            emit_op(s, OP_apply);
            emit_u16(s, 1);
            /* set the 'this' value */
            emit_op(s, OP_dup);
            emit_op(s, OP_scope_put_var_init);
            emit_atom(s, JS_ATOM_this);
            emit_u16(s, 0);

            emit_class_field_init(s);
          } else if (call_type == FUNC_CALL_NEW) {
            /* obj func array -> func obj array */
            emit_op(s, OP_perm3);
            emit_op(s, OP_apply);
            emit_u16(s, 1);
          } else {
            /* func array -> func undef array */
            emit_op(s, OP_undefined);
            emit_op(s, OP_swap);
            emit_op(s, OP_apply);
            emit_u16(s, 0);
          }
          break;
        }
      } else {
        if (next_token(s))
          return -1;
      emit_func_call:
        switch (opcode) {
        case OP_get_field:
        case OP_scope_get_private_field:
        case OP_get_array_el:
        case OP_scope_get_ref:
          s->loc = loc;
          emit_op(s, OP_call_method);
          emit_u16(s, arg_count);
          break;
        case OP_eval:
          emit_op(s, OP_eval);
          emit_u16(s, arg_count);
          emit_u16(s, fd->scope_level);
          fd->has_eval_call = TRUE;
          break;
        default:
          if (call_type == FUNC_CALL_SUPER_CTOR) {
            emit_op(s, OP_call_constructor);
            emit_u16(s, arg_count);

            /* set the 'this' value */
            emit_op(s, OP_dup);
            emit_op(s, OP_scope_put_var_init);
            emit_atom(s, JS_ATOM_this);
            emit_u16(s, 0);

            emit_class_field_init(s);
          } else if (call_type == FUNC_CALL_NEW) {
            s->loc = loc;
            emit_op(s, OP_call_constructor);
            emit_u16(s, arg_count);
          } else {
            s->loc = loc;
            emit_op(s, OP_call);
            emit_u16(s, arg_count);
          }
          break;
        }
      }
      call_type = FUNC_CALL_NORMAL;
    } /* if (s->token.val == '(' && accept_lparen) */
    else if (s->token.val == '.') {
      if (next_token(s))
        return -1;
      // save col_num after `.`
      loc = LOC(s->token.line_num, s->token.col_num);
    parse_property:
      if (s->token.val == TOK_PRIVATE_NAME) {
        /* private class field */
        if (get_prev_opcode(fd) == OP_get_super) {
          return js_parse_error(s, "private class field forbidden after super");
        }
        if (has_optional_chain) {
          optional_chain_test(s, &optional_chaining_label, 1);
        }
        s->loc = loc;
        emit_op(s, OP_scope_get_private_field);
        emit_atom(s, s->token.u.ident.atom);
        emit_u16(s, s->cur_func->scope_level);
      } else {
        if (!token_is_ident(s->token.val)) {
          return js_parse_error(s, "expecting field name");
        }
        if (get_prev_opcode(fd) == OP_get_super) {
          JSValue val;
          int ret;
          val = JS_AtomToValue(s->ctx, s->token.u.ident.atom);
          ret = emit_push_const(s, val, 1);
          JS_FreeValue(s->ctx, val);
          if (ret)
            return -1;
          emit_op(s, OP_get_super_value);
        } else {
          if (has_optional_chain) {
            optional_chain_test(s, &optional_chaining_label, 1);
          }
          s->loc = loc;
          emit_op(s, OP_get_field);
          emit_atom(s, s->token.u.ident.atom);
        }
      }
      if (next_token(s))
        return -1;
    } else if (s->token.val == '[') {
      int prev_op;

    parse_array_access:
      prev_op = get_prev_opcode(fd);
      if (has_optional_chain) {
        optional_chain_test(s, &optional_chaining_label, 1);
      }
      if (next_token(s))
        return -1;
      if (js_parse_expr(s))
        return -1;
      if (js_parse_expect(s, ']'))
        return -1;
      if (prev_op == OP_get_super) {
        emit_op(s, OP_get_super_value);
      } else {
        emit_op(s, OP_get_array_el);
      }
    } else {
      break;
    }
  }
  if (optional_chaining_label >= 0)
    emit_label(s, optional_chaining_label);
  return 0;
}

static __exception int js_parse_delete(JSParseState *s) {
  JSFunctionDef *fd = s->cur_func;
  JSAtom name;
  int opcode;

  if (next_token(s))
    return -1;
  if (js_parse_unary(s, PF_POW_FORBIDDEN))
    return -1;
  switch (opcode = get_prev_opcode(fd)) {
  case OP_get_field: {
    JSValue val;
    int ret;

    name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
    fd->byte_code.size = fd->last_opcode_pos;
    fd->last_opcode_pos = -1;
    val = JS_AtomToValue(s->ctx, name);
    ret = emit_push_const(s, val, 1);
    JS_FreeValue(s->ctx, val);
    JS_FreeAtom(s->ctx, name);
    if (ret)
      return ret;
  }
    goto do_delete;
  case OP_get_array_el:
    fd->byte_code.size = fd->last_opcode_pos;
    fd->last_opcode_pos = -1;
  do_delete:
    emit_op(s, OP_delete);
    break;
  case OP_scope_get_var:
    /* 'delete this': this is not a reference */
    name = get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
    if (name == JS_ATOM_this || name == JS_ATOM_new_target)
      goto ret_true;
    if (fd->js_mode & JS_MODE_STRICT) {
      return js_parse_error(s,
                            "cannot delete a direct reference in strict mode");
    } else {
      fd->byte_code.buf[fd->last_opcode_pos] = OP_scope_delete_var;
    }
    break;
  case OP_scope_get_private_field:
    return js_parse_error(s, "cannot delete a private class field");
  case OP_get_super_value:
    emit_op(s, OP_throw_error);
    emit_atom(s, JS_ATOM_NULL);
    emit_u8(s, JS_THROW_ERROR_DELETE_SUPER);
    break;
  default:
  ret_true:
    emit_op(s, OP_drop);
    emit_op(s, OP_push_true);
    break;
  }
  return 0;
}

/* allowed parse_flags: PF_ARROW_FUNC, PF_POW_ALLOWED, PF_POW_FORBIDDEN */
__exception int js_parse_unary(JSParseState *s, int parse_flags) {
  int op;

  switch (s->token.val) {
  case '+':
  case '-':
  case '!':
  case '~':
  case TOK_VOID:
    op = s->token.val;
    if (next_token(s))
      return -1;
    if (js_parse_unary(s, PF_POW_FORBIDDEN))
      return -1;
    switch (op) {
    case '-':
      emit_op(s, OP_neg);
      break;
    case '+':
      emit_op(s, OP_plus);
      break;
    case '!':
      emit_op(s, OP_lnot);
      break;
    case '~':
      emit_op(s, OP_not);
      break;
    case TOK_VOID:
      emit_op(s, OP_drop);
      emit_op(s, OP_undefined);
      break;
    default:
      abort();
    }
    parse_flags = 0;
    break;
  case TOK_DEC:
  case TOK_INC: {
    int opcode, op, scope, label;
    JSAtom name;
    op = s->token.val;
    if (next_token(s))
      return -1;
    if (js_parse_unary(s, 0))
      return -1;
    if (get_lvalue(s, &opcode, &scope, &name, &label, NULL, TRUE, op))
      return -1;
    emit_op(s, OP_dec + op - TOK_DEC);
    put_lvalue(s, opcode, scope, name, label, PUT_LVALUE_KEEP_TOP, FALSE);
  } break;
  case TOK_TYPEOF: {
    JSFunctionDef *fd;
    if (next_token(s))
      return -1;
    if (js_parse_unary(s, PF_POW_FORBIDDEN))
      return -1;
    /* reference access should not return an exception, so we
       patch the get_var */
    fd = s->cur_func;
    if (get_prev_opcode(fd) == OP_scope_get_var) {
      fd->byte_code.buf[fd->last_opcode_pos] = OP_scope_get_var_undef;
    }
    emit_op(s, OP_typeof);
    parse_flags = 0;
  } break;
  case TOK_DELETE:
    if (js_parse_delete(s))
      return -1;
    parse_flags = 0;
    break;
  case TOK_AWAIT:
    if (!(s->cur_func->func_kind & JS_FUNC_ASYNC))
      return js_parse_error(s, "unexpected 'await' keyword");
    if (!s->cur_func->in_function_body)
      return js_parse_error(s, "await in default expression");
    if (next_token(s))
      return -1;
    if (js_parse_unary(s, PF_POW_FORBIDDEN))
      return -1;
    emit_op(s, OP_await);
    parse_flags = 0;
    break;
  default:
    if (js_parse_postfix_expr(s,
                              (parse_flags & PF_ARROW_FUNC) | PF_POSTFIX_CALL))
      return -1;
    if (!s->got_lf && (s->token.val == TOK_DEC || s->token.val == TOK_INC)) {
      int opcode, op, scope, label;
      JSAtom name;
      op = s->token.val;
      if (get_lvalue(s, &opcode, &scope, &name, &label, NULL, TRUE, op))
        return -1;
      emit_op(s, OP_post_dec + op - TOK_DEC);
      put_lvalue(s, opcode, scope, name, label, PUT_LVALUE_KEEP_SECOND, FALSE);
      if (next_token(s))
        return -1;
    }
    break;
  }
  if (parse_flags & (PF_POW_ALLOWED | PF_POW_FORBIDDEN)) {
#ifdef CONFIG_BIGNUM
    if (s->token.val == TOK_POW || s->token.val == TOK_MATH_POW) {
      /* Extended exponentiation syntax rules: we extend the ES7
         grammar in order to have more intuitive semantics:
         -2**2 evaluates to -4. */
      if (!(s->cur_func->js_mode & JS_MODE_MATH)) {
        if (parse_flags & PF_POW_FORBIDDEN) {
          JS_ThrowSyntaxError(s->ctx, "unparenthesized unary expression can't "
                                      "appear on the left-hand side of '**'");
          return -1;
        }
      }
      if (next_token(s))
        return -1;
      if (js_parse_unary(s, PF_POW_ALLOWED))
        return -1;
      emit_op(s, OP_pow);
    }
#else
    if (s->token.val == TOK_POW) {
      /* Strict ES7 exponentiation syntax rules: To solve
         conflicting semantics between different implementations
         regarding the precedence of prefix operators and the
         postfix exponential, ES7 specifies that -2**2 is a
         syntax error. */
      if (parse_flags & PF_POW_FORBIDDEN) {
        JS_ThrowSyntaxError(s->ctx, "unparenthesized unary expression can't "
                                    "appear on the left-hand side of '**'");
        return -1;
      }
      if (next_token(s))
        return -1;
      if (js_parse_unary(s, PF_POW_ALLOWED))
        return -1;
      emit_op(s, OP_pow);
    }
#endif
  }
  return 0;
}

__exception int js_parse_left_hand_side_expr(JSParseState *s) {
  return js_parse_postfix_expr(s, PF_POSTFIX_CALL);
}

__exception int js_parse_expr_paren(JSParseState *s) {
  if (js_parse_expect(s, '('))
    return -1;
  if (js_parse_expr(s))
    return -1;
  if (js_parse_expect(s, ')'))
    return -1;
  return 0;
}
