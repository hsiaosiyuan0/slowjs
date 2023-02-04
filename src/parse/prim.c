#include "parse.h"

__exception int js_parse_array_literal(JSParseState *s) {
  uint32_t idx;
  BOOL need_length;

  if (next_token(s))
    return -1;
  /* small regular arrays are created on the stack */
  idx = 0;
  while (s->token.val != ']' && idx < 32) {
    if (s->token.val == ',' || s->token.val == TOK_ELLIPSIS)
      break;
    if (js_parse_assign_expr(s))
      return -1;
    idx++;
    /* accept trailing comma */
    if (s->token.val == ',') {
      if (next_token(s))
        return -1;
    } else if (s->token.val != ']')
      goto done;
  }
  emit_op(s, OP_array_from);
  emit_u16(s, idx);

  /* larger arrays and holes are handled with explicit indices */
  need_length = FALSE;
  while (s->token.val != ']' && idx < 0x7fffffff) {
    if (s->token.val == TOK_ELLIPSIS)
      break;
    need_length = TRUE;
    if (s->token.val != ',') {
      if (js_parse_assign_expr(s))
        return -1;
      emit_op(s, OP_define_field);
      emit_u32(s, __JS_AtomFromUInt32(idx));
      need_length = FALSE;
    }
    idx++;
    /* accept trailing comma */
    if (s->token.val == ',') {
      if (next_token(s))
        return -1;
    }
  }
  if (s->token.val == ']') {
    if (need_length) {
      /* Set the length: Cannot use OP_define_field because
         length is not configurable */
      emit_op(s, OP_dup);
      emit_op(s, OP_push_i32);
      emit_u32(s, idx);
      emit_op(s, OP_put_field);
      emit_atom(s, JS_ATOM_length);
    }
    goto done;
  }

  /* huge arrays and spread elements require a dynamic index on the stack */
  emit_op(s, OP_push_i32);
  emit_u32(s, idx);

  /* stack has array, index */
  while (s->token.val != ']') {
    if (s->token.val == TOK_ELLIPSIS) {
      if (next_token(s))
        return -1;
      if (js_parse_assign_expr(s))
        return -1;
#if 1
      emit_op(s, OP_append);
#else
      int label_next, label_done;
      label_next = new_label(s);
      label_done = new_label(s);
      /* enumerate object */
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
      /* close enumeration */
      emit_op(s, OP_drop); /* drop undef val */
      emit_op(s, OP_nip1); /* drop enum_rec */
      emit_op(s, OP_nip1);
      emit_op(s, OP_nip1);
#endif
    } else {
      need_length = TRUE;
      if (s->token.val != ',') {
        if (js_parse_assign_expr(s))
          return -1;
        /* a idx val */
        emit_op(s, OP_define_array_el);
        need_length = FALSE;
      }
      emit_op(s, OP_inc);
    }
    if (s->token.val != ',')
      break;
    if (next_token(s))
      return -1;
  }
  if (need_length) {
    /* Set the length: cannot use OP_define_field because
       length is not configurable */
    emit_op(s, OP_dup1); /* array length - array array length */
    emit_op(s, OP_put_field);
    emit_atom(s, JS_ATOM_length);
  } else {
    emit_op(s, OP_drop); /* array length - array */
  }
done:
  return js_parse_expect(s, ']');
}

/* if the property is an expression, name = JS_ATOM_NULL */
int __exception js_parse_property_name(JSParseState *s, JSAtom *pname,
                                       BOOL allow_method, BOOL allow_var,
                                       BOOL allow_private) {
  int is_private = 0;
  BOOL is_non_reserved_ident;
  JSAtom name;
  int prop_type;

  prop_type = PROP_TYPE_IDENT;
  if (allow_method) {
    if (token_is_pseudo_keyword(s, JS_ATOM_get) ||
        token_is_pseudo_keyword(s, JS_ATOM_set)) {
      /* get x(), set x() */
      name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
      if (next_token(s))
        goto fail1;
      if (s->token.val == ':' || s->token.val == ',' || s->token.val == '}' ||
          s->token.val == '(') {
        is_non_reserved_ident = TRUE;
        goto ident_found;
      }
      prop_type = PROP_TYPE_GET + (name == JS_ATOM_set);
      JS_FreeAtom(s->ctx, name);
    } else if (s->token.val == '*') {
      if (next_token(s))
        goto fail;
      prop_type = PROP_TYPE_STAR;
    } else if (token_is_pseudo_keyword(s, JS_ATOM_async) &&
               peek_token(s, TRUE) != '\n') {
      name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
      if (next_token(s))
        goto fail1;
      if (s->token.val == ':' || s->token.val == ',' || s->token.val == '}' ||
          s->token.val == '(') {
        is_non_reserved_ident = TRUE;
        goto ident_found;
      }
      JS_FreeAtom(s->ctx, name);
      if (s->token.val == '*') {
        if (next_token(s))
          goto fail;
        prop_type = PROP_TYPE_ASYNC_STAR;
      } else {
        prop_type = PROP_TYPE_ASYNC;
      }
    }
  }

  if (token_is_ident(s->token.val)) {
    /* variable can only be a non-reserved identifier */
    is_non_reserved_ident =
        (s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved);
    /* keywords and reserved words have a valid atom */
    name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
    if (next_token(s))
      goto fail1;
  ident_found:
    if (is_non_reserved_ident && prop_type == PROP_TYPE_IDENT && allow_var) {
      if (!(s->token.val == ':' || (s->token.val == '(' && allow_method))) {
        prop_type = PROP_TYPE_VAR;
      }
    }
  } else if (s->token.val == TOK_STRING) {
    name = JS_ValueToAtom(s->ctx, s->token.u.str.str);
    if (name == JS_ATOM_NULL)
      goto fail;
    if (next_token(s))
      goto fail1;
  } else if (s->token.val == TOK_NUMBER) {
    JSValue val;
    val = s->token.u.num.val;
#ifdef CONFIG_BIGNUM
    if (JS_VALUE_GET_TAG(val) == JS_TAG_BIG_FLOAT) {
      JSBigFloat *p = JS_VALUE_GET_PTR(val);
      val = s->ctx->rt->bigfloat_ops.mul_pow10_to_float64(
          s->ctx, &p->num, s->token.u.num.exponent);
      if (JS_IsException(val))
        goto fail;
      name = JS_ValueToAtom(s->ctx, val);
      JS_FreeValue(s->ctx, val);
    } else
#endif
    {
      name = JS_ValueToAtom(s->ctx, val);
    }
    if (name == JS_ATOM_NULL)
      goto fail;
    if (next_token(s))
      goto fail1;
  } else if (s->token.val == '[') {
    if (next_token(s))
      goto fail;
    if (js_parse_expr(s))
      goto fail;
    if (js_parse_expect(s, ']'))
      goto fail;
    name = JS_ATOM_NULL;
  } else if (s->token.val == TOK_PRIVATE_NAME && allow_private) {
    name = JS_DupAtom(s->ctx, s->token.u.ident.atom);
    if (next_token(s))
      goto fail1;
    is_private = PROP_TYPE_PRIVATE;
  } else {
    goto invalid_prop;
  }
  if (prop_type != PROP_TYPE_IDENT && prop_type != PROP_TYPE_VAR &&
      s->token.val != '(') {
    JS_FreeAtom(s->ctx, name);
  invalid_prop:
    js_parse_error(s, "invalid property name");
    goto fail;
  }
  *pname = name;
  return prop_type | is_private;
fail1:
  JS_FreeAtom(s->ctx, name);
fail:
  *pname = JS_ATOM_NULL;
  return -1;
}

void set_object_name(JSParseState *s, JSAtom name) {
  JSFunctionDef *fd = s->cur_func;
  int opcode;

  opcode = get_prev_opcode(fd);
  if (opcode == OP_set_name) {
    /* XXX: should free atom after OP_set_name? */
    fd->byte_code.size = fd->last_opcode_pos;
    fd->last_opcode_pos = -1;
    emit_op(s, OP_set_name);
    emit_atom(s, name);
  } else if (opcode == OP_set_class_name) {
    int define_class_pos;
    JSAtom atom;
    define_class_pos = fd->last_opcode_pos + 1 -
                       get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
    assert(fd->byte_code.buf[define_class_pos] == OP_define_class);
    /* for consistency we free the previous atom which is
       JS_ATOM_empty_string */
    atom = get_u32(fd->byte_code.buf + define_class_pos + 1);
    JS_FreeAtom(s->ctx, atom);
    put_u32(fd->byte_code.buf + define_class_pos + 1, JS_DupAtom(s->ctx, name));
    fd->last_opcode_pos = -1;
  }
}

void set_object_name_computed(JSParseState *s) {
  JSFunctionDef *fd = s->cur_func;
  int opcode;

  opcode = get_prev_opcode(fd);
  if (opcode == OP_set_name) {
    /* XXX: should free atom after OP_set_name? */
    fd->byte_code.size = fd->last_opcode_pos;
    fd->last_opcode_pos = -1;
    emit_op(s, OP_set_name_computed);
  } else if (opcode == OP_set_class_name) {
    int define_class_pos;
    define_class_pos = fd->last_opcode_pos + 1 -
                       get_u32(fd->byte_code.buf + fd->last_opcode_pos + 1);
    assert(fd->byte_code.buf[define_class_pos] == OP_define_class);
    fd->byte_code.buf[define_class_pos] = OP_define_class_computed;
    fd->last_opcode_pos = -1;
  }
}

__exception int js_parse_object_literal(JSParseState *s) {
  JSAtom name = JS_ATOM_NULL;
  const uint8_t *start_ptr;
  int start_line, prop_type;
  BOOL has_proto;

  if (next_token(s))
    goto fail;
  /* XXX: add an initial length that will be patched back */
  emit_op(s, OP_object);
  has_proto = FALSE;
  while (s->token.val != '}') {
    /* specific case for getter/setter */
    start_ptr = s->token.ptr;
    start_line = s->token.line_num;

    if (s->token.val == TOK_ELLIPSIS) {
      if (next_token(s))
        return -1;
      if (js_parse_assign_expr(s))
        return -1;
      emit_op(s, OP_null); /* dummy excludeList */
      emit_op(s, OP_copy_data_properties);
      emit_u8(s, 2 | (1 << 2) | (0 << 5));
      emit_op(s, OP_drop); /* pop excludeList */
      emit_op(s, OP_drop); /* pop src object */
      goto next;
    }

    prop_type = js_parse_property_name(s, &name, TRUE, TRUE, FALSE);
    if (prop_type < 0)
      goto fail;

    if (prop_type == PROP_TYPE_VAR) {
      /* shortcut for x: x */
      emit_op(s, OP_scope_get_var);
      emit_atom(s, name);
      emit_u16(s, s->cur_func->scope_level);
      emit_op(s, OP_define_field);
      emit_atom(s, name);
    } else if (s->token.val == '(') {
      BOOL is_getset =
          (prop_type == PROP_TYPE_GET || prop_type == PROP_TYPE_SET);
      JSParseFunctionEnum func_type;
      JSFunctionKindEnum func_kind;
      int op_flags;

      func_kind = JS_FUNC_NORMAL;
      if (is_getset) {
        func_type = JS_PARSE_FUNC_GETTER + prop_type - PROP_TYPE_GET;
      } else {
        func_type = JS_PARSE_FUNC_METHOD;
        if (prop_type == PROP_TYPE_STAR)
          func_kind = JS_FUNC_GENERATOR;
        else if (prop_type == PROP_TYPE_ASYNC)
          func_kind = JS_FUNC_ASYNC;
        else if (prop_type == PROP_TYPE_ASYNC_STAR)
          func_kind = JS_FUNC_ASYNC_GENERATOR;
      }
      if (js_parse_function_decl(s, func_type, func_kind, JS_ATOM_NULL,
                                 start_ptr, start_line))
        goto fail;
      if (name == JS_ATOM_NULL) {
        emit_op(s, OP_define_method_computed);
      } else {
        emit_op(s, OP_define_method);
        emit_atom(s, name);
      }
      if (is_getset) {
        op_flags = OP_DEFINE_METHOD_GETTER + prop_type - PROP_TYPE_GET;
      } else {
        op_flags = OP_DEFINE_METHOD_METHOD;
      }
      emit_u8(s, op_flags | OP_DEFINE_METHOD_ENUMERABLE);
    } else {
      if (js_parse_expect(s, ':'))
        goto fail;
      if (js_parse_assign_expr(s))
        goto fail;
      if (name == JS_ATOM_NULL) {
        set_object_name_computed(s);
        emit_op(s, OP_define_array_el);
        emit_op(s, OP_drop);
      } else if (name == JS_ATOM___proto__) {
        if (has_proto) {
          js_parse_error(s, "duplicate __proto__ property name");
          goto fail;
        }
        emit_op(s, OP_set_proto);
        has_proto = TRUE;
      } else {
        set_object_name(s, name);
        emit_op(s, OP_define_field);
        emit_atom(s, name);
      }
    }
    JS_FreeAtom(s->ctx, name);
  next:
    name = JS_ATOM_NULL;
    if (s->token.val != ',')
      break;
    if (next_token(s))
      goto fail;
  }
  if (js_parse_expect(s, '}'))
    goto fail;
  return 0;
fail:
  JS_FreeAtom(s->ctx, name);
  return -1;
}
