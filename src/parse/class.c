#include "parse.h"

#include "vm/ops.h"

/* XXX: could generate specific bytecode */
static __exception int js_parse_class_default_ctor(JSParseState *s,
                                                   BOOL has_super,
                                                   JSFunctionDef **pfd) {
  JSParsePos pos;
  const char *str;
  int ret, line_num;
  JSParseFunctionEnum func_type;
  const uint8_t *saved_buf_end;

  js_parse_get_pos(s, &pos);
  if (has_super) {
    /* spec change: no argument evaluation */
    str = "(){super(...arguments);}";
    func_type = JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR;
  } else {
    str = "(){}";
    func_type = JS_PARSE_FUNC_CLASS_CONSTRUCTOR;
  }
  line_num = s->token.line_num;
  saved_buf_end = s->buf_end;
  s->buf_ptr = (uint8_t *)str;
  s->buf_end = (uint8_t *)(str + strlen(str));
  ret = next_token(s);
  if (!ret) {
    ret = js_parse_function_decl2(s, func_type, JS_FUNC_NORMAL, JS_ATOM_NULL,
                                  (uint8_t *)str, line_num,
                                  JS_PARSE_EXPORT_NONE, pfd);
  }
  s->buf_end = saved_buf_end;
  ret |= js_parse_seek_token(s, &pos);
  return ret;
}

/* find field in the current scope */
static int find_private_class_field(JSContext *ctx, JSFunctionDef *fd,
                                    JSAtom name, int scope_level) {
  int idx;
  idx = fd->scopes[scope_level].first;
  while (idx != -1) {
    if (fd->vars[idx].scope_level != scope_level)
      break;
    if (fd->vars[idx].var_name == name)
      return idx;
    idx = fd->vars[idx].scope_next;
  }
  return -1;
}

/* initialize the class fields, called by the constructor. Note:
   super() can be called in an arrow function, so <this> and
   <class_fields_init> can be variable references */
void emit_class_field_init(JSParseState *s) {
  int label_next;

  emit_op(s, OP_scope_get_var);
  emit_atom(s, JS_ATOM_class_fields_init);
  emit_u16(s, s->cur_func->scope_level);

  /* no need to call the class field initializer if not defined */
  emit_op(s, OP_dup);
  label_next = emit_goto(s, OP_if_false, -1);

  emit_op(s, OP_scope_get_var);
  emit_atom(s, JS_ATOM_this);
  emit_u16(s, 0);

  emit_op(s, OP_swap);

  emit_op(s, OP_call_method);
  emit_u16(s, 0);

  emit_label(s, label_next);
  emit_op(s, OP_drop);
}

/* build a private setter function name from the private getter name */
JSAtom get_private_setter_name(JSContext *ctx, JSAtom name) {
  return js_atom_concat_str(ctx, name, "<set>");
}

/* create a function to initialize class fields */
static JSFunctionDef *js_parse_function_class_fields_init(JSParseState *s) {
  JSFunctionDef *fd;

  fd = js_new_function_def(s->ctx, s->cur_func, FALSE, FALSE, s->filename, 0);
  if (!fd)
    return NULL;
  fd->func_name = JS_ATOM_NULL;
  fd->has_prototype = FALSE;
  fd->has_home_object = TRUE;

  fd->has_arguments_binding = FALSE;
  fd->has_this_binding = TRUE;
  fd->is_derived_class_constructor = FALSE;
  fd->new_target_allowed = TRUE;
  fd->super_call_allowed = FALSE;
  fd->super_allowed = fd->has_home_object;
  fd->arguments_allowed = FALSE;

  fd->func_kind = JS_FUNC_NORMAL;
  fd->func_type = JS_PARSE_FUNC_METHOD;
  return fd;
}

static __exception int emit_class_init_start(JSParseState *s,
                                             ClassFieldsDef *cf) {
  int label_add_brand;

  cf->fields_init_fd = js_parse_function_class_fields_init(s);
  if (!cf->fields_init_fd)
    return -1;

  s->cur_func = cf->fields_init_fd;

  /* XXX: would be better to add the code only if needed, maybe in a
     later pass */
  emit_op(s, OP_push_false); /* will be patched later */
  cf->brand_push_pos = cf->fields_init_fd->last_opcode_pos;
  label_add_brand = emit_goto(s, OP_if_false, -1);

  emit_op(s, OP_scope_get_var);
  emit_atom(s, JS_ATOM_this);
  emit_u16(s, 0);

  emit_op(s, OP_scope_get_var);
  emit_atom(s, JS_ATOM_home_object);
  emit_u16(s, 0);

  emit_op(s, OP_add_brand);

  emit_label(s, label_add_brand);

  s->cur_func = s->cur_func->parent;
  return 0;
}

static __exception int add_brand(JSParseState *s, ClassFieldsDef *cf) {
  if (!cf->has_brand) {
    /* define the brand field in 'this' of the initializer */
    if (!cf->fields_init_fd) {
      if (emit_class_init_start(s, cf))
        return -1;
    }
    /* patch the start of the function to enable the OP_add_brand code */
    cf->fields_init_fd->byte_code.buf[cf->brand_push_pos] = OP_push_true;

    cf->has_brand = TRUE;
  }
  return 0;
}

/* add a private field variable in the current scope */
static int add_private_class_field(JSParseState *s, JSFunctionDef *fd,
                                   JSAtom name, JSVarKindEnum var_kind) {
  JSContext *ctx = s->ctx;
  JSVarDef *vd;
  int idx;

  idx = add_scope_var(ctx, fd, name, var_kind);
  if (idx < 0)
    return idx;
  vd = &fd->vars[idx];
  vd->is_lexical = 1;
  vd->is_const = 1;
  return idx;
}

static void emit_class_init_end(JSParseState *s, ClassFieldsDef *cf) {
  int cpool_idx;

  s->cur_func = cf->fields_init_fd;
  emit_op(s, OP_return_undef);
  s->cur_func = s->cur_func->parent;

  cpool_idx = cpool_add(s, JS_NULL);
  cf->fields_init_fd->parent_cpool_idx = cpool_idx;
  emit_op(s, OP_fclosure);
  emit_u32(s, cpool_idx);
  emit_op(s, OP_set_home_object);
}

__exception int js_parse_class(JSParseState *s, BOOL is_class_expr,
                               JSParseExportEnum export_flag) {
  JSContext *ctx = s->ctx;
  JSFunctionDef *fd = s->cur_func;
  JSAtom name = JS_ATOM_NULL, class_name = JS_ATOM_NULL, class_name1;
  JSAtom class_var_name = JS_ATOM_NULL;
  JSFunctionDef *method_fd, *ctor_fd;
  int saved_js_mode, class_name_var_idx, prop_type, ctor_cpool_offset;
  int class_flags = 0, i, define_class_offset;
  BOOL is_static, is_private;
  const uint8_t *class_start_ptr = s->token.ptr;
  const uint8_t *start_ptr;
  ClassFieldsDef class_fields[2];

  /* classes are parsed and executed in strict mode */
  saved_js_mode = fd->js_mode;
  fd->js_mode |= JS_MODE_STRICT;
  if (next_token(s))
    goto fail;
  if (s->token.val == TOK_IDENT) {
    if (s->token.u.ident.is_reserved) {
      js_parse_error_reserved_identifier(s);
      goto fail;
    }
    class_name = JS_DupAtom(ctx, s->token.u.ident.atom);
    if (next_token(s))
      goto fail;
  } else if (!is_class_expr && export_flag != JS_PARSE_EXPORT_DEFAULT) {
    js_parse_error(s, "class statement requires a name");
    goto fail;
  }
  if (!is_class_expr) {
    if (class_name == JS_ATOM_NULL)
      class_var_name = JS_ATOM__default_; /* export default */
    else
      class_var_name = class_name;
    class_var_name = JS_DupAtom(ctx, class_var_name);
  }

  push_scope(s);

  if (s->token.val == TOK_EXTENDS) {
    class_flags = JS_DEFINE_CLASS_HAS_HERITAGE;
    if (next_token(s))
      goto fail;
    if (js_parse_left_hand_side_expr(s))
      goto fail;
  } else {
    emit_op(s, OP_undefined);
  }

  /* add a 'const' definition for the class name */
  if (class_name != JS_ATOM_NULL) {
    class_name_var_idx = define_var(s, fd, class_name, JS_VAR_DEF_CONST);
    if (class_name_var_idx < 0)
      goto fail;
  }

  if (js_parse_expect(s, '{'))
    goto fail;

  /* this scope contains the private fields */
  push_scope(s);

  emit_op(s, OP_push_const);
  ctor_cpool_offset = fd->byte_code.size;
  emit_u32(s, 0); /* will be patched at the end of the class parsing */

  if (class_name == JS_ATOM_NULL) {
    if (class_var_name != JS_ATOM_NULL)
      class_name1 = JS_ATOM_default;
    else
      class_name1 = JS_ATOM_empty_string;
  } else {
    class_name1 = class_name;
  }

  emit_op(s, OP_define_class);
  emit_atom(s, class_name1);
  emit_u8(s, class_flags);
  define_class_offset = fd->last_opcode_pos;

  for (i = 0; i < 2; i++) {
    ClassFieldsDef *cf = &class_fields[i];
    cf->fields_init_fd = NULL;
    cf->computed_fields_count = 0;
    cf->has_brand = FALSE;
  }

  ctor_fd = NULL;
  while (s->token.val != '}') {
    if (s->token.val == ';') {
      if (next_token(s))
        goto fail;
      continue;
    }
    is_static = (s->token.val == TOK_STATIC);
    prop_type = -1;
    if (is_static) {
      if (next_token(s))
        goto fail;
      /* allow "static" field name */
      if (s->token.val == ';' || s->token.val == '=') {
        is_static = FALSE;
        name = JS_DupAtom(ctx, JS_ATOM_static);
        prop_type = PROP_TYPE_IDENT;
      }
    }
    if (is_static)
      emit_op(s, OP_swap);
    start_ptr = s->token.ptr;
    if (prop_type < 0) {
      prop_type = js_parse_property_name(s, &name, TRUE, FALSE, TRUE);
      if (prop_type < 0)
        goto fail;
    }
    is_private = prop_type & PROP_TYPE_PRIVATE;
    prop_type &= ~PROP_TYPE_PRIVATE;

    if ((name == JS_ATOM_constructor && !is_static &&
         prop_type != PROP_TYPE_IDENT) ||
        (name == JS_ATOM_prototype && is_static) ||
        name == JS_ATOM_hash_constructor) {
      js_parse_error(s, "invalid method name");
      goto fail;
    }
    if (prop_type == PROP_TYPE_GET || prop_type == PROP_TYPE_SET) {
      BOOL is_set = prop_type - PROP_TYPE_GET;
      JSFunctionDef *method_fd;

      if (is_private) {
        int idx, var_kind;
        idx = find_private_class_field(ctx, fd, name, fd->scope_level);
        if (idx >= 0) {
          var_kind = fd->vars[idx].var_kind;
          if (var_kind == JS_VAR_PRIVATE_FIELD ||
              var_kind == JS_VAR_PRIVATE_METHOD ||
              var_kind == JS_VAR_PRIVATE_GETTER_SETTER ||
              var_kind == (JS_VAR_PRIVATE_GETTER + is_set)) {
            goto private_field_already_defined;
          }
          fd->vars[idx].var_kind = JS_VAR_PRIVATE_GETTER_SETTER;
        } else {
          if (add_private_class_field(s, fd, name,
                                      JS_VAR_PRIVATE_GETTER + is_set) < 0)
            goto fail;
        }
        if (add_brand(s, &class_fields[is_static]) < 0)
          goto fail;
      }

      if (js_parse_function_decl2(
              s, JS_PARSE_FUNC_GETTER + is_set, JS_FUNC_NORMAL, JS_ATOM_NULL,
              start_ptr, s->token.line_num, JS_PARSE_EXPORT_NONE, &method_fd))
        goto fail;
      if (is_private) {
        method_fd->need_home_object = TRUE; /* needed for brand check */
        emit_op(s, OP_set_home_object);
        /* XXX: missing function name */
        emit_op(s, OP_scope_put_var_init);
        if (is_set) {
          JSAtom setter_name;
          int ret;

          setter_name = get_private_setter_name(ctx, name);
          if (setter_name == JS_ATOM_NULL)
            goto fail;
          emit_atom(s, setter_name);
          ret = add_private_class_field(s, fd, setter_name,
                                        JS_VAR_PRIVATE_SETTER);
          JS_FreeAtom(ctx, setter_name);
          if (ret < 0)
            goto fail;
        } else {
          emit_atom(s, name);
        }
        emit_u16(s, s->cur_func->scope_level);
      } else {
        if (name == JS_ATOM_NULL) {
          emit_op(s, OP_define_method_computed);
        } else {
          emit_op(s, OP_define_method);
          emit_atom(s, name);
        }
        emit_u8(s, OP_DEFINE_METHOD_GETTER + is_set);
      }
    } else if (prop_type == PROP_TYPE_IDENT && s->token.val != '(') {
      ClassFieldsDef *cf = &class_fields[is_static];
      JSAtom field_var_name = JS_ATOM_NULL;

      /* class field */

      /* XXX: spec: not consistent with method name checks */
      if (name == JS_ATOM_constructor || name == JS_ATOM_prototype) {
        js_parse_error(s, "invalid field name");
        goto fail;
      }

      if (is_private) {
        if (find_private_class_field(ctx, fd, name, fd->scope_level) >= 0) {
          goto private_field_already_defined;
        }
        if (add_private_class_field(s, fd, name, JS_VAR_PRIVATE_FIELD) < 0)
          goto fail;
        emit_op(s, OP_private_symbol);
        emit_atom(s, name);
        emit_op(s, OP_scope_put_var_init);
        emit_atom(s, name);
        emit_u16(s, s->cur_func->scope_level);
      }

      if (!cf->fields_init_fd) {
        if (emit_class_init_start(s, cf))
          goto fail;
      }
      if (name == JS_ATOM_NULL) {
        /* save the computed field name into a variable */
        field_var_name = js_atom_concat_num(
            ctx, JS_ATOM_computed_field + is_static, cf->computed_fields_count);
        if (field_var_name == JS_ATOM_NULL)
          goto fail;
        if (define_var(s, fd, field_var_name, JS_VAR_DEF_CONST) < 0) {
          JS_FreeAtom(ctx, field_var_name);
          goto fail;
        }
        emit_op(s, OP_to_propkey);
        emit_op(s, OP_scope_put_var_init);
        emit_atom(s, field_var_name);
        emit_u16(s, s->cur_func->scope_level);
      }
      s->cur_func = cf->fields_init_fd;
      emit_op(s, OP_scope_get_var);
      emit_atom(s, JS_ATOM_this);
      emit_u16(s, 0);

      if (name == JS_ATOM_NULL) {
        emit_op(s, OP_scope_get_var);
        emit_atom(s, field_var_name);
        emit_u16(s, s->cur_func->scope_level);
        cf->computed_fields_count++;
        JS_FreeAtom(ctx, field_var_name);
      } else if (is_private) {
        emit_op(s, OP_scope_get_var);
        emit_atom(s, name);
        emit_u16(s, s->cur_func->scope_level);
      }

      if (s->token.val == '=') {
        if (next_token(s))
          goto fail;
        if (js_parse_assign_expr(s))
          goto fail;
      } else {
        emit_op(s, OP_undefined);
      }
      if (is_private) {
        set_object_name_computed(s);
        emit_op(s, OP_define_private_field);
      } else if (name == JS_ATOM_NULL) {
        set_object_name_computed(s);
        emit_op(s, OP_define_array_el);
        emit_op(s, OP_drop);
      } else {
        set_object_name(s, name);
        emit_op(s, OP_define_field);
        emit_atom(s, name);
      }
      s->cur_func = s->cur_func->parent;
      if (js_parse_expect_semi(s))
        goto fail;
    } else {
      JSParseFunctionEnum func_type;
      JSFunctionKindEnum func_kind;

      func_type = JS_PARSE_FUNC_METHOD;
      func_kind = JS_FUNC_NORMAL;
      if (prop_type == PROP_TYPE_STAR) {
        func_kind = JS_FUNC_GENERATOR;
      } else if (prop_type == PROP_TYPE_ASYNC) {
        func_kind = JS_FUNC_ASYNC;
      } else if (prop_type == PROP_TYPE_ASYNC_STAR) {
        func_kind = JS_FUNC_ASYNC_GENERATOR;
      } else if (name == JS_ATOM_constructor && !is_static) {
        if (ctor_fd) {
          js_parse_error(s, "property constructor appears more than once");
          goto fail;
        }
        if (class_flags & JS_DEFINE_CLASS_HAS_HERITAGE)
          func_type = JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR;
        else
          func_type = JS_PARSE_FUNC_CLASS_CONSTRUCTOR;
      }
      if (is_private) {
        if (add_brand(s, &class_fields[is_static]) < 0)
          goto fail;
      }
      if (js_parse_function_decl2(s, func_type, func_kind, JS_ATOM_NULL,
                                  start_ptr, s->token.line_num,
                                  JS_PARSE_EXPORT_NONE, &method_fd))
        goto fail;
      if (func_type == JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR ||
          func_type == JS_PARSE_FUNC_CLASS_CONSTRUCTOR) {
        ctor_fd = method_fd;
      } else if (is_private) {
        method_fd->need_home_object = TRUE; /* needed for brand check */
        if (find_private_class_field(ctx, fd, name, fd->scope_level) >= 0) {
        private_field_already_defined:
          js_parse_error(s, "private class field is already defined");
          goto fail;
        }
        if (add_private_class_field(s, fd, name, JS_VAR_PRIVATE_METHOD) < 0)
          goto fail;
        emit_op(s, OP_set_home_object);
        emit_op(s, OP_set_name);
        emit_atom(s, name);
        emit_op(s, OP_scope_put_var_init);
        emit_atom(s, name);
        emit_u16(s, s->cur_func->scope_level);
      } else {
        if (name == JS_ATOM_NULL) {
          emit_op(s, OP_define_method_computed);
        } else {
          emit_op(s, OP_define_method);
          emit_atom(s, name);
        }
        emit_u8(s, OP_DEFINE_METHOD_METHOD);
      }
    }
    if (is_static)
      emit_op(s, OP_swap);
    JS_FreeAtom(ctx, name);
    name = JS_ATOM_NULL;
  }

  if (s->token.val != '}') {
    js_parse_error(s, "expecting '%c'", '}');
    goto fail;
  }

  if (!ctor_fd) {
    if (js_parse_class_default_ctor(
            s, class_flags & JS_DEFINE_CLASS_HAS_HERITAGE, &ctor_fd))
      goto fail;
  }
  /* patch the constant pool index for the constructor */
  put_u32(fd->byte_code.buf + ctor_cpool_offset, ctor_fd->parent_cpool_idx);

  /* store the class source code in the constructor. */
  if (!(fd->js_mode & JS_MODE_STRIP)) {
    js_free(ctx, ctor_fd->source);
    ctor_fd->source_len = s->buf_ptr - class_start_ptr;
    ctor_fd->source =
        js_strndup(ctx, (const char *)class_start_ptr, ctor_fd->source_len);
    if (!ctor_fd->source)
      goto fail;
  }

  /* consume the '}' */
  if (next_token(s))
    goto fail;

  /* store the function to initialize the fields to that it can be
     referenced by the constructor */
  {
    ClassFieldsDef *cf = &class_fields[0];
    int var_idx;

    var_idx = define_var(s, fd, JS_ATOM_class_fields_init, JS_VAR_DEF_CONST);
    if (var_idx < 0)
      goto fail;
    if (cf->fields_init_fd) {
      emit_class_init_end(s, cf);
    } else {
      emit_op(s, OP_undefined);
    }
    emit_op(s, OP_scope_put_var_init);
    emit_atom(s, JS_ATOM_class_fields_init);
    emit_u16(s, s->cur_func->scope_level);
  }

  /* drop the prototype */
  emit_op(s, OP_drop);

  /* initialize the static fields */
  if (class_fields[1].fields_init_fd != NULL) {
    ClassFieldsDef *cf = &class_fields[1];
    emit_op(s, OP_dup);
    emit_class_init_end(s, cf);
    emit_op(s, OP_call_method);
    emit_u16(s, 0);
    emit_op(s, OP_drop);
  }

  if (class_name != JS_ATOM_NULL) {
    /* store the class name in the scoped class name variable (it
       is independent from the class statement variable
       definition) */
    emit_op(s, OP_dup);
    emit_op(s, OP_scope_put_var_init);
    emit_atom(s, class_name);
    emit_u16(s, fd->scope_level);
  }
  pop_scope(s);
  pop_scope(s);

  /* the class statements have a block level scope */
  if (class_var_name != JS_ATOM_NULL) {
    if (define_var(s, fd, class_var_name, JS_VAR_DEF_LET) < 0)
      goto fail;
    emit_op(s, OP_scope_put_var_init);
    emit_atom(s, class_var_name);
    emit_u16(s, fd->scope_level);
  } else {
    if (class_name == JS_ATOM_NULL) {
      /* cannot use OP_set_name because the name of the class
         must be defined before the static initializers are
         executed */
      emit_op(s, OP_set_class_name);
      emit_u32(s, fd->last_opcode_pos + 1 - define_class_offset);
    }
  }

  if (export_flag != JS_PARSE_EXPORT_NONE) {
    if (!add_export_entry(s, fd->module, class_var_name,
                          export_flag == JS_PARSE_EXPORT_NAMED
                              ? class_var_name
                              : JS_ATOM_default,
                          JS_EXPORT_TYPE_LOCAL))
      goto fail;
  }

  JS_FreeAtom(ctx, class_name);
  JS_FreeAtom(ctx, class_var_name);
  fd->js_mode = saved_js_mode;
  return 0;
fail:
  JS_FreeAtom(ctx, name);
  JS_FreeAtom(ctx, class_name);
  JS_FreeAtom(ctx, class_var_name);
  fd->js_mode = saved_js_mode;
  return -1;
}