#include "parse.h"

#include "utils/dbuf.h"
#include "vm/error.h"

/* -- Parse Function ----------------------------------- */

__exception int js_parse_directives(JSParseState *s) {
  char str[20];
  JSParsePos pos;
  BOOL has_semi;

  if (s->token.val != TOK_STRING)
    return 0;

  js_parse_get_pos(s, &pos);

  while (s->token.val == TOK_STRING) {
    /* Copy actual source string representation */
    snprintf(str, sizeof str, "%.*s", (int)(s->buf_ptr - s->token.ptr - 2),
             s->token.ptr + 1);

    if (next_token(s))
      return -1;

    has_semi = FALSE;
    switch (s->token.val) {
    case ';':
      if (next_token(s))
        return -1;
      has_semi = TRUE;
      break;
    case '}':
    case TOK_EOF:
      has_semi = TRUE;
      break;
    case TOK_NUMBER:
    case TOK_STRING:
    case TOK_TEMPLATE:
    case TOK_IDENT:
    case TOK_REGEXP:
    case TOK_DEC:
    case TOK_INC:
    case TOK_NULL:
    case TOK_FALSE:
    case TOK_TRUE:
    case TOK_IF:
    case TOK_RETURN:
    case TOK_VAR:
    case TOK_THIS:
    case TOK_DELETE:
    case TOK_TYPEOF:
    case TOK_NEW:
    case TOK_DO:
    case TOK_WHILE:
    case TOK_FOR:
    case TOK_SWITCH:
    case TOK_THROW:
    case TOK_TRY:
    case TOK_FUNCTION:
    case TOK_DEBUGGER:
    case TOK_WITH:
    case TOK_CLASS:
    case TOK_CONST:
    case TOK_ENUM:
    case TOK_EXPORT:
    case TOK_IMPORT:
    case TOK_SUPER:
    case TOK_INTERFACE:
    case TOK_LET:
    case TOK_PACKAGE:
    case TOK_PRIVATE:
    case TOK_PROTECTED:
    case TOK_PUBLIC:
    case TOK_STATIC:
      /* automatic insertion of ';' */
      if (s->got_lf)
        has_semi = TRUE;
      break;
    default:
      break;
    }
    if (!has_semi)
      break;
    if (!strcmp(str, "use strict")) {
      s->cur_func->has_use_strict = TRUE;
      s->cur_func->js_mode |= JS_MODE_STRICT;
    }
#if !defined(DUMP_BYTECODE) || !(DUMP_BYTECODE & 8)
    else if (!strcmp(str, "use strip")) {
      s->cur_func->js_mode |= JS_MODE_STRIP;
    }
#endif
#ifdef CONFIG_BIGNUM
    else if (s->ctx->bignum_ext && !strcmp(str, "use math")) {
      s->cur_func->js_mode |= JS_MODE_MATH;
    }
#endif
  }
  return js_parse_seek_token(s, &pos);
}

__exception int js_parse_source_element(JSParseState *s) {
  JSFunctionDef *fd = s->cur_func;
  int tok;

  if (s->token.val == TOK_FUNCTION ||
      (token_is_pseudo_keyword(s, JS_ATOM_async) &&
       peek_token(s, TRUE) == TOK_FUNCTION)) {
    if (js_parse_function_decl(s, JS_PARSE_FUNC_STATEMENT, JS_FUNC_NORMAL,
                               JS_ATOM_NULL, s->token.ptr, s->token.line_num))
      return -1;
  } else if (s->token.val == TOK_EXPORT && fd->module) {
    if (js_parse_export(s))
      return -1;
  } else if (s->token.val == TOK_IMPORT && fd->module &&
             ((tok = peek_token(s, FALSE)) != '(' && tok != '.')) {
    /* the peek_token is needed to avoid confusion with ImportCall
       (dynamic import) or import.meta */
    if (js_parse_import(s))
      return -1;
  } else {
    if (js_parse_statement_or_decl(s, DECL_MASK_ALL))
      return -1;
  }
  return 0;
}

static int js_parse_function_check_names(JSParseState *s, JSFunctionDef *fd,
                                         JSAtom func_name) {
  JSAtom name;
  int i, idx;

  if (fd->js_mode & JS_MODE_STRICT) {
    if (!fd->has_simple_parameter_list && fd->has_use_strict) {
      return js_parse_error(s, "\"use strict\" not allowed in function with "
                               "default or destructuring parameter");
    }
    if (func_name == JS_ATOM_eval || func_name == JS_ATOM_arguments) {
      return js_parse_error(s, "invalid function name in strict code");
    }
    for (idx = 0; idx < fd->arg_count; idx++) {
      name = fd->args[idx].var_name;

      if (name == JS_ATOM_eval || name == JS_ATOM_arguments) {
        return js_parse_error(s, "invalid argument name in strict code");
      }
    }
  }
  /* check async_generator case */
  if ((fd->js_mode & JS_MODE_STRICT) || !fd->has_simple_parameter_list ||
      (fd->func_type == JS_PARSE_FUNC_METHOD &&
       fd->func_kind == JS_FUNC_ASYNC) ||
      fd->func_type == JS_PARSE_FUNC_ARROW ||
      fd->func_type == JS_PARSE_FUNC_METHOD) {
    for (idx = 0; idx < fd->arg_count; idx++) {
      name = fd->args[idx].var_name;
      if (name != JS_ATOM_NULL) {
        for (i = 0; i < idx; i++) {
          if (fd->args[i].var_name == name)
            goto duplicate;
        }
        /* Check if argument name duplicates a destructuring parameter */
        /* XXX: should have a flag for such variables */
        for (i = 0; i < fd->var_count; i++) {
          if (fd->vars[i].var_name == name && fd->vars[i].scope_level == 0)
            goto duplicate;
        }
      }
    }
  }
  return 0;

duplicate:
  return js_parse_error(s,
                        "duplicate argument names not allowed in this context");
}

/* func_name must be JS_ATOM_NULL for JS_PARSE_FUNC_STATEMENT and
   JS_PARSE_FUNC_EXPR, JS_PARSE_FUNC_ARROW and JS_PARSE_FUNC_VAR */
__exception int
js_parse_function_decl2(JSParseState *s, JSParseFunctionEnum func_type,
                        JSFunctionKindEnum func_kind, JSAtom func_name,
                        const uint8_t *ptr, int function_line_num,
                        JSParseExportEnum export_flag, JSFunctionDef **pfd) {
  JSContext *ctx = s->ctx;
  JSFunctionDef *fd = s->cur_func;
  BOOL is_expr;
  int func_idx, lexical_func_idx = -1;
  BOOL has_opt_arg;
  BOOL create_func_var = FALSE;

  is_expr =
      (func_type != JS_PARSE_FUNC_STATEMENT && func_type != JS_PARSE_FUNC_VAR);

  if (func_type == JS_PARSE_FUNC_STATEMENT || func_type == JS_PARSE_FUNC_VAR ||
      func_type == JS_PARSE_FUNC_EXPR) {
    if (func_kind == JS_FUNC_NORMAL &&
        token_is_pseudo_keyword(s, JS_ATOM_async) &&
        peek_token(s, TRUE) != '\n') {
      if (next_token(s))
        return -1;
      func_kind = JS_FUNC_ASYNC;
    }
    if (next_token(s))
      return -1;
    if (s->token.val == '*') {
      if (next_token(s))
        return -1;
      func_kind |= JS_FUNC_GENERATOR;
    }

    if (s->token.val == TOK_IDENT) {
      if (s->token.u.ident.is_reserved ||
          (s->token.u.ident.atom == JS_ATOM_yield &&
           func_type == JS_PARSE_FUNC_EXPR &&
           (func_kind & JS_FUNC_GENERATOR)) ||
          (s->token.u.ident.atom == JS_ATOM_await &&
           func_type == JS_PARSE_FUNC_EXPR && (func_kind & JS_FUNC_ASYNC))) {
        return js_parse_error_reserved_identifier(s);
      }
    }
    if (s->token.val == TOK_IDENT ||
        (((s->token.val == TOK_YIELD && !(fd->js_mode & JS_MODE_STRICT)) ||
          (s->token.val == TOK_AWAIT && !s->is_module)) &&
         func_type == JS_PARSE_FUNC_EXPR)) {
      func_name = JS_DupAtom(ctx, s->token.u.ident.atom);
      if (next_token(s)) {
        JS_FreeAtom(ctx, func_name);
        return -1;
      }
    } else {
      if (func_type != JS_PARSE_FUNC_EXPR &&
          export_flag != JS_PARSE_EXPORT_DEFAULT) {
        return js_parse_error(s, "function name expected");
      }
    }
  } else if (func_type != JS_PARSE_FUNC_ARROW) {
    func_name = JS_DupAtom(ctx, func_name);
  }

  if (fd->is_eval && fd->eval_type == JS_EVAL_TYPE_MODULE &&
      (func_type == JS_PARSE_FUNC_STATEMENT ||
       func_type == JS_PARSE_FUNC_VAR)) {
    JSGlobalVar *hf;
    hf = find_global_var(fd, func_name);
    /* XXX: should check scope chain */
    if (hf && hf->scope_level == fd->scope_level) {
      js_parse_error(
          s, "invalid redefinition of global identifier in module code");
      JS_FreeAtom(ctx, func_name);
      return -1;
    }
  }

  if (func_type == JS_PARSE_FUNC_VAR) {
    if (!(fd->js_mode & JS_MODE_STRICT) && func_kind == JS_FUNC_NORMAL &&
        find_lexical_decl(ctx, fd, func_name, fd->scope_first, FALSE) < 0 &&
        !((func_idx = find_var(ctx, fd, func_name)) >= 0 &&
          (func_idx & ARGUMENT_VAR_OFFSET)) &&
        !(func_name == JS_ATOM_arguments && fd->has_arguments_binding)) {
      create_func_var = TRUE;
    }
    /* Create the lexical name here so that the function closure
       contains it */
    if (fd->is_eval &&
        (fd->eval_type == JS_EVAL_TYPE_GLOBAL ||
         fd->eval_type == JS_EVAL_TYPE_MODULE) &&
        fd->scope_level == fd->body_scope) {
      /* avoid creating a lexical variable in the global
         scope. XXX: check annex B */
      JSGlobalVar *hf;
      hf = find_global_var(fd, func_name);
      /* XXX: should check scope chain */
      if (hf && hf->scope_level == fd->scope_level) {
        js_parse_error(s, "invalid redefinition of global identifier");
        JS_FreeAtom(ctx, func_name);
        return -1;
      }
    } else {
      /* Always create a lexical name, fail if at the same scope as
         existing name */
      /* Lexical variable will be initialized upon entering scope */
      lexical_func_idx =
          define_var(s, fd, func_name,
                     func_kind != JS_FUNC_NORMAL ? JS_VAR_DEF_NEW_FUNCTION_DECL
                                                 : JS_VAR_DEF_FUNCTION_DECL);
      if (lexical_func_idx < 0) {
        JS_FreeAtom(ctx, func_name);
        return -1;
      }
    }
  }

  fd = js_new_function_def(ctx, fd, FALSE, is_expr, s->filename,
                           function_line_num);
  if (!fd) {
    JS_FreeAtom(ctx, func_name);
    return -1;
  }
  if (pfd)
    *pfd = fd;
  s->cur_func = fd;
  fd->func_name = func_name;
  /* XXX: test !fd->is_generator is always false */
  fd->has_prototype =
      (func_type == JS_PARSE_FUNC_STATEMENT || func_type == JS_PARSE_FUNC_VAR ||
       func_type == JS_PARSE_FUNC_EXPR) &&
      func_kind == JS_FUNC_NORMAL;
  fd->has_home_object =
      (func_type == JS_PARSE_FUNC_METHOD || func_type == JS_PARSE_FUNC_GETTER ||
       func_type == JS_PARSE_FUNC_SETTER ||
       func_type == JS_PARSE_FUNC_CLASS_CONSTRUCTOR ||
       func_type == JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR);
  fd->has_arguments_binding = (func_type != JS_PARSE_FUNC_ARROW);
  fd->has_this_binding = fd->has_arguments_binding;
  fd->is_derived_class_constructor =
      (func_type == JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR);
  if (func_type == JS_PARSE_FUNC_ARROW) {
    fd->new_target_allowed = fd->parent->new_target_allowed;
    fd->super_call_allowed = fd->parent->super_call_allowed;
    fd->super_allowed = fd->parent->super_allowed;
    fd->arguments_allowed = fd->parent->arguments_allowed;
  } else {
    fd->new_target_allowed = TRUE;
    fd->super_call_allowed = fd->is_derived_class_constructor;
    fd->super_allowed = fd->has_home_object;
    fd->arguments_allowed = TRUE;
  }

  /* fd->in_function_body == FALSE prevents yield/await during the parsing
     of the arguments in generator/async functions. They are parsed as
     regular identifiers for other function kinds. */
  fd->func_kind = func_kind;
  fd->func_type = func_type;

  if (func_type == JS_PARSE_FUNC_CLASS_CONSTRUCTOR ||
      func_type == JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR) {
    /* error if not invoked as a constructor */
    emit_op(s, OP_check_ctor);
  }

  if (func_type == JS_PARSE_FUNC_CLASS_CONSTRUCTOR) {
    emit_class_field_init(s);
  }

  /* parse arguments */
  fd->has_simple_parameter_list = TRUE;
  fd->has_parameter_expressions = FALSE;
  has_opt_arg = FALSE;
  if (func_type == JS_PARSE_FUNC_ARROW && s->token.val == TOK_IDENT) {
    JSAtom name;
    if (s->token.u.ident.is_reserved) {
      js_parse_error_reserved_identifier(s);
      goto fail;
    }
    name = s->token.u.ident.atom;
    if (add_arg(ctx, fd, name) < 0)
      goto fail;
    fd->defined_arg_count = 1;
  } else {
    if (s->token.val == '(') {
      int skip_bits;
      /* if there is an '=' inside the parameter list, we
         consider there is a parameter expression inside */
      js_parse_skip_parens_token(s, &skip_bits, FALSE);
      if (skip_bits & SKIP_HAS_ASSIGNMENT)
        fd->has_parameter_expressions = TRUE;
      if (next_token(s))
        goto fail;
    } else {
      if (js_parse_expect(s, '('))
        goto fail;
    }

    if (fd->has_parameter_expressions) {
      fd->scope_level = -1; /* force no parent scope */
      if (push_scope(s) < 0)
        return -1;
    }

    while (s->token.val != ')') {
      JSAtom name;
      BOOL rest = FALSE;
      int idx, has_initializer;

      if (s->token.val == TOK_ELLIPSIS) {
        fd->has_simple_parameter_list = FALSE;
        rest = TRUE;
        if (next_token(s))
          goto fail;
      }
      if (s->token.val == '[' || s->token.val == '{') {
        fd->has_simple_parameter_list = FALSE;
        if (rest) {
          emit_op(s, OP_rest);
          emit_u16(s, fd->arg_count);
        } else {
          /* unnamed arg for destructuring */
          idx = add_arg(ctx, fd, JS_ATOM_NULL);
          emit_op(s, OP_get_arg);
          emit_u16(s, idx);
        }
        has_initializer = js_parse_destructuring_element(
            s, fd->has_parameter_expressions ? TOK_LET : TOK_VAR, 1, TRUE, -1,
            TRUE);
        if (has_initializer < 0)
          goto fail;
        if (has_initializer)
          has_opt_arg = TRUE;
        if (!has_opt_arg)
          fd->defined_arg_count++;
      } else if (s->token.val == TOK_IDENT) {
        if (s->token.u.ident.is_reserved) {
          js_parse_error_reserved_identifier(s);
          goto fail;
        }
        name = s->token.u.ident.atom;
        if (name == JS_ATOM_yield && fd->func_kind == JS_FUNC_GENERATOR) {
          js_parse_error_reserved_identifier(s);
          goto fail;
        }
        if (fd->has_parameter_expressions) {
          if (define_var(s, fd, name, JS_VAR_DEF_LET) < 0)
            goto fail;
        }
        /* XXX: could avoid allocating an argument if rest is true */
        idx = add_arg(ctx, fd, name);
        if (idx < 0)
          goto fail;
        if (next_token(s))
          goto fail;
        if (rest) {
          emit_op(s, OP_rest);
          emit_u16(s, idx);
          if (fd->has_parameter_expressions) {
            emit_op(s, OP_dup);
            emit_op(s, OP_scope_put_var_init);
            emit_atom(s, name);
            emit_u16(s, fd->scope_level);
          }
          emit_op(s, OP_put_arg);
          emit_u16(s, idx);
          fd->has_simple_parameter_list = FALSE;
          has_opt_arg = TRUE;
        } else if (s->token.val == '=') {
          int label;

          fd->has_simple_parameter_list = FALSE;
          has_opt_arg = TRUE;

          if (next_token(s))
            goto fail;

          label = new_label(s);
          emit_op(s, OP_get_arg);
          emit_u16(s, idx);
          emit_op(s, OP_dup);
          emit_op(s, OP_undefined);
          emit_op(s, OP_strict_eq);
          emit_goto(s, OP_if_false, label);
          emit_op(s, OP_drop);
          if (js_parse_assign_expr(s))
            goto fail;
          set_object_name(s, name);
          emit_op(s, OP_dup);
          emit_op(s, OP_put_arg);
          emit_u16(s, idx);
          emit_label(s, label);
          emit_op(s, OP_scope_put_var_init);
          emit_atom(s, name);
          emit_u16(s, fd->scope_level);
        } else {
          if (!has_opt_arg) {
            fd->defined_arg_count++;
          }
          if (fd->has_parameter_expressions) {
            /* copy the argument to the argument scope */
            emit_op(s, OP_get_arg);
            emit_u16(s, idx);
            emit_op(s, OP_scope_put_var_init);
            emit_atom(s, name);
            emit_u16(s, fd->scope_level);
          }
        }
      } else {
        js_parse_error(s, "missing formal parameter");
        goto fail;
      }
      if (rest && s->token.val != ')') {
        js_parse_expect(s, ')');
        goto fail;
      }
      if (s->token.val == ')')
        break;
      if (js_parse_expect(s, ','))
        goto fail;
    }
    if ((func_type == JS_PARSE_FUNC_GETTER && fd->arg_count != 0) ||
        (func_type == JS_PARSE_FUNC_SETTER && fd->arg_count != 1)) {
      js_parse_error(s, "invalid number of arguments for getter or setter");
      goto fail;
    }
  }

  if (fd->has_parameter_expressions) {
    int idx;

    /* Copy the variables in the argument scope to the variable
       scope (see FunctionDeclarationInstantiation() in spec). The
       normal arguments are already present, so no need to copy
       them. */
    idx = fd->scopes[fd->scope_level].first;
    while (idx >= 0) {
      JSVarDef *vd = &fd->vars[idx];
      if (vd->scope_level != fd->scope_level)
        break;
      if (find_var(ctx, fd, vd->var_name) < 0) {
        if (add_var(ctx, fd, vd->var_name) < 0)
          goto fail;
        vd = &fd->vars[idx]; /* fd->vars may have been reallocated */
        emit_op(s, OP_scope_get_var);
        emit_atom(s, vd->var_name);
        emit_u16(s, fd->scope_level);
        emit_op(s, OP_scope_put_var);
        emit_atom(s, vd->var_name);
        emit_u16(s, 0);
      }
      idx = vd->scope_next;
    }

    /* the argument scope has no parent, hence we don't use pop_scope(s) */
    emit_op(s, OP_leave_scope);
    emit_u16(s, fd->scope_level);

    /* set the variable scope as the current scope */
    fd->scope_level = 0;
    fd->scope_first = fd->scopes[fd->scope_level].first;
  }

  if (next_token(s))
    goto fail;

  /* generator function: yield after the parameters are evaluated */
  if (func_kind == JS_FUNC_GENERATOR || func_kind == JS_FUNC_ASYNC_GENERATOR)
    emit_op(s, OP_initial_yield);

  /* in generators, yield expression is forbidden during the parsing
     of the arguments */
  fd->in_function_body = TRUE;
  push_scope(s); /* enter body scope */
  fd->body_scope = fd->scope_level;

  if (s->token.val == TOK_ARROW) {
    if (next_token(s))
      goto fail;

    if (s->token.val != '{') {
      if (js_parse_function_check_names(s, fd, func_name))
        goto fail;

      if (js_parse_assign_expr(s))
        goto fail;

      if (func_kind != JS_FUNC_NORMAL)
        emit_op(s, OP_return_async);
      else
        emit_op(s, OP_return);

      if (!(fd->js_mode & JS_MODE_STRIP)) {
        /* save the function source code */
        /* the end of the function source code is after the last
           token of the function source stored into s->last_ptr */
        fd->source_len = s->last_ptr - ptr;
        fd->source = js_strndup(ctx, (const char *)ptr, fd->source_len);
        if (!fd->source)
          goto fail;
      }
      goto done;
    }
  }

  if (js_parse_expect(s, '{'))
    goto fail;

  if (js_parse_directives(s))
    goto fail;

  /* in strict_mode, check function and argument names */
  if (js_parse_function_check_names(s, fd, func_name))
    goto fail;

  while (s->token.val != '}') {
    if (js_parse_source_element(s))
      goto fail;
  }
  if (!(fd->js_mode & JS_MODE_STRIP)) {
    /* save the function source code */
    fd->source_len = s->buf_ptr - ptr;
    fd->source = js_strndup(ctx, (const char *)ptr, fd->source_len);
    if (!fd->source)
      goto fail;
  }

  if (next_token(s)) {
    /* consume the '}' */
    goto fail;
  }

  /* in case there is no return, add one */
  if (js_is_live_code(s)) {
    emit_return(s, FALSE);
  }
done:
  s->cur_func = fd->parent;

  /* create the function object */
  {
    int idx;
    JSAtom func_name = fd->func_name;

    /* the real object will be set at the end of the compilation */
    idx = cpool_add(s, JS_NULL);
    fd->parent_cpool_idx = idx;

    if (is_expr) {
      /* for constructors, no code needs to be generated here */
      if (func_type != JS_PARSE_FUNC_CLASS_CONSTRUCTOR &&
          func_type != JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR) {
        /* OP_fclosure creates the function object from the bytecode
           and adds the scope information */
        emit_op(s, OP_fclosure);
        emit_u32(s, idx);
        if (func_name == JS_ATOM_NULL) {
          emit_op(s, OP_set_name);
          emit_u32(s, JS_ATOM_NULL);
        }
      }
    } else if (func_type == JS_PARSE_FUNC_VAR) {
      emit_op(s, OP_fclosure);
      emit_u32(s, idx);
      if (create_func_var) {
        if (s->cur_func->is_global_var) {
          JSGlobalVar *hf;
          /* the global variable must be defined at the start of the
             function */
          hf = add_global_var(ctx, s->cur_func, func_name);
          if (!hf)
            goto fail;
          /* it is considered as defined at the top level
             (needed for annex B.3.3.4 and B.3.3.5
             checks) */
          hf->scope_level = 0;
          hf->force_init = ((s->cur_func->js_mode & JS_MODE_STRICT) != 0);
          /* store directly into global var, bypass lexical scope */
          emit_op(s, OP_dup);
          emit_op(s, OP_scope_put_var);
          emit_atom(s, func_name);
          emit_u16(s, 0);
        } else {
          /* do not call define_var to bypass lexical scope check */
          func_idx = find_var(ctx, s->cur_func, func_name);
          if (func_idx < 0) {
            func_idx = add_var(ctx, s->cur_func, func_name);
            if (func_idx < 0)
              goto fail;
          }
          /* store directly into local var, bypass lexical catch scope */
          emit_op(s, OP_dup);
          emit_op(s, OP_scope_put_var);
          emit_atom(s, func_name);
          emit_u16(s, 0);
        }
      }
      if (lexical_func_idx >= 0) {
        /* lexical variable will be initialized upon entering scope */
        s->cur_func->vars[lexical_func_idx].func_pool_idx = idx;
        emit_op(s, OP_drop);
      } else {
        /* store function object into its lexical name */
        /* XXX: could use OP_put_loc directly */
        emit_op(s, OP_scope_put_var_init);
        emit_atom(s, func_name);
        emit_u16(s, s->cur_func->scope_level);
      }
    } else {
      if (!s->cur_func->is_global_var) {
        int var_idx = define_var(s, s->cur_func, func_name, JS_VAR_DEF_VAR);

        if (var_idx < 0)
          goto fail;
        /* the variable will be assigned at the top of the function */
        if (var_idx & ARGUMENT_VAR_OFFSET) {
          s->cur_func->args[var_idx - ARGUMENT_VAR_OFFSET].func_pool_idx = idx;
        } else {
          s->cur_func->vars[var_idx].func_pool_idx = idx;
        }
      } else {
        JSAtom func_var_name;
        JSGlobalVar *hf;
        if (func_name == JS_ATOM_NULL)
          func_var_name = JS_ATOM__default_; /* export default */
        else
          func_var_name = func_name;
        /* the variable will be assigned at the top of the function */
        hf = add_global_var(ctx, s->cur_func, func_var_name);
        if (!hf)
          goto fail;
        hf->cpool_idx = idx;
        if (export_flag != JS_PARSE_EXPORT_NONE) {
          if (!add_export_entry(s, s->cur_func->module, func_var_name,
                                export_flag == JS_PARSE_EXPORT_NAMED
                                    ? func_var_name
                                    : JS_ATOM_default,
                                JS_EXPORT_TYPE_LOCAL))
            goto fail;
        }
      }
    }
  }
  return 0;
fail:
  s->cur_func = fd->parent;
  js_free_function_def(ctx, fd);
  if (pfd)
    *pfd = NULL;
  return -1;
}

__exception int js_parse_function_decl(JSParseState *s,
                                       JSParseFunctionEnum func_type,
                                       JSFunctionKindEnum func_kind,
                                       JSAtom func_name, const uint8_t *ptr,
                                       int function_line_num) {
  return js_parse_function_decl2(s, func_type, func_kind, func_name, ptr,
                                 function_line_num, JS_PARSE_EXPORT_NONE, NULL);
}
/* execute the finally blocks before return */
void emit_return(JSParseState *s, BOOL hasval) {
  BlockEnv *top;
  int drop_count;

  drop_count = 0;
  top = s->cur_func->top_break;
  while (top != NULL) {
    /* XXX: emit the appropriate OP_leave_scope opcodes? Probably not
       required as all local variables will be closed upon returning
       from JS_CallInternal, but not in the same order. */
    if (top->has_iterator) {
      /* with 'yield', the exact number of OP_drop to emit is
         unknown, so we use a specific operation to look for
         the catch offset */
      if (!hasval) {
        emit_op(s, OP_undefined);
        hasval = TRUE;
      }
      emit_op(s, OP_iterator_close_return);
      if (s->cur_func->func_kind == JS_FUNC_ASYNC_GENERATOR) {
        int label_next, label_next2;

        emit_op(s, OP_drop); /* catch offset */
        emit_op(s, OP_drop); /* next */
        emit_op(s, OP_get_field2);
        emit_atom(s, JS_ATOM_return);
        /* stack: iter_obj return_func */
        emit_op(s, OP_dup);
        emit_op(s, OP_is_undefined_or_null);
        label_next = emit_goto(s, OP_if_true, -1);
        emit_op(s, OP_call_method);
        emit_u16(s, 0);
        emit_op(s, OP_iterator_check_object);
        emit_op(s, OP_await);
        label_next2 = emit_goto(s, OP_goto, -1);
        emit_label(s, label_next);
        emit_op(s, OP_drop);
        emit_label(s, label_next2);
        emit_op(s, OP_drop);
      } else {
        emit_op(s, OP_iterator_close);
      }
      drop_count = -3;
    }
    drop_count += top->drop_count;
    if (top->label_finally != -1) {
      while (drop_count) {
        /* must keep the stack top if hasval */
        emit_op(s, hasval ? OP_nip : OP_drop);
        drop_count--;
      }
      if (!hasval) {
        /* must push return value to keep same stack size */
        emit_op(s, OP_undefined);
        hasval = TRUE;
      }
      emit_goto(s, OP_gosub, top->label_finally);
    }
    top = top->prev;
  }
  if (s->cur_func->is_derived_class_constructor) {
    int label_return;

    /* 'this' can be uninitialized, so it may be accessed only if
       the derived class constructor does not return an object */
    if (hasval) {
      emit_op(s, OP_check_ctor_return);
      label_return = emit_goto(s, OP_if_false, -1);
      emit_op(s, OP_drop);
    } else {
      label_return = -1;
    }

    /* XXX: if this is not initialized, should throw the
       ReferenceError in the caller realm */
    emit_op(s, OP_scope_get_var);
    emit_atom(s, JS_ATOM_this);
    emit_u16(s, 0);

    emit_label(s, label_return);
    emit_op(s, OP_return);
  } else if (s->cur_func->func_kind != JS_FUNC_NORMAL) {
    if (!hasval) {
      emit_op(s, OP_undefined);
    } else if (s->cur_func->func_kind == JS_FUNC_ASYNC_GENERATOR) {
      emit_op(s, OP_await);
    }
    emit_op(s, OP_return_async);
  } else {
    emit_op(s, hasval ? OP_return : OP_return_undef);
  }
}

/* -- Closure ----------------------------------- */

int add_closure_var(JSContext *ctx, JSFunctionDef *s, BOOL is_local,
                    BOOL is_arg, int var_idx, JSAtom var_name, BOOL is_const,
                    BOOL is_lexical, JSVarKindEnum var_kind) {
  JSClosureVar *cv;

  /* the closure variable indexes are currently stored on 16 bits */
  if (s->closure_var_count >= JS_MAX_LOCAL_VARS) {
    JS_ThrowInternalError(ctx, "too many closure variables");
    return -1;
  }

  if (js_resize_array(ctx, (void **)&s->closure_var, sizeof(s->closure_var[0]),
                      &s->closure_var_size, s->closure_var_count + 1))
    return -1;
  cv = &s->closure_var[s->closure_var_count++];
  cv->is_local = is_local;
  cv->is_arg = is_arg;
  cv->is_const = is_const;
  cv->is_lexical = is_lexical;
  cv->var_kind = var_kind;
  cv->var_idx = var_idx;
  cv->var_name = JS_DupAtom(ctx, var_name);
  return s->closure_var_count - 1;
}

int find_closure_var(JSContext *ctx, JSFunctionDef *s, JSAtom var_name) {
  int i;
  for (i = 0; i < s->closure_var_count; i++) {
    JSClosureVar *cv = &s->closure_var[i];
    if (cv->var_name == var_name)
      return i;
  }
  return -1;
}

/* 'fd' must be a parent of 's'. Create in 's' a closure referencing a
   local variable (is_local = TRUE) or a closure (is_local = FALSE) in
   'fd' */
int get_closure_var2(JSContext *ctx, JSFunctionDef *s, JSFunctionDef *fd,
                     BOOL is_local, BOOL is_arg, int var_idx, JSAtom var_name,
                     BOOL is_const, BOOL is_lexical, JSVarKindEnum var_kind) {
  int i;

  if (fd != s->parent) {
    var_idx = get_closure_var2(ctx, s->parent, fd, is_local, is_arg, var_idx,
                               var_name, is_const, is_lexical, var_kind);
    if (var_idx < 0)
      return -1;
    is_local = FALSE;
  }
  for (i = 0; i < s->closure_var_count; i++) {
    JSClosureVar *cv = &s->closure_var[i];
    if (cv->var_idx == var_idx && cv->is_arg == is_arg &&
        cv->is_local == is_local)
      return i;
  }
  return add_closure_var(ctx, s, is_local, is_arg, var_idx, var_name, is_const,
                         is_lexical, var_kind);
}

int get_closure_var(JSContext *ctx, JSFunctionDef *s, JSFunctionDef *fd,
                    BOOL is_arg, int var_idx, JSAtom var_name, BOOL is_const,
                    BOOL is_lexical, JSVarKindEnum var_kind) {
  return get_closure_var2(ctx, s, fd, TRUE, is_arg, var_idx, var_name, is_const,
                          is_lexical, var_kind);
}

/* XXX: should handle the argument scope generically */
static BOOL is_var_in_arg_scope(const JSVarDef *vd) {
  return (vd->var_name == JS_ATOM_home_object ||
          vd->var_name == JS_ATOM_this_active_func ||
          vd->var_name == JS_ATOM_new_target || vd->var_name == JS_ATOM_this ||
          vd->var_name == JS_ATOM__arg_var_ ||
          vd->var_kind == JS_VAR_FUNCTION_NAME);
}

int add_var_this(JSContext *ctx, JSFunctionDef *fd) {
  int idx;
  idx = add_var(ctx, fd, JS_ATOM_this);
  if (idx >= 0 && fd->is_derived_class_constructor) {
    JSVarDef *vd = &fd->vars[idx];
    /* XXX: should have is_this flag or var type */
    vd->is_lexical = 1; /* used to trigger 'uninitialized' checks
                           in a derived class constructor */
  }
  return idx;
};

void add_eval_variables(JSContext *ctx, JSFunctionDef *s) {
  JSFunctionDef *fd;
  JSVarDef *vd;
  int i, scope_level, scope_idx;
  BOOL has_arguments_binding, has_this_binding, is_arg_scope;

  /* in non strict mode, variables are created in the caller's
     environment object */
  if (!s->is_eval && !(s->js_mode & JS_MODE_STRICT)) {
    s->var_object_idx = add_var(ctx, s, JS_ATOM__var_);
    if (s->has_parameter_expressions) {
      /* an additional variable object is needed for the
         argument scope */
      s->arg_var_object_idx = add_var(ctx, s, JS_ATOM__arg_var_);
    }
  }

  /* eval can potentially use 'arguments' so we must define it */
  has_this_binding = s->has_this_binding;
  if (has_this_binding) {
    if (s->this_var_idx < 0)
      s->this_var_idx = add_var_this(ctx, s);
    if (s->new_target_var_idx < 0)
      s->new_target_var_idx = add_var(ctx, s, JS_ATOM_new_target);
    if (s->is_derived_class_constructor && s->this_active_func_var_idx < 0)
      s->this_active_func_var_idx = add_var(ctx, s, JS_ATOM_this_active_func);
    if (s->has_home_object && s->home_object_var_idx < 0)
      s->home_object_var_idx = add_var(ctx, s, JS_ATOM_home_object);
  }
  has_arguments_binding = s->has_arguments_binding;
  if (has_arguments_binding) {
    add_arguments_var(ctx, s);
    /* also add an arguments binding in the argument scope to
       raise an error if a direct eval in the argument scope tries
       to redefine it */
    if (s->has_parameter_expressions && !(s->js_mode & JS_MODE_STRICT))
      add_arguments_arg(ctx, s);
  }
  if (s->is_func_expr && s->func_name != JS_ATOM_NULL)
    add_func_var(ctx, s, s->func_name);

  /* eval can use all the variables of the enclosing functions, so
     they must be all put in the closure. The closure variables are
     ordered by scope. It works only because no closure are created
     before. */
  assert(s->is_eval || s->closure_var_count == 0);

  /* XXX: inefficient, but eval performance is less critical */
  fd = s;
  for (;;) {
    scope_level = fd->parent_scope_level;
    fd = fd->parent;
    if (!fd)
      break;
    /* add 'this' if it was not previously added */
    if (!has_this_binding && fd->has_this_binding) {
      if (fd->this_var_idx < 0)
        fd->this_var_idx = add_var_this(ctx, fd);
      if (fd->new_target_var_idx < 0)
        fd->new_target_var_idx = add_var(ctx, fd, JS_ATOM_new_target);
      if (fd->is_derived_class_constructor && fd->this_active_func_var_idx < 0)
        fd->this_active_func_var_idx =
            add_var(ctx, fd, JS_ATOM_this_active_func);
      if (fd->has_home_object && fd->home_object_var_idx < 0)
        fd->home_object_var_idx = add_var(ctx, fd, JS_ATOM_home_object);
      has_this_binding = TRUE;
    }
    /* add 'arguments' if it was not previously added */
    if (!has_arguments_binding && fd->has_arguments_binding) {
      add_arguments_var(ctx, fd);
      has_arguments_binding = TRUE;
    }
    /* add function name */
    if (fd->is_func_expr && fd->func_name != JS_ATOM_NULL)
      add_func_var(ctx, fd, fd->func_name);

    /* add lexical variables */
    scope_idx = fd->scopes[scope_level].first;
    while (scope_idx >= 0) {
      vd = &fd->vars[scope_idx];
      vd->is_captured = 1;
      get_closure_var(ctx, s, fd, FALSE, scope_idx, vd->var_name, vd->is_const,
                      vd->is_lexical, vd->var_kind);
      scope_idx = vd->scope_next;
    }
    is_arg_scope = (scope_idx == ARG_SCOPE_END);
    if (!is_arg_scope) {
      /* add unscoped variables */
      for (i = 0; i < fd->arg_count; i++) {
        vd = &fd->args[i];
        if (vd->var_name != JS_ATOM_NULL) {
          get_closure_var(ctx, s, fd, TRUE, i, vd->var_name, FALSE, FALSE,
                          JS_VAR_NORMAL);
        }
      }
      for (i = 0; i < fd->var_count; i++) {
        vd = &fd->vars[i];
        /* do not close top level last result */
        if (vd->scope_level == 0 && vd->var_name != JS_ATOM__ret_ &&
            vd->var_name != JS_ATOM_NULL) {
          get_closure_var(ctx, s, fd, FALSE, i, vd->var_name, FALSE, FALSE,
                          JS_VAR_NORMAL);
        }
      }
    } else {
      for (i = 0; i < fd->var_count; i++) {
        vd = &fd->vars[i];
        /* do not close top level last result */
        if (vd->scope_level == 0 && is_var_in_arg_scope(vd)) {
          get_closure_var(ctx, s, fd, FALSE, i, vd->var_name, FALSE, FALSE,
                          JS_VAR_NORMAL);
        }
      }
    }
    if (fd->is_eval) {
      int idx;
      /* add direct eval variables (we are necessarily at the
         top level) */
      for (idx = 0; idx < fd->closure_var_count; idx++) {
        JSClosureVar *cv = &fd->closure_var[idx];
        get_closure_var2(ctx, s, fd, FALSE, cv->is_arg, idx, cv->var_name,
                         cv->is_const, cv->is_lexical, cv->var_kind);
      }
    }
  }
}

static void set_closure_from_var(JSContext *ctx, JSClosureVar *cv, JSVarDef *vd,
                                 int var_idx) {
  cv->is_local = TRUE;
  cv->is_arg = FALSE;
  cv->is_const = vd->is_const;
  cv->is_lexical = vd->is_lexical;
  cv->var_kind = vd->var_kind;
  cv->var_idx = var_idx;
  cv->var_name = JS_DupAtom(ctx, vd->var_name);
}

/* for direct eval compilation: add references to the variables of the
   calling function */
__exception int add_closure_variables(JSContext *ctx, JSFunctionDef *s,
                                      JSFunctionBytecode *b, int scope_idx) {
  int i, count;
  JSVarDef *vd;
  BOOL is_arg_scope;

  count = b->arg_count + b->var_count + b->closure_var_count;
  s->closure_var = NULL;
  s->closure_var_count = 0;
  s->closure_var_size = count;
  if (count == 0)
    return 0;
  s->closure_var = js_malloc(ctx, sizeof(s->closure_var[0]) * count);
  if (!s->closure_var)
    return -1;
  /* Add lexical variables in scope at the point of evaluation */
  for (i = scope_idx; i >= 0;) {
    vd = &b->vardefs[b->arg_count + i];
    if (vd->scope_level > 0) {
      JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
      set_closure_from_var(ctx, cv, vd, i);
    }
    i = vd->scope_next;
  }
  is_arg_scope = (i == ARG_SCOPE_END);
  if (!is_arg_scope) {
    /* Add argument variables */
    for (i = 0; i < b->arg_count; i++) {
      JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
      vd = &b->vardefs[i];
      cv->is_local = TRUE;
      cv->is_arg = TRUE;
      cv->is_const = FALSE;
      cv->is_lexical = FALSE;
      cv->var_kind = JS_VAR_NORMAL;
      cv->var_idx = i;
      cv->var_name = JS_DupAtom(ctx, vd->var_name);
    }
    /* Add local non lexical variables */
    for (i = 0; i < b->var_count; i++) {
      vd = &b->vardefs[b->arg_count + i];
      if (vd->scope_level == 0 && vd->var_name != JS_ATOM__ret_) {
        JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
        set_closure_from_var(ctx, cv, vd, i);
      }
    }
  } else {
    /* only add pseudo variables */
    for (i = 0; i < b->var_count; i++) {
      vd = &b->vardefs[b->arg_count + i];
      if (vd->scope_level == 0 && is_var_in_arg_scope(vd)) {
        JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
        set_closure_from_var(ctx, cv, vd, i);
      }
    }
  }
  for (i = 0; i < b->closure_var_count; i++) {
    JSClosureVar *cv0 = &b->closure_var[i];
    JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
    cv->is_local = FALSE;
    cv->is_arg = cv0->is_arg;
    cv->is_const = cv0->is_const;
    cv->is_lexical = cv0->is_lexical;
    cv->var_kind = cv0->var_kind;
    cv->var_idx = i;
    cv->var_name = JS_DupAtom(ctx, cv0->var_name);
  }
  return 0;
}

/* -- JSFunctionDef ----------------------------------- */

JSFunctionDef *js_new_function_def(JSContext *ctx, JSFunctionDef *parent,
                                   BOOL is_eval, BOOL is_func_expr,
                                   const char *filename, int line_num) {
  JSFunctionDef *fd;

  fd = js_mallocz(ctx, sizeof(*fd));
  if (!fd)
    return NULL;

  fd->ctx = ctx;
  init_list_head(&fd->child_list);

  /* insert in parent list */
  fd->parent = parent;
  fd->parent_cpool_idx = -1;
  if (parent) {
    list_add_tail(&fd->link, &parent->child_list);
    fd->js_mode = parent->js_mode;
    fd->parent_scope_level = parent->scope_level;
  }

  fd->is_eval = is_eval;
  fd->is_func_expr = is_func_expr;
  js_dbuf_init(ctx, &fd->byte_code);
  fd->last_opcode_pos = -1;
  fd->func_name = JS_ATOM_NULL;
  fd->var_object_idx = -1;
  fd->arg_var_object_idx = -1;
  fd->arguments_var_idx = -1;
  fd->arguments_arg_idx = -1;
  fd->func_var_idx = -1;
  fd->eval_ret_idx = -1;
  fd->this_var_idx = -1;
  fd->new_target_var_idx = -1;
  fd->this_active_func_var_idx = -1;
  fd->home_object_var_idx = -1;

  /* XXX: should distinguish arg, var and var object and body scopes */
  fd->scopes = fd->def_scope_array;
  fd->scope_size = countof(fd->def_scope_array);
  fd->scope_count = 1;
  fd->scopes[0].first = -1;
  fd->scopes[0].parent = -1;
  fd->scope_level = 0; /* 0: var/arg scope */
  fd->scope_first = -1;
  fd->body_scope = -1;

  fd->filename = JS_NewAtom(ctx, filename);
  fd->line_num = line_num;

  js_dbuf_init(ctx, &fd->pc2line);
  // fd->pc2line_last_line_num = line_num;
  // fd->pc2line_last_pc = 0;
  fd->last_opcode_line_num = line_num;

  return fd;
}

void js_free_function_def(JSContext *ctx, JSFunctionDef *fd) {
  int i;
  struct list_head *el, *el1;

  /* free the child functions */
  list_for_each_safe(el, el1, &fd->child_list) {
    JSFunctionDef *fd1;
    fd1 = list_entry(el, JSFunctionDef, link);
    js_free_function_def(ctx, fd1);
  }

  free_bytecode_atoms(ctx->rt, fd->byte_code.buf, fd->byte_code.size,
                      fd->use_short_opcodes);
  dbuf_free(&fd->byte_code);
  js_free(ctx, fd->jump_slots);
  js_free(ctx, fd->label_slots);
  js_free(ctx, fd->line_number_slots);

  for (i = 0; i < fd->cpool_count; i++) {
    JS_FreeValue(ctx, fd->cpool[i]);
  }
  js_free(ctx, fd->cpool);

  JS_FreeAtom(ctx, fd->func_name);

  for (i = 0; i < fd->var_count; i++) {
    JS_FreeAtom(ctx, fd->vars[i].var_name);
  }
  js_free(ctx, fd->vars);
  for (i = 0; i < fd->arg_count; i++) {
    JS_FreeAtom(ctx, fd->args[i].var_name);
  }
  js_free(ctx, fd->args);

  for (i = 0; i < fd->global_var_count; i++) {
    JS_FreeAtom(ctx, fd->global_vars[i].var_name);
  }
  js_free(ctx, fd->global_vars);

  for (i = 0; i < fd->closure_var_count; i++) {
    JSClosureVar *cv = &fd->closure_var[i];
    JS_FreeAtom(ctx, cv->var_name);
  }
  js_free(ctx, fd->closure_var);

  if (fd->scopes != fd->def_scope_array)
    js_free(ctx, fd->scopes);

  JS_FreeAtom(ctx, fd->filename);
  dbuf_free(&fd->pc2line);

  js_free(ctx, fd->source);

  if (fd->parent) {
    /* remove in parent list */
    list_del(&fd->link);
  }
  js_free(ctx, fd);
}

int add_module_variables(JSContext *ctx, JSFunctionDef *fd) {
  int i, idx;
  JSModuleDef *m = fd->module;
  JSExportEntry *me;
  JSGlobalVar *hf;

  /* The imported global variables were added as closure variables
     in js_parse_import(). We add here the module global
     variables. */

  for (i = 0; i < fd->global_var_count; i++) {
    hf = &fd->global_vars[i];
    if (add_closure_var(ctx, fd, TRUE, FALSE, i, hf->var_name, hf->is_const,
                        hf->is_lexical, FALSE) < 0)
      return -1;
  }

  /* resolve the variable names of the local exports */
  for (i = 0; i < m->export_entries_count; i++) {
    me = &m->export_entries[i];
    if (me->export_type == JS_EXPORT_TYPE_LOCAL) {
      idx = find_closure_var(ctx, fd, me->local_name);
      if (idx < 0) {
        JS_ThrowSyntaxErrorAtom(ctx, "exported variable '%s' does not exist",
                                me->local_name);
        return -1;
      }
      me->u.local.var_idx = idx;
    }
  }
  return 0;
}

/* create a function object from a function definition. The function
   definition is freed. All the child functions are also created. It
   must be done this way to resolve all the variables. */
JSValue js_create_function(JSContext *ctx, JSFunctionDef *fd) {
  JSValue func_obj;
  JSFunctionBytecode *b;
  struct list_head *el, *el1;
  int stack_size, scope, idx;
  int function_size, byte_code_offset, cpool_offset;
  int closure_var_offset, vardefs_offset;

  /* recompute scope linkage */
  for (scope = 0; scope < fd->scope_count; scope++) {
    fd->scopes[scope].first = -1;
  }
  if (fd->has_parameter_expressions) {
    /* special end of variable list marker for the argument scope */
    fd->scopes[ARG_SCOPE_INDEX].first = ARG_SCOPE_END;
  }
  for (idx = 0; idx < fd->var_count; idx++) {
    JSVarDef *vd = &fd->vars[idx];
    vd->scope_next = fd->scopes[vd->scope_level].first;
    fd->scopes[vd->scope_level].first = idx;
  }
  for (scope = 2; scope < fd->scope_count; scope++) {
    JSVarScope *sd = &fd->scopes[scope];
    if (sd->first < 0)
      sd->first = fd->scopes[sd->parent].first;
  }
  for (idx = 0; idx < fd->var_count; idx++) {
    JSVarDef *vd = &fd->vars[idx];
    if (vd->scope_next < 0 && vd->scope_level > 1) {
      scope = fd->scopes[vd->scope_level].parent;
      vd->scope_next = fd->scopes[scope].first;
    }
  }

  /* if the function contains an eval call, the closure variables
     are used to compile the eval and they must be ordered by scope,
     so it is necessary to create the closure variables before any
     other variable lookup is done. */
  if (fd->has_eval_call)
    add_eval_variables(ctx, fd);

  /* add the module global variables in the closure */
  if (fd->module) {
    if (add_module_variables(ctx, fd))
      goto fail;
  }

  /* first create all the child functions */
  list_for_each_safe(el, el1, &fd->child_list) {
    JSFunctionDef *fd1;
    int cpool_idx;

    fd1 = list_entry(el, JSFunctionDef, link);
    cpool_idx = fd1->parent_cpool_idx;
    func_obj = js_create_function(ctx, fd1);
    if (JS_IsException(func_obj))
      goto fail;
    /* save it in the constant pool */
    assert(cpool_idx >= 0);
    fd->cpool[cpool_idx] = func_obj;
  }

#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 4)
  if (!(fd->js_mode & JS_MODE_STRIP)) {
    printf("pass 1\n");
    dump_byte_code(ctx, 1, fd->byte_code.buf, fd->byte_code.size, fd->args,
                   fd->arg_count, fd->vars, fd->var_count, fd->closure_var,
                   fd->closure_var_count, fd->cpool, fd->cpool_count,
                   fd->source, fd->line_num, fd->label_slots, NULL);
    printf("\n");
  }
#endif

  if (resolve_variables(ctx, fd))
    goto fail;

#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 2)
  if (!(fd->js_mode & JS_MODE_STRIP)) {
    printf("pass 2\n");
    dump_byte_code(ctx, 2, fd->byte_code.buf, fd->byte_code.size, fd->args,
                   fd->arg_count, fd->vars, fd->var_count, fd->closure_var,
                   fd->closure_var_count, fd->cpool, fd->cpool_count,
                   fd->source, fd->line_num, fd->label_slots, NULL);
    printf("\n");
  }
#endif

  if (resolve_labels(ctx, fd))
    goto fail;

  if (compute_stack_size(ctx, fd, &stack_size) < 0)
    goto fail;

  if (fd->js_mode & JS_MODE_STRIP) {
    function_size = offsetof(JSFunctionBytecode, debug);
  } else {
    function_size = sizeof(*b);
  }
  cpool_offset = function_size;
  function_size += fd->cpool_count * sizeof(*fd->cpool);
  vardefs_offset = function_size;
  if (!(fd->js_mode & JS_MODE_STRIP) || fd->has_eval_call) {
    function_size += (fd->arg_count + fd->var_count) * sizeof(*b->vardefs);
  }
  closure_var_offset = function_size;
  function_size += fd->closure_var_count * sizeof(*fd->closure_var);
  byte_code_offset = function_size;
  function_size += fd->byte_code.size;

  b = js_mallocz(ctx, function_size);
  if (!b)
    goto fail;
  b->header.ref_count = 1;

  b->byte_code_buf = (void *)((uint8_t *)b + byte_code_offset);
  b->byte_code_len = fd->byte_code.size;
  memcpy(b->byte_code_buf, fd->byte_code.buf, fd->byte_code.size);
  js_free(ctx, fd->byte_code.buf);
  fd->byte_code.buf = NULL;

  b->func_name = fd->func_name;
  if (fd->arg_count + fd->var_count > 0) {
    if ((fd->js_mode & JS_MODE_STRIP) && !fd->has_eval_call) {
      /* Strip variable definitions not needed at runtime */
      int i;
      for (i = 0; i < fd->var_count; i++) {
        JS_FreeAtom(ctx, fd->vars[i].var_name);
      }
      for (i = 0; i < fd->arg_count; i++) {
        JS_FreeAtom(ctx, fd->args[i].var_name);
      }
      for (i = 0; i < fd->closure_var_count; i++) {
        JS_FreeAtom(ctx, fd->closure_var[i].var_name);
        fd->closure_var[i].var_name = JS_ATOM_NULL;
      }
    } else {
      b->vardefs = (void *)((uint8_t *)b + vardefs_offset);
      memcpy(b->vardefs, fd->args, fd->arg_count * sizeof(fd->args[0]));
      memcpy(b->vardefs + fd->arg_count, fd->vars,
             fd->var_count * sizeof(fd->vars[0]));
    }
    b->var_count = fd->var_count;
    b->arg_count = fd->arg_count;
    b->defined_arg_count = fd->defined_arg_count;
    js_free(ctx, fd->args);
    js_free(ctx, fd->vars);
  }
  b->cpool_count = fd->cpool_count;
  if (b->cpool_count) {
    b->cpool = (void *)((uint8_t *)b + cpool_offset);
    memcpy(b->cpool, fd->cpool, b->cpool_count * sizeof(*b->cpool));
  }
  js_free(ctx, fd->cpool);
  fd->cpool = NULL;

  b->stack_size = stack_size;

  if (fd->js_mode & JS_MODE_STRIP) {
    JS_FreeAtom(ctx, fd->filename);
    dbuf_free(&fd->pc2line); // probably useless
  } else {
    /* XXX: source and pc2line info should be packed at the end of the
       JSFunctionBytecode structure, avoiding allocation overhead
     */
    b->has_debug = 1;
    b->debug.filename = fd->filename;
    b->debug.line_num = fd->line_num;

    // DynBuf pc2line;
    // compute_pc2line_info(fd, &pc2line);
    // js_free(ctx, fd->line_number_slots)
    b->debug.pc2line_buf = js_realloc(ctx, fd->pc2line.buf, fd->pc2line.size);
    if (!b->debug.pc2line_buf)
      b->debug.pc2line_buf = fd->pc2line.buf;
    b->debug.pc2line_len = fd->pc2line.size;
    b->debug.source = fd->source;
    b->debug.source_len = fd->source_len;
  }
  if (fd->scopes != fd->def_scope_array)
    js_free(ctx, fd->scopes);

  b->closure_var_count = fd->closure_var_count;
  if (b->closure_var_count) {
    b->closure_var = (void *)((uint8_t *)b + closure_var_offset);
    memcpy(b->closure_var, fd->closure_var,
           b->closure_var_count * sizeof(*b->closure_var));
  }
  js_free(ctx, fd->closure_var);
  fd->closure_var = NULL;

  b->has_prototype = fd->has_prototype;
  b->has_simple_parameter_list = fd->has_simple_parameter_list;
  b->js_mode = fd->js_mode;
  b->is_derived_class_constructor = fd->is_derived_class_constructor;
  b->func_kind = fd->func_kind;
  b->need_home_object = (fd->home_object_var_idx >= 0 || fd->need_home_object);
  b->new_target_allowed = fd->new_target_allowed;
  b->super_call_allowed = fd->super_call_allowed;
  b->super_allowed = fd->super_allowed;
  b->arguments_allowed = fd->arguments_allowed;
  b->backtrace_barrier = fd->backtrace_barrier;
  b->realm = JS_DupContext(ctx);

  add_gc_object(ctx->rt, &b->header, JS_GC_OBJ_TYPE_FUNCTION_BYTECODE);

#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 1)
  if (!(fd->js_mode & JS_MODE_STRIP)) {
    js_dump_function_bytecode(ctx, b);
  }
#endif

  if (fd->parent) {
    /* remove from parent list */
    list_del(&fd->link);
  }

  js_free(ctx, fd);
  return JS_MKPTR(JS_TAG_FUNCTION_BYTECODE, b);
fail:
  js_free_function_def(ctx, fd);
  return JS_EXCEPTION;
}
