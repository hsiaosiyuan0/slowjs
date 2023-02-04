#include "parse.h"

#include "vm/error.h"
#include "vm/mod.h"

__exception JSAtom js_parse_from_clause(JSParseState *s) {
  JSAtom module_name;
  if (!token_is_pseudo_keyword(s, JS_ATOM_from)) {
    js_parse_error(s, "from clause expected");
    return JS_ATOM_NULL;
  }
  if (next_token(s))
    return JS_ATOM_NULL;
  if (s->token.val != TOK_STRING) {
    js_parse_error(s, "string expected");
    return JS_ATOM_NULL;
  }
  module_name = JS_ValueToAtom(s->ctx, s->token.u.str.str);
  if (module_name == JS_ATOM_NULL)
    return JS_ATOM_NULL;
  if (next_token(s)) {
    JS_FreeAtom(s->ctx, module_name);
    return JS_ATOM_NULL;
  }
  return module_name;
}

JSExportEntry *add_export_entry2(JSContext *ctx, JSParseState *s,
                                 JSModuleDef *m, JSAtom local_name,
                                 JSAtom export_name,
                                 JSExportTypeEnum export_type) {
  JSExportEntry *me;

  if (find_export_entry(ctx, m, export_name)) {
    char buf1[ATOM_GET_STR_BUF_SIZE];
    if (s) {
      js_parse_error(s, "duplicate exported name '%s'",
                     JS_AtomGetStr(ctx, buf1, sizeof(buf1), export_name));
    } else {
      JS_ThrowSyntaxErrorAtom(ctx, "duplicate exported name '%s'", export_name);
    }
    return NULL;
  }

  if (js_resize_array(ctx, (void **)&m->export_entries, sizeof(JSExportEntry),
                      &m->export_entries_size, m->export_entries_count + 1))
    return NULL;
  me = &m->export_entries[m->export_entries_count++];
  memset(me, 0, sizeof(*me));
  me->local_name = JS_DupAtom(ctx, local_name);
  me->export_name = JS_DupAtom(ctx, export_name);
  me->export_type = export_type;
  return me;
}

JSExportEntry *add_export_entry(JSParseState *s, JSModuleDef *m,
                                JSAtom local_name, JSAtom export_name,
                                JSExportTypeEnum export_type) {
  return add_export_entry2(s->ctx, s, m, local_name, export_name, export_type);
}

__exception int js_parse_export(JSParseState *s) {
  JSContext *ctx = s->ctx;
  JSModuleDef *m = s->cur_func->module;
  JSAtom local_name, export_name;
  int first_export, idx, i, tok;
  JSAtom module_name;
  JSExportEntry *me;

  if (next_token(s))
    return -1;

  tok = s->token.val;
  if (tok == TOK_CLASS) {
    return js_parse_class(s, FALSE, JS_PARSE_EXPORT_NAMED);
  } else if (tok == TOK_FUNCTION ||
             (token_is_pseudo_keyword(s, JS_ATOM_async) &&
              peek_token(s, TRUE) == TOK_FUNCTION)) {
    return js_parse_function_decl2(
        s, JS_PARSE_FUNC_STATEMENT, JS_FUNC_NORMAL, JS_ATOM_NULL, s->token.ptr,
        s->token.line_num, JS_PARSE_EXPORT_NAMED, NULL);
  }

  if (next_token(s))
    return -1;

  switch (tok) {
  case '{':
    first_export = m->export_entries_count;
    while (s->token.val != '}') {
      if (!token_is_ident(s->token.val)) {
        js_parse_error(s, "identifier expected");
        return -1;
      }
      local_name = JS_DupAtom(ctx, s->token.u.ident.atom);
      export_name = JS_ATOM_NULL;
      if (next_token(s))
        goto fail;
      if (token_is_pseudo_keyword(s, JS_ATOM_as)) {
        if (next_token(s))
          goto fail;
        if (!token_is_ident(s->token.val)) {
          js_parse_error(s, "identifier expected");
          goto fail;
        }
        export_name = JS_DupAtom(ctx, s->token.u.ident.atom);
        if (next_token(s)) {
        fail:
          JS_FreeAtom(ctx, local_name);
        fail1:
          JS_FreeAtom(ctx, export_name);
          return -1;
        }
      } else {
        export_name = JS_DupAtom(ctx, local_name);
      }
      me =
          add_export_entry(s, m, local_name, export_name, JS_EXPORT_TYPE_LOCAL);
      JS_FreeAtom(ctx, local_name);
      JS_FreeAtom(ctx, export_name);
      if (!me)
        return -1;
      if (s->token.val != ',')
        break;
      if (next_token(s))
        return -1;
    }
    if (js_parse_expect(s, '}'))
      return -1;
    if (token_is_pseudo_keyword(s, JS_ATOM_from)) {
      module_name = js_parse_from_clause(s);
      if (module_name == JS_ATOM_NULL)
        return -1;
      idx = add_req_module_entry(ctx, m, module_name);
      JS_FreeAtom(ctx, module_name);
      if (idx < 0)
        return -1;
      for (i = first_export; i < m->export_entries_count; i++) {
        me = &m->export_entries[i];
        me->export_type = JS_EXPORT_TYPE_INDIRECT;
        me->u.req_module_idx = idx;
      }
    }
    break;
  case '*':
    if (token_is_pseudo_keyword(s, JS_ATOM_as)) {
      /* export ns from */
      if (next_token(s))
        return -1;
      if (!token_is_ident(s->token.val)) {
        js_parse_error(s, "identifier expected");
        return -1;
      }
      export_name = JS_DupAtom(ctx, s->token.u.ident.atom);
      if (next_token(s))
        goto fail1;
      module_name = js_parse_from_clause(s);
      if (module_name == JS_ATOM_NULL)
        goto fail1;
      idx = add_req_module_entry(ctx, m, module_name);
      JS_FreeAtom(ctx, module_name);
      if (idx < 0)
        goto fail1;
      me = add_export_entry(s, m, JS_ATOM__star_, export_name,
                            JS_EXPORT_TYPE_INDIRECT);
      JS_FreeAtom(ctx, export_name);
      if (!me)
        return -1;
      me->u.req_module_idx = idx;
    } else {
      module_name = js_parse_from_clause(s);
      if (module_name == JS_ATOM_NULL)
        return -1;
      idx = add_req_module_entry(ctx, m, module_name);
      JS_FreeAtom(ctx, module_name);
      if (idx < 0)
        return -1;
      if (add_star_export_entry(ctx, m, idx) < 0)
        return -1;
    }
    break;
  case TOK_DEFAULT:
    if (s->token.val == TOK_CLASS) {
      return js_parse_class(s, FALSE, JS_PARSE_EXPORT_DEFAULT);
    } else if (s->token.val == TOK_FUNCTION ||
               (token_is_pseudo_keyword(s, JS_ATOM_async) &&
                peek_token(s, TRUE) == TOK_FUNCTION)) {
      return js_parse_function_decl2(
          s, JS_PARSE_FUNC_STATEMENT, JS_FUNC_NORMAL, JS_ATOM_NULL,
          s->token.ptr, s->token.line_num, JS_PARSE_EXPORT_DEFAULT, NULL);
    } else {
      if (js_parse_assign_expr(s))
        return -1;
    }
    /* set the name of anonymous functions */
    set_object_name(s, JS_ATOM_default);

    /* store the value in the _default_ global variable and export
       it */
    local_name = JS_ATOM__default_;
    if (define_var(s, s->cur_func, local_name, JS_VAR_DEF_LET) < 0)
      return -1;
    emit_op(s, OP_scope_put_var_init);
    emit_atom(s, local_name);
    emit_u16(s, 0);

    if (!add_export_entry(s, m, local_name, JS_ATOM_default,
                          JS_EXPORT_TYPE_LOCAL))
      return -1;
    break;
  case TOK_VAR:
  case TOK_LET:
  case TOK_CONST:
    return js_parse_var(s, TRUE, tok, TRUE);
  default:
    return js_parse_error(s, "invalid export syntax");
  }
  return js_parse_expect_semi(s);
}

static int add_import(JSParseState *s, JSModuleDef *m, JSAtom local_name,
                      JSAtom import_name) {
  JSContext *ctx = s->ctx;
  int i, var_idx;
  JSImportEntry *mi;
  BOOL is_local;

  if (local_name == JS_ATOM_arguments || local_name == JS_ATOM_eval)
    return js_parse_error(s, "invalid import binding");

  if (local_name != JS_ATOM_default) {
    for (i = 0; i < s->cur_func->closure_var_count; i++) {
      if (s->cur_func->closure_var[i].var_name == local_name)
        return js_parse_error(s, "duplicate import binding");
    }
  }

  is_local = (import_name == JS_ATOM__star_);
  var_idx =
      add_closure_var(ctx, s->cur_func, is_local, FALSE,
                      m->import_entries_count, local_name, TRUE, TRUE, FALSE);
  if (var_idx < 0)
    return -1;
  if (js_resize_array(ctx, (void **)&m->import_entries, sizeof(JSImportEntry),
                      &m->import_entries_size, m->import_entries_count + 1))
    return -1;
  mi = &m->import_entries[m->import_entries_count++];
  mi->import_name = JS_DupAtom(ctx, import_name);
  mi->var_idx = var_idx;
  return 0;
}

__exception int js_parse_import(JSParseState *s) {
  JSContext *ctx = s->ctx;
  JSModuleDef *m = s->cur_func->module;
  JSAtom local_name, import_name, module_name;
  int first_import, i, idx;

  if (next_token(s))
    return -1;

  first_import = m->import_entries_count;
  if (s->token.val == TOK_STRING) {
    module_name = JS_ValueToAtom(ctx, s->token.u.str.str);
    if (module_name == JS_ATOM_NULL)
      return -1;
    if (next_token(s)) {
      JS_FreeAtom(ctx, module_name);
      return -1;
    }
  } else {
    if (s->token.val == TOK_IDENT) {
      if (s->token.u.ident.is_reserved) {
        return js_parse_error_reserved_identifier(s);
      }
      /* "default" import */
      local_name = JS_DupAtom(ctx, s->token.u.ident.atom);
      import_name = JS_ATOM_default;
      if (next_token(s))
        goto fail;
      if (add_import(s, m, local_name, import_name))
        goto fail;
      JS_FreeAtom(ctx, local_name);

      if (s->token.val != ',')
        goto end_import_clause;
      if (next_token(s))
        return -1;
    }

    if (s->token.val == '*') {
      /* name space import */
      if (next_token(s))
        return -1;
      if (!token_is_pseudo_keyword(s, JS_ATOM_as))
        return js_parse_error(s, "expecting 'as'");
      if (next_token(s))
        return -1;
      if (!token_is_ident(s->token.val)) {
        js_parse_error(s, "identifier expected");
        return -1;
      }
      local_name = JS_DupAtom(ctx, s->token.u.ident.atom);
      import_name = JS_ATOM__star_;
      if (next_token(s))
        goto fail;
      if (add_import(s, m, local_name, import_name))
        goto fail;
      JS_FreeAtom(ctx, local_name);
    } else if (s->token.val == '{') {
      if (next_token(s))
        return -1;

      while (s->token.val != '}') {
        if (!token_is_ident(s->token.val)) {
          js_parse_error(s, "identifier expected");
          return -1;
        }
        import_name = JS_DupAtom(ctx, s->token.u.ident.atom);
        local_name = JS_ATOM_NULL;
        if (next_token(s))
          goto fail;
        if (token_is_pseudo_keyword(s, JS_ATOM_as)) {
          if (next_token(s))
            goto fail;
          if (!token_is_ident(s->token.val)) {
            js_parse_error(s, "identifier expected");
            goto fail;
          }
          local_name = JS_DupAtom(ctx, s->token.u.ident.atom);
          if (next_token(s)) {
          fail:
            JS_FreeAtom(ctx, local_name);
            JS_FreeAtom(ctx, import_name);
            return -1;
          }
        } else {
          local_name = JS_DupAtom(ctx, import_name);
        }
        if (add_import(s, m, local_name, import_name))
          goto fail;
        JS_FreeAtom(ctx, local_name);
        JS_FreeAtom(ctx, import_name);
        if (s->token.val != ',')
          break;
        if (next_token(s))
          return -1;
      }
      if (js_parse_expect(s, '}'))
        return -1;
    }
  end_import_clause:
    module_name = js_parse_from_clause(s);
    if (module_name == JS_ATOM_NULL)
      return -1;
  }
  idx = add_req_module_entry(ctx, m, module_name);
  JS_FreeAtom(ctx, module_name);
  if (idx < 0)
    return -1;
  for (i = first_import; i < m->import_entries_count; i++)
    m->import_entries[i].req_module_idx = idx;

  return js_parse_expect_semi(s);
}
