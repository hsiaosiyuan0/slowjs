#include "parse.h"

/* return the variable index or -1 if not found,
   add ARGUMENT_VAR_OFFSET for argument variables */
int find_arg(JSContext *ctx, JSFunctionDef *fd, JSAtom name) {
  int i;
  for (i = fd->arg_count; i-- > 0;) {
    if (fd->args[i].var_name == name)
      return i | ARGUMENT_VAR_OFFSET;
  }
  return -1;
}

int find_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name) {
  int i;
  for (i = fd->var_count; i-- > 0;) {
    if (fd->vars[i].var_name == name && fd->vars[i].scope_level == 0)
      return i;
  }
  return find_arg(ctx, fd, name);
}

/* find a variable declaration in a given scope */
int find_var_in_scope(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                      int scope_level) {
  int scope_idx;
  for (scope_idx = fd->scopes[scope_level].first; scope_idx >= 0;
       scope_idx = fd->vars[scope_idx].scope_next) {
    if (fd->vars[scope_idx].scope_level != scope_level)
      break;
    if (fd->vars[scope_idx].var_name == name)
      return scope_idx;
  }
  return -1;
}

/* return true if scope == parent_scope or if scope is a child of
   parent_scope */
BOOL is_child_scope(JSContext *ctx, JSFunctionDef *fd, int scope,
                    int parent_scope) {
  while (scope >= 0) {
    if (scope == parent_scope)
      return TRUE;
    scope = fd->scopes[scope].parent;
  }
  return FALSE;
}

/* find a 'var' declaration in the same scope or a child scope */
int find_var_in_child_scope(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                            int scope_level) {
  int i;
  for (i = 0; i < fd->var_count; i++) {
    JSVarDef *vd = &fd->vars[i];
    if (vd->var_name == name && vd->scope_level == 0) {
      if (is_child_scope(ctx, fd, vd->scope_next, scope_level))
        return i;
    }
  }
  return -1;
}

JSGlobalVar *find_global_var(JSFunctionDef *fd, JSAtom name) {
  int i;
  for (i = 0; i < fd->global_var_count; i++) {
    JSGlobalVar *hf = &fd->global_vars[i];
    if (hf->var_name == name)
      return hf;
  }
  return NULL;
}

JSGlobalVar *find_lexical_global_var(JSFunctionDef *fd, JSAtom name) {
  JSGlobalVar *hf = find_global_var(fd, name);
  if (hf && hf->is_lexical)
    return hf;
  else
    return NULL;
}

int find_lexical_decl(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                      int scope_idx, BOOL check_catch_var) {
  while (scope_idx >= 0) {
    JSVarDef *vd = &fd->vars[scope_idx];
    if (vd->var_name == name &&
        (vd->is_lexical || (vd->var_kind == JS_VAR_CATCH && check_catch_var)))
      return scope_idx;
    scope_idx = vd->scope_next;
  }

  if (fd->is_eval && fd->eval_type == JS_EVAL_TYPE_GLOBAL) {
    if (find_lexical_global_var(fd, name))
      return GLOBAL_VAR_OFFSET;
  }
  return -1;
}

int push_scope(JSParseState *s) {
  if (s->cur_func) {
    JSFunctionDef *fd = s->cur_func;
    int scope = fd->scope_count;
    /* XXX: should check for scope overflow */
    if ((fd->scope_count + 1) > fd->scope_size) {
      int new_size;
      size_t slack;
      JSVarScope *new_buf;
      /* XXX: potential arithmetic overflow */
      new_size = max_int(fd->scope_count + 1, fd->scope_size * 3 / 2);
      if (fd->scopes == fd->def_scope_array) {
        new_buf =
            js_realloc2(s->ctx, NULL, new_size * sizeof(*fd->scopes), &slack);
        if (!new_buf)
          return -1;
        memcpy(new_buf, fd->scopes, fd->scope_count * sizeof(*fd->scopes));
      } else {
        new_buf = js_realloc2(s->ctx, fd->scopes,
                              new_size * sizeof(*fd->scopes), &slack);
        if (!new_buf)
          return -1;
      }
      new_size += slack / sizeof(*new_buf);
      fd->scopes = new_buf;
      fd->scope_size = new_size;
    }
    fd->scope_count++;
    fd->scopes[scope].parent = fd->scope_level;
    fd->scopes[scope].first = fd->scope_first;
    emit_op(s, OP_enter_scope);
    emit_u16(s, scope);
    return fd->scope_level = scope;
  }
  return 0;
}

int get_first_lexical_var(JSFunctionDef *fd, int scope) {
  while (scope >= 0) {
    int scope_idx = fd->scopes[scope].first;
    if (scope_idx >= 0)
      return scope_idx;
    scope = fd->scopes[scope].parent;
  }
  return -1;
}

void pop_scope(JSParseState *s) {
  if (s->cur_func) {
    /* disable scoped variables */
    JSFunctionDef *fd = s->cur_func;
    int scope = fd->scope_level;
    emit_op(s, OP_leave_scope);
    emit_u16(s, scope);
    fd->scope_level = fd->scopes[scope].parent;
    fd->scope_first = get_first_lexical_var(fd, fd->scope_level);
  }
}

void close_scopes(JSParseState *s, int scope, int scope_stop) {
  while (scope > scope_stop) {
    emit_op(s, OP_leave_scope);
    emit_u16(s, scope);
    scope = s->cur_func->scopes[scope].parent;
  }
}

/* return the variable index or -1 if error */
int add_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name) {
  JSVarDef *vd;

  /* the local variable indexes are currently stored on 16 bits */
  if (fd->var_count >= JS_MAX_LOCAL_VARS) {
    JS_ThrowInternalError(ctx, "too many local variables");
    return -1;
  }
  if (js_resize_array(ctx, (void **)&fd->vars, sizeof(fd->vars[0]),
                      &fd->var_size, fd->var_count + 1))
    return -1;
  vd = &fd->vars[fd->var_count++];
  memset(vd, 0, sizeof(*vd));
  vd->var_name = JS_DupAtom(ctx, name);
  vd->func_pool_idx = -1;
  return fd->var_count - 1;
}

int add_scope_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                  JSVarKindEnum var_kind) {
  int idx = add_var(ctx, fd, name);
  if (idx >= 0) {
    JSVarDef *vd = &fd->vars[idx];
    vd->var_kind = var_kind;
    vd->scope_level = fd->scope_level;
    vd->scope_next = fd->scope_first;
    fd->scopes[fd->scope_level].first = idx;
    fd->scope_first = idx;
  }
  return idx;
}

int add_func_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name) {
  int idx = fd->func_var_idx;
  if (idx < 0 && (idx = add_var(ctx, fd, name)) >= 0) {
    fd->func_var_idx = idx;
    fd->vars[idx].var_kind = JS_VAR_FUNCTION_NAME;
    if (fd->js_mode & JS_MODE_STRICT)
      fd->vars[idx].is_const = TRUE;
  }
  return idx;
}

int add_arguments_var(JSContext *ctx, JSFunctionDef *fd) {
  int idx = fd->arguments_var_idx;
  if (idx < 0 && (idx = add_var(ctx, fd, JS_ATOM_arguments)) >= 0) {
    fd->arguments_var_idx = idx;
  }
  return idx;
}

/* add an argument definition in the argument scope. Only needed when
   "eval()" may be called in the argument scope. Return 0 if OK. */
int add_arguments_arg(JSContext *ctx, JSFunctionDef *fd) {
  int idx;
  if (fd->arguments_arg_idx < 0) {
    idx = find_var_in_scope(ctx, fd, JS_ATOM_arguments, ARG_SCOPE_INDEX);
    if (idx < 0) {
      /* XXX: the scope links are not fully updated. May be an
         issue if there are child scopes of the argument
         scope */
      idx = add_var(ctx, fd, JS_ATOM_arguments);
      if (idx < 0)
        return -1;
      fd->vars[idx].scope_next = fd->scopes[ARG_SCOPE_INDEX].first;
      fd->scopes[ARG_SCOPE_INDEX].first = idx;
      fd->vars[idx].scope_level = ARG_SCOPE_INDEX;
      fd->vars[idx].is_lexical = TRUE;

      fd->arguments_arg_idx = idx;
    }
  }
  return 0;
}

int add_arg(JSContext *ctx, JSFunctionDef *fd, JSAtom name) {
  JSVarDef *vd;

  /* the local variable indexes are currently stored on 16 bits */
  if (fd->arg_count >= JS_MAX_LOCAL_VARS) {
    JS_ThrowInternalError(ctx, "too many arguments");
    return -1;
  }
  if (js_resize_array(ctx, (void **)&fd->args, sizeof(fd->args[0]),
                      &fd->arg_size, fd->arg_count + 1))
    return -1;
  vd = &fd->args[fd->arg_count++];
  memset(vd, 0, sizeof(*vd));
  vd->var_name = JS_DupAtom(ctx, name);
  vd->func_pool_idx = -1;
  return fd->arg_count - 1;
}

/* add a global variable definition */
JSGlobalVar *add_global_var(JSContext *ctx, JSFunctionDef *s, JSAtom name) {
  JSGlobalVar *hf;

  if (js_resize_array(ctx, (void **)&s->global_vars, sizeof(s->global_vars[0]),
                      &s->global_var_size, s->global_var_count + 1))
    return NULL;
  hf = &s->global_vars[s->global_var_count++];
  hf->cpool_idx = -1;
  hf->force_init = FALSE;
  hf->is_lexical = FALSE;
  hf->is_const = FALSE;
  hf->scope_level = s->scope_level;
  hf->var_name = JS_DupAtom(ctx, name);
  return hf;
}

int define_var(JSParseState *s, JSFunctionDef *fd, JSAtom name,
               JSVarDefEnum var_def_type) {
  JSContext *ctx = s->ctx;
  JSVarDef *vd;
  int idx;

  switch (var_def_type) {
  case JS_VAR_DEF_WITH:
    idx = add_scope_var(ctx, fd, name, JS_VAR_NORMAL);
    break;

  case JS_VAR_DEF_LET:
  case JS_VAR_DEF_CONST:
  case JS_VAR_DEF_FUNCTION_DECL:
  case JS_VAR_DEF_NEW_FUNCTION_DECL:
    idx = find_lexical_decl(ctx, fd, name, fd->scope_first, TRUE);
    if (idx >= 0) {
      if (idx < GLOBAL_VAR_OFFSET) {
        if (fd->vars[idx].scope_level == fd->scope_level) {
          /* same scope: in non strict mode, functions
             can be redefined (annex B.3.3.4). */
          if (!(!(fd->js_mode & JS_MODE_STRICT) &&
                var_def_type == JS_VAR_DEF_FUNCTION_DECL &&
                fd->vars[idx].var_kind == JS_VAR_FUNCTION_DECL)) {
            goto redef_lex_error;
          }
        } else if (fd->vars[idx].var_kind == JS_VAR_CATCH &&
                   (fd->vars[idx].scope_level + 2) == fd->scope_level) {
          goto redef_lex_error;
        }
      } else {
        if (fd->scope_level == fd->body_scope) {
        redef_lex_error:
          /* redefining a scoped var in the same scope: error */
          return js_parse_error(s,
                                "invalid redefinition of lexical identifier");
        }
      }
    }
    if (var_def_type != JS_VAR_DEF_FUNCTION_DECL &&
        var_def_type != JS_VAR_DEF_NEW_FUNCTION_DECL &&
        fd->scope_level == fd->body_scope && find_arg(ctx, fd, name) >= 0) {
      /* lexical variable redefines a parameter name */
      return js_parse_error(s, "invalid redefinition of parameter name");
    }

    if (find_var_in_child_scope(ctx, fd, name, fd->scope_level) >= 0) {
      return js_parse_error(s, "invalid redefinition of a variable");
    }

    if (fd->is_global_var) {
      JSGlobalVar *hf;
      hf = find_global_var(fd, name);
      if (hf && is_child_scope(ctx, fd, hf->scope_level, fd->scope_level)) {
        return js_parse_error(s, "invalid redefinition of global identifier");
      }
    }

    if (fd->is_eval &&
        (fd->eval_type == JS_EVAL_TYPE_GLOBAL ||
         fd->eval_type == JS_EVAL_TYPE_MODULE) &&
        fd->scope_level == fd->body_scope) {
      JSGlobalVar *hf;
      hf = add_global_var(s->ctx, fd, name);
      if (!hf)
        return -1;
      hf->is_lexical = TRUE;
      hf->is_const = (var_def_type == JS_VAR_DEF_CONST);
      idx = GLOBAL_VAR_OFFSET;
    } else {
      JSVarKindEnum var_kind;
      if (var_def_type == JS_VAR_DEF_FUNCTION_DECL)
        var_kind = JS_VAR_FUNCTION_DECL;
      else if (var_def_type == JS_VAR_DEF_NEW_FUNCTION_DECL)
        var_kind = JS_VAR_NEW_FUNCTION_DECL;
      else
        var_kind = JS_VAR_NORMAL;
      idx = add_scope_var(ctx, fd, name, var_kind);
      if (idx >= 0) {
        vd = &fd->vars[idx];
        vd->is_lexical = 1;
        vd->is_const = (var_def_type == JS_VAR_DEF_CONST);
      }
    }
    break;

  case JS_VAR_DEF_CATCH:
    idx = add_scope_var(ctx, fd, name, JS_VAR_CATCH);
    break;

  case JS_VAR_DEF_VAR:
    if (find_lexical_decl(ctx, fd, name, fd->scope_first, FALSE) >= 0) {
    invalid_lexical_redefinition:
      /* error to redefine a var that inside a lexical scope */
      return js_parse_error(s, "invalid redefinition of lexical identifier");
    }
    if (fd->is_global_var) {
      JSGlobalVar *hf;
      hf = find_global_var(fd, name);
      if (hf && hf->is_lexical && hf->scope_level == fd->scope_level &&
          fd->eval_type == JS_EVAL_TYPE_MODULE) {
        goto invalid_lexical_redefinition;
      }
      hf = add_global_var(s->ctx, fd, name);
      if (!hf)
        return -1;
      idx = GLOBAL_VAR_OFFSET;
    } else {
      /* if the variable already exists, don't add it again  */
      idx = find_var(ctx, fd, name);
      if (idx >= 0)
        break;
      idx = add_var(ctx, fd, name);
      if (idx >= 0) {
        if (name == JS_ATOM_arguments && fd->has_arguments_binding)
          fd->arguments_var_idx = idx;
        fd->vars[idx].scope_next = fd->scope_level;
      }
    }
    break;
  default:
    abort();
  }
  return idx;
}

/* XXX: remove */
BOOL has_with_scope(JSFunctionDef *s, int scope_level) {
  /* check if scope chain contains a with statement */
  while (s) {
    int scope_idx = s->scopes[scope_level].first;
    while (scope_idx >= 0) {
      JSVarDef *vd = &s->vars[scope_idx];

      if (vd->var_name == JS_ATOM__with_)
        return TRUE;
      scope_idx = vd->scope_next;
    }
    /* check parent scopes */
    scope_level = s->parent_scope_level;
    s = s->parent;
  }
  return FALSE;
}
