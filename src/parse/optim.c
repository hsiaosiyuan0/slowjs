#include "parse.h"

#include "utils/dbuf.h"
#include "vm/error.h"
#include "vm/vm.h"

/* 'op' is only used for error indication */
static __exception int ss_check(JSContext *ctx, StackSizeState *s, int pos,
                                int op, int stack_len) {
  if ((unsigned)pos >= s->bc_len) {
    JS_ThrowInternalError(ctx, "bytecode buffer overflow (op=%d, pc=%d)", op,
                          pos);
    return -1;
  }
  if (stack_len > s->stack_len_max) {
    s->stack_len_max = stack_len;
    if (s->stack_len_max > JS_STACK_SIZE_MAX) {
      JS_ThrowInternalError(ctx, "stack overflow (op=%d, pc=%d)", op, pos);
      return -1;
    }
  }
  if (s->stack_level_tab[pos] != 0xffff) {
    /* already explored: check that the stack size is consistent */
    if (s->stack_level_tab[pos] != stack_len) {
      JS_ThrowInternalError(ctx, "unconsistent stack size: %d %d (pc=%d)",
                            s->stack_level_tab[pos], stack_len, pos);
      return -1;
    } else {
      return 0;
    }
  }

  /* mark as explored and store the stack size */
  s->stack_level_tab[pos] = stack_len;

  /* queue the new PC to explore */
  if (js_resize_array(ctx, (void **)&s->pc_stack, sizeof(s->pc_stack[0]),
                      &s->pc_stack_size, s->pc_stack_len + 1))
    return -1;
  s->pc_stack[s->pc_stack_len++] = pos;
  return 0;
}

__exception int compute_stack_size(JSContext *ctx, JSFunctionDef *fd,
                                   int *pstack_size) {
  StackSizeState s_s, *s = &s_s;
  int i, diff, n_pop, pos_next, stack_len, pos, op;
  const JSOpCode *oi;
  const uint8_t *bc_buf;

  bc_buf = fd->byte_code.buf;
  s->bc_len = fd->byte_code.size;
  /* bc_len > 0 */
  s->stack_level_tab =
      js_malloc(ctx, sizeof(s->stack_level_tab[0]) * s->bc_len);
  if (!s->stack_level_tab)
    return -1;
  for (i = 0; i < s->bc_len; i++)
    s->stack_level_tab[i] = 0xffff;
  s->stack_len_max = 0;
  s->pc_stack = NULL;
  s->pc_stack_len = 0;
  s->pc_stack_size = 0;

  /* breadth-first graph exploration */
  if (ss_check(ctx, s, 0, OP_invalid, 0))
    goto fail;

  while (s->pc_stack_len > 0) {
    pos = s->pc_stack[--s->pc_stack_len];
    stack_len = s->stack_level_tab[pos];
    op = bc_buf[pos];
    if (op == 0 || op >= OP_COUNT) {
      JS_ThrowInternalError(ctx, "invalid opcode (op=%d, pc=%d)", op, pos);
      goto fail;
    }
    oi = &short_opcode_info(op);
    pos_next = pos + oi->size;
    if (pos_next > s->bc_len) {
      JS_ThrowInternalError(ctx, "bytecode buffer overflow (op=%d, pc=%d)", op,
                            pos);
      goto fail;
    }
    n_pop = oi->n_pop;
    /* call pops a variable number of arguments */
    if (oi->fmt == OP_FMT_npop || oi->fmt == OP_FMT_npop_u16) {
      n_pop += get_u16(bc_buf + pos + 1);
    } else {
#if SHORT_OPCODES
      if (oi->fmt == OP_FMT_npopx) {
        n_pop += op - OP_call0;
      }
#endif
    }

    if (stack_len < n_pop) {
      JS_ThrowInternalError(ctx, "stack underflow (op=%d, pc=%d)", op, pos);
      goto fail;
    }
    stack_len += oi->n_push - n_pop;
    if (stack_len > s->stack_len_max) {
      s->stack_len_max = stack_len;
      if (s->stack_len_max > JS_STACK_SIZE_MAX) {
        JS_ThrowInternalError(ctx, "stack overflow (op=%d, pc=%d)", op, pos);
        goto fail;
      }
    }
    switch (op) {
    case OP_tail_call:
    case OP_tail_call_method:
    case OP_return:
    case OP_return_undef:
    case OP_return_async:
    case OP_throw:
    case OP_throw_error:
    case OP_ret:
      goto done_insn;
    case OP_goto:
      diff = get_u32(bc_buf + pos + 1);
      pos_next = pos + 1 + diff;
      break;
#if SHORT_OPCODES
    case OP_goto16:
      diff = (int16_t)get_u16(bc_buf + pos + 1);
      pos_next = pos + 1 + diff;
      break;
    case OP_goto8:
      diff = (int8_t)bc_buf[pos + 1];
      pos_next = pos + 1 + diff;
      break;
    case OP_if_true8:
    case OP_if_false8:
      diff = (int8_t)bc_buf[pos + 1];
      if (ss_check(ctx, s, pos + 1 + diff, op, stack_len))
        goto fail;
      break;
#endif
    case OP_if_true:
    case OP_if_false:
    case OP_catch:
      diff = get_u32(bc_buf + pos + 1);
      if (ss_check(ctx, s, pos + 1 + diff, op, stack_len))
        goto fail;
      break;
    case OP_gosub:
      diff = get_u32(bc_buf + pos + 1);
      if (ss_check(ctx, s, pos + 1 + diff, op, stack_len + 1))
        goto fail;
      break;
    case OP_with_get_var:
    case OP_with_delete_var:
      diff = get_u32(bc_buf + pos + 5);
      if (ss_check(ctx, s, pos + 5 + diff, op, stack_len + 1))
        goto fail;
      break;
    case OP_with_make_ref:
    case OP_with_get_ref:
    case OP_with_get_ref_undef:
      diff = get_u32(bc_buf + pos + 5);
      if (ss_check(ctx, s, pos + 5 + diff, op, stack_len + 2))
        goto fail;
      break;
    case OP_with_put_var:
      diff = get_u32(bc_buf + pos + 5);
      if (ss_check(ctx, s, pos + 5 + diff, op, stack_len - 1))
        goto fail;
      break;

    default:
      break;
    }
    if (ss_check(ctx, s, pos_next, op, stack_len))
      goto fail;
  done_insn:;
  }
  js_free(ctx, s->stack_level_tab);
  js_free(ctx, s->pc_stack);
  *pstack_size = s->stack_len_max;
  return 0;
fail:
  js_free(ctx, s->stack_level_tab);
  js_free(ctx, s->pc_stack);
  *pstack_size = 0;
  return -1;
}

static int optimize_scope_make_ref(JSContext *ctx, JSFunctionDef *s, DynBuf *bc,
                                   uint8_t *bc_buf, LabelSlot *ls, int pos_next,
                                   int get_op, int var_idx) {
  int label_pos, end_pos, pos;

  /* XXX: should optimize `loc(a) += expr` as `expr add_loc(a)`
     but only if expr does not modify `a`.
     should scan the code between pos_next and label_pos
     for operations that can potentially change `a`:
     OP_scope_make_ref(a), function calls, jumps and gosub.
   */
  /* replace the reference get/put with normal variable
     accesses */
  if (bc_buf[pos_next] == OP_get_ref_value) {
    dbuf_putc(bc, get_op);
    dbuf_put_u16(bc, var_idx);
    pos_next++;
  }
  /* remove the OP_label to make room for replacement */
  /* label should have a refcount of 0 anyway */

  /* XXX: should avoid this patch by inserting nops in phase 1 */
  label_pos = ls->pos;
  pos = label_pos - 5;
  assert(bc_buf[pos] == OP_label);
  /* label points to an instruction pair:
     - insert3 / put_ref_value
     - perm4 / put_ref_value
     - rot3l / put_ref_value
     - nop / put_ref_value
   */
  end_pos = label_pos + 2;
  if (bc_buf[label_pos] == OP_insert3)
    bc_buf[pos++] = OP_dup;
  bc_buf[pos] = get_op + 1;
  put_u16(bc_buf + pos + 1, var_idx);

  pos += 3;
  /* pad with OP_nop */
  while (pos < end_pos)
    bc_buf[pos++] = OP_nop;
  return pos_next;
}

static int optimize_scope_make_global_ref(JSContext *ctx, JSFunctionDef *s,
                                          DynBuf *bc, uint8_t *bc_buf,
                                          LabelSlot *ls, int pos_next,
                                          JSAtom var_name) {
  int label_pos, end_pos, pos, op;
  BOOL is_strict;
  is_strict = ((s->js_mode & JS_MODE_STRICT) != 0);

  /* replace the reference get/put with normal variable
     accesses */
  if (is_strict) {
    /* need to check if the variable exists before evaluating the right
       expression */
    /* XXX: need an extra OP_true if destructuring an array */
    dbuf_putc(bc, OP_check_var);
    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
  } else {
    /* XXX: need 2 extra OP_true if destructuring an array */
  }
  if (bc_buf[pos_next] == OP_get_ref_value) {
    dbuf_putc(bc, OP_get_var);
    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
    pos_next++;
  }
  /* remove the OP_label to make room for replacement */
  /* label should have a refcount of 0 anyway */
  /* XXX: should have emitted several OP_nop to avoid this kludge */
  label_pos = ls->pos;
  pos = label_pos - 5;
  assert(bc_buf[pos] == OP_label);
  end_pos = label_pos + 2;
  op = bc_buf[label_pos];
  if (is_strict) {
    if (op != OP_nop) {
      switch (op) {
      case OP_insert3:
        op = OP_insert2;
        break;
      case OP_perm4:
        op = OP_perm3;
        break;
      case OP_rot3l:
        op = OP_swap;
        break;
      default:
        abort();
      }
      bc_buf[pos++] = op;
    }
  } else {
    if (op == OP_insert3)
      bc_buf[pos++] = OP_dup;
  }
  if (is_strict) {
    bc_buf[pos] = OP_put_var_strict;
    /* XXX: need 1 extra OP_drop if destructuring an array */
  } else {
    bc_buf[pos] = OP_put_var;
    /* XXX: need 2 extra OP_drop if destructuring an array */
  }
  put_u32(bc_buf + pos + 1, JS_DupAtom(ctx, var_name));
  pos += 5;
  /* pad with OP_nop */
  while (pos < end_pos)
    bc_buf[pos++] = OP_nop;
  return pos_next;
}

static int resolve_pseudo_var(JSContext *ctx, JSFunctionDef *s,
                              JSAtom var_name) {
  int var_idx;

  if (!s->has_this_binding)
    return -1;
  switch (var_name) {
  case JS_ATOM_home_object:
    /* 'home_object' pseudo variable */
    if (s->home_object_var_idx < 0)
      s->home_object_var_idx = add_var(ctx, s, var_name);
    var_idx = s->home_object_var_idx;
    break;
  case JS_ATOM_this_active_func:
    /* 'this.active_func' pseudo variable */
    if (s->this_active_func_var_idx < 0)
      s->this_active_func_var_idx = add_var(ctx, s, var_name);
    var_idx = s->this_active_func_var_idx;
    break;
  case JS_ATOM_new_target:
    /* 'new.target' pseudo variable */
    if (s->new_target_var_idx < 0)
      s->new_target_var_idx = add_var(ctx, s, var_name);
    var_idx = s->new_target_var_idx;
    break;
  case JS_ATOM_this:
    /* 'this' pseudo variable */
    if (s->this_var_idx < 0)
      s->this_var_idx = add_var_this(ctx, s);
    var_idx = s->this_var_idx;
    break;
  default:
    var_idx = -1;
    break;
  }
  return var_idx;
}

static BOOL can_opt_put_ref_value(const uint8_t *bc_buf, int pos) {
  int opcode = bc_buf[pos];
  return (bc_buf[pos + 1] == OP_put_ref_value &&
          (opcode == OP_insert3 || opcode == OP_perm4 || opcode == OP_nop ||
           opcode == OP_rot3l));
}

static BOOL can_opt_put_global_ref_value(const uint8_t *bc_buf, int pos) {
  int opcode = bc_buf[pos];
  return (bc_buf[pos + 1] == OP_put_ref_value &&
          (opcode == OP_insert3 || opcode == OP_perm4 || opcode == OP_nop ||
           opcode == OP_rot3l));
}

static int get_with_scope_opcode(int op) {
  if (op == OP_scope_get_var_undef)
    return OP_with_get_var;
  else
    return OP_with_get_var + (op - OP_scope_get_var);
}

/* test if 'var_name' is in the variable object on the stack. If is it
   the case, handle it and jump to 'label_done' */
static void var_object_test(JSContext *ctx, JSFunctionDef *s, JSAtom var_name,
                            int op, DynBuf *bc, int *plabel_done,
                            BOOL is_with) {
  dbuf_putc(bc, get_with_scope_opcode(op));
  dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
  *plabel_done = new_label_fd(s, *plabel_done);
  dbuf_put_u32(bc, *plabel_done);
  dbuf_putc(bc, is_with);
  update_label(s, *plabel_done, 1);
  s->jump_size++;
}

static int resolve_scope_var(JSContext *ctx, JSFunctionDef *s, JSAtom var_name,
                             int scope_level, int op, DynBuf *bc,
                             uint8_t *bc_buf, LabelSlot *ls, int pos_next) {
  int idx, var_idx, is_put;
  int label_done;
  JSFunctionDef *fd;
  JSVarDef *vd;
  BOOL is_pseudo_var, is_arg_scope;

  label_done = -1;

  /* XXX: could be simpler to use a specific function to
     resolve the pseudo variables */
  is_pseudo_var = (var_name == JS_ATOM_home_object ||
                   var_name == JS_ATOM_this_active_func ||
                   var_name == JS_ATOM_new_target || var_name == JS_ATOM_this);

  /* resolve local scoped variables */
  var_idx = -1;
  int idx2 = s->scopes[scope_level].first;
  for (idx = s->scopes[scope_level].first; idx >= 0;) {
    vd = &s->vars[idx];
    if (vd->var_name == var_name) {
      if (op == OP_scope_put_var || op == OP_scope_make_ref) {
        if (vd->is_const) {
          dbuf_putc(bc, OP_throw_error);
          dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
          dbuf_putc(bc, JS_THROW_VAR_RO);
          goto done;
        }
      }
      var_idx = idx;
      break;
    } else if (vd->var_name == JS_ATOM__with_ && !is_pseudo_var) {
      dbuf_putc(bc, OP_get_loc);
      dbuf_put_u16(bc, idx);
      var_object_test(ctx, s, var_name, op, bc, &label_done, 1);
    }
    idx = vd->scope_next;
  }
  is_arg_scope = (idx == ARG_SCOPE_END);
  if (var_idx < 0) {
    /* argument scope: variables are not visible but pseudo
       variables are visible */
    if (!is_arg_scope) {
      var_idx = find_var(ctx, s, var_name);
    }

    if (var_idx < 0 && is_pseudo_var)
      var_idx = resolve_pseudo_var(ctx, s, var_name);

    if (var_idx < 0 && var_name == JS_ATOM_arguments &&
        s->has_arguments_binding) {
      /* 'arguments' pseudo variable */
      var_idx = add_arguments_var(ctx, s);
    }
    if (var_idx < 0 && s->is_func_expr && var_name == s->func_name) {
      /* add a new variable with the function name */
      var_idx = add_func_var(ctx, s, var_name);
    }
  }
  if (var_idx >= 0) {
    if ((op == OP_scope_put_var || op == OP_scope_make_ref) &&
        !(var_idx & ARGUMENT_VAR_OFFSET) && s->vars[var_idx].is_const) {
      /* only happens when assigning a function expression name
         in strict mode */
      dbuf_putc(bc, OP_throw_error);
      dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
      dbuf_putc(bc, JS_THROW_VAR_RO);
      goto done;
    }
    /* OP_scope_put_var_init is only used to initialize a
       lexical variable, so it is never used in a with or var object. It
       can be used with a closure (module global variable case). */
    switch (op) {
    case OP_scope_make_ref:
      if (!(var_idx & ARGUMENT_VAR_OFFSET) &&
          s->vars[var_idx].var_kind == JS_VAR_FUNCTION_NAME) {
        /* Create a dummy object reference for the func_var */
        dbuf_putc(bc, OP_object);
        dbuf_putc(bc, OP_get_loc);
        dbuf_put_u16(bc, var_idx);
        dbuf_putc(bc, OP_define_field);
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        dbuf_putc(bc, OP_push_atom_value);
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
      } else if (label_done == -1 && can_opt_put_ref_value(bc_buf, ls->pos)) {
        int get_op;
        if (var_idx & ARGUMENT_VAR_OFFSET) {
          get_op = OP_get_arg;
          var_idx -= ARGUMENT_VAR_OFFSET;
        } else {
          if (s->vars[var_idx].is_lexical)
            get_op = OP_get_loc_check;
          else
            get_op = OP_get_loc;
        }
        pos_next = optimize_scope_make_ref(ctx, s, bc, bc_buf, ls, pos_next,
                                           get_op, var_idx);
      } else {
        /* Create a dummy object with a named slot that is
           a reference to the local variable */
        if (var_idx & ARGUMENT_VAR_OFFSET) {
          dbuf_putc(bc, OP_make_arg_ref);
          dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
          dbuf_put_u16(bc, var_idx - ARGUMENT_VAR_OFFSET);
        } else {
          dbuf_putc(bc, OP_make_loc_ref);
          dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
          dbuf_put_u16(bc, var_idx);
        }
      }
      break;
    case OP_scope_get_ref:
      dbuf_putc(bc, OP_undefined);
      /* fall thru */
    case OP_scope_get_var_undef:
    case OP_scope_get_var:
    case OP_scope_put_var:
    case OP_scope_put_var_init:
      is_put = (op == OP_scope_put_var || op == OP_scope_put_var_init);
      if (var_idx & ARGUMENT_VAR_OFFSET) {
        dbuf_putc(bc, OP_get_arg + is_put);
        dbuf_put_u16(bc, var_idx - ARGUMENT_VAR_OFFSET);
      } else {
        if (is_put) {
          if (s->vars[var_idx].is_lexical) {
            if (op == OP_scope_put_var_init) {
              /* 'this' can only be initialized once */
              if (var_name == JS_ATOM_this)
                dbuf_putc(bc, OP_put_loc_check_init);
              else
                dbuf_putc(bc, OP_put_loc);
            } else {
              dbuf_putc(bc, OP_put_loc_check);
            }
          } else {
            dbuf_putc(bc, OP_put_loc);
          }
        } else {
          if (s->vars[var_idx].is_lexical) {
            dbuf_putc(bc, OP_get_loc_check);
          } else {
            dbuf_putc(bc, OP_get_loc);
          }
        }
        dbuf_put_u16(bc, var_idx);
      }
      break;
    case OP_scope_delete_var:
      dbuf_putc(bc, OP_push_false);
      break;
    }
    goto done;
  }
  /* check eval object */
  if (!is_arg_scope && s->var_object_idx >= 0 && !is_pseudo_var) {
    dbuf_putc(bc, OP_get_loc);
    dbuf_put_u16(bc, s->var_object_idx);
    var_object_test(ctx, s, var_name, op, bc, &label_done, 0);
  }
  /* check eval object in argument scope */
  if (s->arg_var_object_idx >= 0 && !is_pseudo_var) {
    dbuf_putc(bc, OP_get_loc);
    dbuf_put_u16(bc, s->arg_var_object_idx);
    var_object_test(ctx, s, var_name, op, bc, &label_done, 0);
  }

  /* check parent scopes */
  for (fd = s; fd->parent;) {
    scope_level = fd->parent_scope_level;
    fd = fd->parent;
    for (idx = fd->scopes[scope_level].first; idx >= 0;) {
      vd = &fd->vars[idx];
      if (vd->var_name == var_name) {
        if (op == OP_scope_put_var || op == OP_scope_make_ref) {
          if (vd->is_const) {
            dbuf_putc(bc, OP_throw_error);
            dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
            dbuf_putc(bc, JS_THROW_VAR_RO);
            goto done;
          }
        }
        var_idx = idx;
        break;
      } else if (vd->var_name == JS_ATOM__with_ && !is_pseudo_var) {
        vd->is_captured = 1;
        idx = get_closure_var(ctx, s, fd, FALSE, idx, vd->var_name, FALSE,
                              FALSE, JS_VAR_NORMAL);
        if (idx >= 0) {
          dbuf_putc(bc, OP_get_var_ref);
          dbuf_put_u16(bc, idx);
          var_object_test(ctx, s, var_name, op, bc, &label_done, 1);
        }
      }
      idx = vd->scope_next;
    }
    is_arg_scope = (idx == ARG_SCOPE_END);
    if (var_idx >= 0)
      break;

    if (!is_arg_scope) {
      var_idx = find_var(ctx, fd, var_name);
      if (var_idx >= 0)
        break;
    }
    if (is_pseudo_var) {
      var_idx = resolve_pseudo_var(ctx, fd, var_name);
      if (var_idx >= 0)
        break;
    }
    if (var_name == JS_ATOM_arguments && fd->has_arguments_binding) {
      var_idx = add_arguments_var(ctx, fd);
      break;
    }
    if (fd->is_func_expr && fd->func_name == var_name) {
      /* add a new variable with the function name */
      var_idx = add_func_var(ctx, fd, var_name);
      break;
    }

    /* check eval object */
    if (!is_arg_scope && fd->var_object_idx >= 0 && !is_pseudo_var) {
      vd = &fd->vars[fd->var_object_idx];
      vd->is_captured = 1;
      idx = get_closure_var(ctx, s, fd, FALSE, fd->var_object_idx, vd->var_name,
                            FALSE, FALSE, JS_VAR_NORMAL);
      dbuf_putc(bc, OP_get_var_ref);
      dbuf_put_u16(bc, idx);
      var_object_test(ctx, s, var_name, op, bc, &label_done, 0);
    }

    /* check eval object in argument scope */
    if (fd->arg_var_object_idx >= 0 && !is_pseudo_var) {
      vd = &fd->vars[fd->arg_var_object_idx];
      vd->is_captured = 1;
      idx = get_closure_var(ctx, s, fd, FALSE, fd->arg_var_object_idx,
                            vd->var_name, FALSE, FALSE, JS_VAR_NORMAL);
      dbuf_putc(bc, OP_get_var_ref);
      dbuf_put_u16(bc, idx);
      var_object_test(ctx, s, var_name, op, bc, &label_done, 0);
    }

    if (fd->is_eval)
      break; /* it it necessarily the top level function */
  }

  /* check direct eval scope (in the closure of the eval function
     which is necessarily at the top level) */
  if (!fd)
    fd = s;
  if (var_idx < 0 && fd->is_eval) {
    int idx1;
    for (idx1 = 0; idx1 < fd->closure_var_count; idx1++) {
      JSClosureVar *cv = &fd->closure_var[idx1];
      if (var_name == cv->var_name) {
        if (fd != s) {
          idx = get_closure_var2(ctx, s, fd, FALSE, cv->is_arg, idx1,
                                 cv->var_name, cv->is_const, cv->is_lexical,
                                 cv->var_kind);
        } else {
          idx = idx1;
        }
        goto has_idx;
      } else if ((cv->var_name == JS_ATOM__var_ ||
                  cv->var_name == JS_ATOM__arg_var_ ||
                  cv->var_name == JS_ATOM__with_) &&
                 !is_pseudo_var) {
        int is_with = (cv->var_name == JS_ATOM__with_);
        if (fd != s) {
          idx = get_closure_var2(ctx, s, fd, FALSE, cv->is_arg, idx1,
                                 cv->var_name, FALSE, FALSE, JS_VAR_NORMAL);
        } else {
          idx = idx1;
        }
        dbuf_putc(bc, OP_get_var_ref);
        dbuf_put_u16(bc, idx);
        var_object_test(ctx, s, var_name, op, bc, &label_done, is_with);
      }
    }
  }

  if (var_idx >= 0) {
    /* find the corresponding closure variable */
    if (var_idx & ARGUMENT_VAR_OFFSET) {
      fd->args[var_idx - ARGUMENT_VAR_OFFSET].is_captured = 1;
      idx = get_closure_var(ctx, s, fd, TRUE, var_idx - ARGUMENT_VAR_OFFSET,
                            var_name, FALSE, FALSE, JS_VAR_NORMAL);
    } else {
      fd->vars[var_idx].is_captured = 1;
      idx = get_closure_var(
          ctx, s, fd, FALSE, var_idx, var_name, fd->vars[var_idx].is_const,
          fd->vars[var_idx].is_lexical, fd->vars[var_idx].var_kind);
    }
    if (idx >= 0) {
    has_idx:
      if ((op == OP_scope_put_var || op == OP_scope_make_ref) &&
          s->closure_var[idx].is_const) {
        dbuf_putc(bc, OP_throw_error);
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        dbuf_putc(bc, JS_THROW_VAR_RO);
        goto done;
      }
      switch (op) {
      case OP_scope_make_ref:
        if (s->closure_var[idx].var_kind == JS_VAR_FUNCTION_NAME) {
          /* Create a dummy object reference for the func_var */
          dbuf_putc(bc, OP_object);
          dbuf_putc(bc, OP_get_var_ref);
          dbuf_put_u16(bc, idx);
          dbuf_putc(bc, OP_define_field);
          dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
          dbuf_putc(bc, OP_push_atom_value);
          dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        } else if (label_done == -1 && can_opt_put_ref_value(bc_buf, ls->pos)) {
          int get_op;
          if (s->closure_var[idx].is_lexical)
            get_op = OP_get_var_ref_check;
          else
            get_op = OP_get_var_ref;
          pos_next = optimize_scope_make_ref(ctx, s, bc, bc_buf, ls, pos_next,
                                             get_op, idx);
        } else {
          /* Create a dummy object with a named slot that is
             a reference to the closure variable */
          dbuf_putc(bc, OP_make_var_ref_ref);
          dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
          dbuf_put_u16(bc, idx);
        }
        break;
      case OP_scope_get_ref:
        /* XXX: should create a dummy object with a named slot that is
           a reference to the closure variable */
        dbuf_putc(bc, OP_undefined);
        /* fall thru */
      case OP_scope_get_var_undef:
      case OP_scope_get_var:
      case OP_scope_put_var:
      case OP_scope_put_var_init:
        is_put = (op == OP_scope_put_var || op == OP_scope_put_var_init);
        if (is_put) {
          if (s->closure_var[idx].is_lexical) {
            if (op == OP_scope_put_var_init) {
              /* 'this' can only be initialized once */
              if (var_name == JS_ATOM_this)
                dbuf_putc(bc, OP_put_var_ref_check_init);
              else
                dbuf_putc(bc, OP_put_var_ref);
            } else {
              dbuf_putc(bc, OP_put_var_ref_check);
            }
          } else {
            dbuf_putc(bc, OP_put_var_ref);
          }
        } else {
          if (s->closure_var[idx].is_lexical) {
            dbuf_putc(bc, OP_get_var_ref_check);
          } else {
            dbuf_putc(bc, OP_get_var_ref);
          }
        }
        dbuf_put_u16(bc, idx);
        break;
      case OP_scope_delete_var:
        dbuf_putc(bc, OP_push_false);
        break;
      }
      goto done;
    }
  }

  /* global variable access */

  switch (op) {
  case OP_scope_make_ref:
    if (label_done == -1 && can_opt_put_global_ref_value(bc_buf, ls->pos)) {
      pos_next = optimize_scope_make_global_ref(ctx, s, bc, bc_buf, ls,
                                                pos_next, var_name);
    } else {
      dbuf_putc(bc, OP_make_var_ref);
      dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
    }
    break;
  case OP_scope_get_ref:
    /* XXX: should create a dummy object with a named slot that is
       a reference to the global variable */
    dbuf_putc(bc, OP_undefined);
    dbuf_putc(bc, OP_get_var);
    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
    break;
  case OP_scope_get_var_undef:
  case OP_scope_get_var:
  case OP_scope_put_var:
    dbuf_putc(bc, OP_get_var_undef + (op - OP_scope_get_var_undef));
    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
    break;
  case OP_scope_put_var_init:
    dbuf_putc(bc, OP_put_var_init);
    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
    break;
  case OP_scope_delete_var:
    dbuf_putc(bc, OP_delete_var);
    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
    break;
  }
done:
  if (label_done >= 0) {
    dbuf_putc(bc, OP_label);
    dbuf_put_u32(bc, label_done);
    s->label_slots[label_done].pos2 = bc->size;
  }
  return pos_next;
}

static void instantiate_hoisted_definitions(JSContext *ctx, JSFunctionDef *s,
                                            DynBuf *bc) {
  int i, idx, label_next = -1;

  /* add the hoisted functions in arguments and local variables */
  for (i = 0; i < s->arg_count; i++) {
    JSVarDef *vd = &s->args[i];
    if (vd->func_pool_idx >= 0) {
      dbuf_putc(bc, OP_fclosure);
      dbuf_put_u32(bc, vd->func_pool_idx);
      dbuf_putc(bc, OP_put_arg);
      dbuf_put_u16(bc, i);
    }
  }
  for (i = 0; i < s->var_count; i++) {
    JSVarDef *vd = &s->vars[i];
    if (vd->scope_level == 0 && vd->func_pool_idx >= 0) {
      dbuf_putc(bc, OP_fclosure);
      dbuf_put_u32(bc, vd->func_pool_idx);
      dbuf_putc(bc, OP_put_loc);
      dbuf_put_u16(bc, i);
    }
  }

  /* the module global variables must be initialized before
     evaluating the module so that the exported functions are
     visible if there are cyclic module references */
  if (s->module) {
    label_next = new_label_fd(s, -1);

    /* if 'this' is true, initialize the global variables and return */
    dbuf_putc(bc, OP_push_this);
    dbuf_putc(bc, OP_if_false);
    dbuf_put_u32(bc, label_next);
    update_label(s, label_next, 1);
    s->jump_size++;
  }

  /* add the global variables (only happens if s->is_global_var is
     true) */
  for (i = 0; i < s->global_var_count; i++) {
    JSGlobalVar *hf = &s->global_vars[i];
    int has_closure = 0;
    BOOL force_init = hf->force_init;
    /* we are in an eval, so the closure contains all the
       enclosing variables */
    /* If the outer function has a variable environment,
       create a property for the variable there */
    for (idx = 0; idx < s->closure_var_count; idx++) {
      JSClosureVar *cv = &s->closure_var[idx];
      if (cv->var_name == hf->var_name) {
        has_closure = 2;
        force_init = FALSE;
        break;
      }
      if (cv->var_name == JS_ATOM__var_ || cv->var_name == JS_ATOM__arg_var_) {
        dbuf_putc(bc, OP_get_var_ref);
        dbuf_put_u16(bc, idx);
        has_closure = 1;
        force_init = TRUE;
        break;
      }
    }
    if (!has_closure) {
      int flags;

      flags = 0;
      if (s->eval_type != JS_EVAL_TYPE_GLOBAL)
        flags |= JS_PROP_CONFIGURABLE;
      if (hf->cpool_idx >= 0 && !hf->is_lexical) {
        /* global function definitions need a specific handling */
        dbuf_putc(bc, OP_fclosure);
        dbuf_put_u32(bc, hf->cpool_idx);

        dbuf_putc(bc, OP_define_func);
        dbuf_put_u32(bc, JS_DupAtom(ctx, hf->var_name));
        dbuf_putc(bc, flags);

        goto done_global_var;
      } else {
        if (hf->is_lexical) {
          flags |= DEFINE_GLOBAL_LEX_VAR;
          if (!hf->is_const)
            flags |= JS_PROP_WRITABLE;
        }
        dbuf_putc(bc, OP_define_var);
        dbuf_put_u32(bc, JS_DupAtom(ctx, hf->var_name));
        dbuf_putc(bc, flags);
      }
    }
    if (hf->cpool_idx >= 0 || force_init) {
      if (hf->cpool_idx >= 0) {
        dbuf_putc(bc, OP_fclosure);
        dbuf_put_u32(bc, hf->cpool_idx);
        if (hf->var_name == JS_ATOM__default_) {
          /* set default export function name */
          dbuf_putc(bc, OP_set_name);
          dbuf_put_u32(bc, JS_DupAtom(ctx, JS_ATOM_default));
        }
      } else {
        dbuf_putc(bc, OP_undefined);
      }
      if (has_closure == 2) {
        dbuf_putc(bc, OP_put_var_ref);
        dbuf_put_u16(bc, idx);
      } else if (has_closure == 1) {
        dbuf_putc(bc, OP_define_field);
        dbuf_put_u32(bc, JS_DupAtom(ctx, hf->var_name));
        dbuf_putc(bc, OP_drop);
      } else {
        /* XXX: Check if variable is writable and enumerable */
        dbuf_putc(bc, OP_put_var);
        dbuf_put_u32(bc, JS_DupAtom(ctx, hf->var_name));
      }
    }
  done_global_var:
    JS_FreeAtom(ctx, hf->var_name);
  }

  if (s->module) {
    dbuf_putc(bc, OP_return_undef);

    dbuf_putc(bc, OP_label);
    dbuf_put_u32(bc, label_next);
    s->label_slots[label_next].pos2 = bc->size;
  }

  js_free(ctx, s->global_vars);
  s->global_vars = NULL;
  s->global_var_count = 0;
  s->global_var_size = 0;
}

static int get_label_pos(JSFunctionDef *s, int label) {
  int i, pos;
  for (i = 0; i < 20; i++) {
    pos = s->label_slots[label].pos;
    for (;;) {
      switch (s->byte_code.buf[pos]) {
      case OP_line_num:
      case OP_label:
        pos += 5;
        continue;
      case OP_goto:
        label = get_u32(s->byte_code.buf + pos + 1);
        break;
      default:
        return pos;
      }
      break;
    }
  }
  return pos;
}

/* search in all scopes */
static int find_private_class_field_all(JSContext *ctx, JSFunctionDef *fd,
                                        JSAtom name, int scope_level) {
  int idx;

  idx = fd->scopes[scope_level].first;
  while (idx >= 0) {
    if (fd->vars[idx].var_name == name)
      return idx;
    idx = fd->vars[idx].scope_next;
  }
  return -1;
}

static void get_loc_or_ref(DynBuf *bc, BOOL is_ref, int idx) {
  /* if the field is not initialized, the error is caught when
     accessing it */
  if (is_ref)
    dbuf_putc(bc, OP_get_var_ref);
  else
    dbuf_putc(bc, OP_get_loc);
  dbuf_put_u16(bc, idx);
}

static int resolve_scope_private_field1(JSContext *ctx, BOOL *pis_ref,
                                        int *pvar_kind, JSFunctionDef *s,
                                        JSAtom var_name, int scope_level) {
  int idx, var_kind;
  JSFunctionDef *fd;
  BOOL is_ref;

  fd = s;
  is_ref = FALSE;
  for (;;) {
    idx = find_private_class_field_all(ctx, fd, var_name, scope_level);
    if (idx >= 0) {
      var_kind = fd->vars[idx].var_kind;
      if (is_ref) {
        idx = get_closure_var(ctx, s, fd, FALSE, idx, var_name, TRUE, TRUE,
                              JS_VAR_NORMAL);
        if (idx < 0)
          return -1;
      }
      break;
    }
    scope_level = fd->parent_scope_level;
    if (!fd->parent) {
      if (fd->is_eval) {
        /* closure of the eval function (top level) */
        for (idx = 0; idx < fd->closure_var_count; idx++) {
          JSClosureVar *cv = &fd->closure_var[idx];
          if (cv->var_name == var_name) {
            var_kind = cv->var_kind;
            is_ref = TRUE;
            if (fd != s) {
              idx = get_closure_var2(ctx, s, fd, FALSE, cv->is_arg, idx,
                                     cv->var_name, cv->is_const, cv->is_lexical,
                                     cv->var_kind);
              if (idx < 0)
                return -1;
            }
            goto done;
          }
        }
      }
      /* XXX: no line number info */
      JS_ThrowSyntaxErrorAtom(ctx, "undefined private field '%s'", var_name);
      return -1;
    } else {
      fd = fd->parent;
    }
    is_ref = TRUE;
  }
done:
  *pis_ref = is_ref;
  *pvar_kind = var_kind;
  return idx;
}

/* return 0 if OK or -1 if the private field could not be resolved */
static int resolve_scope_private_field(JSContext *ctx, JSFunctionDef *s,
                                       JSAtom var_name, int scope_level, int op,
                                       DynBuf *bc) {
  int idx, var_kind;
  BOOL is_ref;

  idx = resolve_scope_private_field1(ctx, &is_ref, &var_kind, s, var_name,
                                     scope_level);
  if (idx < 0)
    return -1;
  assert(var_kind != JS_VAR_NORMAL);
  switch (op) {
  case OP_scope_get_private_field:
  case OP_scope_get_private_field2:
    switch (var_kind) {
    case JS_VAR_PRIVATE_FIELD:
      if (op == OP_scope_get_private_field2)
        dbuf_putc(bc, OP_dup);
      get_loc_or_ref(bc, is_ref, idx);
      dbuf_putc(bc, OP_get_private_field);
      break;
    case JS_VAR_PRIVATE_METHOD:
      get_loc_or_ref(bc, is_ref, idx);
      dbuf_putc(bc, OP_check_brand);
      if (op != OP_scope_get_private_field2)
        dbuf_putc(bc, OP_nip);
      break;
    case JS_VAR_PRIVATE_GETTER:
    case JS_VAR_PRIVATE_GETTER_SETTER:
      if (op == OP_scope_get_private_field2)
        dbuf_putc(bc, OP_dup);
      get_loc_or_ref(bc, is_ref, idx);
      dbuf_putc(bc, OP_check_brand);
      dbuf_putc(bc, OP_call_method);
      dbuf_put_u16(bc, 0);
      break;
    case JS_VAR_PRIVATE_SETTER:
      /* XXX: add clearer error message */
      dbuf_putc(bc, OP_throw_error);
      dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
      dbuf_putc(bc, JS_THROW_VAR_RO);
      break;
    default:
      abort();
    }
    break;
  case OP_scope_put_private_field:
    switch (var_kind) {
    case JS_VAR_PRIVATE_FIELD:
      get_loc_or_ref(bc, is_ref, idx);
      dbuf_putc(bc, OP_put_private_field);
      break;
    case JS_VAR_PRIVATE_METHOD:
    case JS_VAR_PRIVATE_GETTER:
      /* XXX: add clearer error message */
      dbuf_putc(bc, OP_throw_error);
      dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
      dbuf_putc(bc, JS_THROW_VAR_RO);
      break;
    case JS_VAR_PRIVATE_SETTER:
    case JS_VAR_PRIVATE_GETTER_SETTER: {
      JSAtom setter_name = get_private_setter_name(ctx, var_name);
      if (setter_name == JS_ATOM_NULL)
        return -1;
      idx = resolve_scope_private_field1(ctx, &is_ref, &var_kind, s,
                                         setter_name, scope_level);
      JS_FreeAtom(ctx, setter_name);
      if (idx < 0)
        return -1;
      assert(var_kind == JS_VAR_PRIVATE_SETTER);
      get_loc_or_ref(bc, is_ref, idx);
      dbuf_putc(bc, OP_swap);
      /* obj func value */
      dbuf_putc(bc, OP_rot3r);
      /* value obj func */
      dbuf_putc(bc, OP_check_brand);
      dbuf_putc(bc, OP_rot3l);
      /* obj func value */
      dbuf_putc(bc, OP_call_method);
      dbuf_put_u16(bc, 1);
    } break;
    default:
      abort();
    }
    break;
  default:
    abort();
  }
  return 0;
}

static void mark_eval_captured_variables(JSContext *ctx, JSFunctionDef *s,
                                         int scope_level) {
  int idx;
  JSVarDef *vd;

  for (idx = s->scopes[scope_level].first; idx >= 0;) {
    vd = &s->vars[idx];
    vd->is_captured = 1;
    idx = vd->scope_next;
  }
}

static void push_short_int(DynBuf *bc_out, int val) {
#if SHORT_OPCODES
  if (val >= -1 && val <= 7) {
    dbuf_putc(bc_out, OP_push_0 + val);
    return;
  }
  if (val == (int8_t)val) {
    dbuf_putc(bc_out, OP_push_i8);
    dbuf_putc(bc_out, val);
    return;
  }
  if (val == (int16_t)val) {
    dbuf_putc(bc_out, OP_push_i16);
    dbuf_put_u16(bc_out, val);
    return;
  }
#endif
  dbuf_putc(bc_out, OP_push_i32);
  dbuf_put_u32(bc_out, val);
}

static void put_short_code(DynBuf *bc_out, int op, int idx) {
#if SHORT_OPCODES
  if (idx < 4) {
    switch (op) {
    case OP_get_loc:
      dbuf_putc(bc_out, OP_get_loc0 + idx);
      return;
    case OP_put_loc:
      dbuf_putc(bc_out, OP_put_loc0 + idx);
      return;
    case OP_set_loc:
      dbuf_putc(bc_out, OP_set_loc0 + idx);
      return;
    case OP_get_arg:
      dbuf_putc(bc_out, OP_get_arg0 + idx);
      return;
    case OP_put_arg:
      dbuf_putc(bc_out, OP_put_arg0 + idx);
      return;
    case OP_set_arg:
      dbuf_putc(bc_out, OP_set_arg0 + idx);
      return;
    case OP_get_var_ref:
      dbuf_putc(bc_out, OP_get_var_ref0 + idx);
      return;
    case OP_put_var_ref:
      dbuf_putc(bc_out, OP_put_var_ref0 + idx);
      return;
    case OP_set_var_ref:
      dbuf_putc(bc_out, OP_set_var_ref0 + idx);
      return;
    case OP_call:
      dbuf_putc(bc_out, OP_call0 + idx);
      return;
    }
  }
  if (idx < 256) {
    switch (op) {
    case OP_get_loc:
      dbuf_putc(bc_out, OP_get_loc8);
      dbuf_putc(bc_out, idx);
      return;
    case OP_put_loc:
      dbuf_putc(bc_out, OP_put_loc8);
      dbuf_putc(bc_out, idx);
      return;
    case OP_set_loc:
      dbuf_putc(bc_out, OP_set_loc8);
      dbuf_putc(bc_out, idx);
      return;
    }
  }
#endif
  dbuf_putc(bc_out, op);
  dbuf_put_u16(bc_out, idx);
}

/* convert global variable accesses to local variables or closure
   variables when necessary */
__exception int resolve_variables(JSContext *ctx, JSFunctionDef *s) {
  int pos, pos_next, bc_len, op, len, i, idx, line_num;
  uint8_t *bc_buf;
  JSAtom var_name;
  DynBuf bc_out;
  CodeContext cc;
  int scope;

  cc.bc_buf = bc_buf = s->byte_code.buf;
  cc.bc_len = bc_len = s->byte_code.size;
  js_dbuf_init(ctx, &bc_out);

  /* first pass for runtime checks (must be done before the
     variables are created) */
  for (i = 0; i < s->global_var_count; i++) {
    JSGlobalVar *hf = &s->global_vars[i];
    int flags;

    /* check if global variable (XXX: simplify) */
    for (idx = 0; idx < s->closure_var_count; idx++) {
      JSClosureVar *cv = &s->closure_var[idx];
      if (cv->var_name == hf->var_name) {
        if (s->eval_type == JS_EVAL_TYPE_DIRECT && cv->is_lexical) {
          /* Check if a lexical variable is
             redefined as 'var'. XXX: Could abort
             compilation here, but for consistency
             with the other checks, we delay the
             error generation. */
          dbuf_putc(&bc_out, OP_throw_error);
          dbuf_put_u32(&bc_out, JS_DupAtom(ctx, hf->var_name));
          dbuf_putc(&bc_out, JS_THROW_VAR_REDECL);
        }
        goto next;
      }
      if (cv->var_name == JS_ATOM__var_ || cv->var_name == JS_ATOM__arg_var_)
        goto next;
    }

    dbuf_putc(&bc_out, OP_check_define_var);
    dbuf_put_u32(&bc_out, JS_DupAtom(ctx, hf->var_name));
    flags = 0;
    if (hf->is_lexical)
      flags |= DEFINE_GLOBAL_LEX_VAR;
    if (hf->cpool_idx >= 0)
      flags |= DEFINE_GLOBAL_FUNC_VAR;
    dbuf_putc(&bc_out, flags);
  next:;
  }

  line_num = 0; /* avoid warning */
  for (pos = 0; pos < bc_len; pos = pos_next) {
    op = bc_buf[pos];
    len = opcode_info[op].size;
    pos_next = pos + len;
    switch (op) {
    case OP_line_num:
      line_num = get_u32(bc_buf + pos + 1);
      s->line_number_size++;
      goto no_change;

    case OP_eval: /* convert scope index to adjusted variable index */
    {
      int call_argc = get_u16(bc_buf + pos + 1);
      scope = get_u16(bc_buf + pos + 1 + 2);
      mark_eval_captured_variables(ctx, s, scope);
      dbuf_putc(&bc_out, op);
      dbuf_put_u16(&bc_out, call_argc);
      dbuf_put_u16(&bc_out, s->scopes[scope].first + 1);
    } break;
    case OP_apply_eval: /* convert scope index to adjusted variable index */
      scope = get_u16(bc_buf + pos + 1);
      mark_eval_captured_variables(ctx, s, scope);
      dbuf_putc(&bc_out, op);
      dbuf_put_u16(&bc_out, s->scopes[scope].first + 1);
      break;
    case OP_scope_get_var_undef:
    case OP_scope_get_var:
    case OP_scope_put_var:
    case OP_scope_delete_var:
    case OP_scope_get_ref:
    case OP_scope_put_var_init:
      var_name = get_u32(bc_buf + pos + 1);
      scope = get_u16(bc_buf + pos + 5);
      pos_next = resolve_scope_var(ctx, s, var_name, scope, op, &bc_out, NULL,
                                   NULL, pos_next);
      JS_FreeAtom(ctx, var_name);
      break;
    case OP_scope_make_ref: {
      int label;
      LabelSlot *ls;
      var_name = get_u32(bc_buf + pos + 1);
      label = get_u32(bc_buf + pos + 5);
      scope = get_u16(bc_buf + pos + 9);
      ls = &s->label_slots[label];
      ls->ref_count--; /* always remove label reference */
      pos_next = resolve_scope_var(ctx, s, var_name, scope, op, &bc_out, bc_buf,
                                   ls, pos_next);
      JS_FreeAtom(ctx, var_name);
    } break;
    case OP_scope_get_private_field:
    case OP_scope_get_private_field2:
    case OP_scope_put_private_field: {
      int ret;
      var_name = get_u32(bc_buf + pos + 1);
      scope = get_u16(bc_buf + pos + 5);
      ret = resolve_scope_private_field(ctx, s, var_name, scope, op, &bc_out);
      if (ret < 0)
        goto fail;
      JS_FreeAtom(ctx, var_name);
    } break;
    case OP_gosub:
      s->jump_size++;
      if (OPTIMIZE) {
        /* remove calls to empty finalizers  */
        int label;
        LabelSlot *ls;

        label = get_u32(bc_buf + pos + 1);
        assert(label >= 0 && label < s->label_count);
        ls = &s->label_slots[label];
        if (code_match(&cc, ls->pos, OP_ret, -1)) {
          ls->ref_count--;
          break;
        }
      }
      goto no_change;
    case OP_drop:
      if (0) {
        /* remove drops before return_undef */
        /* do not perform this optimization in pass2 because
           it breaks patterns recognised in resolve_labels */
        int pos1 = pos_next;
        int line1 = line_num;
        while (code_match(&cc, pos1, OP_drop, -1)) {
          if (cc.line_num >= 0)
            line1 = cc.line_num;
          pos1 = cc.pos;
        }
        if (code_match(&cc, pos1, OP_return_undef, -1)) {
          pos_next = pos1;
          if (line1 != -1 && line1 != line_num) {
            line_num = line1;
            s->line_number_size++;
            dbuf_putc(&bc_out, OP_line_num);
            dbuf_put_u32(&bc_out, line_num);
          }
          break;
        }
      }
      goto no_change;
    case OP_insert3:
      if (OPTIMIZE) {
        /* Transformation: insert3 put_array_el|put_ref_value drop ->
         * put_array_el|put_ref_value */
        if (code_match(&cc, pos_next, M2(OP_put_array_el, OP_put_ref_value),
                       OP_drop, -1)) {
          dbuf_putc(&bc_out, cc.op);
          pos_next = cc.pos;
          if (cc.line_num != -1 && cc.line_num != line_num) {
            line_num = cc.line_num;
            s->line_number_size++;
            dbuf_putc(&bc_out, OP_line_num);
            dbuf_put_u32(&bc_out, line_num);
          }
          break;
        }
      }
      goto no_change;

    case OP_goto:
      s->jump_size++;
      /* fall thru */
    case OP_tail_call:
    case OP_tail_call_method:
    case OP_return:
    case OP_return_undef:
    case OP_throw:
    case OP_throw_error:
    case OP_ret:
      if (OPTIMIZE) {
        /* remove dead code */
        int line = -1;
        dbuf_put(&bc_out, bc_buf + pos, len);
        pos = skip_dead_code(s, bc_buf, bc_len, pos + len, &line);
        pos_next = pos;
        if (pos < bc_len && line >= 0 && line_num != line) {
          line_num = line;
          s->line_number_size++;
          dbuf_putc(&bc_out, OP_line_num);
          dbuf_put_u32(&bc_out, line_num);
        }
        break;
      }
      goto no_change;

    case OP_label: {
      int label;
      LabelSlot *ls;

      label = get_u32(bc_buf + pos + 1);
      assert(label >= 0 && label < s->label_count);
      ls = &s->label_slots[label];
      ls->pos2 = bc_out.size + opcode_info[op].size;
    }
      goto no_change;

    case OP_enter_scope: {
      int scope_idx, scope = get_u16(bc_buf + pos + 1);

      if (scope == s->body_scope) {
        instantiate_hoisted_definitions(ctx, s, &bc_out);
      }

      for (scope_idx = s->scopes[scope].first; scope_idx >= 0;) {
        JSVarDef *vd = &s->vars[scope_idx];
        if (vd->scope_level == scope) {
          if (scope_idx != s->arguments_arg_idx) {
            if (vd->var_kind == JS_VAR_FUNCTION_DECL ||
                vd->var_kind == JS_VAR_NEW_FUNCTION_DECL) {
              /* Initialize lexical variable upon entering scope */
              dbuf_putc(&bc_out, OP_fclosure);
              dbuf_put_u32(&bc_out, vd->func_pool_idx);
              dbuf_putc(&bc_out, OP_put_loc);
              dbuf_put_u16(&bc_out, scope_idx);
            } else {
              /* XXX: should check if variable can be used
                 before initialization */
              dbuf_putc(&bc_out, OP_set_loc_uninitialized);
              dbuf_put_u16(&bc_out, scope_idx);
            }
          }
          scope_idx = vd->scope_next;
        } else {
          break;
        }
      }
    } break;

    case OP_leave_scope: {
      int scope_idx, scope = get_u16(bc_buf + pos + 1);

      for (scope_idx = s->scopes[scope].first; scope_idx >= 0;) {
        JSVarDef *vd = &s->vars[scope_idx];
        if (vd->scope_level == scope) {
          if (vd->is_captured) {
            dbuf_putc(&bc_out, OP_close_loc);
            dbuf_put_u16(&bc_out, scope_idx);
          }
          scope_idx = vd->scope_next;
        } else {
          break;
        }
      }
    } break;

    case OP_set_name: {
      /* remove dummy set_name opcodes */
      JSAtom name = get_u32(bc_buf + pos + 1);
      if (name == JS_ATOM_NULL)
        break;
    }
      goto no_change;

    case OP_if_false:
    case OP_if_true:
    case OP_catch:
      s->jump_size++;
      goto no_change;

    case OP_dup:
      if (OPTIMIZE) {
        /* Transformation: dup if_false(l1) drop, l1: if_false(l2) ->
         * if_false(l2) */
        /* Transformation: dup if_true(l1) drop, l1: if_true(l2) -> if_true(l2)
         */
        if (code_match(&cc, pos_next, M2(OP_if_false, OP_if_true), OP_drop,
                       -1)) {
          int lab0, lab1, op1, pos1, line1, pos2;
          lab0 = lab1 = cc.label;
          assert(lab1 >= 0 && lab1 < s->label_count);
          op1 = cc.op;
          pos1 = cc.pos;
          line1 = cc.line_num;
          while (code_match(&cc, (pos2 = get_label_pos(s, lab1)), OP_dup, op1,
                            OP_drop, -1)) {
            lab1 = cc.label;
          }
          if (code_match(&cc, pos2, op1, -1)) {
            s->jump_size++;
            update_label(s, lab0, -1);
            update_label(s, cc.label, +1);
            dbuf_putc(&bc_out, op1);
            dbuf_put_u32(&bc_out, cc.label);
            pos_next = pos1;
            if (line1 != -1 && line1 != line_num) {
              line_num = line1;
              s->line_number_size++;
              dbuf_putc(&bc_out, OP_line_num);
              dbuf_put_u32(&bc_out, line_num);
            }
            break;
          }
        }
      }
      goto no_change;

    case OP_nop:
      /* remove erased code */
      break;
    case OP_set_class_name:
      /* only used during parsing */
      break;

    default:
    no_change:
      dbuf_put(&bc_out, bc_buf + pos, len);
      break;
    }
  }

  /* set the new byte code */
  dbuf_free(&s->byte_code);
  s->byte_code = bc_out;
  if (dbuf_error(&s->byte_code)) {
    JS_ThrowOutOfMemory(ctx);
    return -1;
  }
  return 0;
fail:
  /* continue the copy to keep the atom refcounts consistent */
  /* XXX: find a better solution ? */
  for (; pos < bc_len; pos = pos_next) {
    op = bc_buf[pos];
    len = opcode_info[op].size;
    pos_next = pos + len;
    dbuf_put(&bc_out, bc_buf + pos, len);
  }
  dbuf_free(&s->byte_code);
  s->byte_code = bc_out;
  return -1;
}

/* the pc2line table gives a line number for each PC value */
static void add_pc2line_info(JSFunctionDef *s, uint32_t pc, int line_num) {
  if (s->line_number_slots != NULL &&
      s->line_number_count < s->line_number_size &&
      pc >= s->line_number_last_pc && line_num != s->line_number_last) {
    s->line_number_slots[s->line_number_count].pc = pc;
    s->line_number_slots[s->line_number_count].line_num = line_num;
    s->line_number_count++;
    s->line_number_last_pc = pc;
    s->line_number_last = line_num;
  }
}

static void compute_pc2line_info(JSFunctionDef *s) {
  if (!(s->js_mode & JS_MODE_STRIP) && s->line_number_slots) {
    int last_line_num = s->line_num;
    uint32_t last_pc = 0;
    int i;

    js_dbuf_init(s->ctx, &s->pc2line);
    for (i = 0; i < s->line_number_count; i++) {
      uint32_t pc = s->line_number_slots[i].pc;
      int line_num = s->line_number_slots[i].line_num;
      int diff_pc, diff_line;

      if (line_num < 0)
        continue;

      diff_pc = pc - last_pc;
      diff_line = line_num - last_line_num;
      if (diff_line == 0 || diff_pc < 0)
        continue;

      if (diff_line >= PC2LINE_BASE &&
          diff_line < PC2LINE_BASE + PC2LINE_RANGE &&
          diff_pc <= PC2LINE_DIFF_PC_MAX) {
        dbuf_putc(&s->pc2line, (diff_line - PC2LINE_BASE) +
                                   diff_pc * PC2LINE_RANGE + PC2LINE_OP_FIRST);
      } else {
        /* longer encoding */
        dbuf_putc(&s->pc2line, 0);
        dbuf_put_leb128(&s->pc2line, diff_pc);
        dbuf_put_sleb128(&s->pc2line, diff_line);
      }
      last_pc = pc;
      last_line_num = line_num;
    }
  }
}

static RelocEntry *add_reloc(JSContext *ctx, LabelSlot *ls, uint32_t addr,
                             int size) {
  RelocEntry *re;
  re = js_malloc(ctx, sizeof(*re));
  if (!re)
    return NULL;
  re->addr = addr;
  re->size = size;
  re->next = ls->first_reloc;
  ls->first_reloc = re;
  return re;
}

static BOOL code_has_label(CodeContext *s, int pos, int label) {
  while (pos < s->bc_len) {
    int op = s->bc_buf[pos];
    if (op == OP_line_num) {
      pos += 5;
      continue;
    }
    if (op == OP_label) {
      int lab = get_u32(s->bc_buf + pos + 1);
      if (lab == label)
        return TRUE;
      pos += 5;
      continue;
    }
    if (op == OP_goto) {
      int lab = get_u32(s->bc_buf + pos + 1);
      if (lab == label)
        return TRUE;
    }
    break;
  }
  return FALSE;
}

/* return the target label, following the OP_goto jumps
   the first opcode at destination is stored in *pop
 */
static int find_jump_target(JSFunctionDef *s, int label, int *pop, int *pline) {
  int i, pos, op;

  update_label(s, label, -1);
  for (i = 0; i < 10; i++) {
    assert(label >= 0 && label < s->label_count);
    pos = s->label_slots[label].pos2;
    for (;;) {
      switch (op = s->byte_code.buf[pos]) {
      case OP_line_num:
        if (pline)
          *pline = get_u32(s->byte_code.buf + pos + 1);
        /* fall thru */
      case OP_label:
        pos += opcode_info[op].size;
        continue;
      case OP_goto:
        label = get_u32(s->byte_code.buf + pos + 1);
        break;
      case OP_drop:
        /* ignore drop opcodes if followed by OP_return_undef */
        while (s->byte_code.buf[++pos] == OP_drop)
          continue;
        if (s->byte_code.buf[pos] == OP_return_undef)
          op = OP_return_undef;
        /* fall thru */
      default:
        goto done;
      }
      break;
    }
  }
  /* cycle detected, could issue a warning */
done:
  *pop = op;
  update_label(s, label, +1);
  return label;
}

/* peephole optimizations and resolve goto/labels */
__exception int resolve_labels(JSContext *ctx, JSFunctionDef *s) {
  int pos, pos_next, bc_len, op, op1, len, i, line_num;
  const uint8_t *bc_buf;
  DynBuf bc_out;
  LabelSlot *label_slots, *ls;
  RelocEntry *re, *re_next;
  CodeContext cc;
  int label;
#if SHORT_OPCODES
  JumpSlot *jp;
#endif

  label_slots = s->label_slots;

  line_num = s->line_num;

  cc.bc_buf = bc_buf = s->byte_code.buf;
  cc.bc_len = bc_len = s->byte_code.size;
  js_dbuf_init(ctx, &bc_out);

#if SHORT_OPCODES
  if (s->jump_size) {
    s->jump_slots = js_mallocz(s->ctx, sizeof(*s->jump_slots) * s->jump_size);
    if (s->jump_slots == NULL)
      return -1;
  }
#endif
  /* XXX: Should skip this phase if not generating SHORT_OPCODES */
  if (s->line_number_size && !(s->js_mode & JS_MODE_STRIP)) {
    s->line_number_slots =
        js_mallocz(s->ctx, sizeof(*s->line_number_slots) * s->line_number_size);
    if (s->line_number_slots == NULL)
      return -1;
    s->line_number_last = s->line_num;
    s->line_number_last_pc = 0;
  }

  /* initialize the 'home_object' variable if needed */
  if (s->home_object_var_idx >= 0) {
    dbuf_putc(&bc_out, OP_special_object);
    dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_HOME_OBJECT);
    put_short_code(&bc_out, OP_put_loc, s->home_object_var_idx);
  }
  /* initialize the 'this.active_func' variable if needed */
  if (s->this_active_func_var_idx >= 0) {
    dbuf_putc(&bc_out, OP_special_object);
    dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_THIS_FUNC);
    put_short_code(&bc_out, OP_put_loc, s->this_active_func_var_idx);
  }
  /* initialize the 'new.target' variable if needed */
  if (s->new_target_var_idx >= 0) {
    dbuf_putc(&bc_out, OP_special_object);
    dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_NEW_TARGET);
    put_short_code(&bc_out, OP_put_loc, s->new_target_var_idx);
  }
  /* initialize the 'this' variable if needed. In a derived class
     constructor, this is initially uninitialized. */
  if (s->this_var_idx >= 0) {
    if (s->is_derived_class_constructor) {
      dbuf_putc(&bc_out, OP_set_loc_uninitialized);
      dbuf_put_u16(&bc_out, s->this_var_idx);
    } else {
      dbuf_putc(&bc_out, OP_push_this);
      put_short_code(&bc_out, OP_put_loc, s->this_var_idx);
    }
  }
  /* initialize the 'arguments' variable if needed */
  if (s->arguments_var_idx >= 0) {
    if ((s->js_mode & JS_MODE_STRICT) || !s->has_simple_parameter_list) {
      dbuf_putc(&bc_out, OP_special_object);
      dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_ARGUMENTS);
    } else {
      dbuf_putc(&bc_out, OP_special_object);
      dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_MAPPED_ARGUMENTS);
    }
    if (s->arguments_arg_idx >= 0)
      put_short_code(&bc_out, OP_set_loc, s->arguments_arg_idx);
    put_short_code(&bc_out, OP_put_loc, s->arguments_var_idx);
  }
  /* initialize a reference to the current function if needed */
  if (s->func_var_idx >= 0) {
    dbuf_putc(&bc_out, OP_special_object);
    dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_THIS_FUNC);
    put_short_code(&bc_out, OP_put_loc, s->func_var_idx);
  }
  /* initialize the variable environment object if needed */
  if (s->var_object_idx >= 0) {
    dbuf_putc(&bc_out, OP_special_object);
    dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_VAR_OBJECT);
    put_short_code(&bc_out, OP_put_loc, s->var_object_idx);
  }
  if (s->arg_var_object_idx >= 0) {
    dbuf_putc(&bc_out, OP_special_object);
    dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_VAR_OBJECT);
    put_short_code(&bc_out, OP_put_loc, s->arg_var_object_idx);
  }

  for (pos = 0; pos < bc_len; pos = pos_next) {
    int val;
    op = bc_buf[pos];
    len = opcode_info[op].size;
    pos_next = pos + len;
    switch (op) {
    case OP_line_num:
      /* line number info (for debug). We put it in a separate
         compressed table to reduce memory usage and get better
         performance */
      line_num = get_u32(bc_buf + pos + 1);
      break;

    case OP_label: {
      label = get_u32(bc_buf + pos + 1);
      assert(label >= 0 && label < s->label_count);
      ls = &label_slots[label];
      assert(ls->addr == -1);
      ls->addr = bc_out.size;
      /* resolve the relocation entries */
      for (re = ls->first_reloc; re != NULL; re = re_next) {
        int diff = ls->addr - re->addr;
        re_next = re->next;
        switch (re->size) {
        case 4:
          put_u32(bc_out.buf + re->addr, diff);
          break;
        case 2:
          assert(diff == (int16_t)diff);
          put_u16(bc_out.buf + re->addr, diff);
          break;
        case 1:
          assert(diff == (int8_t)diff);
          put_u8(bc_out.buf + re->addr, diff);
          break;
        }
        js_free(ctx, re);
      }
      ls->first_reloc = NULL;
    } break;

    case OP_call:
    case OP_call_method: {
      /* detect and transform tail calls */
      int argc;
      argc = get_u16(bc_buf + pos + 1);
      if (code_match(&cc, pos_next, OP_return, -1)) {
        if (cc.line_num >= 0)
          line_num = cc.line_num;
        add_pc2line_info(s, bc_out.size, line_num);
        put_short_code(&bc_out, op + 1, argc);
        pos_next = skip_dead_code(s, bc_buf, bc_len, cc.pos, &line_num);
        break;
      }
      add_pc2line_info(s, bc_out.size, line_num);
      put_short_code(&bc_out, op, argc);
      break;
    }
      goto no_change;

    case OP_return:
    case OP_return_undef:
    case OP_return_async:
    case OP_throw:
    case OP_throw_error:
      pos_next = skip_dead_code(s, bc_buf, bc_len, pos_next, &line_num);
      goto no_change;

    case OP_goto:
      label = get_u32(bc_buf + pos + 1);
    has_goto:
      if (OPTIMIZE) {
        int line1 = -1;
        /* Use custom matcher because multiple labels can follow */
        label = find_jump_target(s, label, &op1, &line1);
        if (code_has_label(&cc, pos_next, label)) {
          /* jump to next instruction: remove jump */
          update_label(s, label, -1);
          break;
        }
        if (op1 == OP_return || op1 == OP_return_undef || op1 == OP_throw) {
          /* jump to return/throw: remove jump, append return/throw */
          /* updating the line number obfuscates assembly listing */
          // if (line1 >= 0) line_num = line1;
          update_label(s, label, -1);
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, op1);
          pos_next = skip_dead_code(s, bc_buf, bc_len, pos_next, &line_num);
          break;
        }
        /* XXX: should duplicate single instructions followed by goto or return
         */
        /* For example, can match one of these followed by return:
           push_i32 / push_const / push_atom_value / get_var /
           undefined / null / push_false / push_true / get_ref_value /
           get_loc / get_arg / get_var_ref
         */
      }
      goto has_label;

    case OP_gosub:
      label = get_u32(bc_buf + pos + 1);
      if (0 && OPTIMIZE) {
        label = find_jump_target(s, label, &op1, NULL);
        if (op1 == OP_ret) {
          update_label(s, label, -1);
          /* empty finally clause: remove gosub */
          break;
        }
      }
      goto has_label;

    case OP_catch:
      label = get_u32(bc_buf + pos + 1);
      goto has_label;

    case OP_if_true:
    case OP_if_false:
      label = get_u32(bc_buf + pos + 1);
      if (OPTIMIZE) {
        label = find_jump_target(s, label, &op1, NULL);
        /* transform if_false/if_true(l1) label(l1) -> drop label(l1) */
        if (code_has_label(&cc, pos_next, label)) {
          update_label(s, label, -1);
          dbuf_putc(&bc_out, OP_drop);
          break;
        }
        /* transform if_false(l1) goto(l2) label(l1) -> if_false(l2) label(l1)
         */
        if (code_match(&cc, pos_next, OP_goto, -1)) {
          int pos1 = cc.pos;
          int line1 = cc.line_num;
          if (code_has_label(&cc, pos1, label)) {
            if (line1 >= 0)
              line_num = line1;
            pos_next = pos1;
            update_label(s, label, -1);
            label = cc.label;
            op ^= OP_if_true ^ OP_if_false;
          }
        }
      }
    has_label:
      add_pc2line_info(s, bc_out.size, line_num);
      if (op == OP_goto) {
        pos_next = skip_dead_code(s, bc_buf, bc_len, pos_next, &line_num);
      }
      assert(label >= 0 && label < s->label_count);
      ls = &label_slots[label];
#if SHORT_OPCODES
      jp = &s->jump_slots[s->jump_count++];
      jp->op = op;
      jp->size = 4;
      jp->pos = bc_out.size + 1;
      jp->label = label;

      if (ls->addr == -1) {
        int diff = ls->pos2 - pos - 1;
        if (diff < 128 &&
            (op == OP_if_false || op == OP_if_true || op == OP_goto)) {
          jp->size = 1;
          jp->op = OP_if_false8 + (op - OP_if_false);
          dbuf_putc(&bc_out, OP_if_false8 + (op - OP_if_false));
          dbuf_putc(&bc_out, 0);
          if (!add_reloc(ctx, ls, bc_out.size - 1, 1))
            goto fail;
          break;
        }
        if (diff < 32768 && op == OP_goto) {
          jp->size = 2;
          jp->op = OP_goto16;
          dbuf_putc(&bc_out, OP_goto16);
          dbuf_put_u16(&bc_out, 0);
          if (!add_reloc(ctx, ls, bc_out.size - 2, 2))
            goto fail;
          break;
        }
      } else {
        int diff = ls->addr - bc_out.size - 1;
        if (diff == (int8_t)diff &&
            (op == OP_if_false || op == OP_if_true || op == OP_goto)) {
          jp->size = 1;
          jp->op = OP_if_false8 + (op - OP_if_false);
          dbuf_putc(&bc_out, OP_if_false8 + (op - OP_if_false));
          dbuf_putc(&bc_out, diff);
          break;
        }
        if (diff == (int16_t)diff && op == OP_goto) {
          jp->size = 2;
          jp->op = OP_goto16;
          dbuf_putc(&bc_out, OP_goto16);
          dbuf_put_u16(&bc_out, diff);
          break;
        }
      }
#endif
      dbuf_putc(&bc_out, op);
      dbuf_put_u32(&bc_out, ls->addr - bc_out.size);
      if (ls->addr == -1) {
        /* unresolved yet: create a new relocation entry */
        if (!add_reloc(ctx, ls, bc_out.size - 4, 4))
          goto fail;
      }
      break;
    case OP_with_get_var:
    case OP_with_put_var:
    case OP_with_delete_var:
    case OP_with_make_ref:
    case OP_with_get_ref:
    case OP_with_get_ref_undef: {
      JSAtom atom;
      int is_with;

      atom = get_u32(bc_buf + pos + 1);
      label = get_u32(bc_buf + pos + 5);
      is_with = bc_buf[pos + 9];
      if (OPTIMIZE) {
        label = find_jump_target(s, label, &op1, NULL);
      }
      assert(label >= 0 && label < s->label_count);
      ls = &label_slots[label];
      add_pc2line_info(s, bc_out.size, line_num);
#if SHORT_OPCODES
      jp = &s->jump_slots[s->jump_count++];
      jp->op = op;
      jp->size = 4;
      jp->pos = bc_out.size + 5;
      jp->label = label;
#endif
      dbuf_putc(&bc_out, op);
      dbuf_put_u32(&bc_out, atom);
      dbuf_put_u32(&bc_out, ls->addr - bc_out.size);
      if (ls->addr == -1) {
        /* unresolved yet: create a new relocation entry */
        if (!add_reloc(ctx, ls, bc_out.size - 4, 4))
          goto fail;
      }
      dbuf_putc(&bc_out, is_with);
    } break;

    case OP_drop:
      if (OPTIMIZE) {
        /* remove useless drops before return */
        if (code_match(&cc, pos_next, OP_return_undef, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          break;
        }
      }
      goto no_change;

    case OP_null:
#if SHORT_OPCODES
      if (OPTIMIZE) {
        /* transform null strict_eq into is_null */
        if (code_match(&cc, pos_next, OP_strict_eq, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_is_null);
          pos_next = cc.pos;
          break;
        }
        /* transform null strict_neq if_false/if_true -> is_null
         * if_true/if_false */
        if (code_match(&cc, pos_next, OP_strict_neq,
                       M2(OP_if_false, OP_if_true), -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_is_null);
          pos_next = cc.pos;
          label = cc.label;
          op = cc.op ^ OP_if_false ^ OP_if_true;
          goto has_label;
        }
      }
#endif
      /* fall thru */
    case OP_push_false:
    case OP_push_true:
      if (OPTIMIZE) {
        val = (op == OP_push_true);
        if (code_match(&cc, pos_next, M2(OP_if_false, OP_if_true), -1)) {
        has_constant_test:
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          if (val == cc.op - OP_if_false) {
            /* transform null if_false(l1) -> goto l1 */
            /* transform false if_false(l1) -> goto l1 */
            /* transform true if_true(l1) -> goto l1 */
            pos_next = cc.pos;
            op = OP_goto;
            label = cc.label;
            goto has_goto;
          } else {
            /* transform null if_true(l1) -> nop */
            /* transform false if_true(l1) -> nop */
            /* transform true if_false(l1) -> nop */
            pos_next = cc.pos;
            update_label(s, cc.label, -1);
            break;
          }
        }
      }
      goto no_change;

    case OP_push_i32:
      if (OPTIMIZE) {
        /* transform i32(val) neg -> i32(-val) */
        val = get_i32(bc_buf + pos + 1);
        if ((val != INT32_MIN && val != 0) &&
            code_match(&cc, pos_next, OP_neg, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          if (code_match(&cc, cc.pos, OP_drop, -1)) {
            if (cc.line_num >= 0)
              line_num = cc.line_num;
          } else {
            add_pc2line_info(s, bc_out.size, line_num);
            push_short_int(&bc_out, -val);
          }
          pos_next = cc.pos;
          break;
        }
        /* remove push/drop pairs generated by the parser */
        if (code_match(&cc, pos_next, OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          pos_next = cc.pos;
          break;
        }
        /* Optimize constant tests: `if (0)`, `if (1)`, `if (!0)`... */
        if (code_match(&cc, pos_next, M2(OP_if_false, OP_if_true), -1)) {
          val = (val != 0);
          goto has_constant_test;
        }
        add_pc2line_info(s, bc_out.size, line_num);
        push_short_int(&bc_out, val);
        break;
      }
      goto no_change;

#if SHORT_OPCODES
    case OP_push_const:
    case OP_fclosure:
      if (OPTIMIZE) {
        int idx = get_u32(bc_buf + pos + 1);
        if (idx < 256) {
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_push_const8 + op - OP_push_const);
          dbuf_putc(&bc_out, idx);
          break;
        }
      }
      goto no_change;

    case OP_get_field:
      if (OPTIMIZE) {
        JSAtom atom = get_u32(bc_buf + pos + 1);
        if (atom == JS_ATOM_length) {
          JS_FreeAtom(ctx, atom);
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_get_length);
          break;
        }
      }
      goto no_change;
#endif
    case OP_push_atom_value:
      if (OPTIMIZE) {
        JSAtom atom = get_u32(bc_buf + pos + 1);
        /* remove push/drop pairs generated by the parser */
        if (code_match(&cc, pos_next, OP_drop, -1)) {
          JS_FreeAtom(ctx, atom);
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          pos_next = cc.pos;
          break;
        }
#if SHORT_OPCODES
        if (atom == JS_ATOM_empty_string) {
          JS_FreeAtom(ctx, atom);
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_push_empty_string);
          break;
        }
#endif
      }
      goto no_change;

    case OP_to_propkey:
    case OP_to_propkey2:
      if (OPTIMIZE) {
        /* remove redundant to_propkey/to_propkey2 opcodes when storing simple
         * data */
        if (code_match(&cc, pos_next,
                       M3(OP_get_loc, OP_get_arg, OP_get_var_ref), -1,
                       OP_put_array_el, -1) ||
            code_match(&cc, pos_next,
                       M3(OP_push_i32, OP_push_const, OP_push_atom_value),
                       OP_put_array_el, -1) ||
            code_match(&cc, pos_next,
                       M4(OP_undefined, OP_null, OP_push_true, OP_push_false),
                       OP_put_array_el, -1)) {
          break;
        }
      }
      goto no_change;

    case OP_undefined:
      if (OPTIMIZE) {
        /* remove push/drop pairs generated by the parser */
        if (code_match(&cc, pos_next, OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          pos_next = cc.pos;
          break;
        }
        /* transform undefined return -> return_undefined */
        if (code_match(&cc, pos_next, OP_return, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_return_undef);
          pos_next = cc.pos;
          break;
        }
        /* transform undefined if_true(l1)/if_false(l1) -> nop/goto(l1) */
        if (code_match(&cc, pos_next, M2(OP_if_false, OP_if_true), -1)) {
          val = 0;
          goto has_constant_test;
        }
#if SHORT_OPCODES
        /* transform undefined strict_eq -> is_undefined */
        if (code_match(&cc, pos_next, OP_strict_eq, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_is_undefined);
          pos_next = cc.pos;
          break;
        }
        /* transform undefined strict_neq if_false/if_true -> is_undefined
         * if_true/if_false */
        if (code_match(&cc, pos_next, OP_strict_neq,
                       M2(OP_if_false, OP_if_true), -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_is_undefined);
          pos_next = cc.pos;
          label = cc.label;
          op = cc.op ^ OP_if_false ^ OP_if_true;
          goto has_label;
        }
#endif
      }
      goto no_change;

    case OP_insert2:
      if (OPTIMIZE) {
        /* Transformation:
           insert2 put_field(a) drop -> put_field(a)
           insert2 put_var_strict(a) drop -> put_var_strict(a)
        */
        if (code_match(&cc, pos_next, M2(OP_put_field, OP_put_var_strict),
                       OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, cc.op);
          dbuf_put_u32(&bc_out, cc.atom);
          pos_next = cc.pos;
          break;
        }
      }
      goto no_change;

    case OP_dup:
      if (OPTIMIZE) {
        /* Transformation: dup put_x(n) drop -> put_x(n) */
        int op1, line2 = -1;
        /* Transformation: dup put_x(n) -> set_x(n) */
        if (code_match(&cc, pos_next,
                       M3(OP_put_loc, OP_put_arg, OP_put_var_ref), -1, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          op1 = cc.op + 1; /* put_x -> set_x */
          pos_next = cc.pos;
          if (code_match(&cc, cc.pos, OP_drop, -1)) {
            if (cc.line_num >= 0)
              line_num = cc.line_num;
            op1 -= 1; /* set_x drop -> put_x */
            pos_next = cc.pos;
            if (code_match(&cc, cc.pos, op1 - 1, cc.idx, -1)) {
              line2 = cc.line_num; /* delay line number update */
              op1 += 1;            /* put_x(n) get_x(n) -> set_x(n) */
              pos_next = cc.pos;
            }
          }
          add_pc2line_info(s, bc_out.size, line_num);
          put_short_code(&bc_out, op1, cc.idx);
          if (line2 >= 0)
            line_num = line2;
          break;
        }
      }
      goto no_change;

    case OP_get_loc:
      if (OPTIMIZE) {
        /* transformation:
           get_loc(n) post_dec put_loc(n) drop -> dec_loc(n)
           get_loc(n) post_inc put_loc(n) drop -> inc_loc(n)
           get_loc(n) dec dup put_loc(n) drop -> dec_loc(n)
           get_loc(n) inc dup put_loc(n) drop -> inc_loc(n)
         */
        int idx;
        idx = get_u16(bc_buf + pos + 1);
        if (idx >= 256)
          goto no_change;
        if (code_match(&cc, pos_next, M2(OP_post_dec, OP_post_inc), OP_put_loc,
                       idx, OP_drop, -1) ||
            code_match(&cc, pos_next, M2(OP_dec, OP_inc), OP_dup, OP_put_loc,
                       idx, OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, (cc.op == OP_inc || cc.op == OP_post_inc)
                                 ? OP_inc_loc
                                 : OP_dec_loc);
          dbuf_putc(&bc_out, idx);
          pos_next = cc.pos;
          break;
        }
        /* transformation:
           get_loc(n) push_atom_value(x) add dup put_loc(n) drop ->
           push_atom_value(x) add_loc(n)
         */
        if (code_match(&cc, pos_next, OP_push_atom_value, OP_add, OP_dup,
                       OP_put_loc, idx, OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
#if SHORT_OPCODES
          if (cc.atom == JS_ATOM_empty_string) {
            JS_FreeAtom(ctx, cc.atom);
            dbuf_putc(&bc_out, OP_push_empty_string);
          } else
#endif
          {
            dbuf_putc(&bc_out, OP_push_atom_value);
            dbuf_put_u32(&bc_out, cc.atom);
          }
          dbuf_putc(&bc_out, OP_add_loc);
          dbuf_putc(&bc_out, idx);
          pos_next = cc.pos;
          break;
        }
        /* transformation:
           get_loc(n) push_i32(x) add dup put_loc(n) drop -> push_i32(x)
           add_loc(n)
         */
        if (code_match(&cc, pos_next, OP_push_i32, OP_add, OP_dup, OP_put_loc,
                       idx, OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          push_short_int(&bc_out, cc.label);
          dbuf_putc(&bc_out, OP_add_loc);
          dbuf_putc(&bc_out, idx);
          pos_next = cc.pos;
          break;
        }
        /* transformation: XXX: also do these:
           get_loc(n) get_loc(x) add dup put_loc(n) drop -> get_loc(x)
           add_loc(n) get_loc(n) get_arg(x) add dup put_loc(n) drop ->
           get_arg(x) add_loc(n) get_loc(n) get_var_ref(x) add dup put_loc(n)
           drop -> get_var_ref(x) add_loc(n)
         */
        if (code_match(&cc, pos_next,
                       M3(OP_get_loc, OP_get_arg, OP_get_var_ref), -1, OP_add,
                       OP_dup, OP_put_loc, idx, OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          put_short_code(&bc_out, cc.op, cc.idx);
          dbuf_putc(&bc_out, OP_add_loc);
          dbuf_putc(&bc_out, idx);
          pos_next = cc.pos;
          break;
        }
        add_pc2line_info(s, bc_out.size, line_num);
        put_short_code(&bc_out, op, idx);
        break;
      }
      goto no_change;
#if SHORT_OPCODES
    case OP_get_arg:
    case OP_get_var_ref:
      if (OPTIMIZE) {
        int idx;
        idx = get_u16(bc_buf + pos + 1);
        add_pc2line_info(s, bc_out.size, line_num);
        put_short_code(&bc_out, op, idx);
        break;
      }
      goto no_change;
#endif
    case OP_put_loc:
    case OP_put_arg:
    case OP_put_var_ref:
      if (OPTIMIZE) {
        /* transformation: put_x(n) get_x(n) -> set_x(n) */
        int idx;
        idx = get_u16(bc_buf + pos + 1);
        if (code_match(&cc, pos_next, op - 1, idx, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          put_short_code(&bc_out, op + 1, idx);
          pos_next = cc.pos;
          break;
        }
        add_pc2line_info(s, bc_out.size, line_num);
        put_short_code(&bc_out, op, idx);
        break;
      }
      goto no_change;

    case OP_post_inc:
    case OP_post_dec:
      if (OPTIMIZE) {
        /* transformation:
           post_inc put_x drop -> inc put_x
           post_inc perm3 put_field drop -> inc put_field
           post_inc perm3 put_var_strict drop -> inc put_var_strict
           post_inc perm4 put_array_el drop -> inc put_array_el
         */
        int op1, idx;
        if (code_match(&cc, pos_next,
                       M3(OP_put_loc, OP_put_arg, OP_put_var_ref), -1, OP_drop,
                       -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          op1 = cc.op;
          idx = cc.idx;
          pos_next = cc.pos;
          if (code_match(&cc, cc.pos, op1 - 1, idx, -1)) {
            if (cc.line_num >= 0)
              line_num = cc.line_num;
            op1 += 1; /* put_x(n) get_x(n) -> set_x(n) */
            pos_next = cc.pos;
          }
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_dec + (op - OP_post_dec));
          put_short_code(&bc_out, op1, idx);
          break;
        }
        if (code_match(&cc, pos_next, OP_perm3,
                       M2(OP_put_field, OP_put_var_strict), OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_dec + (op - OP_post_dec));
          dbuf_putc(&bc_out, cc.op);
          dbuf_put_u32(&bc_out, cc.atom);
          pos_next = cc.pos;
          break;
        }
        if (code_match(&cc, pos_next, OP_perm4, OP_put_array_el, OP_drop, -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          add_pc2line_info(s, bc_out.size, line_num);
          dbuf_putc(&bc_out, OP_dec + (op - OP_post_dec));
          dbuf_putc(&bc_out, OP_put_array_el);
          pos_next = cc.pos;
          break;
        }
      }
      goto no_change;

#if SHORT_OPCODES
    case OP_typeof:
      if (OPTIMIZE) {
        /* simplify typeof tests */
        if (code_match(&cc, pos_next, OP_push_atom_value,
                       M4(OP_strict_eq, OP_strict_neq, OP_eq, OP_neq), -1)) {
          if (cc.line_num >= 0)
            line_num = cc.line_num;
          int op1 = (cc.op == OP_strict_eq || cc.op == OP_eq) ? OP_strict_eq
                                                              : OP_strict_neq;
          int op2 = -1;
          switch (cc.atom) {
          case JS_ATOM_undefined:
            op2 = OP_typeof_is_undefined;
            break;
          case JS_ATOM_function:
            op2 = OP_typeof_is_function;
            break;
          }
          if (op2 >= 0) {
            /* transform typeof(s) == "<type>" into is_<type> */
            if (op1 == OP_strict_eq) {
              add_pc2line_info(s, bc_out.size, line_num);
              dbuf_putc(&bc_out, op2);
              JS_FreeAtom(ctx, cc.atom);
              pos_next = cc.pos;
              break;
            }
            if (op1 == OP_strict_neq &&
                code_match(&cc, cc.pos, OP_if_false, -1)) {
              /* transform typeof(s) != "<type>" if_false into is_<type> if_true
               */
              if (cc.line_num >= 0)
                line_num = cc.line_num;
              add_pc2line_info(s, bc_out.size, line_num);
              dbuf_putc(&bc_out, op2);
              JS_FreeAtom(ctx, cc.atom);
              pos_next = cc.pos;
              label = cc.label;
              op = OP_if_true;
              goto has_label;
            }
          }
        }
      }
      goto no_change;
#endif

    default:
    no_change:
      add_pc2line_info(s, bc_out.size, line_num);
      dbuf_put(&bc_out, bc_buf + pos, len);
      break;
    }
  }

  /* check that there were no missing labels */
  for (i = 0; i < s->label_count; i++) {
    assert(label_slots[i].first_reloc == NULL);
  }
#if SHORT_OPCODES
  if (OPTIMIZE) {
    /* more jump optimizations */
    int patch_offsets = 0;
    for (i = 0, jp = s->jump_slots; i < s->jump_count; i++, jp++) {
      LabelSlot *ls;
      JumpSlot *jp1;
      int j, pos, diff, delta;

      delta = 3;
      switch (op = jp->op) {
      case OP_goto16:
        delta = 1;
        /* fall thru */
      case OP_if_false:
      case OP_if_true:
      case OP_goto:
        pos = jp->pos;
        diff = s->label_slots[jp->label].addr - pos;
        if (diff >= -128 && diff <= 127 + delta) {
          // put_u8(bc_out.buf + pos, diff);
          jp->size = 1;
          if (op == OP_goto16) {
            bc_out.buf[pos - 1] = jp->op = OP_goto8;
          } else {
            bc_out.buf[pos - 1] = jp->op = OP_if_false8 + (op - OP_if_false);
          }
          goto shrink;
        } else if (diff == (int16_t)diff && op == OP_goto) {
          // put_u16(bc_out.buf + pos, diff);
          jp->size = 2;
          delta = 2;
          bc_out.buf[pos - 1] = jp->op = OP_goto16;
        shrink:
          /* XXX: should reduce complexity, using 2 finger copy scheme */
          memmove(bc_out.buf + pos + jp->size,
                  bc_out.buf + pos + jp->size + delta,
                  bc_out.size - pos - jp->size - delta);
          bc_out.size -= delta;
          patch_offsets++;
          for (j = 0, ls = s->label_slots; j < s->label_count; j++, ls++) {
            if (ls->addr > pos)
              ls->addr -= delta;
          }
          for (j = i + 1, jp1 = jp + 1; j < s->jump_count; j++, jp1++) {
            if (jp1->pos > pos)
              jp1->pos -= delta;
          }
          for (j = 0; j < s->line_number_count; j++) {
            if (s->line_number_slots[j].pc > pos)
              s->line_number_slots[j].pc -= delta;
          }
          continue;
        }
        break;
      }
    }
    if (patch_offsets) {
      JumpSlot *jp1;
      int j;
      for (j = 0, jp1 = s->jump_slots; j < s->jump_count; j++, jp1++) {
        int diff1 = s->label_slots[jp1->label].addr - jp1->pos;
        switch (jp1->size) {
        case 1:
          put_u8(bc_out.buf + jp1->pos, diff1);
          break;
        case 2:
          put_u16(bc_out.buf + jp1->pos, diff1);
          break;
        case 4:
          put_u32(bc_out.buf + jp1->pos, diff1);
          break;
        }
      }
    }
  }
  js_free(ctx, s->jump_slots);
  s->jump_slots = NULL;
#endif
  js_free(ctx, s->label_slots);
  s->label_slots = NULL;
  /* XXX: should delay until copying to runtime bytecode function */
  compute_pc2line_info(s);
  js_free(ctx, s->line_number_slots);
  s->line_number_slots = NULL;
  /* set the new byte code */
  dbuf_free(&s->byte_code);
  s->byte_code = bc_out;
  s->use_short_opcodes = TRUE;
  if (dbuf_error(&s->byte_code)) {
    JS_ThrowOutOfMemory(ctx);
    return -1;
  }
  return 0;
fail:
  /* XXX: not safe */
  dbuf_free(&bc_out);
  return -1;
}

BOOL code_match(CodeContext *s, int pos, ...) {
  const uint8_t *tab = s->bc_buf;
  int op, len, op1, line_num, pos_next;
  va_list ap;
  BOOL ret = FALSE;

  line_num = -1;
  va_start(ap, pos);

  for (;;) {
    op1 = va_arg(ap, int);
    if (op1 == -1) {
      s->pos = pos;
      s->line_num = line_num;
      ret = TRUE;
      break;
    }
    for (;;) {
      if (pos >= s->bc_len)
        goto done;
      op = tab[pos];
      len = opcode_info[op].size;
      pos_next = pos + len;
      if (pos_next > s->bc_len)
        goto done;
      if (op == OP_line_num) {
        line_num = get_u32(tab + pos + 1);
        pos = pos_next;
      } else {
        break;
      }
    }
    if (op != op1) {
      if (op1 == (uint8_t)op1 || !op)
        break;
      if (op != (uint8_t)op1 && op != (uint8_t)(op1 >> 8) &&
          op != (uint8_t)(op1 >> 16) && op != (uint8_t)(op1 >> 24)) {
        break;
      }
      s->op = op;
    }

    pos++;
    switch (opcode_info[op].fmt) {
    case OP_FMT_loc8:
    case OP_FMT_u8: {
      int idx = tab[pos];
      int arg = va_arg(ap, int);
      if (arg == -1) {
        s->idx = idx;
      } else {
        if (arg != idx)
          goto done;
      }
      break;
    }
    case OP_FMT_u16:
    case OP_FMT_npop:
    case OP_FMT_loc:
    case OP_FMT_arg:
    case OP_FMT_var_ref: {
      int idx = get_u16(tab + pos);
      int arg = va_arg(ap, int);
      if (arg == -1) {
        s->idx = idx;
      } else {
        if (arg != idx)
          goto done;
      }
      break;
    }
    case OP_FMT_i32:
    case OP_FMT_u32:
    case OP_FMT_label:
    case OP_FMT_const: {
      s->label = get_u32(tab + pos);
      break;
    }
    case OP_FMT_label_u16: {
      s->label = get_u32(tab + pos);
      s->val = get_u16(tab + pos + 4);
      break;
    }
    case OP_FMT_atom: {
      s->atom = get_u32(tab + pos);
      break;
    }
    case OP_FMT_atom_u8: {
      s->atom = get_u32(tab + pos);
      s->val = get_u8(tab + pos + 4);
      break;
    }
    case OP_FMT_atom_u16: {
      s->atom = get_u32(tab + pos);
      s->val = get_u16(tab + pos + 4);
      break;
    }
    case OP_FMT_atom_label_u8: {
      s->atom = get_u32(tab + pos);
      s->label = get_u32(tab + pos + 4);
      s->val = get_u8(tab + pos + 8);
      break;
    }
    default:
      break;
    }
    pos = pos_next;
  }
done:
  va_end(ap);
  return ret;
}

int skip_dead_code(JSFunctionDef *s, const uint8_t *bc_buf, int bc_len, int pos,
                   int *linep) {
  int op, len, label;

  for (; pos < bc_len; pos += len) {
    op = bc_buf[pos];
    len = opcode_info[op].size;
    if (op == OP_line_num) {
      *linep = get_u32(bc_buf + pos + 1);
    } else if (op == OP_label) {
      label = get_u32(bc_buf + pos + 1);
      if (update_label(s, label, 0) > 0)
        break;
#if 0
            if (s->label_slots[label].first_reloc) {
                printf("line %d: unreferenced label %d:%d has relocations\n",
                       *linep, label, s->label_slots[label].pos2);
            }
#endif
      assert(s->label_slots[label].first_reloc == NULL);
    } else {
      /* XXX: output a warning for unreachable code? */
      JSAtom atom;
      switch (opcode_info[op].fmt) {
      case OP_FMT_label:
      case OP_FMT_label_u16:
        label = get_u32(bc_buf + pos + 1);
        update_label(s, label, -1);
        break;
      case OP_FMT_atom_label_u8:
      case OP_FMT_atom_label_u16:
        label = get_u32(bc_buf + pos + 5);
        update_label(s, label, -1);
        /* fall thru */
      case OP_FMT_atom:
      case OP_FMT_atom_u8:
      case OP_FMT_atom_u16:
        atom = get_u32(bc_buf + pos + 1);
        JS_FreeAtom(s->ctx, atom);
        break;
      default:
        break;
      }
    }
  }
  return pos;
}
