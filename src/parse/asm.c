#include "libs/cutils.h"
#include "parse.h"

BOOL js_is_live_code(JSParseState *s) {
  switch (get_prev_opcode(s->cur_func)) {
  case OP_tail_call:
  case OP_tail_call_method:
  case OP_return:
  case OP_return_undef:
  case OP_return_async:
  case OP_throw:
  case OP_throw_error:
  case OP_goto:
#if SHORT_OPCODES
  case OP_goto8:
  case OP_goto16:
#endif
  case OP_ret:
    return FALSE;
  default:
    return TRUE;
  }
}

void emit_u8(JSParseState *s, uint8_t val) {
  dbuf_putc(&s->cur_func->byte_code, val);
}

void emit_u16(JSParseState *s, uint16_t val) {
  dbuf_put_u16(&s->cur_func->byte_code, val);
}

void emit_u32(JSParseState *s, uint32_t val) {
  dbuf_put_u32(&s->cur_func->byte_code, val);
}

void emit_op(JSParseState *s, uint8_t val) {
  JSFunctionDef *fd = s->cur_func;
  DynBuf *bc = &fd->byte_code;

  /* Use the line number of the last token used, not the next token,
     nor the current offset in the source file.
   */
  if (unlikely(fd->last_opcode_line_num != s->last_line_num)) {
    dbuf_putc(bc, OP_line_num);
    dbuf_put_u32(bc, s->last_line_num);
    fd->last_opcode_line_num = s->last_line_num;
  }

  if (s->col_num2emit != 0) {
    dbuf_putc(bc, OP_col_num);
    dbuf_put_u64(bc, s->col_num2emit);
    s->col_num2emit = 0;
  }

  fd->last_opcode_pos = bc->size;
  dbuf_putc(bc, val);
}

void emit_atom(JSParseState *s, JSAtom name) {
  emit_u32(s, JS_DupAtom(s->ctx, name));
}

int update_label(JSFunctionDef *s, int label, int delta) {
  LabelSlot *ls;

  assert(label >= 0 && label < s->label_count);
  ls = &s->label_slots[label];
  ls->ref_count += delta;
  assert(ls->ref_count >= 0);
  return ls->ref_count;
}

int new_label_fd(JSFunctionDef *fd, int label) {
  LabelSlot *ls;

  if (label < 0) {
    if (js_resize_array(fd->ctx, (void *)&fd->label_slots,
                        sizeof(fd->label_slots[0]), &fd->label_size,
                        fd->label_count + 1))
      return -1;
    label = fd->label_count++;
    ls = &fd->label_slots[label];
    ls->ref_count = 0;
    ls->pos = -1;
    ls->pos2 = -1;
    ls->addr = -1;
    ls->first_reloc = NULL;
  }
  return label;
}

int new_label(JSParseState *s) { return new_label_fd(s->cur_func, -1); }

/* return the label ID offset */
int emit_label(JSParseState *s, int label) {
  if (label >= 0) {
    emit_op(s, OP_label);
    emit_u32(s, label);
    s->cur_func->label_slots[label].pos = s->cur_func->byte_code.size;
    return s->cur_func->byte_code.size - 4;
  } else {
    return -1;
  }
}

/* return label or -1 if dead code */
int emit_goto(JSParseState *s, int opcode, int label) {
  if (js_is_live_code(s)) {
    if (label < 0)
      label = new_label(s);
    emit_op(s, opcode);
    emit_u32(s, label);
    s->cur_func->label_slots[label].ref_count++;
    return label;
  }
  return -1;
}

/* return the constant pool index. 'val' is not duplicated. */
int cpool_add(JSParseState *s, JSValue val) {
  JSFunctionDef *fd = s->cur_func;

  if (js_resize_array(s->ctx, (void *)&fd->cpool, sizeof(fd->cpool[0]),
                      &fd->cpool_size, fd->cpool_count + 1))
    return -1;
  fd->cpool[fd->cpool_count++] = val;
  return fd->cpool_count - 1;
}

__exception int emit_push_const(JSParseState *s, JSValueConst val,
                                BOOL as_atom) {
  int idx;

  if (JS_VALUE_GET_TAG(val) == JS_TAG_STRING && as_atom) {
    JSAtom atom;
    /* warning: JS_NewAtomStr frees the string value */
    JS_DupValue(s->ctx, val);
    atom = JS_NewAtomStr(s->ctx, JS_VALUE_GET_STRING(val));
    if (atom != JS_ATOM_NULL && !__JS_AtomIsTaggedInt(atom)) {
      emit_op(s, OP_push_atom_value);
      emit_u32(s, atom);
      return 0;
    }
  }

  idx = cpool_add(s, JS_DupValue(s->ctx, val));
  if (idx < 0)
    return -1;
  emit_op(s, OP_push_const);
  emit_u32(s, idx);
  return 0;
}
