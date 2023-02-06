#include "dump.h"

#include "class.h"
#include "func.h"
#include "intrins/intrins.h"
#include "num.h"
#include "obj.h"
#include "shape.h"
#include "str.h"

__maybe_unused void JS_DumpString(JSRuntime *rt, const JSString *p) {
  int i, c, sep;

  if (p == NULL) {
    printf("<null>");
    return;
  }
  printf("%d", p->header.ref_count);
  sep = (p->header.ref_count == 1) ? '\"' : '\'';
  putchar(sep);
  for (i = 0; i < p->len; i++) {
    if (p->is_wide_char)
      c = p->u.str16[i];
    else
      c = p->u.str8[i];
    if (c == sep || c == '\\') {
      putchar('\\');
      putchar(c);
    } else if (c >= ' ' && c <= 126) {
      putchar(c);
    } else if (c == '\n') {
      putchar('\\');
      putchar('n');
    } else {
      printf("\\u%04x", c);
    }
  }
  putchar(sep);
}

__maybe_unused void JS_DumpAtoms(JSRuntime *rt) {
  JSAtomStruct *p;
  int h, i;
  /* This only dumps hashed atoms, not JS_ATOM_TYPE_SYMBOL atoms */
  printf("JSAtom count=%d size=%d hash_size=%d:\n", rt->atom_count,
         rt->atom_size, rt->atom_hash_size);
  printf("JSAtom hash table: {\n");
  for (i = 0; i < rt->atom_hash_size; i++) {
    h = rt->atom_hash[i];
    if (h) {
      printf("  %d:", i);
      while (h) {
        p = rt->atom_array[h];
        printf(" ");
        JS_DumpString(rt, p);
        h = p->hash_next;
      }
      printf("\n");
    }
  }
  printf("}\n");
  printf("JSAtom table: {\n");
  for (i = 0; i < rt->atom_size; i++) {
    p = rt->atom_array[i];
    if (!atom_is_free(p)) {
      printf("  %d: { %d %08x ", i, p->atom_type, p->hash);
      if (!(p->len == 0 && p->is_wide_char != 0))
        JS_DumpString(rt, p);
      printf(" %d }\n", p->hash_next);
    }
  }
  printf("}\n");
}

__maybe_unused void JS_DumpObjectHeader(JSRuntime *rt) {
  printf("%14s %4s %4s %14s %10s %s\n", "ADDRESS", "REFS", "SHRF", "PROTO",
         "CLASS", "PROPS");
}

/* for debug only: dump an object without side effect */
__maybe_unused void JS_DumpObject(JSRuntime *rt, JSObject *p) {
  uint32_t i;
  char atom_buf[ATOM_GET_STR_BUF_SIZE];
  JSShape *sh;
  JSShapeProperty *prs;
  JSProperty *pr;
  BOOL is_first = TRUE;

  /* XXX: should encode atoms with special characters */
  sh = p->shape; /* the shape can be NULL while freeing an object */
  printf("%14p %4d ", (void *)p, p->header.ref_count);
  if (sh) {
    printf("%3d%c %14p ", sh->header.ref_count, " *"[sh->is_hashed],
           (void *)sh -> proto);
  } else {
    printf("%3s  %14s ", "-", "-");
  }
  printf("%10s ", JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf),
                                  rt->class_array[p->class_id].class_name));
  if (p->is_exotic && p->fast_array) {
    printf("[ ");
    for (i = 0; i < p->u.array.count; i++) {
      if (i != 0)
        printf(", ");
      switch (p->class_id) {
      case JS_CLASS_ARRAY:
      case JS_CLASS_ARGUMENTS:
        JS_DumpValueShort(rt, p->u.array.u.values[i]);
        break;
      case JS_CLASS_UINT8C_ARRAY:
      case JS_CLASS_INT8_ARRAY:
      case JS_CLASS_UINT8_ARRAY:
      case JS_CLASS_INT16_ARRAY:
      case JS_CLASS_UINT16_ARRAY:
      case JS_CLASS_INT32_ARRAY:
      case JS_CLASS_UINT32_ARRAY:
#ifdef CONFIG_BIGNUM
      case JS_CLASS_BIG_INT64_ARRAY:
      case JS_CLASS_BIG_UINT64_ARRAY:
#endif
      case JS_CLASS_FLOAT32_ARRAY:
      case JS_CLASS_FLOAT64_ARRAY: {
        int size = 1 << typed_array_size_log2(p->class_id);
        const uint8_t *b = p->u.array.u.uint8_ptr + i * size;
        while (size-- > 0)
          printf("%02X", *b++);
      } break;
      }
    }
    printf(" ] ");
  }

  if (sh) {
    printf("{ ");
    for (i = 0, prs = get_shape_prop(sh); i < sh->prop_count; i++, prs++) {
      if (prs->atom != JS_ATOM_NULL) {
        pr = &p->prop[i];
        if (!is_first)
          printf(", ");
        printf("%s: ",
               JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf), prs->atom));
        if ((prs->flags & JS_PROP_TMASK) == JS_PROP_GETSET) {
          printf("[getset %p %p]", (void *)pr->u.getset.getter,
                 (void *)pr->u.getset.setter);
        } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_VARREF) {
          printf("[varref %p]", (void *)pr->u.var_ref);
        } else if ((prs->flags & JS_PROP_TMASK) == JS_PROP_AUTOINIT) {
          printf("[autoinit %p %d %p]", (void *)js_autoinit_get_realm(pr),
                 js_autoinit_get_id(pr), (void *)pr->u.init.opaque);
        } else {
          JS_DumpValueShort(rt, pr->u.value);
        }
        is_first = FALSE;
      }
    }
    printf(" }");
  }

  if (js_class_has_bytecode(p->class_id)) {
    JSFunctionBytecode *b = p->u.func.function_bytecode;
    JSVarRef **var_refs;
    if (b->closure_var_count) {
      var_refs = p->u.func.var_refs;
      printf(" Closure:");
      for (i = 0; i < b->closure_var_count; i++) {
        printf(" ");
        JS_DumpValueShort(rt, var_refs[i]->value);
      }
      if (p->u.func.home_object) {
        printf(" HomeObject: ");
        JS_DumpValueShort(rt, JS_MKPTR(JS_TAG_OBJECT, p->u.func.home_object));
      }
    }
  }
  printf("\n");
}

__maybe_unused void JS_DumpGCObject(JSRuntime *rt, JSGCObjectHeader *p) {
  if (p->gc_obj_type == JS_GC_OBJ_TYPE_JS_OBJECT) {
    JS_DumpObject(rt, (JSObject *)p);
  } else {
    printf("%14p %4d ", (void *)p, p->ref_count);
    switch (p->gc_obj_type) {
    case JS_GC_OBJ_TYPE_FUNCTION_BYTECODE:
      printf("[function bytecode]");
      break;
    case JS_GC_OBJ_TYPE_SHAPE:
      printf("[shape]");
      break;
    case JS_GC_OBJ_TYPE_VAR_REF:
      printf("[var_ref]");
      break;
    case JS_GC_OBJ_TYPE_ASYNC_FUNCTION:
      printf("[async_function]");
      break;
    case JS_GC_OBJ_TYPE_JS_CONTEXT:
      printf("[js_context]");
      break;
    default:
      printf("[unknown %d]", p->gc_obj_type);
      break;
    }
    printf("\n");
  }
}

__maybe_unused void JS_DumpValueShort(JSRuntime *rt, JSValueConst val) {
  uint32_t tag = JS_VALUE_GET_NORM_TAG(val);
  const char *str;

  switch (tag) {
  case JS_TAG_INT:
    printf("%d", JS_VALUE_GET_INT(val));
    break;
  case JS_TAG_BOOL:
    if (JS_VALUE_GET_BOOL(val))
      str = "true";
    else
      str = "false";
    goto print_str;
  case JS_TAG_NULL:
    str = "null";
    goto print_str;
  case JS_TAG_EXCEPTION:
    str = "exception";
    goto print_str;
  case JS_TAG_UNINITIALIZED:
    str = "uninitialized";
    goto print_str;
  case JS_TAG_UNDEFINED:
    str = "undefined";
  print_str:
    printf("%s", str);
    break;
  case JS_TAG_FLOAT64:
    printf("%.14g", JS_VALUE_GET_FLOAT64(val));
    break;
#ifdef CONFIG_BIGNUM
  case JS_TAG_BIG_INT: {
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    char *str;
    str = bf_ftoa(NULL, &p->num, 10, 0, BF_RNDZ | BF_FTOA_FORMAT_FRAC);
    printf("%sn", str);
    bf_realloc(&rt->bf_ctx, str, 0);
  } break;
  case JS_TAG_BIG_FLOAT: {
    JSBigFloat *p = JS_VALUE_GET_PTR(val);
    char *str;
    str = bf_ftoa(NULL, &p->num, 16, BF_PREC_INF,
                  BF_RNDZ | BF_FTOA_FORMAT_FREE | BF_FTOA_ADD_PREFIX);
    printf("%sl", str);
    bf_free(&rt->bf_ctx, str);
  } break;
  case JS_TAG_BIG_DECIMAL: {
    JSBigDecimal *p = JS_VALUE_GET_PTR(val);
    char *str;
    str = bfdec_ftoa(NULL, &p->num, BF_PREC_INF, BF_RNDZ | BF_FTOA_FORMAT_FREE);
    printf("%sm", str);
    bf_free(&rt->bf_ctx, str);
  } break;
#endif
  case JS_TAG_STRING: {
    JSString *p;
    p = JS_VALUE_GET_STRING(val);
    JS_DumpString(rt, p);
  } break;
  case JS_TAG_FUNCTION_BYTECODE: {
    JSFunctionBytecode *b = JS_VALUE_GET_PTR(val);
    char buf[ATOM_GET_STR_BUF_SIZE];
    printf("[bytecode %s]",
           JS_AtomGetStrRT(rt, buf, sizeof(buf), b->func_name));
  } break;
  case JS_TAG_OBJECT: {
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSAtom atom = rt->class_array[p->class_id].class_name;
    char atom_buf[ATOM_GET_STR_BUF_SIZE];
    printf("[%s %p]", JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf), atom),
           (void *)p);
  } break;
  case JS_TAG_SYMBOL: {
    JSAtomStruct *p = JS_VALUE_GET_PTR(val);
    char atom_buf[ATOM_GET_STR_BUF_SIZE];
    printf("Symbol(%s)", JS_AtomGetStrRT(rt, atom_buf, sizeof(atom_buf),
                                         js_get_atom_index(rt, p)));
  } break;
  case JS_TAG_MODULE:
    printf("[module]");
    break;
  default:
    printf("[unknown tag %d]", tag);
    break;
  }
}

__maybe_unused void JS_DumpValue(JSContext *ctx, JSValueConst val) {
  JS_DumpValueShort(ctx->rt, val);
}

__maybe_unused void JS_PrintValue(JSContext *ctx, const char *str,
                                  JSValueConst val) {
  printf("%s=", str);
  JS_DumpValueShort(ctx->rt, val);
  printf("\n");
}

#ifdef DUMP_BYTECODE
const char *skip_lines(const char *p, int n) {
  while (n-- > 0 && *p) {
    while (*p && *p++ != '\n')
      continue;
  }
  return p;
}

void print_lines(const char *source, int line, int line1) {
  const char *s = source;
  const char *p = skip_lines(s, line);
  if (*p) {
    while (line++ < line1) {
      p = skip_lines(s = p, 1);
      printf(";; %.*s", (int)(p - s), s);
      if (!*p) {
        if (p[-1] != '\n')
          printf("\n");
        break;
      }
    }
  }
}

void dump_byte_code(JSContext *ctx, int pass, const uint8_t *tab, int len,
                    const JSVarDef *args, int arg_count, const JSVarDef *vars,
                    int var_count, const JSClosureVar *closure_var,
                    int closure_var_count, const JSValue *cpool,
                    uint32_t cpool_count, const char *source, int line_num,
                    const LabelSlot *label_slots, JSFunctionBytecode *b) {
  const JSOpCode *oi;
  int pos, pos_next, op, size, idx, addr, line, line1, in_source;
  uint8_t *bits = js_mallocz(ctx, len * sizeof(*bits));
  BOOL use_short_opcodes = (b != NULL);

  /* scan for jump targets */
  for (pos = 0; pos < len; pos = pos_next) {
    op = tab[pos];
    if (use_short_opcodes)
      oi = &short_opcode_info(op);
    else
      oi = &opcode_info[op];
    pos_next = pos + oi->size;
    if (op < OP_COUNT) {
      switch (oi->fmt) {
#if SHORT_OPCODES
      case OP_FMT_label8:
        pos++;
        addr = (int8_t)tab[pos];
        goto has_addr;
      case OP_FMT_label16:
        pos++;
        addr = (int16_t)get_u16(tab + pos);
        goto has_addr;
#endif
      case OP_FMT_atom_label_u8:
      case OP_FMT_atom_label_u16:
        pos += 4;
        /* fall thru */
      case OP_FMT_label:
      case OP_FMT_label_u16:
        pos++;
        addr = get_u32(tab + pos);
        goto has_addr;
      has_addr:
        if (pass == 1)
          addr = label_slots[addr].pos;
        if (pass == 2)
          addr = label_slots[addr].pos2;
        if (pass == 3)
          addr += pos;
        if (addr >= 0 && addr < len)
          bits[addr] |= 1;
        break;
      }
    }
  }
  in_source = 0;
  if (source) {
    /* Always print first line: needed if single line */
    print_lines(source, 0, 1);
    in_source = 1;
  }
  line1 = line = 1;
  pos = 0;
  while (pos < len) {
    op = tab[pos];
    if (source) {
      if (b) {
        line1 = find_line_num(ctx, b, pos) - line_num + 1;
      } else if (op == OP_line_num) {
        line1 = get_u32(tab + pos + 1) - line_num + 1;
      }
      if (line1 > line) {
        if (!in_source)
          printf("\n");
        in_source = 1;
        print_lines(source, line, line1);
        line = line1;
        // bits[pos] |= 2;
      }
    }
    if (in_source)
      printf("\n");
    in_source = 0;
    if (op >= OP_COUNT) {
      printf("invalid opcode (0x%02x)\n", op);
      pos++;
      continue;
    }
    if (use_short_opcodes)
      oi = &short_opcode_info(op);
    else
      oi = &opcode_info[op];
    size = oi->size;
    if (pos + size > len) {
      printf("truncated opcode (0x%02x)\n", op);
      break;
    }
#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 16)
    {
      int i, x, x0;
      x = x0 = printf("%5d ", pos);
      for (i = 0; i < size; i++) {
        if (i == 6) {
          printf("\n%*s", x = x0, "");
        }
        x += printf(" %02X", tab[pos + i]);
      }
      printf("%*s", x0 + 20 - x, "");
    }
#endif
    if (bits[pos]) {
      printf("%5d:  ", pos);
    } else {
      printf("        ");
    }
    printf("%s", oi->name);
    pos++;
    switch (oi->fmt) {
    case OP_FMT_none_int:
      printf(" %d", op - OP_push_0);
      break;
    case OP_FMT_npopx:
      printf(" %d", op - OP_call0);
      break;
    case OP_FMT_u8:
      printf(" %u", get_u8(tab + pos));
      break;
    case OP_FMT_i8:
      printf(" %d", get_i8(tab + pos));
      break;
    case OP_FMT_u16:
    case OP_FMT_npop:
      printf(" %u", get_u16(tab + pos));
      break;
    case OP_FMT_npop_u16:
      printf(" %u,%u", get_u16(tab + pos), get_u16(tab + pos + 2));
      break;
    case OP_FMT_i16:
      printf(" %d", get_i16(tab + pos));
      break;
    case OP_FMT_i32:
      printf(" %d", get_i32(tab + pos));
      break;
    case OP_FMT_u32:
      printf(" %u", get_u32(tab + pos));
      break;
#if SHORT_OPCODES
    case OP_FMT_label8:
      addr = get_i8(tab + pos);
      goto has_addr1;
    case OP_FMT_label16:
      addr = get_i16(tab + pos);
      goto has_addr1;
#endif
    case OP_FMT_label:
      addr = get_u32(tab + pos);
      goto has_addr1;
    has_addr1:
      if (pass == 1)
        printf(" %u:%u", addr, label_slots[addr].pos);
      if (pass == 2)
        printf(" %u:%u", addr, label_slots[addr].pos2);
      if (pass == 3)
        printf(" %u", addr + pos);
      break;
    case OP_FMT_label_u16:
      addr = get_u32(tab + pos);
      if (pass == 1)
        printf(" %u:%u", addr, label_slots[addr].pos);
      if (pass == 2)
        printf(" %u:%u", addr, label_slots[addr].pos2);
      if (pass == 3)
        printf(" %u", addr + pos);
      printf(",%u", get_u16(tab + pos + 4));
      break;
#if SHORT_OPCODES
    case OP_FMT_const8:
      idx = get_u8(tab + pos);
      goto has_pool_idx;
#endif
    case OP_FMT_const:
      idx = get_u32(tab + pos);
      goto has_pool_idx;
    has_pool_idx:
      printf(" %u: ", idx);
      if (idx < cpool_count) {
        JS_DumpValue(ctx, cpool[idx]);
      }
      break;
    case OP_FMT_atom:
      printf(" ");
      print_atom(ctx, get_u32(tab + pos));
      break;
    case OP_FMT_atom_u8:
      printf(" ");
      print_atom(ctx, get_u32(tab + pos));
      printf(",%d", get_u8(tab + pos + 4));
      break;
    case OP_FMT_atom_u16:
      printf(" ");
      print_atom(ctx, get_u32(tab + pos));
      printf(",%d", get_u16(tab + pos + 4));
      break;
    case OP_FMT_atom_label_u8:
    case OP_FMT_atom_label_u16:
      printf(" ");
      print_atom(ctx, get_u32(tab + pos));
      addr = get_u32(tab + pos + 4);
      if (pass == 1)
        printf(",%u:%u", addr, label_slots[addr].pos);
      if (pass == 2)
        printf(",%u:%u", addr, label_slots[addr].pos2);
      if (pass == 3)
        printf(",%u", addr + pos + 4);
      if (oi->fmt == OP_FMT_atom_label_u8)
        printf(",%u", get_u8(tab + pos + 8));
      else
        printf(",%u", get_u16(tab + pos + 8));
      break;
    case OP_FMT_none_loc:
      idx = (op - OP_get_loc0) % 4;
      goto has_loc;
    case OP_FMT_loc8:
      idx = get_u8(tab + pos);
      goto has_loc;
    case OP_FMT_loc:
      idx = get_u16(tab + pos);
    has_loc:
      printf(" %d: ", idx);
      if (idx < var_count) {
        print_atom(ctx, vars[idx].var_name);
      }
      break;
    case OP_FMT_none_arg:
      idx = (op - OP_get_arg0) % 4;
      goto has_arg;
    case OP_FMT_arg:
      idx = get_u16(tab + pos);
    has_arg:
      printf(" %d: ", idx);
      if (idx < arg_count) {
        print_atom(ctx, args[idx].var_name);
      }
      break;
    case OP_FMT_none_var_ref:
      idx = (op - OP_get_var_ref0) % 4;
      goto has_var_ref;
    case OP_FMT_var_ref:
      idx = get_u16(tab + pos);
    has_var_ref:
      printf(" %d: ", idx);
      if (idx < closure_var_count) {
        print_atom(ctx, closure_var[idx].var_name);
      }
      break;
    default:
      break;
    }
    printf("\n");
    pos += oi->size - 1;
  }
  if (source) {
    if (!in_source)
      printf("\n");
    print_lines(source, line, INT32_MAX);
  }
  js_free(ctx, bits);
}

__maybe_unused void dump_pc2line(JSContext *ctx, const uint8_t *buf, int len,
                                 int line_num) {
  const uint8_t *p_end, *p_next, *p;
  int pc, v;
  unsigned int op;

  if (len <= 0)
    return;

  printf("%5s %5s\n", "PC", "LINE");

  p = buf;
  p_end = buf + len;
  pc = 0;
  while (p < p_end) {
    op = *p++;
    if (op == 0) {
      v = unicode_from_utf8(p, p_end - p, &p_next);
      if (v < 0)
        goto fail;
      pc += v;
      p = p_next;
      v = unicode_from_utf8(p, p_end - p, &p_next);
      if (v < 0) {
      fail:
        printf("invalid pc2line encode pos=%d\n", (int)(p - buf));
        return;
      }
      if (!(v & 1)) {
        v = v >> 1;
      } else {
        v = -(v >> 1) - 1;
      }
      line_num += v;
      p = p_next;
    } else {
      op -= PC2LINE_OP_FIRST;
      pc += (op / PC2LINE_RANGE);
      line_num += (op % PC2LINE_RANGE) + PC2LINE_BASE;
    }
    printf("%5d %5d\n", pc, line_num);
  }
}

__maybe_unused void js_dump_function_bytecode(JSContext *ctx,
                                              JSFunctionBytecode *b) {
  int i;
  char atom_buf[ATOM_GET_STR_BUF_SIZE];
  const char *str;

  if (b->has_debug && b->debug.filename != JS_ATOM_NULL) {
    str = JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf), b->debug.filename);
    printf("%s:%d: ", str, b->debug.line_num);
  }

  str = JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf), b->func_name);
  printf("function: %s%s\n", &"*"[b->func_kind != JS_FUNC_GENERATOR], str);
  if (b->js_mode) {
    printf("  mode:");
    if (b->js_mode & JS_MODE_STRICT)
      printf(" strict");
#ifdef CONFIG_BIGNUM
    if (b->js_mode & JS_MODE_MATH)
      printf(" math");
#endif
    printf("\n");
  }
  if (b->arg_count && b->vardefs) {
    printf("  args:");
    for (i = 0; i < b->arg_count; i++) {
      printf(" %s", JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf),
                                  b->vardefs[i].var_name));
    }
    printf("\n");
  }
  if (b->var_count && b->vardefs) {
    printf("  locals:\n");
    for (i = 0; i < b->var_count; i++) {
      JSVarDef *vd = &b->vardefs[b->arg_count + i];
      printf("%5d: %s %s", i,
             vd->var_kind == JS_VAR_CATCH ? "catch"
             : (vd->var_kind == JS_VAR_FUNCTION_DECL ||
                vd->var_kind == JS_VAR_NEW_FUNCTION_DECL)
                 ? "function"
             : vd->is_const   ? "const"
             : vd->is_lexical ? "let"
                              : "var",
             JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf), vd->var_name));
      if (vd->scope_level)
        printf(" [level:%d next:%d]", vd->scope_level, vd->scope_next);
      printf("\n");
    }
  }
  if (b->closure_var_count) {
    printf("  closure vars:\n");
    for (i = 0; i < b->closure_var_count; i++) {
      JSClosureVar *cv = &b->closure_var[i];
      printf("%5d: %s %s:%s%d %s\n", i,
             JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf), cv->var_name),
             cv->is_local ? "local" : "parent", cv->is_arg ? "arg" : "loc",
             cv->var_idx,
             cv->is_const     ? "const"
             : cv->is_lexical ? "let"
                              : "var");
    }
  }
  printf("  stack_size: %d\n", b->stack_size);
  printf("  opcodes:\n");
  dump_byte_code(ctx, 3, b->byte_code_buf, b->byte_code_len, b->vardefs,
                 b->arg_count, b->vardefs ? b->vardefs + b->arg_count : NULL,
                 b->var_count, b->closure_var, b->closure_var_count, b->cpool,
                 b->cpool_count, b->has_debug ? b->debug.source : NULL,
                 b->has_debug ? b->debug.line_num : -1, NULL, b);
#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 32)
  if (b->has_debug)
    dump_pc2line(ctx, b->debug.pc2line_buf, b->debug.pc2line_len,
                 b->debug.line_num);
#endif
  printf("\n");
}
#endif