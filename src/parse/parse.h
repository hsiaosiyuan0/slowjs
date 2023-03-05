#ifndef QUICKJS_PARSE_H
#define QUICKJS_PARSE_H

#include "def.h"

#include "libs/cutils.h"
#include "vm/func.h"
#include "vm/instr.h"
#include "vm/mod.h"
#include "vm/str.h"

enum {
  TOK_NUMBER = -128,
  TOK_STRING,
  TOK_TEMPLATE,
  TOK_IDENT,
  TOK_REGEXP,
  /* warning: order matters (see js_parse_assign_expr) */
  TOK_MUL_ASSIGN,
  TOK_DIV_ASSIGN,
  TOK_MOD_ASSIGN,
  TOK_PLUS_ASSIGN,
  TOK_MINUS_ASSIGN,
  TOK_SHL_ASSIGN,
  TOK_SAR_ASSIGN,
  TOK_SHR_ASSIGN,
  TOK_AND_ASSIGN,
  TOK_XOR_ASSIGN,
  TOK_OR_ASSIGN,
#ifdef CONFIG_BIGNUM
  TOK_MATH_POW_ASSIGN,
#endif
  TOK_POW_ASSIGN,
  TOK_LAND_ASSIGN,
  TOK_LOR_ASSIGN,
  TOK_DOUBLE_QUESTION_MARK_ASSIGN,
  TOK_DEC,
  TOK_INC,
  TOK_SHL,
  TOK_SAR,
  TOK_SHR,
  TOK_LT,
  TOK_LTE,
  TOK_GT,
  TOK_GTE,
  TOK_EQ,
  TOK_STRICT_EQ,
  TOK_NEQ,
  TOK_STRICT_NEQ,
  TOK_LAND,
  TOK_LOR,
#ifdef CONFIG_BIGNUM
  TOK_MATH_POW,
#endif
  TOK_POW,
  TOK_ARROW,
  TOK_ELLIPSIS,
  TOK_DOUBLE_QUESTION_MARK,
  TOK_QUESTION_MARK_DOT,
  TOK_ERROR,
  TOK_PRIVATE_NAME,
  TOK_EOF,
  /* keywords: WARNING: same order as atoms */
  TOK_NULL, /* must be first */
  TOK_FALSE,
  TOK_TRUE,
  TOK_IF,
  TOK_ELSE,
  TOK_RETURN,
  TOK_VAR,
  TOK_THIS,
  TOK_DELETE,
  TOK_VOID,
  TOK_TYPEOF,
  TOK_NEW,
  TOK_IN,
  TOK_INSTANCEOF,
  TOK_DO,
  TOK_WHILE,
  TOK_FOR,
  TOK_BREAK,
  TOK_CONTINUE,
  TOK_SWITCH,
  TOK_CASE,
  TOK_DEFAULT,
  TOK_THROW,
  TOK_TRY,
  TOK_CATCH,
  TOK_FINALLY,
  TOK_FUNCTION,
  TOK_DEBUGGER,
  TOK_WITH,
  /* FutureReservedWord */
  TOK_CLASS,
  TOK_CONST,
  TOK_ENUM,
  TOK_EXPORT,
  TOK_EXTENDS,
  TOK_IMPORT,
  TOK_SUPER,
  /* FutureReservedWords when parsing strict mode code */
  TOK_IMPLEMENTS,
  TOK_INTERFACE,
  TOK_LET,
  TOK_PACKAGE,
  TOK_PRIVATE,
  TOK_PROTECTED,
  TOK_PUBLIC,
  TOK_STATIC,
  TOK_YIELD,
  TOK_AWAIT, /* must be last */
  TOK_OF,    /* only used for js_parse_skip_parens_token() */
};

#define TOK_FIRST_KEYWORD TOK_NULL
#define TOK_LAST_KEYWORD TOK_AWAIT

/* unicode code points */
#define CP_NBSP 0x00a0
#define CP_BOM 0xfeff

#define CP_LS 0x2028
#define CP_PS 0x2029

typedef struct JSToken {
  int val;
  int line_num; /* line number of token start, starts from 1 */
  int col_num;  /* column number of token start, starts from 1 */
  const uint8_t *ptr;
  union {
    struct {
      JSValue str;
      int sep;
    } str;
    struct {
      JSValue val;
#ifdef CONFIG_BIGNUM
      slimb_t exponent; /* may be != 0 only if val is a float */
#endif
    } num;
    struct {
      JSAtom atom;
      BOOL has_escape;
      BOOL is_reserved;
    } ident;
    struct {
      JSValue body;
      JSValue flags;
    } regexp;
  } u;
} JSToken;

typedef struct BlockEnv {
  struct BlockEnv *prev;
  JSAtom label_name; /* JS_ATOM_NULL if none */
  int label_break;   /* -1 if none */
  int label_cont;    /* -1 if none */
  int drop_count;    /* number of stack elements to drop */
  int label_finally; /* -1 if none */
  int scope_level;
  int has_iterator;
} BlockEnv;

typedef struct JSGlobalVar {
  int cpool_idx;          /* if >= 0, index in the constant pool for hoisted
                             function defintion*/
  uint8_t force_init : 1; /* force initialization to undefined */
  uint8_t is_lexical : 1; /* global let/const definition */
  uint8_t is_const : 1;   /* const definition */
  int scope_level;        /* scope of definition */
  JSAtom var_name;        /* variable name */
} JSGlobalVar;

typedef struct RelocEntry {
  struct RelocEntry *next;
  uint32_t addr; /* address to patch */
  int size;      /* address size: 1, 2 or 4 bytes */
} RelocEntry;

typedef struct JumpSlot {
  int op;
  int size;
  int pos;
  int label;
} JumpSlot;

typedef struct LabelSlot {
  int ref_count;
  int pos;  /* phase 1 address, -1 means not resolved yet */
  int pos2; /* phase 2 address, -1 means not resolved yet */
  int addr; /* phase 3 address, -1 means not resolved yet */
  RelocEntry *first_reloc;
} LabelSlot;

typedef struct LocSlot {
  uint32_t pc;
  int line_num;
  int col_num;
} LocSlot;

typedef enum JSParseFunctionEnum {
  JS_PARSE_FUNC_STATEMENT,
  JS_PARSE_FUNC_VAR,
  JS_PARSE_FUNC_EXPR,
  JS_PARSE_FUNC_ARROW,
  JS_PARSE_FUNC_GETTER,
  JS_PARSE_FUNC_SETTER,
  JS_PARSE_FUNC_METHOD,
  JS_PARSE_FUNC_CLASS_CONSTRUCTOR,
  JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR,
} JSParseFunctionEnum;

typedef enum JSParseExportEnum {
  JS_PARSE_EXPORT_NONE,
  JS_PARSE_EXPORT_NAMED,
  JS_PARSE_EXPORT_DEFAULT,
} JSParseExportEnum;

typedef struct JSFunctionDef {
  JSContext *ctx;
  struct JSFunctionDef *parent;
  int parent_cpool_idx;   /* index in the constant pool of the parent
                             or -1 if none */
  int parent_scope_level; /* scope level in parent at point of definition */
  struct list_head child_list; /* list of JSFunctionDef.link */
  struct list_head link;

  BOOL is_eval;         /* TRUE if eval code */
  int eval_type;        /* only valid if is_eval = TRUE */
  BOOL is_global_var;   /* TRUE if variables are not defined locally:
                           eval global, eval module or non strict eval */
  BOOL is_func_expr;    /* TRUE if function expression */
  BOOL has_home_object; /* TRUE if the home object is available */
  BOOL has_prototype;   /* true if a prototype field is necessary */
  BOOL has_simple_parameter_list;
  BOOL has_parameter_expressions; /* if true, an argument scope is created */
  BOOL has_use_strict;            /* to reject directive in special cases */
  BOOL has_eval_call; /* true if the function contains a call to eval() */
  BOOL has_arguments_binding; /* true if the 'arguments' binding is
                                 available in the function */
  BOOL has_this_binding;      /* true if the 'this' and new.target binding are
                                 available in the function */
  BOOL new_target_allowed;    /* true if the 'new.target' does not
                                 throw a syntax error */
  BOOL super_call_allowed;    /* true if super() is allowed */
  BOOL super_allowed;         /* true if super. or super[] is allowed */
  BOOL arguments_allowed; /* true if the 'arguments' identifier is allowed */
  BOOL is_derived_class_constructor;
  BOOL in_function_body;
  BOOL backtrace_barrier;
  JSFunctionKindEnum func_kind : 8;
  JSParseFunctionEnum func_type : 8;
  uint8_t js_mode;  /* bitmap of JS_MODE_x */
  JSAtom func_name; /* JS_ATOM_NULL if no name */

  JSVarDef *vars;
  int var_size; /* allocated size for vars[] */
  int var_count;
  JSVarDef *args;
  int arg_size;  /* allocated size for args[] */
  int arg_count; /* number of arguments */
  int defined_arg_count;
  int var_object_idx;     /* -1 if none */
  int arg_var_object_idx; /* -1 if none (var object for the argument scope) */
  int arguments_var_idx;  /* -1 if none */
  int arguments_arg_idx;  /* argument variable definition in argument scope,
                             -1 if none */
  int func_var_idx;       /* variable containing the current function (-1
                             if none, only used if is_func_expr is true) */
  int eval_ret_idx; /* variable containing the return value of the eval, -1 if
                       none */
  int this_var_idx; /* variable containing the 'this' value, -1 if none */
  int new_target_var_idx; /* variable containing the 'new.target' value, -1 if
                             none */
  int this_active_func_var_idx; /* variable containing the 'this.active_func'
                                   value, -1 if none */
  int home_object_var_idx;
  BOOL need_home_object;

  int scope_level; /* index into fd->scopes if the current lexical scope */
  int scope_first; /* index into vd->vars of first lexically scoped variable */
  int scope_size;  /* allocated size of fd->scopes array */
  int scope_count; /* number of entries used in the fd->scopes array */
  JSVarScope *scopes;
  JSVarScope def_scope_array[4];
  int body_scope; /* scope of the body of the function or eval */

  int global_var_count;
  int global_var_size;
  JSGlobalVar *global_vars;

  DynBuf byte_code;
  int last_opcode_pos;    /* -1 if no last opcode */
  BOOL use_short_opcodes; /* true if short opcodes are used in byte_code */

  LabelSlot *label_slots;
  int label_size; /* allocated size for label_slots[] */
  int label_count;
  BlockEnv *top_break; /* break/continue label stack */

  /* constant pool (strings, functions, numbers) */
  JSValue *cpool;
  int cpool_count;
  int cpool_size;

  /* list of variables in the closure */
  int closure_var_count;
  int closure_var_size;
  JSClosureVar *closure_var;

  JumpSlot *jump_slots;
  int jump_size;
  int jump_count;

  LocSlot *loc_slots;
  int loc_size;
  int loc_count;
  uint64_t loc_last;
  int loc_last_pc;

  /* pc2line table */
  JSAtom filename;
  int line_num; /* base line for all the lines in pc2line table */
  DynBuf pc2line;

  char *source; /* raw source, utf-8 encoded */
  int source_len;

  JSModuleDef *module; /* != NULL when parsing a module */
} JSFunctionDef;

#define LOC(line_num, col_num) ((uint64_t)line_num << 32 | col_num)
#define LOC_LINE(lc) ((int)(lc >> 32))
#define LOC_COL(lc) ((int)lc)

typedef struct JSParseState {
  JSContext *ctx;
  int last_line_num; /* line number of last token */
  int line_num;      /* line number of current offset, starts from 1 */
  int col_num;       /* column number of current offset , starts from 1 */
  /* LoC to emit to the bytecode stream, `0` to skip emitting, use
    `uint64_t` as its size since we will encode line_num an col_num in it in a
     form of `line_num:col_num` */
  uint64_t loc;
  const char *filename;
  JSToken token;
  BOOL got_lf; /* true if got line feed before the current token */
  const uint8_t *last_ptr;
  const uint8_t *buf_ptr;
  const uint8_t *buf_end;
  BOOL debug; /* true if in debug mode */

  /* current function code */
  JSFunctionDef *cur_func;
  BOOL is_module; /* parsing a module */
  BOOL allow_html_comments;
  BOOL ext_json; /* true if accepting JSON superset */
} JSParseState;

/* -- Parser interfaces ----------------------------------- */

void js_parse_init(JSContext *ctx, JSParseState *s, const char *input,
                   size_t input_len, const char *filename);
__exception int js_parse_program(JSParseState *s);

/* return true if 'input' contains the source of a module
   (heuristic). 'input' must be a zero terminated.

   Heuristic: skip comments and expect 'import' keyword not followed
   by '(' or '.' or export keyword.
*/
BOOL JS_DetectModule(const char *input, size_t input_len);

/* -- Parsing utils ----------------------------------- */

int __attribute__((format(printf, 2, 3)))
js_parse_error(JSParseState *s, const char *fmt, ...);
int js_parse_expect(JSParseState *s, int tok);
int js_parse_expect_semi(JSParseState *s);
int js_parse_error_reserved_identifier(JSParseState *s);

typedef struct JSParsePos {
  int last_line_num;
  int line_num;
  int col_num;
  BOOL got_lf;
  const uint8_t *ptr;
} JSParsePos;

int js_parse_get_pos(JSParseState *s, JSParsePos *sp);

BOOL is_regexp_allowed(int tok);

#define SKIP_HAS_SEMI (1 << 0)
#define SKIP_HAS_ELLIPSIS (1 << 1)
#define SKIP_HAS_ASSIGNMENT (1 << 2)
int js_parse_skip_parens_token(JSParseState *s, int *pbits,
                               BOOL no_line_terminator);

/* test if the current token is a label. Use simplistic look-ahead scanner */
BOOL is_label(JSParseState *s);
/* test if the current token is a let keyword. Use simplistic look-ahead scanner
 */
int is_let(JSParseState *s, int decl_mask);

/* -- Lexer ----------------------------------- */

void skip_shebang(JSParseState *s);
int next_token(JSParseState *s);
void free_token(JSParseState *s, JSToken *token);

static inline BOOL token_is_pseudo_keyword(JSParseState *s, JSAtom atom) {
  return s->token.val == TOK_IDENT && s->token.u.ident.atom == atom &&
         !s->token.u.ident.has_escape;
}

BOOL token_is_ident(int tok);
int js_unsupported_keyword(JSParseState *s, JSAtom atom);
__exception int js_parse_seek_token(JSParseState *s, const JSParsePos *sp);

/* only used for ':' and '=>', 'let' or 'function' look-ahead. *pp is
   only set if TOK_IMPORT is returned */
/* XXX: handle all unicode cases */
int simple_next_token(const uint8_t **pp, BOOL no_line_terminator);
int peek_token(JSParseState *s, BOOL no_line_terminator);
void __attribute((unused)) dump_token(JSParseState *s, const JSToken *token);

__exception int js_parse_template_part(JSParseState *s, const uint8_t *p);
__exception int js_parse_template(JSParseState *s, int call, int *argc);
__exception int js_parse_string(JSParseState *s, int sep, BOOL do_throw,
                                const uint8_t *p, JSToken *token,
                                const uint8_t **pp);
__exception int js_parse_regexp(JSParseState *s);
int ident_realloc(JSContext *ctx, char **pbuf, size_t *psize, char *static_buf);
/* 'c' is the first character. Return JS_ATOM_NULL in case of error */
JSAtom parse_ident(JSParseState *s, const uint8_t **pp, BOOL *pident_has_escape,
                   int c, BOOL is_private);

/* -- Assembling opcodes ----------------------------------- */

static inline int get_prev_opcode(JSFunctionDef *fd) {
  if (fd->last_opcode_pos < 0)
    return OP_invalid;
  else
    return fd->byte_code.buf[fd->last_opcode_pos];
}

BOOL js_is_live_code(JSParseState *s);
void emit_u8(JSParseState *s, uint8_t val);
void emit_u16(JSParseState *s, uint16_t val);
void emit_u32(JSParseState *s, uint32_t val);
void emit_op(JSParseState *s, uint8_t val);
void emit_atom(JSParseState *s, JSAtom name);
int update_label(JSFunctionDef *s, int label, int delta);
int new_label_fd(JSFunctionDef *fd, int label);
int new_label(JSParseState *s);
/* return the label ID offset */
int emit_label(JSParseState *s, int label);
/* return label or -1 if dead code */
int emit_goto(JSParseState *s, int opcode, int label);
/* return the constant pool index. 'val' is not duplicated. */
int cpool_add(JSParseState *s, JSValue val);
__exception int emit_push_const(JSParseState *s, JSValueConst val,
                                BOOL as_atom);

/* -- Scope ----------------------------------- */

#define GLOBAL_VAR_OFFSET 0x40000000
#define ARGUMENT_VAR_OFFSET 0x20000000

/* return the variable index or -1 if not found,
   add ARGUMENT_VAR_OFFSET for argument variables */
int find_arg(JSContext *ctx, JSFunctionDef *fd, JSAtom name);
int find_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name);
/* find a variable declaration in a given scope */
int find_var_in_scope(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                      int scope_level);
/* return true if scope == parent_scope or if scope is a child of
parent_scope */
BOOL is_child_scope(JSContext *ctx, JSFunctionDef *fd, int scope,
                    int parent_scope);
/* find a 'var' declaration in the same scope or a child scope */
int find_var_in_child_scope(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                            int scope_level);
JSGlobalVar *find_global_var(JSFunctionDef *fd, JSAtom name);
JSGlobalVar *find_lexical_global_var(JSFunctionDef *fd, JSAtom name);
int find_lexical_decl(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                      int scope_idx, BOOL check_catch_var);

int push_scope(JSParseState *s);
void pop_scope(JSParseState *s);
void close_scopes(JSParseState *s, int scope, int scope_stop);
/* return the variable index or -1 if error */
int add_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name);
int add_scope_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name,
                  JSVarKindEnum var_kind);
int add_func_var(JSContext *ctx, JSFunctionDef *fd, JSAtom name);
int get_first_lexical_var(JSFunctionDef *fd, int scope);

int add_arguments_var(JSContext *ctx, JSFunctionDef *fd);
/* add an argument definition in the argument scope. Only needed when
   "eval()" may be called in the argument scope. Return 0 if OK. */
int add_arguments_arg(JSContext *ctx, JSFunctionDef *fd);
int add_arg(JSContext *ctx, JSFunctionDef *fd, JSAtom name);

/* add a global variable definition */
JSGlobalVar *add_global_var(JSContext *ctx, JSFunctionDef *s, JSAtom name);

typedef enum {
  JS_VAR_DEF_WITH,
  JS_VAR_DEF_LET,
  JS_VAR_DEF_CONST,
  JS_VAR_DEF_FUNCTION_DECL,     /* function declaration */
  JS_VAR_DEF_NEW_FUNCTION_DECL, /* async/generator function declaration */
  JS_VAR_DEF_CATCH,
  JS_VAR_DEF_VAR,
} JSVarDefEnum;

int define_var(JSParseState *s, JSFunctionDef *fd, JSAtom name,
               JSVarDefEnum var_def_type);
/* XXX: remove */
BOOL has_with_scope(JSFunctionDef *s, int scope_level);

/* -- Parse Primary expressions ----------------------------------- */

__exception int js_parse_array_literal(JSParseState *s);

void set_object_name(JSParseState *s, JSAtom name);
void set_object_name_computed(JSParseState *s);

#define PROP_TYPE_IDENT 0
#define PROP_TYPE_VAR 1
#define PROP_TYPE_GET 2
#define PROP_TYPE_SET 3
#define PROP_TYPE_STAR 4
#define PROP_TYPE_ASYNC 5
#define PROP_TYPE_ASYNC_STAR 6

#define PROP_TYPE_PRIVATE (1 << 4)

/* if the property is an expression, name = JS_ATOM_NULL */
int __exception js_parse_property_name(JSParseState *s, JSAtom *pname,
                                       BOOL allow_method, BOOL allow_var,
                                       BOOL allow_private);

__exception int js_parse_object_literal(JSParseState *s);

/* -- lvalue ----------------------------------- */

typedef enum {
  PUT_LVALUE_NOKEEP,        /* [depth] v -> */
  PUT_LVALUE_NOKEEP_DEPTH,  /* [depth] v -> , keep depth (currently
                               just disable optimizations) */
  PUT_LVALUE_KEEP_TOP,      /* [depth] v -> v */
  PUT_LVALUE_KEEP_SECOND,   /* [depth] v0 v -> v0 */
  PUT_LVALUE_NOKEEP_BOTTOM, /* v [depth] -> */
} PutLValueEnum;

/* name has a live reference. 'is_let' is only used with opcode =
   OP_scope_get_var which is never generated by get_lvalue(). */
void put_lvalue(JSParseState *s, int opcode, int scope, JSAtom name, int label,
                PutLValueEnum special, BOOL is_let);
__exception int get_lvalue(JSParseState *s, int *popcode, int *pscope,
                           JSAtom *pname, int *plabel, int *pdepth, BOOL keep,
                           int tok);

/* -- Parse Expressions ----------------------------------- */

/* allowed parse_flags: PF_IN_ACCEPTED */
__exception int js_parse_expr2(JSParseState *s, int parse_flags);
__exception int js_parse_expr(JSParseState *s);
__exception int js_parse_assign_expr2(JSParseState *s, int parse_flags);
__exception int js_parse_assign_expr(JSParseState *s);
/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
__exception int js_parse_cond_expr(JSParseState *s, int parse_flags);
__exception int js_parse_coalesce_expr(JSParseState *s, int parse_flags);
/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
__exception int js_parse_logical_and_or(JSParseState *s, int op,
                                        int parse_flags);
/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
__exception int js_parse_expr_binary(JSParseState *s, int level,
                                     int parse_flags);
__exception int js_parse_unary(JSParseState *s, int parse_flags);

/* allow the 'in' binary operator */
#define PF_IN_ACCEPTED (1 << 0)
/* allow function calls parsing in js_parse_postfix_expr() */
#define PF_POSTFIX_CALL (1 << 1)
/* allow arrow functions parsing in js_parse_postfix_expr() */
#define PF_ARROW_FUNC (1 << 2)
/* allow the exponentiation operator in js_parse_unary() */
#define PF_POW_ALLOWED (1 << 3)
/* forbid the exponentiation operator in js_parse_unary() */
#define PF_POW_FORBIDDEN (1 << 4)
__exception int js_parse_postfix_expr(JSParseState *s, int parse_flags);
__exception int js_parse_left_hand_side_expr(JSParseState *s);

__exception int js_parse_expr_paren(JSParseState *s);

/* -- Parse Function ----------------------------------- */

__exception int js_parse_directives(JSParseState *s);
__exception int js_parse_source_element(JSParseState *s);
/* func_name must be JS_ATOM_NULL for JS_PARSE_FUNC_STATEMENT and
   JS_PARSE_FUNC_EXPR, JS_PARSE_FUNC_ARROW and JS_PARSE_FUNC_VAR */
__exception int
js_parse_function_decl2(JSParseState *s, JSParseFunctionEnum func_type,
                        JSFunctionKindEnum func_kind, JSAtom func_name,
                        const uint8_t *ptr, int function_line_num,
                        JSParseExportEnum export_flag, JSFunctionDef **pfd);
__exception int js_parse_function_decl(JSParseState *s,
                                       JSParseFunctionEnum func_type,
                                       JSFunctionKindEnum func_kind,
                                       JSAtom func_name, const uint8_t *ptr,
                                       int function_line_num);

/* execute the finally blocks before return */
void emit_return(JSParseState *s, BOOL hasval);

/* -- Closure ----------------------------------- */

int add_closure_var(JSContext *ctx, JSFunctionDef *s, BOOL is_local,
                    BOOL is_arg, int var_idx, JSAtom var_name, BOOL is_const,
                    BOOL is_lexical, JSVarKindEnum var_kind);
int find_closure_var(JSContext *ctx, JSFunctionDef *s, JSAtom var_name);
/* 'fd' must be a parent of 's'. Create in 's' a closure referencing a
   local variable (is_local = TRUE) or a closure (is_local = FALSE) in
   'fd' */
int get_closure_var2(JSContext *ctx, JSFunctionDef *s, JSFunctionDef *fd,
                     BOOL is_local, BOOL is_arg, int var_idx, JSAtom var_name,
                     BOOL is_const, BOOL is_lexical, JSVarKindEnum var_kind);
int get_closure_var(JSContext *ctx, JSFunctionDef *s, JSFunctionDef *fd,
                    BOOL is_arg, int var_idx, JSAtom var_name, BOOL is_const,
                    BOOL is_lexical, JSVarKindEnum var_kind);
int add_var_this(JSContext *ctx, JSFunctionDef *fd);
void add_eval_variables(JSContext *ctx, JSFunctionDef *s);
/* for direct eval compilation: add references to the variables of the
calling function */
__exception int add_closure_variables(JSContext *ctx, JSFunctionDef *s,
                                      JSFunctionBytecode *b, int scope_idx);

/* -- JSFunctionDef ----------------------------------- */

JSFunctionDef *js_new_function_def(JSContext *ctx, JSFunctionDef *parent,
                                   BOOL is_eval, BOOL is_func_expr,
                                   const char *filename, int line_num);
void js_free_function_def(JSContext *ctx, JSFunctionDef *fd);

int add_module_variables(JSContext *ctx, JSFunctionDef *fd);

/* create a function object from a function definition. The function
   definition is freed. All the child functions are also created. It
   must be done this way to resolve all the variables. */
JSValue js_create_function(JSContext *ctx, JSFunctionDef *fd);

/* -- Parse VarDec and Destruction ----------------------------------- */

/* Return -1 if error, 0 if no initializer, 1 if an initializer is
   present at the top level. */
int js_parse_destructuring_element(JSParseState *s, int tok, int is_arg,
                                   int hasval, int has_ellipsis,
                                   BOOL allow_initializer);

void js_emit_spread_code(JSParseState *s, int depth);

/* allowed parse_flags: PF_IN_ACCEPTED */
__exception int js_parse_var(JSParseState *s, int parse_flags, int tok,
                             BOOL export_flag);
__exception int js_define_var(JSParseState *s, JSAtom name, int tok);

/* -- Parse Block ----------------------------------- */

void push_break_entry(JSFunctionDef *fd, BlockEnv *be, JSAtom label_name,
                      int label_break, int label_cont, int drop_count);
void pop_break_entry(JSFunctionDef *fd);

__exception int emit_break(JSParseState *s, JSAtom name, int is_cont);

__exception int js_parse_block(JSParseState *s);

/* -- Parse Class ----------------------------------- */

typedef struct {
  JSFunctionDef *fields_init_fd;
  int computed_fields_count;
  BOOL has_brand;
  int brand_push_pos;
} ClassFieldsDef;

__exception int js_parse_class(JSParseState *s, BOOL is_class_expr,
                               JSParseExportEnum export_flag);

/* initialize the class fields, called by the constructor. Note:
super() can be called in an arrow function, so <this> and
<class_fields_init> can be variable references */
void emit_class_field_init(JSParseState *s);
/* build a private setter function name from the private getter name */
JSAtom get_private_setter_name(JSContext *ctx, JSAtom name);

/* -- Parse import export ----------------------------------- */

__exception int js_parse_import(JSParseState *s);
__exception JSAtom js_parse_from_clause(JSParseState *s);
__exception int js_parse_export(JSParseState *s);

JSExportEntry *add_export_entry2(JSContext *ctx, JSParseState *s,
                                 JSModuleDef *m, JSAtom local_name,
                                 JSAtom export_name,
                                 JSExportTypeEnum export_type);
JSExportEntry *add_export_entry(JSParseState *s, JSModuleDef *m,
                                JSAtom local_name, JSAtom export_name,
                                JSExportTypeEnum export_type);

/* -- Parse Statements ----------------------------------- */

#define DECL_MASK_FUNC (1 << 0) /* allow normal function declaration */
/* ored with DECL_MASK_FUNC if function declarations are allowed with a label */
#define DECL_MASK_FUNC_WITH_LABEL (1 << 1)
#define DECL_MASK_OTHER (1 << 2) /* all other declarations */
#define DECL_MASK_ALL                                                          \
  (DECL_MASK_FUNC | DECL_MASK_FUNC_WITH_LABEL | DECL_MASK_OTHER)

__exception int js_parse_statement_or_decl(JSParseState *s, int decl_mask);
__exception int js_parse_statement(JSParseState *s);

/* -- Parse JSON ---------------------------------------- */

/* 'c' is the first character. Return JS_ATOM_NULL in case of error */
JSAtom json_parse_ident(JSParseState *s, const uint8_t **pp, int c);
__exception int json_next_token(JSParseState *s);
int json_parse_expect(JSParseState *s, int tok);
JSValue json_parse_value(JSParseState *s);

/* -- Optimizing opcodes ----------------------------------- */

/* convert global variable accesses to local variables or closure
   variables when necessary */
__exception int resolve_variables(JSContext *ctx, JSFunctionDef *s);
__exception int resolve_labels(JSContext *ctx, JSFunctionDef *s);

typedef struct StackSizeState {
  int bc_len;
  int stack_len_max;
  uint16_t *stack_level_tab;
  int *pc_stack;
  int pc_stack_len;
  int pc_stack_size;
} StackSizeState;

__exception int compute_stack_size(JSContext *ctx, JSFunctionDef *fd,
                                   int *pstack_size);

typedef struct CodeContext {
  const uint8_t *bc_buf; /* code buffer */
  int bc_len;            /* length of the code buffer */
  int pos;               /* position past the matched code pattern */
  uint64_t loc;          /* last visited OP_loc parameter or 0 */
  int op;
  int idx;
  int label;
  int val;
  JSAtom atom;
} CodeContext;

#define M2(op1, op2) ((op1) | ((op2) << 8))
#define M3(op1, op2, op3) ((op1) | ((op2) << 8) | ((op3) << 16))
#define M4(op1, op2, op3, op4)                                                 \
  ((op1) | ((op2) << 8) | ((op3) << 16) | ((op4) << 24))

BOOL code_match(CodeContext *s, int pos, ...);

int skip_dead_code(JSFunctionDef *s, const uint8_t *bc_buf, int bc_len, int pos,
                   uint64_t *locp);

#endif
