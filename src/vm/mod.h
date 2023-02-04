#ifndef QUICKJS_MODULE_H
#define QUICKJS_MODULE_H

#include "def.h"

typedef enum JSExportTypeEnum {
  JS_EXPORT_TYPE_LOCAL,
  JS_EXPORT_TYPE_INDIRECT,
} JSExportTypeEnum;

typedef struct JSExportEntry {
  union {
    struct {
      int var_idx;       /* closure variable index */
      JSVarRef *var_ref; /* if != NULL, reference to the variable */
    } local;             /* for local export */
    int req_module_idx;  /* module for indirect export */
  } u;
  JSExportTypeEnum export_type;
  JSAtom local_name;  /* '*' if export ns from. not used for local
                         export after compilation */
  JSAtom export_name; /* exported variable name */
} JSExportEntry;

typedef struct JSStarExportEntry {
  int req_module_idx; /* in req_module_entries */
} JSStarExportEntry;

typedef struct JSImportEntry {
  int var_idx; /* closure variable index */
  JSAtom import_name;
  int req_module_idx; /* in req_module_entries */
} JSImportEntry;

typedef struct JSReqModuleEntry {
  JSAtom module_name;
  JSModuleDef *module; /* used using resolution */
} JSReqModuleEntry;

struct JSModuleDef {
  JSRefCountHeader header; /* must come first, 32-bit */
  JSAtom module_name;
  struct list_head link;

  JSReqModuleEntry *req_module_entries;
  int req_module_entries_count;
  int req_module_entries_size;

  JSExportEntry *export_entries;
  int export_entries_count;
  int export_entries_size;

  JSStarExportEntry *star_export_entries;
  int star_export_entries_count;
  int star_export_entries_size;

  JSImportEntry *import_entries;
  int import_entries_count;
  int import_entries_size;

  JSValue module_ns;
  JSValue func_obj;            /* only used for JS modules */
  JSModuleInitFunc *init_func; /* only used for C modules */
  BOOL resolved : 8;
  BOOL func_created : 8;
  BOOL instantiated : 8;
  BOOL evaluated : 8;
  BOOL eval_mark : 8; /* temporary use during js_evaluate_module() */
  /* true if evaluation yielded an exception. It is saved in
     eval_exception */
  BOOL eval_has_exception : 8;
  JSValue eval_exception;
  JSValue meta_obj; /* for import.meta */
};

typedef struct JSResolveEntry {
  JSModuleDef *module;
  JSAtom name;
} JSResolveEntry;

typedef struct JSResolveState {
  JSResolveEntry *array;
  int size;
  int count;
} JSResolveState;

typedef enum JSResolveResultEnum {
  JS_RESOLVE_RES_EXCEPTION = -1, /* memory alloc error */
  JS_RESOLVE_RES_FOUND = 0,
  JS_RESOLVE_RES_NOT_FOUND,
  JS_RESOLVE_RES_CIRCULAR,
  JS_RESOLVE_RES_AMBIGUOUS,
} JSResolveResultEnum;

typedef enum {
  EXPORTED_NAME_AMBIGUOUS,
  EXPORTED_NAME_NORMAL,
  EXPORTED_NAME_NS,
} ExportedNameEntryEnum;

typedef struct ExportedNameEntry {
  JSAtom export_name;
  ExportedNameEntryEnum export_type;
  union {
    JSExportEntry *me;   /* using when the list is built */
    JSVarRef *var_ref;   /* EXPORTED_NAME_NORMAL */
    JSModuleDef *module; /* for EXPORTED_NAME_NS */
  } u;
} ExportedNameEntry;

typedef struct GetExportNamesState {
  JSModuleDef **modules;
  int modules_size;
  int modules_count;

  ExportedNameEntry *exported_names;
  int exported_names_size;
  int exported_names_count;
} GetExportNamesState;

/* 'name' is freed */
JSModuleDef *js_new_module_def(JSContext *ctx, JSAtom name);

/* must be done before js_link_module() because of cyclic references */
int js_create_module_function(JSContext *ctx, JSModuleDef *m);

JSExportEntry *find_export_entry(JSContext *ctx, JSModuleDef *m,
                                 JSAtom export_name);
int add_req_module_entry(JSContext *ctx, JSModuleDef *m, JSAtom module_name);
int add_star_export_entry(JSContext *ctx, JSModuleDef *m, int req_module_idx);

JSValue js_import_meta(JSContext *ctx);
JSValue js_dynamic_import(JSContext *ctx, JSValueConst specifier);

typedef enum JSFreeModuleEnum {
  JS_FREE_MODULE_ALL,
  JS_FREE_MODULE_NOT_RESOLVED,
  JS_FREE_MODULE_NOT_EVALUATED,
} JSFreeModuleEnum;

int js_resolve_module(JSContext *ctx, JSModuleDef *m);
int js_link_module(JSContext *ctx, JSModuleDef *m);

void js_free_module_def(JSContext *ctx, JSModuleDef *m);
void js_free_modules(JSContext *ctx, JSFreeModuleEnum flag);

#endif