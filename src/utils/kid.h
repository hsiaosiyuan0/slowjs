#ifndef KID_H
#define KID_H

#include <stdbool.h>
#include <stddef.h>

/* -- Allocator ----------------------------------- */

typedef struct KidAllocator KidAllocator;
typedef void *KidMallocFunc(void *opaque, size_t size);
typedef void KidFreeFunc(void *opaque, void *ptr);
typedef void *KidReallocFunc(void *opaque, void *ptr, size_t size);
typedef struct KidAllocator {
  void *opaque;
  KidMallocFunc *malloc;
  KidFreeFunc *free;
  KidReallocFunc *realloc;
} KidAllocator;

void kid_set_allocator(KidAllocator *alloc);

void *kid_malloc(size_t size);
void kid_free(void *ptr);
void *kid_malloc(size_t size);

/* -- String ----------------------------------- */

typedef struct KidString {
  size_t len;
  unsigned char *data;
} KidString;

KidString kid_string_from_cstr(const char *cstr);

/* -- Array ----------------------------------- */

typedef struct KidArray {
  void *slots;
  size_t slot_size;
  size_t len;
  size_t cap;
} KidArray;

int kid_array_init(KidArray *arr, size_t slot_size, size_t cap);
void kid_array_free(KidArray *arr);
int kid_array_push(KidArray *arr, void *item);

/* -- List ----------------------------------- */

typedef struct KidListHead KidListHead;
struct KidListHead {
  KidListHead *prev;
  KidListHead *next;
};

static inline void kid_list_init_head(KidListHead *head) {
  head->prev = head;
  head->next = head;
}

static inline int kid_list_empty(KidListHead *el) { return el->next == el; }

/* return the pointer of type 'type *' containing 'el' as field 'member' */
#define kid_list_entry(el, type, member)                                       \
  ((type *)((uint8_t *)(el)-offsetof(type, member)))

/* insert 'el' between 'prev' and 'next' */
static inline void __kid_list_add(KidListHead *el, struct KidListHead *prev,
                                  KidListHead *next) {
  prev->next = el;
  el->prev = prev;
  el->next = next;
  next->prev = el;
}

/* add 'el' at the head of the list 'head' (= after element head) */
static inline void kid_list_add(KidListHead *el, KidListHead *head) {
  __kid_list_add(el, head, head->next);
}

/* add 'el' at the end of the list 'head' (= before element head) */
static inline void kid_list_add_tail(KidListHead *el, KidListHead *head) {
  __kid_list_add(el, head->prev, head);
}

static inline void kid_list_del(KidListHead *el) {
  KidListHead *prev, *next;
  prev = el->prev;
  next = el->next;
  prev->next = next;
  next->prev = prev;
  el->prev = NULL; /* fail safe */
  el->next = NULL; /* fail safe */
}

#define kid_list_for_each(el, head)                                            \
  for (el = (head)->next; el != (__typeof__(el))(head); el = el->next)

#define kid_list_for_each_safe(el, el1, head)                                  \
  for (el = (head)->next, el1 = el->next; el != (__typeof__(el))(head);        \
       el = el1, el1 = el->next)

#define kid_list_for_each_prev(el, head)                                       \
  for (el = (head)->prev; el != (__typeof__(el))(head); el = el->prev)

#define kid_list_for_each_prev_safe(el, el1, head)                             \
  for (el = (head)->prev, el1 = el->prev; el != (__typeof__(el))(head);        \
       el = el1, el1 = el->prev)

/* -- Hashmap ----------------------------------- */

typedef struct KidHashkey KidHashkey;
struct KidHashkey {
  KidListHead link;
  size_t size;
  void *opaque;
  unsigned int hash;
};

#define KID_HASHMAP_BUCKETS_LEN 64
#define KID_HASHMAP_BUCKETS_MASK (KID_HASHMAP_BUCKETS_LEN - 1)

typedef struct KidHashmap KidHashmap;
typedef struct KidHashmapEntry KidHashmapEntry;
typedef void KidHashmapValueFreeFunc(void *ptr);
struct KidHashmapEntry {
  KidListHead link;
  KidHashkey *key;
  void *value;
};

typedef KidHashkey *KidHashmapKeyCopyFunc(KidHashkey *key);
typedef void KidHashmapKeyFreeFunc(KidHashkey *key);
struct KidHashmap {
  KidListHead keys;     // List<KidHashkey>
  KidListHead *buckets; // Array<List<KidHashmapEntry>>

  KidHashmapKeyCopyFunc *key_copy;
  KidHashmapKeyFreeFunc *key_free;
  KidHashmapValueFreeFunc *value_free;
};

void kid_hashmap_init(KidHashmap *map, KidHashmapKeyCopyFunc *key_copy,
                      KidHashmapKeyFreeFunc *key_free,
                      KidHashmapValueFreeFunc *value_free);

KidHashkey *kid_hashmap_key_copy(KidHashkey *key);
void kid_hashmap_key_free(KidHashkey *key);

int kid_hashmap_set(KidHashmap *map, KidHashkey *key, void *value,
                    bool free_old);
KidHashmapEntry *kid_hashmap_get(KidHashmap *map, KidHashkey *key);
KidHashmapEntry *kid_hashmap_del(KidHashmap *map, KidHashkey *key,
                                 bool free_old);

#endif