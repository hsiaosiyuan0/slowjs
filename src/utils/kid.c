#include "kid.h"
#include "libs/cutils.h"
#include <stddef.h>
#include <string.h>

/* -- Allocator ----------------------------------- */

static KidAllocator *allocator = NULL;

void kid_set_allocator(KidAllocator *alloc) { allocator = alloc; }

void *kid_malloc(size_t size) {
  return allocator->malloc(allocator->opaque, size);
}

void kid_free(void *ptr) { allocator->free(allocator->opaque, ptr); }

void *kid_realloc(void *ptr, size_t size) {
  return allocator->realloc(allocator->opaque, ptr, size);
}

void *kid_mallocz(size_t size) {
  void *ptr = kid_malloc(size);
  if (!ptr)
    return NULL;
  return memset(ptr, 0, size);
}

/* -- String ----------------------------------- */

KidString kid_string_new(const char *data, size_t len) {
  char *d = NULL;
  if (len)
    d = kid_malloc(len);

  memcpy(d, data, len);
  return (KidString){d, len};
}

void kid_string_free(KidString str) { kid_free(str.data); }

/* -- Array ----------------------------------- */

static int kid_array_grow(KidArray *arr) {
  if (arr->slots == NULL) {
    arr->slots = kid_mallocz(arr->slot_size * arr->cap);
  } else if (arr->len == arr->cap) {
    if (arr->cap < 1024) {
      arr->cap += arr->cap;
    } else {
      arr->cap += arr->cap / 4;
    }
    arr->slots = kid_realloc(arr->slots, arr->slot_size * arr->cap);
  }
  return !arr->slots;
}

int kid_array_init(KidArray *arr, size_t slot_size, size_t cap) {
  if (!cap)
    cap = 8;
  arr->slot_size = slot_size;
  arr->cap = cap;
  arr->len = 0;
  arr->slots = NULL;
  return kid_array_grow(arr);
}

void kid_array_free(KidArray *arr) { kid_free(arr->slots); }

int kid_array_push(KidArray *arr, void *item) {
  if (kid_array_grow(arr))
    return -1;
  int i = arr->len++;
  memcpy(arr->slots + (i * arr->slot_size), item, arr->slot_size);
  return i;
}

/* -- List ----------------------------------- */

/* -- Hashmap ----------------------------------- */

// https://www.partow.net/programming/hashfunctions/#ELFHashFunction
unsigned int elf_Hash(const unsigned char *data, unsigned int size) {
  unsigned int hash = 0;
  unsigned int x = 0;
  unsigned int i = 0;

  for (i = 0; i < size; ++data, ++i) {
    hash = (hash << 4) + (*data);

    if ((x = hash & 0xF0000000L) != 0) {
      hash ^= (x >> 24);
    }

    hash &= ~x;
  }

  return hash;
}

void kid_hashmap_init(KidHashmap *map, KidHashmapKeyCopyFunc *key_copy,
                      KidHashmapKeyFreeFunc *key_free,
                      KidHashmapValueFreeFunc *value_free) {
  kid_list_init_head(&map->keys);

  map->buckets = kid_mallocz(sizeof(*map->buckets) * KID_HASHMAP_BUCKETS_LEN);
  if (!map->buckets) {
    kid_free(map);
    return;
  }

  for (int i = 0; i < KID_HASHMAP_BUCKETS_LEN; i++) {
    kid_list_init_head(map->buckets + i);
  }

  map->key_copy = key_copy;
  map->key_free = key_free;
  map->value_free = value_free;
  return;
}

KidHashkey *kid_hashmap_key_copy(KidHashkey *key) {
  KidHashkey *k = kid_malloc(sizeof(*k));
  if (!k)
    return NULL;

  kid_list_init_head(&k->link);
  k->hash = -1;

  k->size = key->size;
  k->opaque = kid_malloc(key->size);
  if (!k->opaque) {
    kid_free(k);
    return NULL;
  }
  memcpy(k->opaque, key->opaque, key->size);
  return k;
}

void kid_hashmap_key_free(KidHashkey *key) {
  kid_free(key->opaque);
  kid_free(key);
}

KidHashkey *kid_hashmap_key_shallow_copy(KidHashkey *key) {
  KidHashkey *k = kid_malloc(sizeof(*k));
  if (!k)
    return NULL;

  kid_list_init_head(&k->link);
  k->hash = -1;

  k->size = key->size;
  k->opaque = key->opaque;
  return k;
}

void kid_hashmap_key_shallow_free(KidHashkey *key) { kid_free(key); }

unsigned int kid_hashmap_hash(KidHashkey *key) {
  if (key->hash & 0x80000000) {
    key->hash = elf_Hash(key->opaque, key->size) & KID_HASHMAP_BUCKETS_MASK;
  }
  return key->hash;
}

KidHashmapEntry *kid_hashmap_get(KidHashmap *map, KidHashkey *key) {
  unsigned int hash = kid_hashmap_hash(key);
  KidListHead *bucket = map->buckets + hash;

  if (kid_list_empty(bucket))
    return NULL;

  KidListHead *el;
  KidHashmapEntry *e;
  kid_list_for_each(el, bucket) {
    e = kid_list_entry(el, KidHashmapEntry, link);
    if (e->key->size != key->size)
      continue;
    if (memcmp(e->key->opaque, key->opaque, key->size))
      continue;
    return e;
  }

  return NULL;
}

// TODO: resize buckets
int kid_hashmap_set(KidHashmap *map, KidHashkey *key, void *value,
                    bool free_old) {
  KidHashmapEntry *old = kid_hashmap_get(map, key);
  if (old) {
    if (free_old && map->value_free) {
      map->value_free(old->value);
    }
    old->value = value;
    return 0;
  }

  KidHashkey *keyp = map->key_copy(key);
  if (!keyp)
    return -1;

  kid_list_add_tail(&keyp->link, &map->keys);

  KidHashmapEntry *e = kid_mallocz(sizeof(*e));
  if (!e) {
    map->key_free(keyp);
    return -1;
  }

  kid_list_init_head(&e->link);
  e->key = keyp;
  e->value = value;

  kid_hashmap_hash(keyp);
  KidListHead *bucket = map->buckets + keyp->hash;
  kid_list_add(&e->link, bucket);
  return 0;
}

void kid_hashmap_del(KidHashmap *map, KidHashkey *key) {
  KidHashmapEntry *e = kid_hashmap_get(map, key);
  if (!e)
    return;

  kid_list_del(&e->link);

  kid_list_del(&e->key->link);
  if (map->key_free)
    map->key_free(e->key);

  if (map->value_free) {
    map->value_free(e->value);
  }
}

void kid_hashmap_free(KidHashmap *map) {
  struct KidListHead *el, *el1;
  if (map->key_free) {
    kid_list_for_each(el, &map->keys) {
      KidHashkey *k = kid_list_entry(el, KidHashkey, link);
      map->key_free(k);
    }
  }

  for (int i = 0; i < KID_HASHMAP_BUCKETS_LEN; i++) {
    KidListHead *bucket = (KidListHead *)map->buckets + i;
    kid_list_for_each_safe(el, el1, bucket) {
      KidHashmapEntry *e = kid_list_entry(el, KidHashmapEntry, link);
      if (map->value_free)
        map->value_free(e->value);
      kid_list_del(&e->link);
      kid_free(e);
    }
  }

  kid_free(map->buckets);
}