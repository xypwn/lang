#include "map.h"

#include <stdlib.h>

#include "util.h"

#define MAP_INITIAL_CAP 32 /* must be a power of 2 in this implementation */
#define MAP_REHASH_THRESHOLD 0.7

static void init_with_cap(Map *m, size_t val_size, size_t cap);
static void rehash(Map *m);

static void init_with_cap(Map *m, size_t val_size, size_t cap) {
	m->len = 0;
	m->cap = cap;
	m->val_size = val_size;
	void *data = malloc(sizeof(MapSlot) * cap + val_size * cap);
	m->slots = data;
	m->vals = m->slots + cap;
	for (size_t i = 0; i < cap; i++) {
		m->slots[i] = (MapSlot){
			.empty = true,
		};
	}
}

static void rehash(Map *m) {
	size_t old_cap = m->cap;
	MapSlot *old_slots = m->slots;
	void *old_vals = m->vals;
	init_with_cap(m, m->val_size, m->cap * 2);
	for (size_t i = 0; i < old_cap; i++) {
		if (!old_slots[i].empty) {
			map_insert(m, old_slots[i].key, (uint8_t*)old_vals + m->val_size * i);
			if (old_slots[i].heap_alloc)
				free(old_slots[i].key);
			/* TODO: Don't reallocate big keys. */
		}
	}
	free(old_slots);
}

void map_init(Map *m, size_t val_size) {
	init_with_cap(m, val_size, MAP_INITIAL_CAP);
}

void map_term(Map *m) {
	for (size_t i = 0; i < m->cap; i++) {
		if (!m->slots[i].empty && m->slots[i].heap_alloc)
			free(m->slots[i].key);
	}
	free(m->slots);
}

bool map_insert(Map *m, const char *key, const void *val) {
	if ((double)m->len / (double)m->cap > MAP_REHASH_THRESHOLD)
		rehash(m);
	size_t idx = fnv1a32(key, strlen(key)) & (m->cap - 1);
	bool replaced;
	for (;;) {
		if (m->slots[idx].empty) {
			replaced = false;
			m->len++;
			break;
		} else {
			if (streq(m->slots[idx].key, key)) {
				replaced = true;
				break;
			} else {
				if (++idx == m->cap) idx = 0;
			}
		}
	}
	m->slots[idx].empty = false;
	size_t keylen = strlen(key);
	if (keylen <= MAP_SMALLKEY_SIZE-1) {
		strcpy(m->slots[idx].smallkey, key);
		m->slots[idx].key = m->slots[idx].smallkey;
		m->slots[idx].heap_alloc = false;
	} else {
		m->slots[idx].key = sndup(key, keylen);
		m->slots[idx].heap_alloc = true;
	}
	memcpy((uint8_t*)m->vals + m->val_size * idx, val, m->val_size);
	return replaced;
}

bool map_get(const Map *m, const char *key, void *out_val) {
	size_t idx = fnv1a32(key, strlen(key)) & (m->cap - 1);
	for (;;) {
		if (m->slots[idx].empty)
			return false;
		else {
			if (streq(m->slots[idx].key, key)) {
				if (out_val)
					memcpy(out_val, (uint8_t*)m->vals + m->val_size * idx, m->val_size);
				return true;
			} else {
				if (++idx == m->cap) idx = 0;
			}
		}
	}
}
