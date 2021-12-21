#ifndef __MAP_H__
#define __MAP_H__

#include <stdbool.h>
#include <stddef.h>

#define MAP_SMALLKEY_SIZE 32

typedef struct MapSlot {
	bool empty : 1;
	bool heap_alloc : 1;
	char *key;
	char smallkey[MAP_SMALLKEY_SIZE]; /* reduce unneeded mallocs */
} MapSlot;

typedef struct Map {
	size_t len, cap; /* len: used slots; cap: available slots */
	MapSlot *slots;
	size_t val_size;
	void *vals;
} Map;

void map_init(Map *m, size_t val_size);
void map_term(Map *m);
/* returns true if the value was replaced */
bool map_insert(Map *m, const char *key, const void *val);
/* Returns true if the key was found, returns false if it wasn't found.
 * out_val may be set to NULL. */
bool map_get(Map *m, const char *key, void *out_val);

#endif /* MAP_H */
