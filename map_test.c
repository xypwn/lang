#include <assert.h>

#include "map.c"
#include "util.c"

int main(void) {
	Map m;
	map_init(&m, sizeof(size_t));
	size_t a = 2;
	map_insert(&m, "test", &a);
	a = 55;
	map_insert(&m, "some super long string that is definitely over thirty-one characters long", &a);
#define test_key(key, should_exist, expected_value) { \
	size_t v; \
	bool exists = map_get(&m, key, &v); \
	assert(should_exist == exists); \
	if (should_exist) \
		assert(v == expected_value); \
}
	test_key("test", true, 2);
	test_key("test1", false, 0);
	test_key("some super long string that is definitely over thirty-one characters long", true, 55);
	for (size_t i = 0; i < 999; i++) {
		char buf[32];
		sprintf(buf, "number: %zu", i);
		map_insert(&m, buf, &i);
	}
	for (size_t i = 0; i < 999; i++) {
		char buf[32];
		sprintf(buf, "number: %zu", i);
		test_key(buf, true, i);
	}
	test_key("test", true, 2);
	test_key("test1", false, 0);
	assert(m.len == 1001);
	map_term(&m);
	printf("Passed map test!\n");
}
