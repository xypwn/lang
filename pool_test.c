#include "util.c"

typedef struct Test {
	char a_string[64];
} Test;

int main(void) {
	Pool *p = pool_new(1);
	Test *t = pool_alloc(p, sizeof(Test));
	strcpy(t->a_string, "a test string");
	Test *tarr = pool_alloc(p, sizeof(Test) * 32);
	strcpy(tarr[31].a_string, "another test string");
	char *c = pool_alloc(p, 1);
	*c = 'a';
	Test *largearr = pool_alloc(p, sizeof(Test) * 1024);
	strcpy(largearr[1023].a_string, "yet another test string");
	pool_term(p);
}
