#ifndef __UTIL_H__
#define __UTIL_H__

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* some ANSI color codes */
#define C_RED     "\x1b[31m"
#define C_GREEN   "\x1b[32m"
#define C_YELLOW  "\x1b[33m"
#define C_BLUE    "\x1b[34m"
#define C_MAGENTA "\x1b[35m"
#define C_CYAN    "\x1b[36m"
#define C_WHITE   "\x1b[37m"

#define C_IRED     "\x1b[31;1m"
#define C_IGREEN   "\x1b[32;1m"
#define C_IYELLOW  "\x1b[33;1m"
#define C_IBLUE    "\x1b[34;1m"
#define C_IMAGENTA "\x1b[35;1m"
#define C_IWHITE   "\x1b[37;1m"
#define C_ICYAN    "\x1b[36;1m"

#define C_RESET   "\x1b[m"

#define ERRSZ 4096
extern char errbuf[ERRSZ];
extern bool err;
extern size_t err_ln, err_col;
#define TRY(expr) {expr; if (err) return;}
#define TRY_RET(expr, ret) {expr; if (err) return (ret);}
void set_err(const char *fmt, ...);

#define ASSERT_UNREACHED() { fprintf(stderr, "Illegal code position reached in %s:%d\n", __FILE__, __LINE__); exit(1); }

#define IS_NUM(c)   (c >= '0' && c <= '9')
#define IS_ALPHA(c) ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
#define IS_ALNUM(c) (IS_ALPHA(c) || IS_NUM(c))

#define streq(a, b) (strcmp(a, b) == 0)
/* check if a null-terminated string and a non-null-terminated string are equal */
static inline bool streq_0_n(const char *a, const char *b, size_t bn) { return bn == strlen(a) ? strncmp(a, b, bn) == 0 : false; }
/* a more trusting version of strndup; also for systems that don't have strndup */
char *sndup(const char *s, size_t n);
/* convert a non-null-terminated string to an intmax_t */
intmax_t stoimax(const char *s, size_t n, size_t base, ssize_t *endpos /* -1 on success */);
/* convert a non-null-terminated string to a double */
double stod(const char *s, size_t n, ssize_t *endpos /* -1 on success */);

/* sets errno on failure */
char *mreadfile(FILE *fp);

uint32_t fnv1a32(const void *data, size_t n);

#endif /* UTIL_H */
