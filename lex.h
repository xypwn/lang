#ifndef __LEX_H__
#define __LEX_H__

#include "tok.h"

TokList lex(const char *s, Pool *static_vars);

#endif /* LEX_H */
