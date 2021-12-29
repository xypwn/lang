#ifndef __PARSE_H__
#define __PARSE_H__

#include "ir.h"
#include "tok.h"
#include "util.h"

IRList parse(TokList *toks, BuiltinFunc *builtin_funcs, size_t n_builtin_funcs);

#endif /* PARSE_H */
