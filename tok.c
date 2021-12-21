#include "tok.h"

#include <stdio.h>
#include <stdlib.h>

#include "util.h"

int8_t op_prec[OperatorEnumSize] = {
	[OpEOF]    = PREC_DELIM,
	[OpNewLn]  = PREC_DELIM,
	[OpLCurl]  = PREC_DELIM,
	[OpRParen] = PREC_DELIM,
	[OpComma]  = PREC_DELIM,
	[OpAdd]    = 0,
	[OpSub]    = 0,
	[OpMul]    = 1,
	[OpDiv]    = 1,
};

const char *op_str[OperatorEnumSize] = {
	[OpLCurl]  = "{",
	[OpRCurl]  = "}",
	[OpLParen] = "(",
	[OpRParen] = ")",
	[OpComma]  = ",",
	[OpAdd]    = "+",
	[OpSub]    = "-",
	[OpMul]    = "*",
	[OpDiv]    = "/",
	[OpNewLn]  = "\\n",
	[OpEOF]    = "EOF",
};

const char *tok_str[TokKindEnumSize] = {
	[TokAssign]  = "=",
	[TokDeclare] = ":=",
	[TokIf]      = "if",
	[TokWhile]   = "while",
};

#define TOKLIST_MEMPOOL_INIT_CAP 4096

static inline TokListItem *toklist_alloc_item(TokList *l) {
	if (l->curr_mempool_cap < l->mempool_sizes[l->curr_mempool]+1) {
		if (l->curr_mempool+1 >= 32)
			ASSERT_UNREACHED();
		l->curr_mempool++;
		l->curr_mempool_cap *= 2;
		l->mempool_sizes[l->curr_mempool] = 0;
		l->mempools[l->curr_mempool] = malloc(sizeof(TokListItem) * l->curr_mempool_cap);
	}
	TokListItem *itm = l->mempools[l->curr_mempool] + l->mempool_sizes[l->curr_mempool]++;
	itm->prev = itm->next = NULL;
	return itm;
}

void toklist_init(TokList *l) {
	l->begin = l->end = NULL;
	l->curr_mempool = 0;
	l->mempools[l->curr_mempool] = malloc(sizeof(TokListItem) * TOKLIST_MEMPOOL_INIT_CAP);
	l->curr_mempool_cap = TOKLIST_MEMPOOL_INIT_CAP;
	l->mempool_sizes[0] = 0;
}

void toklist_term(TokList *l) {
	for (size_t i = 0; i <= l->curr_mempool; i++) {
		for (size_t j = 0; j < l->mempool_sizes[i]; j++) {
			TokListItem *itm = &l->mempools[i][j];
			if (itm->tok.kind == TokIdent && itm->tok.Ident.kind == IdentName) {
				free(itm->tok.Ident.Name);
			}
		}
		free(l->mempools[i]);
	}
}

void toklist_append(TokList *l, Tok t) {
	TokListItem *itm = toklist_alloc_item(l);
	itm->tok = t;
	if (l->begin == NULL) {
		l->begin = l->end = itm;
		return;
	}
	l->end->next = itm;
	itm->prev = l->end;
	l->end = itm;
}

void toklist_del(TokList *l, TokListItem *from, TokListItem *to) {
	if (from == l->begin) {
		l->begin = to->next;
		if (to->next)
			to->next->prev = NULL;
	} else
		from->prev->next = to->next;

	if (to == l->end) {
		l->end = from->prev;
		if (from->prev)
			from->prev->next = NULL;
	} else
		to->next->prev = from->prev;
}

void print_toks(TokList *l) {
	for (TokListItem *i = l->begin; i != NULL; i = i->next) {
		printf("( ");
		switch (i->tok.kind) {
			case TokOp:
				printf(C_IYELLOW "Op" C_RESET);
				printf(": " C_ICYAN "%s" C_RESET, op_str[i->tok.Op]);
				break;
			case TokVal:
				printf(C_IYELLOW "Val" C_RESET);
				switch (i->tok.Val.type.kind) {
					case TypeFloat:
						printf(": " C_ICYAN "%f" C_RESET, i->tok.Val.Float);
						break;
					case TypeInt:
						printf(": " C_ICYAN "%zd" C_RESET, i->tok.Val.Int);
						break;
					default:
						printf(" " C_ICYAN "(unknown type)" C_RESET);
						break;
				}
				break;
			case TokIdent:
				printf(C_IYELLOW "Ident" C_RESET);
				if (i->tok.Ident.kind == IdentName)
					printf(": " C_ICYAN "Name" C_RESET ": " C_IGREEN "'%s'" C_RESET, i->tok.Ident.Name);
				else if (i->tok.Ident.kind == IdentAddr)
					printf(": " C_ICYAN "Addr" C_RESET ": " C_IGREEN "%zu" C_RESET, i->tok.Ident.Addr);
				break;
			default:
				if (tok_str[i->tok.kind]) {
					printf(C_IYELLOW "%s" C_RESET, tok_str[i->tok.kind]);
				}
		}
		printf(" | %zu:%zu )\n", i->tok.ln, i->tok.col);
	}

}
