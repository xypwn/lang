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
	[OpEq]     = 0,
	[OpLt]     = 0,
	[OpGt]     = 0,
	[OpLe]     = 0,
	[OpGe]     = 0,
	[OpAdd]    = 1,
	[OpSub]    = 1,
	[OpMul]    = 2,
	[OpDiv]    = 2,
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
	[OpNot]    = "!",
	[OpNewLn]  = "\\n",
	[OpEOF]    = "EOF",
	[OpEq]     = "==",
	[OpLt]     = "<",
	[OpGt]     = ">",
	[OpLe]     = "<=",
	[OpGe]     = ">=",
};

const char *tok_str[TokKindEnumSize] = {
	[TokAssign]  = "=",
	[TokDeclare] = ":=",
	[TokIf]      = "if",
	[TokElse]    = "else",
	[TokWhile]   = "while",
};

#define TOKLIST_MEMPOOL_INIT_CAP 32768

static inline TokListItem *toklist_alloc_item(TokList *l) {
	TokListItem *itm = pool_alloc(l->p, sizeof(TokListItem));
	itm->prev = itm->next = NULL;
	return itm;
}

void toklist_init(TokList *l) {
	l->begin = l->end = NULL;
	l->p = pool_new(TOKLIST_MEMPOOL_INIT_CAP);
}

void toklist_term(TokList *l) {
	pool_term(l->p);
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
					case TypeBool:
						printf(": " C_ICYAN "%s" C_RESET, i->tok.Val.Bool ? "true" : "false");
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
