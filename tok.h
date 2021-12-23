#ifndef __TOK_H__
#define __TOK_H__

#include <stdint.h>
#include <unistd.h>

#include "util.h"

typedef struct Type {
	enum {
		TypeVoid = 0,
		TypeFloat,
		TypeInt,
		TypeBool,
		TypeChar,
	} kind;

	/*union {
	};*/
} Type;

typedef struct Value {
	Type type;

	union {
		double Float;
		ssize_t Int;
		bool Bool;
		char Char;
	};
} Value;

enum Operator {
	OpLCurl  = '{',
	OpRCurl  = '}',
	OpLParen = '(',
	OpRParen = ')',
	OpComma  = ',',
	OpAdd    = '+',
	OpSub    = '-',
	OpMul    = '*',
	OpDiv    = '/',
	OpNot    = '!',
	OpBeginNonchars = 256,
	OpEq,
	OpLt,
	OpGt,
	OpLe,
	OpGe,
	OpAnd,
	OpOr,
	OpNewLn,
	OpEOF,
	OperatorEnumSize,
};
typedef enum Operator Operator;

#define PREC_DELIM -1
extern int8_t op_prec[OperatorEnumSize];
extern const char *op_str[OperatorEnumSize];

typedef struct Identifier {
	enum {
		IdentName,
		IdentAddr,
	} kind;

	union {
		char *Name;
		size_t Addr;
	};
} Identifier;

typedef struct Tok {
	size_t ln, col;

	enum {
		TokOp,
		TokVal,
		TokIdent,
		TokAssign,
		TokDeclare,
		TokIf,
		TokElse,
		TokWhile,
		TokKindEnumSize,
	} kind;

	union {
		Operator Op;
		Value Val;
		Identifier Ident;
	};
} Tok;

extern const char *tok_str[TokKindEnumSize];

typedef struct TokListItem {
	struct TokListItem *prev, *next;
	Tok tok;
} TokListItem;

typedef struct TokList {
	TokListItem *begin, *end;
	Pool *p;
} TokList;

void toklist_init(TokList *l);
void toklist_term(TokList *l);
void toklist_append(TokList *l, Tok t);
void toklist_del(TokList *l, TokListItem *from, TokListItem *to);
void print_toks(TokList *l);

#endif /* TOK_H */
