#ifndef __IR_H__
#define __IR_H__

#include "tok.h"

typedef struct BuiltinFunc {
	enum {
		FuncFixedArgs,
		FuncVarArgs,
	} kind;

	bool returns : 1;
	bool side_effects : 1;
	char *name;
	size_t fid; /* function ID, assigned automatically */

	union {
		struct {
			size_t n_args;
			union {
				struct { Value (*func)(Value *args); } WithRet;
				struct { void  (*func)(Value *args); } NoRet;
			};
		} FixedArgs;

		struct {
			size_t min_args;
			union {
				struct { Value (*func)(size_t extra_args, Value *args); } WithRet;
				struct { void  (*func)(size_t extra_args, Value *args); } NoRet;
			};
		} VarArgs;
	};
} BuiltinFunc;

enum IRInstr {
	IRSet,
	IRNeg,
	IRAdd,
	IRSub,
	IRMul,
	IRDiv,
	IREq,
	IRNeq,
	IRLt,
	IRLe,
	IRNot,
	IRAnd,
	IROr,
	IRJmp,
	IRJnz,
	IRCallInternal,
	IRAddrOf,
	IRInstrEnumSize,
};
typedef enum IRInstr IRInstr;

extern const char *irinstr_str[IRInstrEnumSize];

typedef struct IRParam {
	enum {
		IRParamNull = 0,
		IRParamLiteral,
		IRParamAddr,
	} kind;

	union {
		Value Literal;
		size_t Addr;
	};
} IRParam;

typedef struct IRArgs {
	struct IRArgs *next;
	IRParam param;
} IRArgs;

typedef struct IRTok {
	size_t ln, col;

	IRInstr instr;

	union {
		struct {
			size_t addr;
			IRParam val;
		} Unary;

		struct {
			size_t addr;
			IRParam lhs, rhs;
		} Binary;

		struct {
			size_t iaddr;
		} Jmp;
		
		struct {
			size_t iaddr;
			IRParam condition;
		} CJmp;

		struct {
			size_t ret_addr;
			size_t fid;
			size_t n_args;
			IRParam *args;
		} CallI;
	};
} IRTok;

typedef struct IRItem {
	struct IRItem *next;
	IRTok tok;
} IRItem;

typedef struct IRList {
	IRItem *begin, *end;
	Pool *p;
	IRItem **index; /* index to pointer, irlist_update_index() must be called before use */
	size_t len;
} IRList;

void irlist_init_long(IRList *v);
void irlist_init_short(IRList *v);
void irlist_term(IRList *v);
void irlist_app(IRList *v, IRTok t);
void irlist_eat_irlist(IRList *v, IRList *other);
void irlist_update_index(IRList *v); /* should be used very conservatively */

void print_ir(IRList *v, const BuiltinFunc *builtin_funcs);

void optimize_ir(IRList *v);

#endif /* IR_H */
