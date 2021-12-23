#ifndef __IR_H__
#define __IR_H__

#include "tok.h"

typedef struct BuiltinFunc {
	char *name;
	bool side_effects : 1;
	size_t n_args;
	Value (*func)(Value *args);
	size_t fid; /* function ID, assigned automatically */
} BuiltinFunc;

enum IRInstr {
	IRSet,
	IRNeg,
	IRAdd,
	IRSub,
	IRMul,
	IRDiv,
	IREq,
	IRLt,
	IRLe,
	IRNot,
	IRAnd,
	IROr,
	IRJmp,
	IRJnz,
	IRCallInternal,
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
			IRParam *args;
		} CallI;
	};
} IRTok;

typedef struct IRToks {
	size_t len, cap;
	IRTok *toks;
} IRToks;

void irtoks_init_long(IRToks *v);
void irtoks_init_short(IRToks *v);
void irtoks_term(IRToks *v);
void irtoks_app(IRToks *v, IRTok t);
void irtoks_eat_irtoks(IRToks *v, IRToks *other, size_t jmp_offset);

void print_ir(IRToks *v, const BuiltinFunc *builtin_funcs);

#endif /* IR_H */
