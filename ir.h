#ifndef __IR_H__
#define __IR_H__

#include "tok.h"

enum IRInstr {
	IRSet,
	IRNeg,
	IRAdd,
	IRSub,
	IRMul,
	IRDiv,
	IRPrint,
	IRJmp,
	IRJnz,
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
		} Arith;

		struct {
			IRArgs *args;
			size_t args_size;
		} Print;

		struct {
			size_t iaddr;
		} Jmp;
		
		struct {
			size_t iaddr;
			IRParam condition;
		} CJmp;
	};
} IRTok;

typedef struct IRToks {
	size_t len, cap;
	IRTok *toks;
} IRToks;

void irtoks_init(IRToks *v);
void irtoks_term(IRToks *v);
void irtoks_app(IRToks *v, IRTok t);

void print_ir(IRToks *v);

#endif /* IR_H */
