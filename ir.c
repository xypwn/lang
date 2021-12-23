#include "ir.h"

#include <stdio.h>
#include <stdlib.h>

const char *irinstr_str[IRInstrEnumSize] = {
	[IRSet] = "set",
	[IRNeg] = "neg",
	[IRAdd] = "add",
	[IRSub] = "sub",
	[IRMul] = "mul",
	[IRDiv] = "div",
	[IREq]  = "eq",
	[IRLt]  = "lt",
	[IRLe]  = "le",
	[IRNot] = "not",
	[IRJmp] = "jmp",
	[IRJnz] = "jnz",
	[IRCallInternal] = "calli",
};

#define IRTOKS_INIT_CAP_LONG 4096
#define IRTOKS_INIT_CAP_SHORT 16

static void irtoks_init_with_cap(IRToks *v, size_t cap);

static void irtoks_init_with_cap(IRToks *v, size_t cap) {
	v->toks = xmalloc(sizeof(IRTok) * cap);
	v->len = 0;
	v->cap = cap;
}

void irtoks_init_long(IRToks *v) {
	irtoks_init_with_cap(v, IRTOKS_INIT_CAP_LONG);

}

void irtoks_init_short(IRToks *v) {
	irtoks_init_with_cap(v, IRTOKS_INIT_CAP_SHORT);
}

void irtoks_term(IRToks *v) {
	for (size_t i = 0; i < v->len; i++) {
		if (v->toks[i].instr == IRCallInternal)
			free(v->toks[i].CallI.args);
	}
	free(v->toks);
}

void irtoks_app(IRToks *v, IRTok t) {
	if (v->len+1 > v->cap)
		v->toks = xrealloc(v->toks, sizeof(IRTok) * (v->cap *= 2));
	v->toks[v->len++] = t;
}

void irtoks_eat_irtoks(IRToks *v, IRToks *other, size_t jmp_offset) {
	if (v->len+other->len > v->cap)
		v->toks = xrealloc(v->toks, sizeof(IRTok) * (other->len + (v->cap *= 2)));
	for (size_t i = 0; i < other->len; i++) {
		/* correct for changed jump addresses */
		if (other->toks[i].instr == IRJmp)
			other->toks[i].Jmp.iaddr += jmp_offset;
		else if (other->toks[i].instr == IRJnz)
			other->toks[i].CJmp.iaddr += jmp_offset;

		v->toks[v->len++] = other->toks[i];
	}
	/* We're not calling irtoks_term() because we don't want associated items
	 * (for example function arguments) to get deallocated as well. */
	free(other->toks);
}

static void print_val(const Value *v);
static void print_irparam(const IRParam *p);

static void print_val(const Value *v) {
	switch (v->type.kind) {
		case TypeFloat:
			printf("%f", v->Float);
			break;
		case TypeInt:
			printf("%zd", v->Int);
			break;
		case TypeBool:
			printf("%s", v->Bool ? "true" : "false");
			break;
		default:
			printf("(unknown type)");
			break;
	}
}

static void print_irparam(const IRParam *p) {
	if (p->kind == IRParamLiteral) {
		print_val(&p->Literal);
	} else if (p->kind == IRParamAddr) {
		printf("%%%zd", p->Addr);
	}
}

void print_ir(IRToks *v, const BuiltinFunc *builtin_funcs) {
	for (size_t i = 0; i < v->len; i++) {
		printf("%04zx ", i);
		printf("%s", irinstr_str[v->toks[i].instr]);
		switch (v->toks[i].instr) {
			case IRSet:
			case IRNeg:
			case IRNot:
				printf(" %%%zx ", v->toks[i].Unary.addr);
				print_irparam(&v->toks[i].Unary.val);
				break;
			case IRAdd:
			case IRSub:
			case IRDiv:
			case IRMul:
			case IREq:
			case IRLt:
			case IRLe:
				printf(" %%%zx ", v->toks[i].Binary.addr);
				print_irparam(&v->toks[i].Binary.lhs);
				printf(" ");
				print_irparam(&v->toks[i].Binary.rhs);
				break;
			case IRJmp:
				printf(" %zx", v->toks[i].Jmp.iaddr);
				break;
			case IRJnz:
				printf(" ");
				print_irparam(&v->toks[i].CJmp.condition);
				printf(" %zx", v->toks[i].CJmp.iaddr);
				break;
			case IRCallInternal: {
				const BuiltinFunc *f = &builtin_funcs[v->toks[i].CallI.fid];
				printf(" %s %%%zx", f->name, v->toks[i].CallI.ret_addr);
				for (size_t j = 0; j < f->n_args; j++) {
					printf(" ");
					print_irparam(&v->toks[i].CallI.args[j]);
				}
				break;
			}
			default: ASSERT_UNREACHED(); break;
		}
		printf(" ; %zu:%zu", v->toks[i].ln, v->toks[i].col);
		printf("\n");
	}
}
