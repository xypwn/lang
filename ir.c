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
	[IRPrint] = "print",
	[IRJmp] = "jmp",
	[IRJnz] = "jnz",
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
		if (v->toks[i].instr == IRPrint) {
			for (IRArgs *a = v->toks[i].Print.args; a != NULL;) {
				IRArgs *next = a->next;
				free(a);
				a = next;
			}
		}
	}
	free(v->toks);
}

void irtoks_app(IRToks *v, IRTok t) {
	if (v->len+1 > v->cap)
		v->toks = xrealloc(v->toks, sizeof(IRTok) * (v->cap *= 2));
	v->toks[v->len++] = t;
}

void irtoks_app_irtoks(IRToks *v, IRToks *other) {
	if (v->len+other->len > v->cap)
		v->toks = xrealloc(v->toks, sizeof(IRTok) * (other->len + (v->cap *= 2)));
	for (size_t i = 0; i < other->len; i++)
		v->toks[v->len++] = other->toks[i];
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

void print_ir(IRToks *v) {
	for (size_t i = 0; i < v->len; i++) {
		printf("%04zx ", i);
		printf("%s", irinstr_str[v->toks[i].instr]);
		switch (v->toks[i].instr) {
			case IRSet:
			case IRNeg:
				printf(" %%%zu ", v->toks[i].Unary.addr);
				print_irparam(&v->toks[i].Unary.val);
				break;
			case IRAdd:
			case IRSub:
			case IRDiv:
			case IRMul:
				printf(" %%%zu ", v->toks[i].Arith.addr);
				print_irparam(&v->toks[i].Arith.lhs);
				printf(" ");
				print_irparam(&v->toks[i].Arith.rhs);
				break;
			case IRPrint:
				for (IRArgs *a = v->toks[i].Print.args; a != NULL; a = a->next) {
					printf(" ");
					print_irparam(&a->param);
				}
				break;
			case IRJmp:
				printf(" %zx", v->toks[i].Jmp.iaddr);
				break;
			case IRJnz:
				printf(" ");
				print_irparam(&v->toks[i].CJmp.condition);
				printf(" %zx", v->toks[i].CJmp.iaddr);
				break;
			default:
				break;
		}
		printf(" ; %zu:%zu", v->toks[i].ln, v->toks[i].col);
		printf("\n");
	}
}
