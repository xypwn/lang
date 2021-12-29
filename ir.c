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
	[IRNeq] = "neq",
	[IRLt]  = "lt",
	[IRLe]  = "le",
	[IRNot] = "not",
	[IRAnd] = "and",
	[IROr]  = "or",
	[IRJmp] = "jmp",
	[IRJnz] = "jnz",
	[IRCallInternal] = "calli",
	[IRAddrOf] = "addrof",
};

#define IRLIST_INIT_CAP_LONG 4096
#define IRLIST_INIT_CAP_SHORT 16

static void irlist_init_with_cap(IRList *v, size_t cap);
static IRItem *irlist_new_item(IRList *v);

static void irlist_init_with_cap(IRList *v, size_t cap) {
	v->begin = NULL;
	v->end = NULL;
	v->p = pool_new(sizeof(IRItem) * cap);
	v->index = NULL;
	v->len = 0;
}

static IRItem *irlist_new_item(IRList *v) {
	IRItem *ret = pool_alloc(v->p, sizeof(IRItem));
	ret->next = NULL;
	return ret;
}

void irlist_init_long(IRList *v) {
	irlist_init_with_cap(v, IRLIST_INIT_CAP_LONG);
}

void irlist_init_short(IRList *v) {
	irlist_init_with_cap(v, IRLIST_INIT_CAP_SHORT);
}

void irlist_term(IRList *v) {
	for (IRItem *i = v->begin; i; i = i->next) {
		if (i->tok.instr == IRCallInternal && i->tok.CallI.args)
			free(i->tok.CallI.args);
	}
	pool_term(v->p);
}

void irlist_app(IRList *v, IRTok t) {
	v->index = NULL; /* invalidate index */
	IRItem *itm = irlist_new_item(v);
	itm->tok = t;

	if (!v->begin && !v->end)
		v->begin = v->end = itm;
	else {
		v->end->next = itm;
		v->end = itm;
	}

	v->len++;
}

void irlist_eat_irlist(IRList *v, IRList *other) {
	v->index = NULL; /* invalidate index */
	size_t jmp_offset = v->len-1;
	for (IRItem *i = other->begin; i; i = i->next) {
		/* correct for changed jump addresses */
		if (i->tok.instr == IRJmp)
			i->tok.Jmp.iaddr += jmp_offset;
		else if (i->tok.instr == IRJnz)
			i->tok.CJmp.iaddr += jmp_offset;

		irlist_app(v, i->tok);
	}
	/* We're not calling irlist_term() because we don't want associated items
	 * (for example function arguments) to get deallocated as well. */
	pool_term(other->p);
}

void irlist_update_index(IRList *v) {
	if (v->index)
		return;
	v->index = pool_alloc(v->p, v->len);
	size_t num_idx = 0;
	for (IRItem *i = v->begin; i; i = i->next, num_idx++)
		v->index[num_idx] = i;
}

static void print_irparam(const IRParam *p);

static void print_irparam(const IRParam *p) {
	if (p->kind == IRParamLiteral) {
		print_value(&p->Literal, false);
	} else if (p->kind == IRParamAddr) {
		printf("%%%zd", p->Addr);
	}
}

void print_ir(IRList *v, const BuiltinFunc *builtin_funcs) {
	size_t iaddr = 0;
	for (IRItem *i = v->begin; i; i = i->next, iaddr++) {
		printf("%04zx ", iaddr);
		printf("%s", irinstr_str[i->tok.instr]);
		switch (i->tok.instr) {
			case IRSet:
			case IRNeg:
			case IRNot:
			case IRAddrOf:
				printf(" %%%zx ", i->tok.Unary.addr);
				print_irparam(&i->tok.Unary.val);
				break;
			case IRAdd:
			case IRSub:
			case IRDiv:
			case IRMul:
			case IREq:
			case IRNeq:
			case IRLt:
			case IRLe:
			case IRAnd:
			case IROr:
				printf(" %%%zx ", i->tok.Binary.addr);
				print_irparam(&i->tok.Binary.lhs);
				printf(" ");
				print_irparam(&i->tok.Binary.rhs);
				break;
			case IRJmp:
				printf(" %zx", i->tok.Jmp.iaddr);
				break;
			case IRJnz:
				printf(" ");
				print_irparam(&i->tok.CJmp.condition);
				printf(" %zx", i->tok.CJmp.iaddr);
				break;
			case IRCallInternal: {
				const BuiltinFunc *f = &builtin_funcs[i->tok.CallI.fid];
				if (f->returns)
					printf(" %%%zx", i->tok.CallI.ret_addr);
				printf(" %s", f->name);
				for (size_t j = 0; j < i->tok.CallI.n_args; j++) {
					printf(" ");
					print_irparam(&i->tok.CallI.args[j]);
				}
				break;
			}
			default: ASSERT_UNREACHED();
		}
		printf(" ; %zu:%zu", i->tok.ln, i->tok.col);
		printf("\n");
	}
}

void optimize_ir(IRList *v) {
	irlist_update_index(v);
	for (IRItem *i = v->begin; i; i = i->next) {
		switch (i->tok.instr) {
			case IRJmp: {
				/* resolve jump chains (esp. produced by if-else-if... statements) */
				size_t ja = i->tok.Jmp.iaddr;
				while (ja < v->len && v->index[ja]->tok.instr == IRJmp)
					ja = v->index[ja]->tok.Jmp.iaddr;
				i->tok.Jmp.iaddr = ja;
			}
			default: break;
		}
	}
}
