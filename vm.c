#include "vm.h"

#include "runtime.h"
#include "util.h"

#define INIT_STACK_CAP 256

typedef struct Stack {
	Value *mem;
	size_t len, cap;
} Stack;

static Stack stack_make(void);
static void stack_term(Stack *s);
static void stack_fit(Stack *s, size_t idx);

static Stack stack_make(void) {
	Stack s;
	s.mem = xmalloc(sizeof(Value) * INIT_STACK_CAP);
	s.cap = INIT_STACK_CAP;
	s.len = 0;
	return s;
}

static void stack_term(Stack *s) {
	free(s->mem);
}

static void stack_fit(Stack *s, size_t idx) {
	size_t size = idx+1;
	if (size > s->cap) {
		s->mem = xrealloc(s->mem, sizeof(Value) * (size + (s->cap *= 2)));
	}
}

static Value *irparam_to_val(Stack *s, IRParam *v) {
	if (v->kind == IRParamLiteral)
		return &v->Literal;
	else if (v->kind == IRParamAddr)
		return &s->mem[v->Addr];
	else
		ASSERT_UNREACHED();
}

void run(const IRToks *ir, const BuiltinFunc *builtin_funcs) {
	/* so we don't have to call malloc on every function call */
	size_t fn_args_cap = 16;
	Value *fn_args = xmalloc(sizeof(Value) * fn_args_cap);

	Stack s = stack_make();
	for (size_t i = 0; i < ir->len;) {
		IRTok *instr = &ir->toks[i];
		err_ln = instr->ln;
		err_col = instr->col;
		switch (instr->instr) {
			case IRSet:
			case IRNeg:
			case IRNot:
				stack_fit(&s, instr->Unary.addr);
				TRY_ELSE(s.mem[instr->Unary.addr] = eval_unary(instr->instr, irparam_to_val(&s, &instr->Unary.val)),
					{free(fn_args); stack_term(&s);});
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
				stack_fit(&s, instr->Binary.addr);
				TRY_ELSE(s.mem[instr->Binary.addr] = eval_binary(instr->instr,
					irparam_to_val(&s, &instr->Binary.lhs),
					irparam_to_val(&s, &instr->Binary.rhs)),
					{free(fn_args); stack_term(&s);});
				break;
			case IRJmp:
				i = instr->Jmp.iaddr;
				continue;
			case IRJnz:
				if (is_nonzero(irparam_to_val(&s, &instr->CJmp.condition))) {
					i = instr->Jmp.iaddr;
					continue;
				}
				break;
			case IRCallInternal: {
				const BuiltinFunc *f = &builtin_funcs[instr->CallI.fid];
				/* make sure enough space for our arguments is allocated */
				if (f->n_args > fn_args_cap)
					fn_args = xrealloc(fn_args, sizeof(Value) * (fn_args_cap = f->n_args));
				/* copy arguments into buffer */
				for (size_t i = 0; i < f->n_args; i++)
					fn_args[i] = *irparam_to_val(&s, &instr->CallI.args[i]);

				stack_fit(&s, instr->CallI.ret_addr);
				TRY_ELSE(s.mem[instr->CallI.ret_addr] = f->func(fn_args),
					{free(fn_args); stack_term(&s);});
				break;
			}
			default:
				ASSERT_UNREACHED();
		}

		i++;
	}
	stack_term(&s);
	free(fn_args);
}
