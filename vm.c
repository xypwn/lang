#include "vm.h"

#include "runtime.h"
#include "util.h"

#define INIT_STACK_CAP 128

typedef struct Stack {
	Value *mem;
	bool *holds_value;
	size_t len, cap;
} Stack;

static Stack stack_make(void);
static void stack_term(Stack *s);
static void stack_fit(Stack *s, size_t idx);
static void stack_assign(Stack *s, size_t idx, const Value *v);

static Stack stack_make(void) {
	Stack s;
	s.mem = xmalloc(sizeof(Value) * INIT_STACK_CAP);
	s.holds_value = xmalloc(sizeof(bool) * INIT_STACK_CAP);
	s.cap = INIT_STACK_CAP;
	s.len = 0;
	for (size_t i = 0; i < s.cap; i++)
		s.holds_value[i] = false;
	return s;
}

static void stack_term(Stack *s) {
	/* free any dynamically allocated objects still alive */
	for (size_t i = 0; i < s->cap; i++) {
		if (s->holds_value[i])
			free_value(&s->mem[i], false);
	}
	/* free the stack memory itself */
	free(s->mem);
	free(s->holds_value);
}

static void stack_fit(Stack *s, size_t idx) {
	size_t size = idx+1;
	if (size > s->cap) {
		size_t new_cap = size + s->cap * 2;
		s->mem = xrealloc(s->mem, sizeof(Value) * new_cap);
		s->holds_value = xrealloc(s->holds_value, sizeof(bool) * new_cap);
		for (size_t i = s->cap; i < new_cap; i++)
			s->holds_value[i] = false;
		s->cap = new_cap;
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

static void stack_assign(Stack *s, size_t idx, const Value *v) {
	stack_fit(s, idx);
	if (s->holds_value[idx])
		free_value(&s->mem[idx], false); /* free any overwritten heap-allocated values */
	s->mem[idx] = *v;
	s->holds_value[idx] = true;
}

void run(IRList *ir, const BuiltinFunc *builtin_funcs) {
	/* so we don't have to call malloc on every function call */
	size_t fn_args_cap = 16;
	Value *fn_args = xmalloc(sizeof(Value) * fn_args_cap);

	/* so we can use index-based addressing */
	irlist_update_index(ir);

	Stack s = stack_make();
	for (IRItem *i = ir->begin; i;) {
		IRTok *instr = &i->tok;
		err_ln = instr->ln;
		err_col = instr->col;
		switch (instr->instr) {
			case IRSet:
			case IRNeg:
			case IRNot: {
				Value res;
				TRY_ELSE(res = eval_unary(instr->instr, irparam_to_val(&s, &instr->Unary.val)),
					{free(fn_args); stack_term(&s);});
				stack_assign(&s, instr->Unary.addr, &res);
				break;
			}
			case IRAddrOf: {
				if (instr->Unary.val.kind != IRParamAddr) {
					set_err("Unable to take the address of a literal");
					free(fn_args);
					stack_term(&s);
					return;
				}
				Value *v = &s.mem[instr->Unary.val.Addr];
				Value res = {
					.type = TypePtr,
					.Ptr = {
						.type = v->type,
						.val = &v->Void,
					},
				};
				stack_assign(&s, instr->Unary.addr, &res);
				break;
			}
			case IRAdd:
			case IRSub:
			case IRDiv:
			case IRMul:
			case IREq:
			case IRNeq:
			case IRLt:
			case IRLe:
			case IRAnd:
			case IROr: {
				Value res;
				TRY_ELSE(res = eval_binary(instr->instr,
					irparam_to_val(&s, &instr->Binary.lhs),
					irparam_to_val(&s, &instr->Binary.rhs)),
					{free(fn_args); stack_term(&s);});
				stack_assign(&s, instr->Binary.addr, &res);
				break;
			}
			case IRJmp:
				if (instr->Jmp.iaddr < ir->len)
					i = ir->index[instr->Jmp.iaddr];
				else
					i = NULL;
				continue;
			case IRJnz:
				if (is_nonzero(irparam_to_val(&s, &instr->CJmp.condition))) {
					if (instr->Jmp.iaddr < ir->len)
						i = ir->index[instr->CJmp.iaddr];
					else
						i = NULL;
					continue;
				}
				break;
			case IRCallInternal: {
				const BuiltinFunc *f = &builtin_funcs[instr->CallI.fid];
				size_t n_args = instr->CallI.n_args;
				/* make sure enough space for our arguments is allocated */
				if (n_args > fn_args_cap)
					fn_args = xrealloc(fn_args, sizeof(Value) * (fn_args_cap = n_args));
				/* copy arguments into buffer */
				for (size_t i = 0; i < n_args; i++)
					fn_args[i] = *irparam_to_val(&s, &instr->CallI.args[i]);

				if (f->returns) {
					Value res;
					if (f->kind == FuncVarArgs) {
						size_t min_args = f->VarArgs.min_args;
						TRY_ELSE(res = f->VarArgs.WithRet.func(n_args - min_args, fn_args),
							{free(fn_args); stack_term(&s);});
					} else if (f->kind == FuncFixedArgs) {
						TRY_ELSE(res = f->FixedArgs.WithRet.func(fn_args),
							{free(fn_args); stack_term(&s);});
					} else
						ASSERT_UNREACHED();
					stack_assign(&s, instr->CallI.ret_addr, &res);
				} else {
					if (f->kind == FuncVarArgs) {
						size_t min_args = f->VarArgs.min_args;
						TRY_ELSE(f->VarArgs.NoRet.func(n_args - min_args, fn_args),
							{free(fn_args); stack_term(&s);});
					} else if (f->kind == FuncFixedArgs) {
						TRY_ELSE(f->FixedArgs.NoRet.func(fn_args),
							{free(fn_args); stack_term(&s);});
					} else
						ASSERT_UNREACHED();
				}
				break;
			}
			case IRArrMake: {
				size_t arr_len = instr->ArrMake.len, arr_cap = instr->ArrMake.cap;
				Value arr = {
					.type = TypeArr,
					.Arr = {
						.type = TypeVoid,
						.is_string = false,
						.dynamically_allocated = true,
						.vals = NULL,
						.len = arr_len,
						.cap = arr_len ? arr_cap : 0,
					},
				};
				if (arr_len) {
					Type arr_ty = irparam_to_val(&s, &instr->ArrMake.vals[0])->type;
					void *arr_vals = xmalloc(type_size[arr_ty] * arr_cap);
					for (size_t j = 0; j < arr_len; j++) {
						Value *v = irparam_to_val(&s, &instr->ArrMake.vals[j]);
						if (v->type != arr_ty) {
							set_err("Type of array item %zu (%s) differs from array type (%s)", j, type_str[v->type], type_str[arr_ty]);
							free(arr_vals);
							free(fn_args);
							stack_term(&s);
							return;
						}
						memcpy((uint8_t*)arr_vals + type_size[arr_ty] * j, &v->Void, type_size[arr_ty]);
					}
					arr.Arr.type = arr_ty;
					arr.Arr.vals = arr_vals;
				}
				stack_assign(&s, instr->ArrMake.arr_addr, &arr);
				break;
			}
			default:
				ASSERT_UNREACHED();
		}

		i = i->next;
	}
	stack_term(&s);
	free(fn_args);
}
