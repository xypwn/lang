#include "parse.h"

#include <stdbool.h>

#include "map.h"
#include "runtime.h"

typedef struct Scope {
	struct Scope *parent;
	size_t mem_addr;
	bool has_idents;
	Map ident_addrs;
} Scope;

typedef struct ExprRet {
	enum {
		ExprRetVal,
		ExprRetIdent,
		ExprRetLastInstr,
	} kind;

	union {
		IRTok LastInstr;
	};
} ExprRet;

static void mark_err(const Tok *t);
static void set_irtok_dest_addr(IRTok *t, size_t addr);
static size_t get_ident_addr(const Scope *sc, const char *name, const Tok *errpos);
static IRParam tok_to_irparam(Scope *sc, Tok *t);
static Scope make_scope(Scope *parent, bool with_idents);
static void term_scope(Scope *sc);
static bool expr_flush_ir_and_maybe_return(IRList *out_ir, TokList *toks, IRTok instr, TokListItem *expr_start, Scope *expr_scope, TokListItem *t, ExprRet *out_ret);
static void make_value_statically_allocated(Value *v);
static ExprRet expr(IRList *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t);
static void expr_into_addr(IRList *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t, size_t addr);
static IRParam expr_into_irparam(IRList *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t);
static void skip_newlns(TokList *toks, TokListItem *from);
static void stmt(IRList *out_ir, TokList *toks, Map *funcs, Scope *sc, TokListItem *t);

static void mark_err(const Tok *t) {
	err_ln = t->ln;
	err_col = t->col;
}

static void set_irtok_dest_addr(IRTok *t, size_t addr) {
	switch (t->instr) {
		case IRSet:
		case IRNeg:
		case IRNot:
		case IRAddrOf:
			t->Unary.addr = addr;
			break;
		case IRAdd:
		case IRSub:
		case IRMul:
		case IRDiv:
		case IREq:
		case IRNeq:
		case IRLt:
		case IRLe:
		case IRAnd:
		case IROr:
			t->Binary.addr = addr;
			break;
		case IRCallInternal:
			t->CallI.ret_addr = addr;
			break;
		default:
			ASSERT_UNREACHED();
	}
}

static size_t get_ident_addr(const Scope *sc, const char *name, const Tok *errpos) {
	size_t addr;
	bool exists = false;
	for (const Scope *i = sc; i != NULL; i = i->parent) {
		if (!i->has_idents)
			continue;
		exists = map_get(&i->ident_addrs, name, &addr);
		if (exists)
			break;
	}
	if (!exists) {
		mark_err(errpos);
		set_err("Identifier '%s' not recognized in this scope", name);
		return 0;
	}
	return addr;
}

static IRParam tok_to_irparam(Scope *sc, Tok *t) {
	if (t->kind == TokIdent) {
		size_t addr;
		if (t->Ident.kind == IdentName) {
			TRY_RET(addr = get_ident_addr(sc, t->Ident.Name, t), (IRParam){0});
		} else if (t->Ident.kind == IdentAddr)
			addr = t->Ident.Addr;
		else
			ASSERT_UNREACHED();
		return (IRParam){
			.kind = IRParamAddr,
			.Addr = addr,
		};
	} else if (t->kind == TokVal) {
		return (IRParam){
			.kind = IRParamLiteral,
			.Literal = t->Val,
		};
	} else
		ASSERT_UNREACHED();
}

/* term_scope doesn't have to be called if with_idents is set to false. */
static Scope make_scope(Scope *parent, bool with_idents) {
	Scope s = { .parent = parent, .mem_addr = parent ? parent->mem_addr : 0, .has_idents = with_idents };
	if (with_idents)
		map_init(&s.ident_addrs, sizeof(size_t));
	return s;
}

static void term_scope(Scope *sc) {
	if (sc->has_idents)
		map_term(&sc->ident_addrs);
}

/* If ir_tok is the underlying expr() call's last evaluation, this function
 * deletes t from toks, sets *out_ret and tells the caller it can return
 * *out_ret by returning true.
 *
 * If ir_tok is not the expression's last instruction, ir_tok is written to
 * out_ir and t is replaced by a pointer to the result's memory address.
 *  */
static bool expr_flush_ir_and_maybe_return(IRList *out_ir, TokList *toks, IRTok ir_tok, TokListItem *expr_start, Scope *expr_scope, TokListItem *t, ExprRet *out_ret) {
	if (t == expr_start && t->next->tok.kind == TokOp && op_prec[t->next->tok.Op] == PREC_DELIM) {
		/* ir_tok was the expression's last IR instruction. */

		toklist_del(toks, t, t);

		*out_ret = (ExprRet){
			.kind = ExprRetLastInstr,
			.LastInstr = ir_tok,
		};
		return true;
	} else {
		/* ir_tok was not the expression's last IR instruction. */

		size_t dest_addr = expr_scope->mem_addr++;

		set_irtok_dest_addr(&ir_tok, dest_addr);
		irlist_app(out_ir, ir_tok);

		t->tok = (Tok){
			.kind = TokIdent,
			.Ident = {
				.kind = IdentAddr,
				.Addr = dest_addr,
			},
		};
		return false;
	}
}

static void make_value_statically_allocated(Value *v) {
	switch (v->type) {
		case TypeArr: v->Arr.dynamically_allocated = false; break;
		default: break;
	}
}

/* The job of this function is to reduce the expression to the most simple form
 * writing the least IR instructions possible (without overanalyzing).
 * This means that the only IR instructions it will be writing are those for
 * calculating intermediate values.
 * In the case of ExprRetVal and ExprRetIdent, the value isn't 'returned' in
 * the traditional sense, but rather the result is left in the token stream.
 * The 'return' value can be of 3 different types:
 * - ExprRetVal: The expression yields a constant value as a result.
 *   Examples: '5', '5 + 2 * 3' or '5 + (2 + 1) * 3'
 * - ExprRetIdent: The expression yields an identifier as a result.
 *   Examples: 'a' or '(((a)))'
 * - ExprRetLastInstr: The expression is a more complex sequence of
 *   instructions. Here the last instruction is returned so the caller can
 *   manually set the destination address.
 *   Examples: 'a + 1', '2 + a * b' or '2 + 4 * (b * b) / 5'
 *
 * Here is also a simplified example of how the operator precedence parsing works:
 * ________________________________
 *  Where t points to (between l_op and r_op in each step)
 *     |
 *     v
 *     5  +  2  *  2  \n
 *  ^     ^
 *  |     |
 * l_op  r_op
 * precedence of '+' is higher than that of the front delimiter => move forward
 * ________________________________
 *     5  +  2  *  2  \n
 *        ^     ^
 *        |     |
 *       l_op  r_op
 * precedence of '*' is higher than that of '+' => move forward
 * ________________________________
 *      5  +  2  *  2  \n
 *               ^     ^
 *               |     |
 *              l_op  r_op
 * precedence of '\n' (a delimiter) is lower than that of '*' => evaluate and move l_op 2 back
 * ________________________________
 *      5  +  4  \n
 *         ^     ^
 *         |     |
 *        l_op  r_op
 * precedence of '\n' (a delimiter) is lower than that of '+' => evaluate and move l_op 2 back
 * ________________________________
 *      9  \n
 *   ^     ^
 *   |     |
 *  l_op  r_op
 *  both l_op and r_op are delimiters (their precedence is PREC_DELIM) => done
 */
static ExprRet expr(IRList *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t) {
	TokListItem *start = t;

	Scope sc = make_scope(parent_sc, false);

	for (;;) {
		/* Prepare to collapse unary operation. */
		bool perform_unary = false;
		IRInstr unary_op;
		if (t->tok.kind == TokOp) {
			if (t->tok.Op == OpSub) {
				t = t->next;
				perform_unary = true;
				unary_op = IRNeg;
			} else if (t->tok.Op == OpNot) {
				t = t->next;
				perform_unary = true;
				unary_op = IRNot;
			} else if (t->tok.Op == OpAddrOf) {
				t = t->next;
				perform_unary = true;
				unary_op = IRAddrOf;
			}
		}

		/* Delete newline if we're definitely expecting an operand. */
		if (t->tok.kind == TokOp && t->tok.Op == OpNewLn) {
			if (t == start)
				start = t->next;
			t = t->next;
			toklist_del(toks, t->prev, t->prev);
		}

		/* Collapse parentheses. */
		if (t->tok.kind == TokOp && t->tok.Op == OpLParen) {
			ExprRet r;
			TRY_RET(r = expr(out_ir, toks, funcs, &sc, t->next), (ExprRet){0});
			if (r.kind == ExprRetLastInstr) {
				size_t res_addr = sc.mem_addr++;
				set_irtok_dest_addr(&r.LastInstr, res_addr);
				irlist_app(out_ir, r.LastInstr);
				t->tok = (Tok){
					.ln = t->tok.ln,
					.col = t->tok.col,
					.kind = TokIdent,
					.Ident = {
						.kind = IdentAddr,
						.Addr = res_addr,
					},
				};
			} else if (r.kind == ExprRetVal || r.kind == ExprRetIdent) {
				t->tok = t->next->tok;
				toklist_del(toks, t->next, t->next);
			} else
				ASSERT_UNREACHED();
			toklist_del(toks, t->next, t->next);
		}

		/* Collapse function call. */
		else if (t->tok.kind == TokIdent && t->tok.Ident.kind == IdentName && t->next->tok.kind == TokOp && t->next->tok.Op == OpLParen) {
			/* get function */
			BuiltinFunc func;
			bool exists = map_get(funcs, t->tok.Ident.Name, &func);
			if (!exists) {
				mark_err(&t->tok);
				set_err("Unrecognized function: %s()", t->tok.Ident.Name);
				return (ExprRet){0};
			}
			TokListItem *func_ident = t;
			t = func_ident->next;

			/* we want to try to eliminate function calls at runtime if possible */
			bool eval_func_in_place = !func.side_effects;

			size_t args_len = 0;
			IRParam *args = NULL;

			if (t->next->tok.kind == TokOp && t->next->tok.Op == OpRParen) {
				/* no args */
				toklist_del(toks, t->next, t->next); /* delete right parenthesis */
			} else {
				/* go through the arguments, evaluate them and put them into the args array */
				size_t args_cap = 16;
				args = xmalloc(sizeof(IRParam) * args_cap);
				for (;;) {
					if (args_len+1 > args_cap)
						args = xrealloc(args, (args_cap *= 2));
					IRParam a;
					TRY_RET_ELSE(a = expr_into_irparam(out_ir, toks, funcs, &sc, t->next), (ExprRet){0}, free(args));
					args[args_len++] = a;
					if (a.kind != IRParamLiteral)
						eval_func_in_place = false;
					if (t->next->tok.kind == TokOp) {
						if (t->next->tok.Op == OpComma) {
							toklist_del(toks, t->next, t->next); /* delete right parenthesis */
							continue;
						} else if (t->next->tok.Op == OpRParen) {
							toklist_del(toks, t->next, t->next); /* delete right parenthesis */
							break;
						}
					}
					mark_err(&t->next->tok);
					set_err("Expected ',' or ')' after function argument");
					free(args);
					return (ExprRet){0};
				}
			}

			t = func_ident;
			toklist_del(toks, t->next, t->next); /* delete left parenthesis */

			if (func.kind == FuncFixedArgs && args_len != func.FixedArgs.n_args) {
				mark_err(&func_ident->tok);
				const char *plural = func.FixedArgs.n_args == 1 ? "" : "s";
				set_err("Function %s() takes %zu argument%s but got %zu", func.name, func.FixedArgs.n_args, plural, args_len);
				if (args)
					free(args);
				return (ExprRet){0};
			} else if (func.kind == FuncVarArgs && args_len < func.VarArgs.min_args) {
				mark_err(&func_ident->tok);
				const char *plural = func.VarArgs.min_args == 1 ? "" : "s";
				set_err("Function %s() requires at least %zu argument%s but only got %zu", func.name, func.VarArgs.min_args, plural, args_len);
				if (args)
					free(args);
				return (ExprRet){0};
			}

			if (eval_func_in_place) {
				/* evaluate the function in place */
				if (!func.returns)
					/* If the function had no side effects and returned nothing,
					 * that function would do absolutely nothing, which would
					 * make no sense. */
					ASSERT_UNREACHED();
				Value *arg_vals = args_len ? xmalloc(sizeof(Value) * args_len) : NULL;
				for (size_t i = 0; i < args_len; i++)
					arg_vals[i] = args[i].Literal;
				mark_err(&func_ident->tok);
				func_ident->tok = (Tok) {
					.kind = TokVal,
					.Val = func.kind == FuncVarArgs ?
						func.VarArgs.WithRet.func(args_len - func.VarArgs.min_args, arg_vals)
						: func.FixedArgs.WithRet.func(arg_vals),
				};
				/* since we have a literal return value, we want it to be fully treated like one by the memory manager */
				make_value_statically_allocated(&func_ident->tok.Val);
				/* immediately free any heap-allocated literals that are no longer needed */
				for (size_t i = 0; i < args_len; i++)
					free_value(&arg_vals[i], true);
				/* free buffers */
				if (arg_vals)
					free(arg_vals);
				if (args)
					free(args);
			} else {
				/* function call IR instruction */
				IRTok ir_tok = {
					.ln =  func_ident->tok.ln,
					.col = func_ident->tok.col,
					.instr = IRCallInternal,
					.CallI = {
						.ret_addr = 0,
						.fid = func.fid,
						.n_args = args_len,
						.args = args,
					},
				};

				/* return if we've just evaluated the last instruction */
				ExprRet ret;
				if (expr_flush_ir_and_maybe_return(out_ir, toks, ir_tok, start, &sc, func_ident, &ret))
					return ret;
			}
		}

		/* Collapse unary operation. */
		if (perform_unary) {
			Tok *v = &t->tok; /* what we want to perform the operation on */
			t = t->prev; /* go back to the '-' sign */
			toklist_del(toks, t->next, t->next); /* again, just removing the reference */

			if (v->kind == TokVal) {
				/* immediately perform operation */
				t->tok.kind = TokVal;
				mark_err(&t->tok);
				TRY_RET(t->tok.Val = eval_unary(unary_op, &v->Val), (ExprRet){0});
			} else {
				/* unary IR instruction */
				IRParam v_irparam;
				TRY_RET(v_irparam = tok_to_irparam(&sc, v), (ExprRet){0});
				IRTok ir_tok = {
					.ln = t->tok.ln,
					.col = t->tok.col,
					.instr = unary_op,
					.Unary = {
						.addr = 0,
						.val = v_irparam,
					},
				};

				/* return if we've just evaluated the last instruction */
				ExprRet ret;
				if (expr_flush_ir_and_maybe_return(out_ir, toks, ir_tok, start, &sc, t, &ret))
					return ret;
			}
		}

		/* Find out operator precedence of l_op and r_op. */
		int8_t l_op_prec;
		Tok *l_op;
		if (t == start) {
			l_op_prec = PREC_DELIM;
			l_op = NULL;
		} else {
			l_op = &t->prev->tok;
			if (l_op->kind != TokOp) {
				mark_err(l_op);
				set_err("Expected operator");
				return (ExprRet){0};
			}
			l_op_prec = op_prec[l_op->Op];
		}
		int8_t r_op_prec;
		Tok *r_op = &t->next->tok;
		if (r_op->kind != TokOp) {
			mark_err(r_op);
			set_err("Expected operator");
			return (ExprRet){0};
		}
		r_op_prec = op_prec[r_op->Op];

		/* If l_op and r_op are both delimiters, we don't have to evaluate
		 * anything. */
		if (l_op_prec == PREC_DELIM && r_op_prec == PREC_DELIM) {
			if (t->tok.kind == TokIdent) {
				return (ExprRet){ .kind = ExprRetIdent };
			} else if (t->tok.kind == TokVal) {
				return (ExprRet){ .kind = ExprRetVal };
			} else {
				mark_err(&t->tok);
				set_err("Expected literal or identifier");
				return (ExprRet){0};
			}
		}

		/* This is the operator precedence parser described above. */
		if (r_op_prec > l_op_prec)
			t = t->next->next;
		else {
			Tok *rhs = &t->tok;
			if (rhs->kind != TokVal && rhs->kind != TokIdent) {
				mark_err(rhs);
				set_err("Expected literal or identifier");
				return (ExprRet){0};
			}

			t = t->prev->prev;

			Tok *lhs = &t->tok;
			if (lhs->kind != TokVal && lhs->kind != TokIdent) {
				mark_err(lhs);
				set_err("Expected literal or identifier");
				return (ExprRet){0};
			}

			/* delete the tokens that fall away from collapsing the expression 
			 * (NOTE: only their references are deleted here, that's important
			 * because we're still using their values later on) */
			toklist_del(toks, t->next, t->next->next);

			bool swap_operands = false;

			IRInstr instr;
			switch (l_op->Op) {
				case OpAdd: instr = IRAdd; break;
				case OpSub: instr = IRSub; break;
				case OpMul: instr = IRMul; break;
				case OpDiv: instr = IRDiv; break;
				case OpEq:  instr = IREq;  break;
				case OpNeq: instr = IRNeq; break;
				case OpLt:  instr = IRLt;  break;
				case OpLe:  instr = IRLe;  break;
				case OpGt:  instr = IRLt; swap_operands = true; break;
				case OpGe:  instr = IRLe; swap_operands = true; break;
				case OpAnd: instr = IRAnd; break;
				case OpOr:  instr = IROr;  break;
				default:
					mark_err(l_op);
					set_err("Unknown operation: '%s'", op_str[l_op->Op]);
					return (ExprRet){0};
			}

			if (lhs->kind == TokVal && rhs->kind == TokVal) {
				/* evaluate the constant expression immediately */
				Value *lhs_val = swap_operands ? &rhs->Val : &lhs->Val;
				Value *rhs_val = swap_operands ? &lhs->Val : &rhs->Val;
				lhs->kind = TokVal;
				mark_err(l_op);
				TRY_RET(lhs->Val = eval_binary(instr, lhs_val, rhs_val), (ExprRet){0});
			} else {
				IRParam lhs_irparam, rhs_irparam;
				TRY_RET(lhs_irparam = tok_to_irparam(&sc, lhs), (ExprRet){0});
				TRY_RET(rhs_irparam = tok_to_irparam(&sc, rhs), (ExprRet){0});

				/* binary IR instruction */
				IRTok ir_tok = {
					.ln = l_op->ln,
					.col = l_op->col,
					.instr = instr,
					.Binary = {
						.addr = 0,
						.lhs = swap_operands ? rhs_irparam : lhs_irparam,
						.rhs = swap_operands ? lhs_irparam : rhs_irparam,
					},
				};

				/* return if we've just evaluated the last instruction */
				ExprRet ret;
				if (expr_flush_ir_and_maybe_return(out_ir, toks, ir_tok, start, &sc, t, &ret))
					return ret;
			}
		}
	}
}

static void expr_into_addr(IRList *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t, size_t addr) {
	ExprRet r;
	TRY(r = expr(out_ir, toks, funcs, parent_sc, t));
	if (r.kind == ExprRetLastInstr) {
		set_irtok_dest_addr(&r.LastInstr, addr);
		irlist_app(out_ir, r.LastInstr);
		t->tok = (Tok){
			.ln = t->tok.ln,
			.col = t->tok.col,
			.kind = TokIdent,
			.Ident = {
				.kind = IdentAddr,
				.Addr = addr,
			},
		};
	} else if (r.kind == ExprRetVal || r.kind == ExprRetIdent) {
		IRParam res;
		TRY(res = tok_to_irparam(parent_sc, &t->tok));
		irlist_app(out_ir, (IRTok){
			.ln = t->tok.ln,
			.col = t->tok.col,
			.instr = IRSet,
			.Unary = {
				.addr = addr,
				.val = res,
			},
		});
		toklist_del(toks, t, t);
	} else
		ASSERT_UNREACHED();
}

static IRParam expr_into_irparam(IRList *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t) {
	ExprRet r;
	TRY_RET(r = expr(out_ir, toks, funcs, parent_sc, t), (IRParam){0});
	if (r.kind == ExprRetLastInstr) {
		Scope sc = make_scope(parent_sc, false);
		size_t addr = sc.mem_addr++;
		set_irtok_dest_addr(&r.LastInstr, addr);
		irlist_app(out_ir, r.LastInstr);
		return (IRParam){
			.kind = IRParamAddr,
			.Addr = addr,
		};
	} else if (r.kind == ExprRetVal || r.kind == ExprRetIdent) {
		IRParam ret;
		TRY_RET(ret = tok_to_irparam(parent_sc, &t->tok), (IRParam){0});
		toklist_del(toks, t, t);
		return ret;
	} else
		ASSERT_UNREACHED();
}

/* This WILL invalidate *from, so the caller should only call it on a
 * TokListItem after any ones that are in use (e.g. skip_newlns(t->next)). */
static void skip_newlns(TokList *toks, TokListItem *from) {
	TokListItem *curr = from;
	while (curr->tok.kind == TokOp && curr->tok.Op == OpNewLn)
		curr = curr->next;
	if (curr != from)
		toklist_del(toks, from, curr->prev);
}

static void stmt(IRList *out_ir, TokList *toks, Map *funcs, Scope *sc, TokListItem *t) {
	TokListItem *start = t;
	if (t->tok.kind == TokIdent && t->tok.Ident.kind == IdentName && (t->next->tok.kind == TokDeclare || t->next->tok.kind == TokAssign)) {
		char *name = t->tok.Ident.Name;
		t = t->next;
		if (t->tok.kind == TokDeclare) {
			size_t addr = sc->mem_addr++;
			TRY(expr_into_addr(out_ir, toks, funcs, sc, t->next, addr));
			bool replaced = map_insert(&sc->ident_addrs, name, &addr);
			if (replaced) {
				mark_err(&start->tok);
				set_err("'%s' already declared in this scope", name);
				return;
			}
		} else if (t->tok.kind == TokAssign) {
			size_t addr;
			TRY(addr = get_ident_addr(sc, name, &start->tok));
			TRY(expr_into_addr(out_ir, toks, funcs, sc, t->next, addr));
		} else
			ASSERT_UNREACHED();
	} else if (t->tok.kind == TokOp && t->tok.Op == OpLCurl) {
		Scope inner_sc = make_scope(sc, true);
		for (;;) {
			skip_newlns(toks, t->next);
			if (t->next->tok.kind == TokOp) {
				if (t->next->tok.Op == OpEOF) {
					term_scope(&inner_sc);
					mark_err(&start->tok);
					set_err("Unclosed '{'");
					return;
				}
				if (t->next->tok.Op == OpRCurl)
					break;
			}
			TRY_ELSE(stmt(out_ir, toks, funcs, &inner_sc, t->next), term_scope(&inner_sc));
		}
		term_scope(&inner_sc);
		t = t->next;
	} else if (t->tok.kind == TokWhile) {
		/* How while is generally implemented in IR: 
		 * 0: jmp to 3
		 * 1: some_code
		 * 2: some_code
		 * 3: some stuff evaluating condition xyz
		 * 4: jmp to 1 if condition xyz is met
		 * */

		/* add initial jmp instruction */
		irlist_app(out_ir, (IRTok){
			.ln = t->tok.ln,
			.col = t->tok.col,
			.instr = IRJmp,
			.Jmp = {
				.iaddr = 0, /* unknown for now */
			},
		});
		IRItem *jmp_instr = out_ir->end;
		size_t body_iaddr = out_ir->len;

		/* parse condition */
		IRList cond_ir;
		irlist_init_short(&cond_ir);
		IRParam cond;
		TRY_ELSE(cond = expr_into_irparam(&cond_ir, toks, funcs, sc, t->next), irlist_term(&cond_ir));

		/* parse loop body */
		skip_newlns(toks, t->next);
		TRY_ELSE(stmt(out_ir, toks, funcs, sc, t->next), irlist_term(&cond_ir));

		/* finally we know where the jmp from the beginning has to jump to */
		jmp_instr->tok.Jmp.iaddr = out_ir->len;

		/* append condition IR to program IR, then terminate condition IR stream */
		irlist_eat_irlist(out_ir, &cond_ir);

		/* add conditional jump */
		irlist_app(out_ir, (IRTok){
			.ln = t->next->tok.ln,
			.col = t->next->tok.col,
			.instr = IRJnz,
			.CJmp = {
				.iaddr = body_iaddr,
				.condition = cond,
			},
		});

		t = t->next;
	} else if (t->tok.kind == TokIf) {
		/* How if is generally implemented in IR: 
		 * 0: some stuff evaluating condition xyz
		 * 1: jmp to 5 if condition xyz is met
		 * 2: some_code in else
		 * 4: jmp to 6
		 * 5: some_code in if
		 * */

		/* parse condition */
		IRParam cond;
		TRY(cond = expr_into_irparam(out_ir, toks, funcs, sc, t->next));

		/* add conditional jmp instruction */
		irlist_app(out_ir, (IRTok){
			.ln = t->tok.ln,
			.col = t->tok.col,
			.instr = IRJnz,
			.CJmp = {
				.iaddr = 0, /* unknown for now */
				.condition = cond,
			},
		});
		IRItem *if_cjmp_instr = out_ir->end;

		/* parse if body */
		skip_newlns(toks, t->next);
		IRList if_body;
		irlist_init_short(&if_body);
		TRY_ELSE(stmt(&if_body, toks, funcs, sc, t->next), irlist_term(&if_body));

		skip_newlns(toks, t->next);
		if (t->next->tok.kind == TokElse) {
			toklist_del(toks, t->next, t->next);

			/* parse and add else body */
			skip_newlns(toks, t->next);
			TRY_ELSE(stmt(out_ir, toks, funcs, sc, t->next), irlist_term(&if_body));
		}

		/* add jmp instruction to jump back to common code */
		irlist_app(out_ir, (IRTok){
			.ln = t->tok.ln,
			.col = t->tok.col,
			.instr = IRJmp,
			.Jmp = {
				.iaddr = 0, /* unknown for now */
			},
		});
		IRItem *else_jmp_instr = out_ir->end;

		/* set if condition jmp target */
		if_cjmp_instr->tok.CJmp.iaddr = out_ir->len;
		
		/* add if body */
		irlist_eat_irlist(out_ir, &if_body);

		/* set else jmp target */
		else_jmp_instr->tok.CJmp.iaddr = out_ir->len;
	} else {
		/* assume expression */
		TRY(expr_into_irparam(out_ir, toks, funcs, sc, t));
		return;
	}
	toklist_del(toks, start, t);
}

IRList parse(TokList *toks, BuiltinFunc *builtin_funcs, size_t n_builtin_funcs) {
	Map funcs;
	map_init(&funcs, sizeof(BuiltinFunc));
	for (size_t i = 0; i < n_builtin_funcs; i++) {
		builtin_funcs[i].fid = i;
		bool replaced = map_insert(&funcs, builtin_funcs[i].name, &builtin_funcs[i]);
		if (replaced) {
			err_ln = 0; err_col = 0;
			set_err("Builtin function %s() declared more than once", builtin_funcs[i].name);
			map_term(&funcs);
			return (IRList){0};
		}
	}

	IRList ir;
	irlist_init_long(&ir);
	Scope global_scope = make_scope(NULL, true);
	for (;;) {
		skip_newlns(toks, toks->begin);
		if (toks->begin->tok.kind == TokOp && toks->begin->tok.Op == OpEOF)
			break;
		TRY_RET_ELSE(stmt(&ir, toks, &funcs, &global_scope, toks->begin), ir,
				{ term_scope(&global_scope); map_term(&funcs); });
	}
	term_scope(&global_scope);
	map_term(&funcs);
	return ir;
}
