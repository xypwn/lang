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
static ExprRet expr(IRToks *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t);
static void expr_into_addr(IRToks *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t, size_t addr);
static IRParam expr_into_irparam(IRToks *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t);
static void stmt(IRToks *out_ir, TokList *toks, Map *funcs, Scope *sc, TokListItem *t);

static void mark_err(const Tok *t) {
	err_ln = t->ln;
	err_col = t->col;
}

static void set_irtok_dest_addr(IRTok *t, size_t addr) {
	switch (t->instr) {
		case IRSet:
		case IRNeg:
			t->Unary.addr = addr;
			break;
		case IRAdd:
		case IRSub:
		case IRMul:
		case IRDiv:
			t->Arith.addr = addr;
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
static ExprRet expr(IRToks *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t) {
	TokListItem *start = t;

	Scope sc = make_scope(parent_sc, false);

	for (;;) {
		/* Prepare to collapse negative factor. */
		bool negate = false;
		if (t->tok.kind == TokOp && t->tok.Op == OpSub) {
			t = t->next;
			negate = true;
		}

		/* Collapse parentheses. */
		if (t->tok.kind == TokOp && t->tok.Op == OpLParen) {
			ExprRet r;
			TRY_RET(r = expr(out_ir, toks, funcs, &sc, t->next), (ExprRet){0});
			if (r.kind == ExprRetLastInstr) {
				size_t res_addr = sc.mem_addr++;
				set_irtok_dest_addr(&r.LastInstr, res_addr);
				irtoks_app(out_ir, r.LastInstr);
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
			size_t args_cap = 16;
			IRParam *args = xmalloc(sizeof(IRParam) * args_cap);
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
						toklist_del(toks, t->next, t->next);
						continue;
					} else if (t->next->tok.Op == OpRParen) {
						toklist_del(toks, t->next, t->next);
						break;
					}
				}
				mark_err(&t->next->tok);
				set_err("Expected ',' or ')' after function argument");
				free(args);
				return (ExprRet){0};
			}

			t = func_ident;
			toklist_del(toks, t->next, t->next); /* delete left parenthesis */

			if (func.n_args != args_len) {
				mark_err(&func_ident->tok);
				const char *plural = func.n_args == 1 ? "" : "s";
				set_err("Function %s() takes %zu argument%s but got %zu", func.name, func.n_args, plural, args_len);
				free(args);
				return (ExprRet){0};
			}

			if (eval_func_in_place) {
				/* evaluate the function in place */
				Value *arg_vals = xmalloc(sizeof(Value) * args_len);
				for (size_t i = 0; i < args_len; i++)
					arg_vals[i] = args[i].Literal;
				func_ident->tok = (Tok) {
					.kind = TokVal,
					.Val = func.func(arg_vals),
				};
				free(arg_vals);
				free(args);
			} else {
				bool is_last_operation = t == start && t->next->tok.kind == TokOp && op_prec[t->next->tok.Op] == PREC_DELIM;

				size_t res_addr = is_last_operation ? 0 : sc.mem_addr++;

				/* function call instruction */
				IRTok ir = {
					.ln =  func_ident->tok.ln,
					.col = func_ident->tok.col,
					.instr = IRCallInternal,
					.CallI = {
						.ret_addr = res_addr,
						.fid = func.fid,
						.args = args,
					},
				};

				if (is_last_operation) {
					/* done */
					toklist_del(toks, t, t);
					return (ExprRet){ .kind = ExprRetLastInstr, .LastInstr = ir };
				} else {
					/* write IR instruction */
					irtoks_app(out_ir, ir);

					/* leave new memory address as result */
					t->tok = (Tok){
						.kind = TokIdent,
						.Ident = {
							.kind = IdentAddr,
							.Addr = res_addr,
						},
					};
				}
			}
		}

		/* Collapse negative factor. */
		if (negate) {
			Tok *v = &t->tok; /* what we want to negate */
			t = t->prev; /* go back to the '-' sign */
			toklist_del(toks, t->next, t->next); /* again, just removing the reference */

			bool is_last_operation = t == start && t->next->tok.kind == TokOp && op_prec[t->next->tok.Op] == PREC_DELIM;

			if (v->kind == TokVal) {
				/* immediately negate value */
				t->tok.kind = TokVal;
				t->tok.Val = eval_unary(IRNeg, &v->Val);
			} else {
				size_t res_addr = is_last_operation ? 0 : sc.mem_addr++;

				/* Instruction to negate. */
				IRParam v_irparam;
				TRY_RET(v_irparam = tok_to_irparam(&sc, v), (ExprRet){0});
				IRTok ir = {
					.ln = t->tok.ln,
					.col = t->tok.col,
					.instr = IRNeg,
					.Unary = {
						.addr = res_addr,
						.val = v_irparam,
					},
				};

				if (is_last_operation) {
					/* done */
					toklist_del(toks, t, t);
					return (ExprRet){ .kind = ExprRetLastInstr, .LastInstr = ir };
				} else {
					/* write IR instruction */
					irtoks_app(out_ir, ir);

					/* leave new memory address as result */
					t->tok.kind = TokIdent;
					t->tok.Ident = (Identifier){
						.kind = IdentAddr,
						.Addr = res_addr,
					};
				}
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

			IRInstr instr;
			switch (l_op->Op) {
				case OpAdd: instr = IRAdd; break;
				case OpSub: instr = IRSub; break;
				case OpMul: instr = IRMul; break;
				case OpDiv: instr = IRDiv; break;
				default:
					mark_err(l_op);
					set_err("Unknown operation: '%s'", op_str[l_op->Op]);
					return (ExprRet){0};
			}

			if (lhs->kind == TokVal && rhs->kind == TokVal) {
				/* evaluate the constant expression immediately */
				lhs->kind = TokVal;
				TRY_RET(lhs->Val = eval_arith(instr, &lhs->Val, &rhs->Val), (ExprRet){0});
			} else {
				bool is_last_operation = t == start && r_op_prec == PREC_DELIM;

				IRParam lhs_irparam, rhs_irparam;
				TRY_RET(lhs_irparam = tok_to_irparam(&sc, lhs), (ExprRet){0});
				TRY_RET(rhs_irparam = tok_to_irparam(&sc, rhs), (ExprRet){0});

				size_t res_addr = is_last_operation ? 0 : sc.mem_addr++;

				IRTok ir = {
					.ln = l_op->ln,
					.col = l_op->col,
					.instr = instr,
					.Arith = {
						.addr = res_addr,
						.lhs = lhs_irparam,
						.rhs = rhs_irparam,
					},
				};
				
				if (is_last_operation) {
					/* done */
					toklist_del(toks, t, t);
					return (ExprRet){ .kind = ExprRetLastInstr, .LastInstr = ir };
				} else {
					/* write IR instruction */
					irtoks_app(out_ir, ir);

					/* leave new memory address as result */
					lhs->kind = TokIdent;
					lhs->Ident = (Identifier){
						.kind = IdentAddr,
						.Addr = res_addr,
					};
				}
			}
		}
	}
}

static void expr_into_addr(IRToks *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t, size_t addr) {
	ExprRet r;
	TRY(r = expr(out_ir, toks, funcs, parent_sc, t));
	if (r.kind == ExprRetLastInstr) {
		set_irtok_dest_addr(&r.LastInstr, addr);
		irtoks_app(out_ir, r.LastInstr);
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
		irtoks_app(out_ir, (IRTok){
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

static IRParam expr_into_irparam(IRToks *out_ir, TokList *toks, Map *funcs, Scope *parent_sc, TokListItem *t) {
	ExprRet r;
	TRY_RET(r = expr(out_ir, toks, funcs, parent_sc, t), (IRParam){0});
	if (r.kind == ExprRetLastInstr) {
		Scope sc = make_scope(parent_sc, false);
		size_t addr = sc.mem_addr++;
		set_irtok_dest_addr(&r.LastInstr, addr);
		irtoks_app(out_ir, r.LastInstr);
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

static void stmt(IRToks *out_ir, TokList *toks, Map *funcs, Scope *sc, TokListItem *t) {
	TokListItem *start = t;
	if (t->tok.kind == TokIdent && t->tok.Ident.kind == IdentName && (t->next->tok.kind == TokDeclare || t->next->tok.kind == TokAssign)) {
		char *name = t->tok.Ident.Name;
		t = t->next;
		if (t->tok.kind == TokDeclare) {
			size_t addr = sc->mem_addr++;
			bool replaced = map_insert(&sc->ident_addrs, name, &addr);
			if (replaced) {
				mark_err(&start->tok);
				set_err("'%s' already declared in this scope", name);
				return;
			}
			TRY(expr_into_addr(out_ir, toks, funcs, sc, t->next, addr));
		} else if (t->tok.kind == TokAssign) {
			size_t addr;
			TRY(addr = get_ident_addr(sc, name, &start->tok));
			TRY(expr_into_addr(out_ir, toks, funcs, sc, t->next, addr));
		} else
			ASSERT_UNREACHED();
	} else if (t->tok.kind == TokOp && t->tok.Op == OpLCurl) {
		Scope inner_sc = make_scope(sc, true);
		for (;;) {
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
		size_t jmp_instr_iaddr = out_ir->len;
		irtoks_app(out_ir, (IRTok){
			.ln = t->tok.ln,
			.col = t->tok.col,
			.instr = IRJmp,
			.Jmp = {
				.iaddr = 0, /* unknown for now */
			},
		});

		/* parse condition */
		IRToks cond_ir;
		irtoks_init_short(&cond_ir);
		IRParam cond;
		TRY_ELSE(cond = expr_into_irparam(&cond_ir, toks, funcs, sc, t->next), irtoks_term(&cond_ir));

		/* add conditional jump */
		irtoks_app(&cond_ir, (IRTok){
			.ln = t->next->tok.ln,
			.col = t->next->tok.col,
			.instr = IRJnz,
			.CJmp = {
				.iaddr = jmp_instr_iaddr + 1,
				.condition = cond,
			},
		});

		/* parse loop body */
		TRY_ELSE(stmt(out_ir, toks, funcs, sc, t->next), irtoks_term(&cond_ir));

		/* finally we know where the jmp from the beginning has to jump to */
		out_ir->toks[jmp_instr_iaddr].Jmp.iaddr = out_ir->len;

		/* append condition IR to program IR, then terminate condition IR stream */
		irtoks_app_irtoks(out_ir, &cond_ir);
		irtoks_term(&cond_ir);

		t = t->next;
	} else if (t->tok.kind == TokOp && t->tok.Op == OpNewLn) {
	} else {
		/* assume expression */
		TRY(expr_into_irparam(out_ir, toks, funcs, sc, t));
		return;
	}
	toklist_del(toks, start, t);
}

IRToks parse(TokList *toks, BuiltinFunc *builtin_funcs, size_t n_builtin_funcs) {
	Map funcs;
	map_init(&funcs, sizeof(BuiltinFunc));
	for (size_t i = 0; i < n_builtin_funcs; i++) {
		builtin_funcs[i].fid = i;
		bool replaced = map_insert(&funcs, builtin_funcs[i].name, &builtin_funcs[i]);
		if (replaced) {
			err_ln = 0; err_col = 0;
			set_err("Builtin function %s() declared more than once", builtin_funcs[i].name);
			map_term(&funcs);
			return (IRToks){0};
		}
	}

	IRToks ir;
	irtoks_init_long(&ir);
	Scope global_scope = make_scope(NULL, true);
	for (;;) {
		if (toks->begin->tok.kind == TokOp && toks->begin->tok.Op == OpEOF)
			break;
		TRY_RET_ELSE(stmt(&ir, toks, &funcs, &global_scope, toks->begin), ir,
				{ term_scope(&global_scope); map_term(&funcs); });
	}
	term_scope(&global_scope);
	map_term(&funcs);
	return ir;
}
