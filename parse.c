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

typedef struct ExprMode {
	bool ignore_newln;

	enum {
		ExprModeJustCollapse, /* should leave either a literal or an own address as result */
		ExprModeStorageAddr,  /* should use the supplied storage address in any case; should leave no token behind */
	} kind;

	union {
		size_t StorageAddr;
	};
} ExprMode;

static void mark_err(const Tok *t);
static size_t get_ident_addr(const Scope *sc, const char *name, const Tok *errpos);
static IRParam tok_to_irparam(Scope *sc, Tok *t);
static Scope make_scope(Scope *parent, bool with_idents);
static void term_scope(Scope *sc);
static void expr(IRToks *out_ir, TokList *toks, Scope *parent_sc, TokListItem *t, ExprMode mode);
static void stmt(IRToks *out_ir, TokList *toks, Scope *sc, TokListItem *t);

static void mark_err(const Tok *t) {
	err_ln = t->ln;
	err_col = t->col;
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

static void expr(IRToks *out_ir, TokList *toks, Scope *parent_sc, TokListItem *t, ExprMode mode) {
	/* A simplified example of how the operator precedence parsing works:
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

	TokListItem *start = t;

	/* Each expression and subexpression has its own scope. */
	Scope sc = make_scope(parent_sc, false);

	for (;;) {
		/* Prepare to collapse negative factor. */
		bool negate = false;
		if (t->tok.kind == TokOp && t->tok.Op == OpSub) {
			t = t->next;
			negate = true;
		}

		/* Ignore newlines if told to do so. */
		if (mode.ignore_newln && t->next->tok.kind == TokOp && t->next->tok.Op == OpNewLn)
			toklist_del(toks, t->next, t->next);

		/* Collapse negative factor. */
		if (negate) {
			bool is_last_operation = t->prev == start && t->next->tok.kind == TokOp && op_prec[t->next->tok.Op] == PREC_DELIM;
			Tok *v = &t->tok;
			t = t->prev;
			toklist_del(toks, t->next, t->next);

			if (v->kind == TokVal) {
				/* immediately negate value */
				t->tok.kind = TokVal;
				t->tok.Val.type.kind = v->Val.type.kind;
				switch (v->Val.type.kind) {
					case TypeInt:   t->tok.Val.Int   = -v->Val.Int;   break;
					case TypeFloat: t->tok.Val.Float = -v->Val.Float; break;
					default: ASSERT_UNREACHED();
				}
			} else {
				/* use the predefined storage address if it was requested and we're on the last operation */
				size_t res_addr;
				if (mode.kind == ExprModeStorageAddr && is_last_operation)
					res_addr = mode.StorageAddr;
				else
					res_addr = sc.mem_addr++;

				/* add IR instruction to negate the value */
				IRParam v_irparam;
				TRY(v_irparam = tok_to_irparam(&sc, v));
				irtoks_app(out_ir, (IRTok){
					.ln = t->tok.ln,
					.col = t->tok.col,
					.instr = IRNeg,
					.Unary = {
						.addr = res_addr,
						.val = v_irparam,
					},
				});

				if (mode.kind == ExprModeStorageAddr && is_last_operation)  {
					/* done */
					toklist_del(toks, t, t);
					return;
				} else {
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
				return;
			}
			l_op_prec = op_prec[l_op->Op];
		}
		int8_t r_op_prec;
		Tok *r_op = &t->next->tok;
		if (r_op->kind != TokOp) {
			mark_err(r_op);
			set_err("Expected operator");
			return;
		}
		r_op_prec = op_prec[r_op->Op];

		/* If l_op and r_op are both delimiters, the expression is fully evaluated.
		 * NOTE: Sometimes, we don't reach this point because the function already
		 * exits directly after the last operation. */
		if (l_op_prec == PREC_DELIM && r_op_prec == PREC_DELIM) {
			if (t->tok.kind != TokVal && t->tok.kind != TokIdent) {
				mark_err(&t->tok);
				set_err("Expected literal or identifier");
				return;
			}
			if (mode.kind == ExprModeStorageAddr) {
				IRParam res;
				TRY(res = tok_to_irparam(&sc, &t->tok));
				irtoks_app(out_ir, (IRTok){
					.ln = t->tok.ln,
					.col = t->tok.col,
					.instr = IRSet,
					.Unary = {
						.addr = mode.StorageAddr,
						.val = res,
					},
				});
				toklist_del(toks, t, t);
			}
			return;
		}

		bool is_last_operation = t->prev && t->prev->prev == start && r_op_prec == PREC_DELIM;

		/* This is the actual operator precedence parser as described above. */
		if (r_op_prec > l_op_prec)
			t = t->next->next;
		else {
			/* some basic checks */
			Tok *rhs = &t->tok;
			if (rhs->kind != TokVal && rhs->kind != TokIdent) {
				mark_err(rhs);
				set_err("Expected literal or identifier");
				return;
			}
			t = t->prev->prev;
			Tok *lhs = &t->tok;
			if (lhs->kind != TokVal && lhs->kind != TokIdent) {
				mark_err(lhs);
				set_err("Expected literal or identifier");
				return;
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
					return;
			}
			if (lhs->kind == TokVal && rhs->kind == TokVal) {
				/* evaluate the constant expression immediately */
				lhs->kind = TokVal;
				TRY(lhs->Val = eval_arith(instr, &lhs->Val, &rhs->Val));
			} else {
				IRParam lhs_irparam, rhs_irparam;
				TRY(lhs_irparam = tok_to_irparam(&sc, lhs));
				TRY(rhs_irparam = tok_to_irparam(&sc, rhs));

				/* use the predefined storage address if it was requested and we're on the last operation */
				size_t res_addr;
				if (mode.kind == ExprModeStorageAddr && is_last_operation)
					res_addr = mode.StorageAddr;
				else
					res_addr = sc.mem_addr++;

				/* emit IR code to evaluate the non-constant expression */
				irtoks_app(out_ir, (IRTok){
					.ln = l_op->ln,
					.col = l_op->col,
					.instr = instr,
					.Arith = {
						.addr = res_addr,
						.lhs = lhs_irparam,
						.rhs = rhs_irparam,
					},
				});

				if (mode.kind == ExprModeStorageAddr && is_last_operation) {
					/* done */
					toklist_del(toks, t, t);
					break;
				} else {
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

static void stmt(IRToks *out_ir, TokList *toks, Scope *sc, TokListItem *t) {
	TokListItem *start = t;
	if (t->tok.kind == TokIdent && t->tok.Ident.kind == IdentName) {
		char *name = t->tok.Ident.Name;
		t = t->next;
		if (t->tok.kind == TokDeclare) {
			t = t->next;
			size_t addr = sc->mem_addr++;
			bool replaced = map_insert(&sc->ident_addrs, name, &addr);
			if (replaced) {
				mark_err(&start->tok);
				set_err("'%s' already declared in this scope", name);
				return;
			}
			TRY(expr(out_ir, toks, sc, t, (ExprMode){ .kind = ExprModeStorageAddr, .ignore_newln = false, .StorageAddr = addr }));
		} else if (t->tok.kind == TokAssign) {
			t = t->next;
			size_t addr;
			TRY(addr = get_ident_addr(sc, name, &start->tok));
			TRY(expr(out_ir, toks, sc, t, (ExprMode){ .kind = ExprModeStorageAddr, .ignore_newln = false, .StorageAddr = addr }));
		}
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
			TRY_ELSE(stmt(out_ir, toks, &inner_sc, t->next), term_scope(&inner_sc));
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

		t = t->next;

		/* parse condition */
		IRToks cond_ir;
		irtoks_init_short(&cond_ir);
		TRY_ELSE(expr(&cond_ir, toks, sc, t, (ExprMode){ .kind = ExprModeJustCollapse, .ignore_newln = false }), irtoks_term(&cond_ir));
		IRParam cond_irparam;
		TRY_ELSE(cond_irparam = tok_to_irparam(sc, &t->tok), irtoks_term(&cond_ir));
		/* add conditional jump */
		irtoks_app(&cond_ir, (IRTok){
			.ln = t->tok.ln,
			.col = t->tok.col,
			.instr = IRJnz,
			.CJmp = {
				.iaddr = jmp_instr_iaddr + 1,
				.condition = cond_irparam,
			},
		});

		t = t->next;

		/* parse loop body */
		TRY_ELSE(stmt(out_ir, toks, sc, t), irtoks_term(&cond_ir));

		/* finally we know where the jmp from the beginning has to jump to */
		out_ir->toks[jmp_instr_iaddr].Jmp.iaddr = out_ir->len;

		/* append condition IR to program IR, then terminate condition IR stream */
		irtoks_app_irtoks(out_ir, &cond_ir);
		irtoks_term(&cond_ir);
	}
	toklist_del(toks, start, t);
}

IRToks parse(TokList *toks) {
	IRToks ir;
	irtoks_init_long(&ir);
	Scope global_scope = make_scope(NULL, true);
	for (;;) {
		if (toks->begin->tok.kind == TokOp && toks->begin->tok.Op == OpEOF)
			break;
		TRY_RET_ELSE(stmt(&ir, toks, &global_scope, toks->begin), ir, term_scope(&global_scope));
	}
	term_scope(&global_scope);
	return ir;
}
