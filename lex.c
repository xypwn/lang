#include "lex.h"

#include "util.h"

typedef struct Pos {
	size_t ln, col;     /* current position */
	size_t m_ln, m_col; /* marked position */
} Pos;

static void consume(Pos *p, char c);
static void emit(TokList *toks, const Pos *p, Tok t);
static void mark(Pos *p);
static void mark_err(const Pos *p);

static void consume(Pos *p, char c) {
	if (c == '\n') {
		p->ln++;
		p->col = 1;
	} else
		p->col++;
}

static void emit(TokList *toks, const Pos *p, Tok t) {
	t.ln  = p->m_ln;
	t.col = p->m_col;
	toklist_append(toks, t);
}

static void mark(Pos *p) {
	p->m_ln  = p->ln;
	p->m_col = p->col;
}

static void mark_err(const Pos *p) {
	err_ln  = p->m_ln;
	err_col = p->m_col;
}

TokList lex(const char *s) {
	TokList toks;
	toklist_init(&toks);
	Pos pos = { .ln = 1, .col = 1 };
	for (;;) {
		mark(&pos);
		mark_err(&pos);

		if (IS_ALPHA(s[0])) {
			size_t i = 1;
			const char *start = s;
			consume(&pos, *(s++));
			while (IS_ALNUM(s[0])) {
				consume(&pos, *(s++));
				i++;
			}
			if (streq_0_n("if", start, i))
				emit(&toks, &pos, (Tok){ .kind = TokIf });
			else if (streq_0_n("while", start, i))
				emit(&toks, &pos, (Tok){ .kind = TokWhile });
			else {
				emit(&toks, &pos, (Tok){
						.kind = TokIdent,
						.Ident = {
							.kind = IdentName,
							.Name = psndup(toks.p, start, i),
						},
					});
			}
			continue;
		}

		if (IS_NUM(s[0]) || s[0] == '.') {
			const char *start = s;
			size_t base = 10;
			bool num_end = false;
			bool is_float = false;
			if (s[0] == '0') {
				consume(&pos, *(s++));
				if (s[0] == 'x' || s[0] == 'X') {
					base = 16;
					consume(&pos, *(s++));
					start = s;
				} else if (s[0] == 'b' || s[0] == 'B') {
					base = 2;
					consume(&pos, *(s++));
					start = s;
				} else if (!IS_NUM(s[0]) && s[0] != '.')
					num_end = true;
			}
			if (!num_end) {
				for (;;) {
					if (s[0] == '.') {
						if (is_float) {
							mark(&pos);
							mark_err(&pos);
							set_err("Too many decimal points in number");
							return toks;
						}
						if (base != 10) {
							set_err("Only decimal floats are supported");
							return toks;
						}
						is_float = true;
					} else if (!IS_ALNUM(s[0]))
						break;
					consume(&pos, *(s++));
				}
			}

			if (is_float) {
				ssize_t endpos;
				double num = stod(start, s - start, &endpos);
				if (endpos != -1) {
					err_col += endpos;
					set_err("Invalid decimal float character: '%c'", start[endpos]);
					return toks;
				}

				emit(&toks, &pos, (Tok){
						.kind = TokVal,
						.Val = {
							.type = {
								.kind = TypeFloat,
							},
							.Float = num,
						},
					});
			} else {
				ssize_t endpos;
				intmax_t num = stoimax(start, s - start, base, &endpos);
				if (endpos != -1) {
					err_col += endpos;
					set_err("Invalid base %zu numerical character: '%c'", base, start[endpos]);
					return toks;
				}

				emit(&toks, &pos, (Tok){
						.kind = TokVal,
						.Val = {
							.type = {
								.kind = TypeInt,
							},
							.Int = num,
						},
					});
			}
			continue;
		}

		switch (s[0]) {
			case 0:
				goto end_of_file;
			case ' ':
			case '\t':
				break;
			case '\n':
				emit(&toks, &pos, (Tok){
						.kind = TokOp,
						.Op = OpNewLn,
					});
				break;
			case ':':
				consume(&pos, *(s++));
				if (s[0] == '=') {
					emit(&toks, &pos, (Tok){ .kind = TokDeclare });
				} else {
					set_err("Expected ':='");
					return toks;
				}
				break;
			case '=':
				emit(&toks, &pos, (Tok){ .kind = TokAssign });
				break;
			case '{':
			case '}':
			case '(':
			case ')':
			case ',':
			case '+':
			case '-':
			case '*':
				emit(&toks, &pos, (Tok){
						.kind = TokOp,
						.Op = s[0],
					});
				break;
			case '/':
				consume(&pos, *(s++));
				if (s[0] == '/') {
					consume(&pos, *(s++));
					while (s[0] != '\n') {
						if (s[0] == 0)
							goto end_of_file;
						consume(&pos, *(s++));
					}
				} else if (s[0] == '*') {
					size_t depth = 1;
					while (depth) {
						consume(&pos, *(s++));
						if (s[0] == '/') {
							consume(&pos, *(s++));
							if (s[0] == '*')
								depth++;
						} else if (s[0] == '*') {
							consume(&pos, *(s++));
							if (s[0] == '/')
								depth--;
						} else if (s[0] == 0) {
							set_err("Unclosed comment");
							return toks;
						}
					}
					consume(&pos, *(s++));
				} else {
					emit(&toks, &pos, (Tok){
							.kind = TokOp,
							.Op = '/',
						});
				}
				continue;
			default:
				set_err("Unrecognized character: '%c'", s[0]);
				return toks;
		}
		consume(&pos, *(s++));
	}
end_of_file:
	emit(&toks, &pos, (Tok){
			.kind = TokOp,
			.Op = OpEOF,
		});
	return toks;
}