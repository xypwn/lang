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
static char get_esc_char(char c);

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

static char get_esc_char(char c) {
	switch(c) {
		case 'a':  return '\a';
		case 'b':  return '\b';
		case 'e':  return '\033';
		case 'f':  return '\f';
		case 'n':  return '\n';
		case 'r':  return '\r';
		case 't':  return '\t';
		case 'v':  return '\v';
		case '\\': return '\\';
		case '\'': return '\'';
		case '"':  return '\"';
		default:   return 0;
	}
}

TokList lex(const char *s, Pool *static_vars) {
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
			else if (streq_0_n("else", start, i))
				emit(&toks, &pos, (Tok){ .kind = TokElse });
			else if (streq_0_n("while", start, i))
				emit(&toks, &pos, (Tok){ .kind = TokWhile });
			else if (streq_0_n("true", start, i))
				emit(&toks, &pos, (Tok){ .kind = TokVal, .Val = { .type = { .kind = TypeBool, }, .Bool = true, }, });
			else if (streq_0_n("false", start, i))
				emit(&toks, &pos, (Tok){ .kind = TokVal, .Val = { .type = { .kind = TypeBool, }, .Bool = false, }, });
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
				if (s[0] == '=')
					emit(&toks, &pos, (Tok){ .kind = TokDeclare });
				else {
					set_err("Expected ':='");
					return toks;
				}
				break;
			case '=':
				consume(&pos, *(s++));
				if (s[0] == '=')
					emit(&toks, &pos, (Tok){ .kind = TokOp, .Op = OpEq });
				else {
					emit(&toks, &pos, (Tok){ .kind = TokAssign });
					continue;
				}
				break;
			case '<':
				consume(&pos, *(s++));
				if (s[0] == '=')
					emit(&toks, &pos, (Tok){ .kind = TokOp, .Op = OpLe });
				else {
					emit(&toks, &pos, (Tok){ .kind = TokOp, .Op = OpLt });
					continue;
				}
				break;
			case '>':
				consume(&pos, *(s++));
				if (s[0] == '=')
					emit(&toks, &pos, (Tok){ .kind = TokOp, .Op = OpGe });
				else {
					emit(&toks, &pos, (Tok){ .kind = TokOp, .Op = OpGt });
					continue;
				}
				break;
			case '&':
				consume(&pos, *(s++));
				if (s[0] == '&') {
					emit(&toks, &pos, (Tok){ .kind = TokOp, .Op = OpAnd });
				} else
					continue;
				break;
			case '|':
				consume(&pos, *(s++));
				if (s[0] == '|') {
					emit(&toks, &pos, (Tok){ .kind = TokOp, .Op = OpOr });
				} else
					continue;
				break;
			case '{':
			case '}':
			case '(':
			case ')':
			case ',':
			case '+':
			case '-':
			case '*':
			case '!':
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
			case '\'': {
				consume(&pos, *(s++));
				char c = s[0];
				if (c == '\\') {
					consume(&pos, *(s++));
					c = get_esc_char(s[0]);
					if (!c) {
						set_err("Unrecognized escape sequence: '\\%c'", c);
						return toks;
					}
				}
				consume(&pos, *(s++));
				if (s[0] != '\'') {
					set_err("Unclosed char literal");
					return toks;
				}
				emit(&toks, &pos, (Tok){ .kind = TokVal, .Val = { .type = { .kind = TypeChar, }, .Char = c, }, });
				break;
			}
			case '"': {
				consume(&pos, *(s++));
				const char *start = s;
				Pos start_pos = pos;
				size_t size = 0;

				/* count the string size before allocating */
				while (s[0] != '"') {
					if (!s[0]) {
						set_err("Unexpected EOF in string literal");
						return toks;
					} else if (s[0] == '\\')
						consume(&pos, *(s++));
					consume(&pos, *(s++));
					size++;
				}

				/* go through the actual string */
				s = start;
				pos = start_pos;
				char *str = pool_alloc(static_vars, type_size[TypeChar] * size);
				for (size_t i = 0; i < size; i++) {
					char c = s[0];
					if (c == '\\') {
						consume(&pos, *(s++));
						c = get_esc_char(s[0]);
						if (!c) {
							set_err("Unrecognized escape sequence: '\\%c'", c);
							return toks;
						}
					}
					consume(&pos, *(s++));
					str[i] = c;
				}
				emit(&toks, &pos, (Tok){ .kind = TokVal, .Val = {
						.type.kind = TypeArr,
						.Arr = {
							.is_string = true,
							.type.kind = TypeChar,
							.vals = str,
							.len = size,
							.cap = size,
						},
					},});
				break;
			}
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
