#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ir.h"
#include "lex.h"
#include "parse.h"
#include "util.h"
#include "vm.h"

static void usage(const char *prgname);
static void die(const char *fmt, ...);

static void usage(const char *prgname) {
	fprintf(stderr, "Usage:\n"
			"  %s [OPTIONS] <FILENAME>\n"
			"Options:\n"
			"  -emit-tokens\n"
			"  -emit-ir\n"
			"  -dry             --  don't execute the script (just process it)\n"
			, prgname);
}

static void die(const char *fmt, ...) {
	fprintf(stderr, C_IRED "Error: " C_RESET);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	exit(1);
}

static Value fn_put(Value *args) {
	print_value(&args[0], true);
	return (Value){0};
}

static Value fn_print(Value *args) {
	fn_put(args);
	printf("\n");
	return (Value){0};
}

static Value fn_int(Value *args) {
	Value ret = {
		.type.kind = TypeInt,
		.Int = 0,
	};
	switch (args[0].type.kind) {
		case TypeVoid: break;
		case TypeFloat: ret.Int = (ssize_t)args[0].Float; break;
		case TypeInt:   ret.Int = args[0].Int;            break;
		default: ASSERT_UNREACHED();
	}
	return ret;
}

static Value fn_float(Value *args) {
	Value ret = {
		.type.kind = TypeFloat,
		.Float = 0.0,
	};
	switch (args[0].type.kind) {
		case TypeVoid: break;
		case TypeFloat: ret.Float = args[0].Float;       break;
		case TypeInt:   ret.Float = (double)args[0].Int; break;
		default: ASSERT_UNREACHED();
	}
	return ret;
}

static Value fn_pow(Value *args) {
	if (!(args[0].type.kind == TypeFloat && args[1].type.kind == TypeFloat)) {
		set_err("pow() requires arguments of type float");
		return (Value){0};
	}
	return (Value){
		.type.kind = TypeFloat,
		.Float = pow(args[0].Float, args[1].Float),
	};
}

static Value fn_sleep(Value *args) {
	if (!(args[0].type.kind == TypeFloat && args[0].Float >= 0.0)) {
		set_err("sleep() requires a positive float");
		return (Value){0};
	}
	sleep_secs(args[0].Float);
	return (Value){0};
}

int main(int argc, const char **argv) {
	/* parse arguments */
	size_t nargs = argc - 1;
	const char *prgname = argv[0];
	const char **args = argv + 1;
	bool opt_emit_tokens = false;
	bool opt_emit_ir = false;
	bool opt_dry = false;
	const char *filename = NULL;
	for (size_t i = 0; i < nargs; i++) {
		if (args[i][0] == '-') {
			if (streq(args[i], "-h") || streq(args[i], "-help") || streq(args[i], "--help")) {
				usage(prgname);
				return 0;
			} else if (streq(args[i], "-emit-ir"))
				opt_emit_ir = true;
			else if (streq(args[i], "-emit-tokens"))
				opt_emit_tokens = true;
			else if (streq(args[i], "-dry"))
				opt_dry = true;
			else {
				die("Unknown option: %s\n", args[i]);
			}
		} else {
			if (filename) {
				die("Filename already set to '%s'\n", filename);
			}
			filename = args[i];
		}
	}
	if (!filename) {
		die("Please specify a filename\n");
	}
	/* read source file */
	FILE *fp = fopen(filename, "r");
	if (!fp) {
		die("Failed to open '%s': %s\n", filename, strerror(errno));
	}
	char *file = mreadfile(fp);
	if (!file) {
		fclose(fp);
		die("Failed to read '%s': %s\n", filename, strerror(errno));
	}
	fclose(fp);
	/* lex source file */
	Pool *static_vars = pool_new(4096);
	TokList tokens = lex(file, static_vars);
	if (err) {
		toklist_term(&tokens);
		pool_term(static_vars);
		free(file);
		fprintf(stderr, C_IRED "Lexer error" C_RESET " in " C_CYAN "%s" C_RESET ":%zu:%zu: %s\n", filename, err_ln, err_col, errbuf);
		return 1;
	}
	free(file);
	if (opt_emit_tokens)
		print_toks(&tokens);
	/* parse tokens into IR code */
	BuiltinFunc funcs[] = {
		{ .name = "put",   .side_effects = true,  .n_args = 1, .func = fn_put,   },
		{ .name = "print", .side_effects = true,  .n_args = 1, .func = fn_print, },
		{ .name = "int",   .side_effects = false, .n_args = 1, .func = fn_int,   },
		{ .name = "float", .side_effects = false, .n_args = 1, .func = fn_float, },
		{ .name = "pow",   .side_effects = false, .n_args = 2, .func = fn_pow,   },
		{ .name = "sleep", .side_effects = true,  .n_args = 1, .func = fn_sleep, },
	};
	IRToks ir = parse(&tokens, funcs, sizeof(funcs) / sizeof(funcs[0]));
	if (err) {
		irtoks_term(&ir);
		toklist_term(&tokens);
		pool_term(static_vars);
		fprintf(stderr, C_IRED "Parser error" C_RESET " in " C_CYAN "%s" C_RESET ":%zu:%zu: %s\n", filename, err_ln, err_col, errbuf);
		return 1;
	}
	toklist_term(&tokens);
	optimize_ir(&ir);
	if (opt_emit_ir)
		print_ir(&ir, funcs);
	/* run the IR */
	if (!opt_dry) {
		run(&ir, funcs);
		if (err) {
			irtoks_term(&ir);
			pool_term(static_vars);
			fprintf(stderr, C_IRED "Runtime error" C_RESET " in " C_CYAN "%s" C_RESET ":%zu:%zu: %s\n", filename, err_ln, err_col, errbuf);
			return 1;
		}
	}
	irtoks_term(&ir);
	pool_term(static_vars);
}
