#include "runtime.h"

#include "util.h"

Value eval_arith(IRInstr instr, const Value *lhs, const Value *rhs) {
	switch (instr) {
		case IRAdd:
		case IRSub:
		case IRMul:
		case IRDiv: {
			if (lhs->type.kind == TypeInt && rhs->type.kind == TypeInt) {
				ssize_t res;
				switch (instr) {
					case IRAdd: res = lhs->Int + rhs->Int; break;
					case IRSub: res = lhs->Int - rhs->Int; break;
					case IRMul: res = lhs->Int * rhs->Int; break;
					case IRDiv: res = lhs->Int / rhs->Int; break;
					default: ASSERT_UNREACHED();
				}
				return (Value){
					.type.kind = TypeInt,
					.Int = res,
				};
			} else if (lhs->type.kind == TypeFloat && rhs->type.kind == TypeFloat) {
				float res;
				switch (instr) {
					case IRAdd: res = lhs->Float + rhs->Float; break;
					case IRSub: res = lhs->Float - rhs->Float; break;
					case IRMul: res = lhs->Float * rhs->Float; break;
					case IRDiv: res = lhs->Float / rhs->Float; break;
					default: ASSERT_UNREACHED();
				}
				return (Value){
					.type.kind = TypeFloat,
					.Float = res,
				};
			} else {
				set_err("Unsupported types for operation '%s'", irinstr_str[instr]);
				return (Value){0};
			}
		}
		default:
			ASSERT_UNREACHED();
	}
	return (Value){0};
}

Value zero_val(Type ty) {
	Value ret;
	ret.type = ty;
	switch (ty.kind) {
		case TypeInt:   ret.Int   = 0;   break;
		case TypeFloat: ret.Float = 0.0; break;
		default: ASSERT_UNREACHED();
	}
	return ret;
}
