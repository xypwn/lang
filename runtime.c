#include "runtime.h"

#include "util.h"

Value eval_binary(IRInstr instr, const Value *lhs, const Value *rhs) {
	switch (instr) {
		case IRAdd:
		case IRSub:
		case IRMul:
		case IRDiv: {
			if (lhs->type == TypeInt && rhs->type == TypeInt) {
				ssize_t res;
				switch (instr) {
					case IRAdd: res = lhs->Int + rhs->Int; break;
					case IRSub: res = lhs->Int - rhs->Int; break;
					case IRMul: res = lhs->Int * rhs->Int; break;
					case IRDiv: res = lhs->Int / rhs->Int; break;
					default: ASSERT_UNREACHED();
				}
				return (Value){
					.type = TypeInt,
					.Int = res,
				};
			} else if (lhs->type == TypeFloat && rhs->type == TypeFloat) {
				double res;
				switch (instr) {
					case IRAdd: res = lhs->Float + rhs->Float; break;
					case IRSub: res = lhs->Float - rhs->Float; break;
					case IRMul: res = lhs->Float * rhs->Float; break;
					case IRDiv: res = lhs->Float / rhs->Float; break;
					default: ASSERT_UNREACHED();
				}
				return (Value){
					.type = TypeFloat,
					.Float = res,
				};
			} else {
				set_err("Unsupported types for operation '%s': %s and %s", irinstr_str[instr], type_str[lhs->type], type_str[rhs->type]);
				return (Value){0};
			}
		}
		case IREq:
		case IRNeq:
		case IRLt:
		case IRLe: {
			bool res;
			if (lhs->type == TypeInt && rhs->type == TypeInt) {
				switch (instr) {
					case IREq:  res = lhs->Int == rhs->Int; break;
					case IRNeq: res = lhs->Int != rhs->Int; break;
					case IRLt:  res = lhs->Int <  rhs->Int; break;
					case IRLe:  res = lhs->Int <= rhs->Int; break;
					default: ASSERT_UNREACHED();
				};
			} else if (lhs->type == TypeFloat && rhs->type == TypeFloat) {
				switch (instr) {
					case IREq:  res = lhs->Float == rhs->Float; break;
					case IRNeq: res = lhs->Float != rhs->Float; break;
					case IRLt:  res = lhs->Float <  rhs->Float; break;
					case IRLe:  res = lhs->Float <= rhs->Float; break;
					default: ASSERT_UNREACHED();
				};
			} else if (lhs->type == TypeArr && lhs->Arr.type == TypeChar && lhs->Arr.is_string &&
					rhs->type == TypeArr && rhs->Arr.type == TypeChar && rhs->Arr.is_string) {
				switch (instr) {
					case IREq:
						res = lhs->Arr.len == rhs->Arr.len ? strncmp(lhs->Arr.vals, rhs->Arr.vals, lhs->Arr.len) == 0 : false;
						break;
					case IRNeq:
						res = lhs->Arr.len == rhs->Arr.len ? strncmp(lhs->Arr.vals, rhs->Arr.vals, lhs->Arr.len) != 0 : true;
						break;
					default:
						set_err("String operation '%s' not supported", irinstr_str[instr]);
						break;
				};
			} else {
				set_err("Unsupported types for operation '%s': %s and %s", irinstr_str[instr], type_str[lhs->type], type_str[rhs->type]);
				return (Value){0};
			}
			return (Value){
				.type = TypeBool,
				.Bool = res,
			};
		}
		case IRAnd:
			return (Value){
				.type = TypeBool,
				.Bool = is_nonzero(lhs) && is_nonzero(rhs),
			};
			break;
		case IROr:
			return (Value){
				.type = TypeBool,
				.Bool = is_nonzero(lhs) || is_nonzero(rhs),
			};
			break;
		default:
			ASSERT_UNREACHED();
	}
	return (Value){0};
}

Value eval_unary(IRInstr instr, const Value *v) {
	switch (instr) {
		case IRSet:
			return *v;
		case IRNeg:
			if (v->type == TypeInt)
				return (Value){ .type = TypeInt, .Int = -v->Int };
			else if (v->type == TypeFloat)
				return (Value){ .type = TypeFloat, .Float = -v->Float };
			else {
				set_err("Unsupported type for operation '%s': %s", irinstr_str[instr], type_str[v->type]);
				return (Value){0};
			}
		case IRNot:
			if (v->type == TypeBool) {
				return (Value){ .type = TypeBool, .Bool = !v->Bool };
			} else {
				set_err("Unsupported type for operation '%s': %s", irinstr_str[instr], type_str[v->type]);
				return (Value){0};
			}
		case IRAddrOf:
			set_err("Unable to take the address of a literal");
			return (Value){0};
		default:
			ASSERT_UNREACHED();
	}
}

bool is_nonzero(const Value *v) {
	switch (v->type) {
		case TypeInt:   return v->Int     != 0;
		case TypeFloat: return v->Float   != 0.0;
		case TypeBool:  return v->Bool;
		case TypeChar:  return v->Char    != 0;
		case TypePtr:   return v->Ptr.val != NULL;
		case TypeArr:   return v->Arr.len != 0;
		default: ASSERT_UNREACHED();
	}
}

Value zero_val(Type ty) {
	Value ret;
	ret.type = ty;
	switch (ty) {
		case TypeInt:   ret.Int   = 0;     break;
		case TypeFloat: ret.Float = 0.0;   break;
		case TypeBool:  ret.Bool  = false; break;
		default: ASSERT_UNREACHED();
	}
	return ret;
}
