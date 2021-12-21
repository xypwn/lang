#ifndef __RUNTIME_H__
#define __RUNTIME_H__

#include "ir.h"

Value eval_arith(IRInstr instr, const Value *lhs, const Value *rhs);
Value zero_val(Type ty);

#endif /* RUNTIME_H */
