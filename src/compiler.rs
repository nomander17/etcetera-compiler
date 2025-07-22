use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FloatValue, IntValue, PointerValue};

use crate::{
    ast::{Expression, Program, Statement},
    token::Token,
};

#[derive(Debug, PartialEq, Clone, Copy)]
enum TypeInfo {
    Int,
    Float,
    Bool,
}

impl<'ctx> From<IntValue<'ctx>> for TypedValue<'ctx> {
    fn from(value: IntValue<'ctx>) -> Self {
        TypedValue::Int(value)
    }
}

impl<'ctx> From<FloatValue<'ctx>> for TypedValue<'ctx> {
    fn from(value: FloatValue<'ctx>) -> Self {
        TypedValue::Float(value)
    }
}

#[derive(Debug, Clone, Copy)]
enum TypedValue<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
    Bool(IntValue<'ctx>), // boolean is represented as i1 in LLVM
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,                                          // core
    builder: Builder<'ctx>,                                          // build llvm instructions
    module: Module<'ctx>, // container; holds global vars and funs
    variables: Vec<HashMap<String, (PointerValue<'ctx>, TypeInfo)>>, // variable scope
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("etc_module");
        Compiler {
            context,
            builder,
            module,
            variables: vec![HashMap::new()],
        }
    }

    fn new_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.variables.pop();
    }

    fn get_variable(&self, name: &str) -> Option<&(PointerValue<'ctx>, TypeInfo)> {
        for scope in self.variables.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }

    fn insert_variable(&mut self, name: String, value: PointerValue<'ctx>, ty: TypeInfo) {
        if let Some(scope) = self.variables.last_mut() {
            let _ = scope.insert(name, (value, ty));
        };
    }

    pub fn compile(&mut self, program: Program) -> Result<String, String> {
        let main_fn_type = self.context.i32_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);

        for statement in program.statements {
            self.compile_statement(statement)?;
        }

        // return 0
        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)))
            .map_err(|e| e.to_string())?;
        Ok(self.module.print_to_string().to_string())
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<(), String> {
        match statement {
            Statement::Let(name, expr) => self.compile_let_statement(name, expr),
            Statement::Print(expr) => self.compile_print_statement(expr),
            Statement::If(condition, consequence, alternative) => {
                self.compile_if_statement(condition, consequence, alternative)
            }
            Statement::Loop(var_name, start, end, body) => {
                self.compile_loop_statement(var_name, start, end, body)
            }
            Statement::Assignment(name, expr) => self.compile_assignment_statement(name, expr),
        }
    }

    fn compile_let_statement(&mut self, name: String, expr: Expression) -> Result<(), String> {
        let (value, ty) = self.compile_expression(expr)?;
        // allocate memory on stack for variable
        let alloca = self.create_alloca_for_type(ty, &name)?;

        // store value in allocated space
        self.store_typed_value(&value, alloca)?;

        // add reference to it in our symbol table
        self.insert_variable(name, alloca, ty);
        Ok(())
    }

    fn compile_assignment_statement(
        &mut self,
        name: String,
        expr: Expression,
    ) -> Result<(), String> {
        let (value, ty) = self.compile_expression(expr)?;

        if let Some(&(var_ptr, var_ty)) = self.get_variable(&name) {
            // variable exists, update it
            // TODO promote type
            if var_ty != ty {
                return Err(format!(
                    "Type mismtach on assignment to '{}': {:?} vs {:?}",
                    name, var_ty, ty
                ));
            }
            self.store_typed_value(&value, var_ptr)?;
        } else {
            // variable doesn't exist, create new one
            let alloca = self.create_alloca_for_type(ty, &name)?;
            self.store_typed_value(&value, alloca)?;
            self.insert_variable(name, alloca, ty);
        }
        Ok(())
    }

    fn create_alloca_for_type(
        &self,
        ty: TypeInfo,
        name: &str,
    ) -> Result<PointerValue<'ctx>, String> {
        let alloca = match ty {
            TypeInfo::Int => self.builder.build_alloca(self.context.i32_type(), name),
            TypeInfo::Float => self.builder.build_alloca(self.context.f64_type(), name),
            TypeInfo::Bool => self.builder.build_alloca(self.context.bool_type(), name),
        };
        alloca.map_err(|e| e.to_string())
    }

    fn store_typed_value(
        &self,
        value: &TypedValue<'ctx>,
        ptr: PointerValue<'ctx>,
    ) -> Result<(), String> {
        let basic_value: BasicValueEnum = match value {
            TypedValue::Int(v) => BasicValueEnum::IntValue(*v),
            TypedValue::Float(v) => BasicValueEnum::FloatValue(*v),
            TypedValue::Bool(v) => BasicValueEnum::IntValue(*v),
        };
        self.builder
            .build_store(ptr, basic_value)
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    fn promote_to_common_type(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypedValue<'ctx>), String> {
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                Ok((TypedValue::Int(l), TypedValue::Int(r)))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                Ok((TypedValue::Float(l), TypedValue::Float(r)))
            }
            (TypedValue::Int(l), TypedValue::Float(r)) => {
                let l_promoted = self
                    .builder
                    .build_signed_int_to_float(l, self.context.f64_type(), "l_promoted")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Float(l_promoted), TypedValue::Float(r)))
            }
            (TypedValue::Float(l), TypedValue::Int(r)) => {
                let r_promoted = self
                    .builder
                    .build_signed_int_to_float(r, self.context.f64_type(), "r_promoted")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Float(l), TypedValue::Float(r_promoted)))
            }
            _ => Err("Type promotion not implemented for given types".into()),
        }
    }

    fn build_add(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let sum = self
                    .builder
                    .build_int_add(l, r, "sum")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Int(sum), TypeInfo::Int))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let sum = self
                    .builder
                    .build_float_add(l, r, "sum")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Float(sum), TypeInfo::Float))
            }
            _ => Err("Invalid operands for '+'".into()),
        }
    }

    fn build_sub(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let diff = self
                    .builder
                    .build_int_sub(l, r, "diff")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Int(diff), TypeInfo::Int))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let diff = self
                    .builder
                    .build_float_sub(l, r, "diff")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Float(diff), TypeInfo::Float))
            }
            _ => Err("Invalid operands for '-'".into()),
        }
    }

    fn build_mul(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let prod = self
                    .builder
                    .build_int_mul(l, r, "prod")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Int(prod), TypeInfo::Int))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let prod = self
                    .builder
                    .build_float_mul(l, r, "prod")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Float(prod), TypeInfo::Float))
            }
            _ => Err("Invalid operands for '*'".into()),
        }
    }

    fn build_div(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                // convert integers to floats for division to get proper decimal results
                let l_float = self
                    .builder
                    .build_signed_int_to_float(l, self.context.f64_type(), "l_div_float")
                    .map_err(|e| e.to_string())?;
                let r_float = self
                    .builder
                    .build_signed_int_to_float(r, self.context.f64_type(), "r_div_float")
                    .map_err(|e| e.to_string())?;
                let quot = self
                    .builder
                    .build_float_div(l_float, r_float, "quot")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Float(quot), TypeInfo::Float))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let quot = self
                    .builder
                    .build_float_div(l, r, "quot")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Float(quot), TypeInfo::Float))
            }
            _ => Err("Division not supported for given operands".into()),
        }
    }

    fn build_floor_div(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                // floor division is truncation for now
                let quot = self
                    .builder
                    .build_int_signed_div(l, r, "floor_div")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Int(quot), TypeInfo::Int))
            }
            _ => Err("Floor division only supported for integers".into()),
        }
    }

    fn build_modulo(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let rem = self
                    .builder
                    .build_int_signed_rem(l, r, "rem")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Int(rem), TypeInfo::Int))
            }
            _ => Err("Modulo only supported for integers".into()),
        }
    }

    fn build_gt(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SGT, l, r, "gt_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OGT, l, r, "gt_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            _ => Err("Invalid operands for '>'".into()),
        }
    }

    fn build_gte(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SGE, l, r, "gte_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OGE, l, r, "gte_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            _ => Err("Invalid operands for '>='".into()),
        }
    }

    fn build_lt(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, l, r, "lt_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OLT, l, r, "lt_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            _ => Err("Invalid operands for '<'".into()),
        }
    }

    fn build_lte(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLE, l, r, "lte_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OLE, l, r, "lte_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            _ => Err("Invalid operands for '<='".into()),
        }
    }

    fn build_eq(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::EQ, l, r, "eq_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OEQ, l, r, "eq_cmp")
                    .map_err(|e| e.to_string())?;
                Ok((TypedValue::Bool(cmp), TypeInfo::Bool))
            }
            _ => Err("Invalid operands for '=='".into()),
        }
    }

    fn build_and(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let lhs_bool = self.convert_to_bool(lhs)?;
        let rhs_bool = self.convert_to_bool(rhs)?;
        let result = self
            .builder
            .build_and(lhs_bool, rhs_bool, "and")
            .map_err(|e| e.to_string())?;
        Ok((TypedValue::Bool(result), TypeInfo::Bool))
    }

    fn build_or(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        let lhs_bool = self.convert_to_bool(lhs)?;
        let rhs_bool = self.convert_to_bool(rhs)?;
        let result = self
            .builder
            .build_or(lhs_bool, rhs_bool, "or")
            .map_err(|e| e.to_string())?;
        Ok((TypedValue::Bool(result), TypeInfo::Bool))
    }

    fn convert_to_bool(&self, value: TypedValue<'ctx>) -> Result<IntValue<'ctx>, String> {
        match value {
            TypedValue::Bool(b) => Ok(b),
            TypedValue::Int(i) => {
                // Non-zero is true
                let zero = self.context.i32_type().const_int(0, false);
                self.builder
                    .build_int_compare(inkwell::IntPredicate::NE, i, zero, "to_bool")
                    .map_err(|e| e.to_string())
            }
            TypedValue::Float(f) => {
                // Non-zero is true
                let zero = self.context.f64_type().const_float(0.0);
                self.builder
                    .build_float_compare(inkwell::FloatPredicate::ONE, f, zero, "to_bool")
                    .map_err(|e| e.to_string())
            }
        }
    }

    fn compile_expression(
        &mut self,
        expr: Expression,
    ) -> Result<(TypedValue<'ctx>, TypeInfo), String> {
        match expr {
            Expression::IntLiteral(val) => {
                let value = self.context.i32_type().const_int(val as u64, false);
                Ok((TypedValue::Int(value), TypeInfo::Int))
            }
            Expression::FloatLiteral(val) => {
                let value = self.context.f64_type().const_float(val);
                Ok((TypedValue::Float(value), TypeInfo::Float))
            }
            Expression::BoolLiteral(val) => {
                let value = self.context.bool_type().const_int(val as u64, false);
                Ok((TypedValue::Bool(value), TypeInfo::Bool))
            }
            Expression::Identifer(name) => {
                let actual_name = if name.starts_with('$') {
                    &name[1..]
                } else {
                    &name
                };

                let &(ptr, var_ty) = self
                    .get_variable(actual_name)
                    .ok_or(format!("Unknown variable: {}", actual_name))?;

                let loaded = match var_ty {
                    TypeInfo::Int => {
                        let val = self
                            .builder
                            .build_load(self.context.i32_type(), ptr, actual_name)
                            .map_err(|e| e.to_string())?;
                        TypedValue::Int(val.into_int_value())
                    }
                    TypeInfo::Float => {
                        let val = self
                            .builder
                            .build_load(self.context.f64_type(), ptr, actual_name)
                            .map_err(|e| e.to_string())?;
                        TypedValue::Float(val.into_float_value())
                    }
                    TypeInfo::Bool => {
                        let val = self
                            .builder
                            .build_load(self.context.bool_type(), ptr, actual_name)
                            .map_err(|e| e.to_string())?;
                        TypedValue::Bool(val.into_int_value())
                    }
                };

                Ok((loaded, var_ty))
            }

            Expression::Infix(left, op, right) => {
                // recursively compile left and right side
                let (lhs, _) = self.compile_expression(*left)?;
                let (rhs, _) = self.compile_expression(*right)?;

                // dispatch table using match - Match for type and operation
                // let the old method handle types for now
                // TODO: update dispatcher for with TypeInfo
                match op {
                    Token::Plus => self.build_add(lhs, rhs),
                    Token::Minus => self.build_sub(lhs, rhs),
                    Token::Multiply => self.build_mul(lhs, rhs),
                    Token::Divide => self.build_div(lhs, rhs),
                    Token::FloorDivide => self.build_floor_div(lhs, rhs),
                    Token::Modulo => self.build_modulo(lhs, rhs),
                    Token::GreaterThan => self.build_gt(lhs, rhs),
                    Token::GreaterThanOrEqual => self.build_gte(lhs, rhs),
                    Token::LessThan => self.build_lt(lhs, rhs),
                    Token::LessThanOrEqual => self.build_lte(lhs, rhs),
                    Token::Equal => self.build_eq(lhs, rhs),
                    Token::And => self.build_and(lhs, rhs),
                    Token::Or => self.build_or(lhs, rhs),
                    _ => Err(format!("Operator {:?} not implemented", op)),
                }
            }
            _ => Err("Unsupported expression type".into()),
        }
    }

    fn get_printf(&self) -> inkwell::values::FunctionValue<'ctx> {
        if let Some(function) = self.module.get_function("printf") {
            return function;
        }

        // declare printf for linker
        let i32_type = self.context.i32_type();
        let str_type = self.context.ptr_type(inkwell::AddressSpace::from(0));
        // printf returns an i32, C-style
        let printf_type = i32_type.fn_type(&[str_type.into()], true);
        self.module.add_function(
            "printf",
            printf_type,
            Some(inkwell::module::Linkage::External),
        )
    }

    fn compile_print_statement(&mut self, expr: Expression) -> Result<(), String> {
        let (val, ty) = self.compile_expression(expr)?;
        let printf = self.get_printf();

        let (format_str, value): (&str, inkwell::values::BasicValueEnum) = match (ty, val) {
            (TypeInfo::Int, TypedValue::Int(val)) => ("%d\n", val.into()),
            (TypeInfo::Float, TypedValue::Float(val)) => ("%f\n", val.into()),
            (TypeInfo::Bool, TypedValue::Bool(val)) => ("%d\n", val.into()),
            _ => unreachable!(
                "Type and value type mismatch in print statement. How did we get here?"
            ),
        };

        let format_str = self
            .builder
            .build_global_string_ptr(format_str, "format_str")
            .map_err(|e| e.to_string())?;
        let format_str_ptr = format_str.as_pointer_value();
        // call printf with formatted string
        self.builder
            .build_call(
                printf,
                &[format_str_ptr.into(), value.into()],
                "printf_call",
            )
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    fn compile_if_statement(
        &mut self,
        condition: Expression,
        consequence: Vec<Statement>,
        alternative: Option<Vec<Statement>>,
    ) -> Result<(), String> {
        let (cond, ty) = self.compile_expression(condition)?;
        let condition: IntValue = match (ty, cond) {
            (TypeInfo::Int, TypedValue::Int(val)) => val,
            (TypeInfo::Float, TypedValue::Float(val)) => {
                let zero = self.context.f64_type().const_float(0.0);
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::ONE, val, zero, "float_cond")
                    .map_err(|e| e.to_string())?;
                cmp
            }
            (TypeInfo::Bool, TypedValue::Bool(val)) => val,
            _ => unreachable!("Type and value type mismatch in if statement! How did we get here?"),
        };

        // get current function
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let then_block = self.context.append_basic_block(function, "then");
        let else_block = self.context.append_basic_block(function, "else");
        let merge_block = self.context.append_basic_block(function, "ifcont"); // where it will return to

        // the main if then else
        self.builder
            .build_conditional_branch(condition, then_block, else_block)
            .map_err(|e| e.to_string())?;

        // generate code for then block
        self.builder.position_at_end(then_block);
        self.new_scope();
        for statement in consequence {
            self.compile_statement(statement)?;
        }
        self.end_scope();
        // jump back
        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(|e| e.to_string())?;

        // generate code for else, if it exists
        self.builder.position_at_end(else_block);
        if let Some(alt) = alternative {
            self.new_scope();
            for statement in alt {
                self.compile_statement(statement)?;
            }
            self.end_scope();
        }
        // jump back and continue from there
        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(|e| e.to_string())?;
        self.builder.position_at_end(merge_block);

        Ok(())
    }

    fn compile_loop_statement(
        &mut self,
        var_name: String,
        start: Expression,
        end: Expression,
        body: Vec<Statement>,
    ) -> Result<(), String> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        // loop var should only be int by design
        let alloca = self.create_alloca_for_type(TypeInfo::Int, &var_name)?;

        let (start_val, start_ty) = self.compile_expression(start)?;
        if start_ty != TypeInfo::Int {
            return Err(format!(
                "Loop variable must be an integer but got type {:?} instead.",
                start_ty
            ));
        }
        let (end_val, end_ty) = self.compile_expression(end)?;
        if end_ty != TypeInfo::Int {
            return Err(format!(
                "Loop variable must be an integer but got type {:?} instead.",
                end_ty
            ));
        }

        let start_val: IntValue = match start_val {
            TypedValue::Int(val) => val,
            _ => {
                return Err("Internal compiler error.".to_string());
            }
        };
        let end_val: IntValue = match end_val {
            TypedValue::Int(val) => val,
            _ => {
                return Err("Internal compiler error.".to_string());
            }
        };

        // init loop var
        self.builder
            .build_store(alloca, start_val)
            .map_err(|e| e.to_string())?;
        // create loop block and jump to it
        let loop_block = self.context.append_basic_block(function, "loop");
        self.builder
            .build_unconditional_branch(loop_block)
            .map_err(|e| e.to_string())?;
        self.builder.position_at_end(loop_block);

        // loop scope
        self.new_scope();
        self.insert_variable(var_name.clone(), alloca, TypeInfo::Int);

        for statement in body {
            self.compile_statement(statement)?;
        }
        self.end_scope();

        // compute next_var and update loop var
        let i32_type = self.context.i32_type();
        let curr_val = self
            .builder
            .build_load(i32_type, alloca, &var_name)
            .map_err(|e| e.to_string())?
            .into_int_value();
        let next_val = self
            .builder
            .build_int_add(curr_val, i32_type.const_int(1, false), "nextvar")
            .map_err(|e| e.to_string())?;
        self.builder
            .build_store(alloca, next_val)
            .map_err(|e| e.to_string())?;

        // condtional branch with the loop condition curr_val <= end
        // loop are INCLUSIVE of end_val so 1 to 3  = 1, 2, 3
        let loop_cond = self
            .builder
            .build_int_compare(inkwell::IntPredicate::SLT, curr_val, end_val, "loopcond")
            .map_err(|e| e.to_string())?;
        let after_block = self.context.append_basic_block(function, "afterloop");
        self.builder
            .build_conditional_branch(loop_cond, loop_block, after_block)
            .map_err(|e| e.to_string())?;

        // return to original position
        self.builder.position_at_end(after_block);
        Ok(())
    }
}
