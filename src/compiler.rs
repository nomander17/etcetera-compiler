use std::collections::HashMap;

use inkwell::basic_block;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FloatValue, IntValue, PointerValue};

use crate::{
    ast::{Expression, Program, Statement},
    token::Token,
};

#[derive(Debug, Clone)]
enum TypedValue<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
    Bool(IntValue<'ctx>), // boolean is represented as i1 in LLVM
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,                              // core
    builder: Builder<'ctx>,                              // build llvm instructions
    module: Module<'ctx>,                                // container; holds global vars and funs
    variables: Vec<HashMap<String, PointerValue<'ctx>>>, // variable scope
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

    fn get_variable(&self, name: &str) -> Option<&PointerValue<'ctx>> {
        for scope in self.variables.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }

    fn insert_variable(&mut self, name: String, value: PointerValue<'ctx>) {
        self.variables.last_mut().unwrap().insert(name, value);
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
        let value = self.compile_expression(expr)?;
        // allocate memory on stack for variable
        let alloca = self.create_alloca_for_typed_value(&value, &name)?;

        // store value in allocated space
        self.store_typed_value(&value, alloca)?;

        // add reference to it in our symbol table
        self.insert_variable(name, alloca);
        Ok(())
    }

    fn compile_assignment_statement(
        &mut self,
        name: String,
        expr: Expression,
    ) -> Result<(), String> {
        let value = self.compile_expression(expr)?;

        if let Some(&var_ptr) = self.get_variable(&name) {
            // Variable exists, update it
            self.store_typed_value(&value, var_ptr)?;
        } else {
            // Variable doesn't exist, create new one
            let alloca = self.create_alloca_for_typed_value(&value, &name)?;
            self.store_typed_value(&value, alloca)?;
            self.insert_variable(name, alloca);
        }
        Ok(())
    }

    /// Helper function to create alloca based on TypedValue
    fn create_alloca_for_typed_value(
        &self,
        value: &TypedValue<'ctx>,
        name: &str,
    ) -> Result<PointerValue<'ctx>, String> {
        let alloca = match value {
            TypedValue::Int(_) => self.builder.build_alloca(self.context.i32_type(), name),
            TypedValue::Float(_) => self.builder.build_alloca(self.context.f64_type(), name),
            TypedValue::Bool(_) => self.builder.build_alloca(self.context.bool_type(), name),
        };
        alloca.map_err(|e| e.to_string())
    }

    /// Helper function to store TypedValue into allocated memory
    fn store_typed_value(
        &self,
        value: &TypedValue<'ctx>,
        ptr: PointerValue<'ctx>,
    ) -> Result<(), String> {
        let basic_value = match value {
            TypedValue::Int(v) => (*v).into(),
            TypedValue::Float(v) => (*v).into(),
            TypedValue::Bool(v) => (*v).into(),
        };
        self.builder
            .build_store(ptr, basic_value)
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    // Type promotion helper
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

    // Arithmetic operations
    fn build_add(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let sum = self
                    .builder
                    .build_int_add(l, r, "sum")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Int(sum))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let sum = self
                    .builder
                    .build_float_add(l, r, "sum")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Float(sum))
            }
            _ => Err("Invalid operands for '+'".into()),
        }
    }

    fn build_sub(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let diff = self
                    .builder
                    .build_int_sub(l, r, "diff")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Int(diff))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let diff = self
                    .builder
                    .build_float_sub(l, r, "diff")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Float(diff))
            }
            _ => Err("Invalid operands for '-'".into()),
        }
    }

    fn build_mul(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let prod = self
                    .builder
                    .build_int_mul(l, r, "prod")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Int(prod))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let prod = self
                    .builder
                    .build_float_mul(l, r, "prod")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Float(prod))
            }
            _ => Err("Invalid operands for '*'".into()),
        }
    }

    fn build_div(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                // Convert integers to floats for division to get proper decimal results
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
                Ok(TypedValue::Float(quot))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let quot = self
                    .builder
                    .build_float_div(l, r, "quot")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Float(quot))
            }
            _ => Err("Division not supported for given operands".into()),
        }
    }

    fn build_floor_div(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                // floor division is truncation for now
                let quot = self
                    .builder
                    .build_int_signed_div(l, r, "floor_div")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Int(quot))
            }
            _ => Err("Floor division only supported for integers".into()),
        }
    }

    fn build_modulo(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let rem = self
                    .builder
                    .build_int_signed_rem(l, r, "rem")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Int(rem))
            }
            _ => Err("Modulo only supported for integers".into()),
        }
    }

    // Comparison operations
    fn build_gt(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SGT, l, r, "gt_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OGT, l, r, "gt_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            _ => Err("Invalid operands for '>'".into()),
        }
    }

    fn build_gte(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SGE, l, r, "gte_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OGE, l, r, "gte_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            _ => Err("Invalid operands for '>='".into()),
        }
    }

    fn build_lt(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, l, r, "lt_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OLT, l, r, "lt_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            _ => Err("Invalid operands for '<'".into()),
        }
    }

    fn build_lte(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLE, l, r, "lte_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OLE, l, r, "lte_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            _ => Err("Invalid operands for '<='".into()),
        }
    }

    fn build_eq(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let (lhs, rhs) = self.promote_to_common_type(lhs, rhs)?;
        match (lhs, rhs) {
            (TypedValue::Int(l), TypedValue::Int(r)) => {
                let cmp = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::EQ, l, r, "eq_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            (TypedValue::Float(l), TypedValue::Float(r)) => {
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OEQ, l, r, "eq_cmp")
                    .map_err(|e| e.to_string())?;
                Ok(TypedValue::Bool(cmp))
            }
            _ => Err("Invalid operands for '=='".into()),
        }
    }

    // Logical operations
    fn build_and(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let lhs_bool = self.convert_to_bool(lhs)?;
        let rhs_bool = self.convert_to_bool(rhs)?;
        let result = self
            .builder
            .build_and(lhs_bool, rhs_bool, "and")
            .map_err(|e| e.to_string())?;
        Ok(TypedValue::Bool(result))
    }

    fn build_or(
        &self,
        lhs: TypedValue<'ctx>,
        rhs: TypedValue<'ctx>,
    ) -> Result<TypedValue<'ctx>, String> {
        let lhs_bool = self.convert_to_bool(lhs)?;
        let rhs_bool = self.convert_to_bool(rhs)?;
        let result = self
            .builder
            .build_or(lhs_bool, rhs_bool, "or")
            .map_err(|e| e.to_string())?;
        Ok(TypedValue::Bool(result))
    }

    // Helper to convert any TypedValue to boolean
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

    fn compile_expression(&mut self, expr: Expression) -> Result<TypedValue<'ctx>, String> {
        match expr {
            Expression::IntLiteral(val) => {
                let value = self.context.i32_type().const_int(val as u64, false);
                Ok(TypedValue::Int(value))
            }
            Expression::FloatLiteral(val) => {
                let value = self.context.f64_type().const_float(val);
                Ok(TypedValue::Float(value))
            }
            Expression::BoolLiteral(val) => {
                let value = self.context.bool_type().const_int(val as u64, false);
                Ok(TypedValue::Bool(value))
            }
            Expression::Identifier(name) => {
                if let Some(&var_ptr) = self.get_variable(&name) {
                    let loaded = self
                        .builder
                        .build_load(var_ptr, &name)
                        .map_err(|e| e.to_string())?;
                    // Determine type based on the loaded value type
                    if loaded.is_int_value() {
                        Ok(TypedValue::Int(loaded.into_int_value()))
                    } else if loaded.is_float_value() {
                        Ok(TypedValue::Float(loaded.into_float_value()))
                    } else {
                        Ok(TypedValue::Bool(loaded.into_int_value()))
                    }
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }
            Expression::Infix(left, op, right) => {
                // recursively compile left and right side
                let lhs = self.compile_expression(*left)?;
                let rhs = self.compile_expression(*right)?;

                // Dispatch table using match - Match for type and operation
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

    // Placeholder methods for missing Statement handlers
    fn compile_print_statement(&mut self, _expr: Expression) -> Result<(), String> {
        // TODO: Implement print statement
        Ok(())
    }

    fn compile_if_statement(
        &mut self,
        _condition: Expression,
        _consequence: Vec<Statement>,
        _alternative: Option<Vec<Statement>>,
    ) -> Result<(), String> {
        // TODO: Implement if statement
        Ok(())
    }

    fn compile_loop_statement(
        &mut self,
        _var_name: String,
        _start: Expression,
        _end: Expression,
        _body: Vec<Statement>,
    ) -> Result<(), String> {
        // TODO: Implement loop statement
        Ok(())
    }
}

// Optional: Implement From traits for cleaner conversion
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
