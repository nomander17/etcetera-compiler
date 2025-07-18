use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    // let <identifier> = <value>
    Let(String, Expression),
    Print(Expression),
    Assignment(String, Expression),
    // if condition, do vec of statements, else optional
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
    Loop(String, Expression, Expression, Vec<Statement>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifer(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    // <left> operator <right>
    // exp has to use box because size unknown
    Infix(Box<Expression>, Token, Box<Expression>),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}
