#[derive(Debug, PartialEq, Clone)]

pub enum Token {
    // Keywords
    If,
    Then,
    Else,
    End,
    Loop,
    From,
    To,
    In,
    Print,
    Read,

    // Types
    IntType,
    FloatType,
    CharType,
    StringType,
    BoolType,

    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    CharLiteral(char),
    StringLiteral(String),
    BoolLiteral(bool),

    // Identifier
    Identifier(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    FloorDivide,
    Not,
    And,
    Or,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Dollar,
    Modulo,
    // Delimiters
    Colon,
    Newline,

    // EOF
    Eof,
    Illegal,
}
