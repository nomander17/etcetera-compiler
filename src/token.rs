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
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Bool(bool),

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
