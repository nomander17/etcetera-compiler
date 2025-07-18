use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > and <
    Sum,         // +
    Product,     // *
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
            errors: Vec::new(),
        };

        // Read two tokens so current and peek tokens are both set
        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.current_token != Token::Eof {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Identifier(_) => {
                // identifier can be a let or assign
                if self.peek_token == Token::Assign {
                    self.parse_assignment_statement()
                } else {
                    self.parse_let_statement()
                }
            }
            Token::Print => self.parse_print_statement(),
            Token::If => self.parse_if_statement(),
            Token::Loop => self.parse_loop_statement(),
            _ => None,
        }
    }

    fn parse_assignment_statement(&mut self) -> Option<Statement> {
        // Our assignment statements look like:
        // variable: type = value
        // check for each step and exit when the syntax is wrong
        let ident = if let Token::Identifier(name) = self.current_token.clone() {
            name
        } else {
            return None;
        };

        if !self.expect_peek(Token::Colon) {
            return None;
        }
        self.next_token();

        if !self.peek_token_is_type() {
            return None;
        }
        self.next_token();

        if !self.expect_peek(Token::Assign) {
            return None;
        }
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::Assignment(ident, expr))
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        // For variable definitation we use let
        // let statements look like:
        // let ident: type = value
        let ident = if let Token::Identifier(name) = self.current_token.clone() {
            name
        } else {
            return None;
        };

        if !self.expect_peek(Token::Colon) {
            return None;
        }

        if !self.peek_token_is_type() {
            return None;
        }
        self.next_token();

        if !self.expect_peek(Token::Assign) {
            return None;
        }
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::Let(ident, expr))
    }

    fn parse_print_statement(&mut self) -> Option<Statement> {
        self.next_token(); // Consume print
        let expr = self.parse_expression(Precedence::Lowest)?;

        Some(Statement::Print(expr))
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        // Our if else look like:
        // if condition then statements else statements end if
        self.next_token(); // Consume if
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::Then) {
            return None;
        }

        let then_block = self.parse_block_statement();

        let mut else_block = None;

        if self.current_token == Token::Else {
            else_block = Some(self.parse_block_statement());
        }

        if self.current_token != Token::End {
            return None;
        }

        if !self.expect_peek(Token::If) {
            return None;
        }

        Some(Statement::If(condition, then_block, else_block))
    }

    fn parse_loop_statement(&mut self) -> Option<Statement> {
        // Our loop statements look like:
        // loop from 1 to 10 in loop_variable statements end loop
        if !self.expect_peek(Token::From) {
            return None;
        }
        self.next_token(); // Consume from
        let start = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::To) {
            return None;
        }
        self.next_token();
        let end = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::In) {
            return None;
        }

        let var_name = if let Token::Identifier(name) = self.peek_token.clone() {
            self.next_token();
            name
        } else {
            return None;
        };

        let body = self.parse_block_statement();

        if self.current_token != Token::End {
            return None;
        }

        if !self.expect_peek(Token::Loop) {
            return None;
        }

        Some(Statement::Loop(var_name, start, end, body))
    }

    fn parse_block_statement(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        self.next_token(); // Consume the then else or loop var

        while self.current_token != Token::End
            && self.current_token != Token::Else
            && self.current_token != Token::Eof
        {
            if let Some(s) = self.parse_statement() {
                statements.push(s);
            }
            self.next_token();
        }

        statements
    }

    // Using Pratt Parsing method
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        // parse for - prefix first
        let mut left_exp = self.parse_prefix()?;

        // loop as long as there is an infix operator of higher precedence
        while self.peek_token != Token::Newline
            && self.peek_token != Token::Eof
            && precedence < self.peek_precedence()
        {
            let infix_token = self.peek_token.clone();
            if !self.is_infix_token(&infix_token) {
                return Some(left_exp);
            }

            self.next_token(); // Consume infix operator

            // recusrive parse right hand side
            left_exp = self.parse_infix(left_exp, infix_token)?;
        }
        Some(left_exp)
    }

    fn is_infix_token(&self, token: &Token) -> bool {
        matches!(
            token,
            Token::Plus
                | Token::Minus
                | Token::Multiply
                | Token::Divide
                | Token::FloorDivide
                | Token::Equal
                | Token::NotEqual
                | Token::GreaterThan
                | Token::GreaterThanOrEqual
                | Token::LessThan
                | Token::LessThanOrEqual
                | Token::Modulo
        )
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        match self.current_token.clone() {
            Token::Identifier(name) => Some(Expression::Identifer(name)),
            Token::IntLiteral(value) => Some(Expression::IntLiteral(value)),
            Token::FloatLiteral(value) => Some(Expression::FloatLiteral(value)),
            Token::CharLiteral(value) => Some(Expression::CharLiteral(value)),
            Token::StringLiteral(value) => Some(Expression::StringLiteral(value)),
            Token::BoolLiteral(value) => Some(Expression::BoolLiteral(value)),
            Token::Dollar => {
                // $ is used to get variable value
                self.next_token();
                if let Token::Identifier(name) = self.current_token.clone() {
                    Some(Expression::Identifer(format!("${}", name)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn parse_infix(&mut self, left: Expression, operator: Token) -> Option<Expression> {
        let precedence = self.get_precedence(&operator);
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Some(Expression::Infix(Box::new(left), operator, Box::new(right)))
    }

    fn get_precedence(&self, token: &Token) -> Precedence {
        match token {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::GreaterThan
            | Token::GreaterThanOrEqual
            | Token::LessThan
            | Token::LessThanOrEqual => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Multiply | Token::Divide | Token::FloorDivide | Token::Modulo => {
                Precedence::Product
            }
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.get_precedence(&self.peek_token)
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token == token {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn peek_token_is_type(&self) -> bool {
        matches!(
            self.peek_token,
            Token::IntType | Token::FloatType | Token::CharType | Token::StringType
        )
    }
    fn peek_error(&mut self, token: Token) {
        let msg = format!(
            "Expected next token to be {:?}, got {:?} instead",
            token, self.peek_token
        );
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_let_statement() {
        let input = "a: int = 5";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);

        let expected = Statement::Let("a".to_string(), Expression::IntLiteral(5));
        assert_eq!(program.statements[0], expected);
    }

    #[test]
    fn test_print_statement() {
        let input = "print $a";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);

        let expected = Statement::Print(Expression::Identifer("$a".to_string()));
        assert_eq!(program.statements[0], expected);
    }

    #[test]
    fn test_if_statement() {
        let input = "if 1 > 0 then\nprint 1\nend if";
        let lexer = Lexer::new(input);
        let mut lexer2 = lexer.clone();
        for _ in 0..11 {
            print!("{:?}\n", lexer2.next_token());
        }

        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);

        let expected = Statement::If(
            Expression::Infix(
                Box::new(Expression::IntLiteral(1)),
                Token::GreaterThan,
                Box::new(Expression::IntLiteral(0)),
            ),
            vec![Statement::Print(Expression::IntLiteral(1))], // do what if true
            None,                                              // else
        );
        assert_eq!(program.statements[0], expected);
    }
}
