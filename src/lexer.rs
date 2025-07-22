use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,            // Input string
            position: 0,      // Points to current char
            read_position: 0, // Points ahead of current char
            ch: 0,            // Current char
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            b':' => Token::Colon,
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Multiply,
            b'/' => {
                if self.peek_char() == b'/' {
                    self.read_char();
                    Token::FloorDivide
                } else {
                    Token::Divide
                }
            }
            b'%' => Token::Modulo,
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::GreaterThanOrEqual
                } else {
                    Token::GreaterThan
                }
            }
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::LessThanOrEqual
                } else {
                    Token::LessThan
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    // Comment
                    self.skip_comment();
                    return self.next_token();
                }
            }
            b'\n' | b'\r' => Token::Newline,
            b'"' => self.read_string(),
            b'\'' => self.read_char_literal(),
            b'$' => Token::Dollar,
            b'0'..=b'9' => return self.read_number(),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_identifier();
                return match ident.as_str() {
                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    "end" => Token::End,
                    "loop" => Token::Loop,
                    "from" => Token::From,
                    "to" => Token::To,
                    "in" => Token::In,
                    "print" => Token::Print,
                    "read" => Token::Read,
                    "int" => Token::IntType,
                    "float" => Token::FloatType,
                    "string" => Token::StringType,
                    "char" => Token::CharType,
                    "bool" => Token::BoolType,
                    "true" => Token::BoolLiteral(true),
                    "false" => Token::BoolLiteral(false),
                    "not" => Token::Not,
                    "and" => Token::And,
                    "or" => Token::Or,
                    _ => Token::Identifier(ident),
                };
            }
            0 => Token::Eof,
            _ => Token::Illegal,
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            // We use 0 as a convention to mean EOF basically
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() && self.ch != b'\n' && self.ch != b'r' {
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        if self.peek_char() == b'!' {
            // Multiline comment
            // Consume 1st !
            self.read_char();
            // Consume 2nd !
            self.read_char();
            loop {
                if self.ch == b'!' && self.peek_char() == b'!' {
                    self.read_char();
                    self.read_char();
                    break;
                }
                if self.ch == 0 {
                    break;
                }
                self.read_char();
            }
        } else {
            while self.ch != b'\n' && self.ch != 0 {
                self.read_char();
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let posititon = self.position;
        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char();
        }
        self.input[posititon..self.position].to_string()
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        if self.ch == b'.' {
            self.read_char();
            while self.ch.is_ascii_digit() {
                self.read_char();
            }
            let number = &self.input[position..self.position];
            return Token::FloatLiteral(number.parse().unwrap());
        }
        let number = &self.input[position..self.position];
        return Token::IntLiteral(number.parse().unwrap());
    }

    fn read_string(&mut self) -> Token {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }
        let s = self.input[position..self.position].to_string();
        Token::StringLiteral(s)
    }
    fn read_char_literal(&mut self) -> Token {
        self.read_char(); // Consume opening '
        let ch = self.ch as char;
        self.read_char(); // Consume char
        if self.ch == b'\'' {
            Token::CharLiteral(ch)
        } else {
            Token::Illegal
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        let input = "a: int = 5";
        let mut lexer = Lexer::new(input);

        let tokens = vec![
            Token::Identifier("a".to_string()),
            Token::Colon,
            Token::IntType,
            Token::Assign,
            Token::IntLiteral(5),
            Token::Eof,
        ];

        for token in tokens {
            assert_eq!(lexer.next_token(), token);
        }
    }

    #[test]
    fn test_if_statement() {
        let input = "if a > b then print \"$a is greater than b\" end if";
        let mut lexer = Lexer::new(input);

        let tokens = vec![
            Token::If,
            Token::Identifier("a".to_string()),
            Token::GreaterThan,
            Token::Identifier("b".to_string()),
            Token::Then,
            Token::Print,
            Token::StringLiteral("$a is greater than b".to_string()),
            Token::End,
            Token::If,
        ];

        for token in tokens {
            assert_eq!(lexer.next_token(), token);
        }
    }

    #[test]
    fn test_loop_statement() {
        let input = "loop from 1 to 10 in i\nprint i\nend loop";
        let mut lexer = Lexer::new(input);

        let tokens = vec![
            Token::Loop,
            Token::From,
            Token::IntLiteral(1),
            Token::To,
            Token::IntLiteral(10),
            Token::In,
            Token::Identifier("i".to_string()),
            Token::Newline,
            Token::Print,
            Token::Identifier("i".to_string()),
            Token::Newline,
            Token::End,
            Token::Loop,
        ];

        for token in tokens {
            assert_eq!(lexer.next_token(), token);
        }
    }
}
