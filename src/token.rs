use core::fmt;
use std::fmt::Display;

use crate::token_type::TokenType;

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String, // String or should i make it a slice referencing a part of the source code?
    pub line: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let x = match &self.token_type {
            TokenType::String(x) => x,
            TokenType::Number(x) => {
                if x.fract() == 0.0 {
                    &format!("{:.1}", x)
                } else {
                    &format!("{}", x)
                }
            }
            _ => "null",
        };
        write!(f, "{} {} {}", self.token_type, self.lexeme, x)
    }
}
