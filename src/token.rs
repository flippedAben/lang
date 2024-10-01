use crate::token_type::TokenType;

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String, // String or should i make it a slice referencing a part of the source code?
    pub line: usize,
}
