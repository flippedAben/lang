use std::error::Error;
use std::fmt;

use crate::{token::Token, token_type::TokenType};

#[derive(Debug)]
pub enum ScanError {
    UnexpectedCharacter(usize, char),
    UnterminatedString(usize),
}

impl Error for ScanError {}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScanError::UnexpectedCharacter(line, char) => {
                write!(f, "[line {}] Error: Unexpected character: {}", line, char)
            }
            ScanError::UnterminatedString(line) => {
                write!(f, "[line {}] Error: Unterminated string.", line)
            }
        }
    }
}

pub fn scan(source: String) -> Result<Vec<Token>, ScanError> {
    let mut tokens = Vec::new();
    let mut position = 0;
    let mut line = 1;
    let mut error: Option<ScanError> = None;
    while position < source.len() {
        match scan_token(&source, position, line) {
            Ok((token, next_position, next_line)) => {
                if let Some(token) = token {
                    tokens.push(token);
                }
                position = next_position;
                line = next_line;
            }
            Err(e) => {
                eprintln!("{}", e);
                error = Some(e);
                position += 1;
            }
        }
    }

    tokens.push(Token {
        token_type: TokenType::Eof,
        lexeme: "".to_string(),
        line: line,
    });

    match error {
        Some(error) => Err(error),
        None => Ok(tokens),
    }
}

fn scan_token(
    source: &str,
    position: usize,
    line: usize,
) -> Result<(Option<Token>, usize, usize), ScanError> {
    let char = source.as_bytes()[position] as char;
    let mut line = line;
    let (token_type, next_position): (Option<TokenType>, usize) = match char {
        '(' => (Some(TokenType::LeftParen), position + 1),
        ')' => (Some(TokenType::RightParen), position + 1),
        '{' => (Some(TokenType::LeftBrace), position + 1),
        '}' => (Some(TokenType::RightBrace), position + 1),
        ',' => (Some(TokenType::Comma), position + 1),
        '.' => (Some(TokenType::Dot), position + 1),
        '-' => (Some(TokenType::Minus), position + 1),
        '+' => (Some(TokenType::Plus), position + 1),
        ';' => (Some(TokenType::Semicolon), position + 1),
        '*' => (Some(TokenType::Star), position + 1),
        '!' => {
            if position + 1 < source.as_bytes().len()
                && source.as_bytes()[position + 1] as char == '='
            {
                (Some(TokenType::BangEqual), position + 2)
            } else {
                (Some(TokenType::Bang), position + 1)
            }
        }
        '=' => {
            if position + 1 < source.as_bytes().len()
                && source.as_bytes()[position + 1] as char == '='
            {
                (Some(TokenType::EqualEqual), position + 2)
            } else {
                (Some(TokenType::Equal), position + 1)
            }
        }
        '<' => {
            if position + 1 < source.as_bytes().len()
                && source.as_bytes()[position + 1] as char == '='
            {
                (Some(TokenType::LessEqual), position + 2)
            } else {
                (Some(TokenType::Less), position + 1)
            }
        }
        '>' => {
            if position + 1 < source.as_bytes().len()
                && source.as_bytes()[position + 1] as char == '='
            {
                (Some(TokenType::GreaterEqual), position + 2)
            } else {
                (Some(TokenType::Greater), position + 1)
            }
        }
        '/' => {
            if position + 1 < source.as_bytes().len()
                && source.as_bytes()[position + 1] as char == '/'
            {
                let mut new_position = position + 2;
                while new_position < source.len() && source.as_bytes()[new_position] as char != '\n'
                {
                    new_position += 1;
                }
                line += 1;
                // Return None for comments, skipping token generation
                (None, new_position + 1)
            } else {
                (Some(TokenType::Slash), position + 1)
            }
        }
        ' ' | '\r' | '\t' => (None, position + 1),
        '\n' => {
            line += 1;
            (None, position + 1)
        }
        '"' => {
            let mut new_position = position + 1;
            while new_position < source.len() && source.as_bytes()[new_position] as char != '"' {
                let char = source.as_bytes()[new_position] as char;
                if char == '\n' {
                    line += 1;
                }
                new_position += 1;
            }
            if new_position == source.len() {
                return Err(ScanError::UnterminatedString(line));
            }

            (
                Some(TokenType::String(
                    std::str::from_utf8(source.as_bytes()).unwrap()[position + 1..new_position]
                        .to_string(),
                )),
                new_position + 1,
            )
        }
        '0'..='9' => {
            let mut new_position = position + 1;
            while new_position < source.len() {
                let char = source.as_bytes()[new_position] as char;
                if char.is_digit(10) {
                    new_position += 1;
                } else {
                    break;
                }
            }
            if new_position < source.len()
                && (source.as_bytes()[new_position] as char) == '.'
                && new_position + 1 < source.len()
                && (source.as_bytes()[new_position + 1] as char).is_digit(10)
            {
                new_position += 1;
                while new_position < source.len() {
                    let char = source.as_bytes()[new_position] as char;
                    if char.is_digit(10) {
                        new_position += 1;
                    } else {
                        break;
                    }
                }
            }
            let float = std::str::from_utf8(source.as_bytes()).unwrap()[position..new_position]
                .to_string()
                .parse()
                .expect("Failed to parse float");
            (Some(TokenType::Number(float)), new_position)
        }
        'a'..='z' | 'A'..'Z' | '_' => {
            let mut new_position = position + 1;
            while new_position < source.len() {
                let char = source.as_bytes()[new_position] as char;
                if char.is_ascii_alphanumeric() || char == '_' {
                    new_position += 1;
                } else {
                    break;
                }
            }

            let raw_text =
                std::str::from_utf8(source.as_bytes()).unwrap()[position..new_position].to_string();
            let token_type = match raw_text.as_str() {
                "let" => TokenType::Let,
                "fn" => TokenType::Fn,
                "return" => TokenType::Return,
                "class" => TokenType::Class,
                "this" => TokenType::This,
                "super" => TokenType::Super,
                "and" => TokenType::And,
                "or" => TokenType::Or,
                "if" => TokenType::If,
                "else" => TokenType::Else,
                "true" => TokenType::True,
                "false" => TokenType::False,
                "for" => TokenType::For,
                "while" => TokenType::While,
                "nil" => TokenType::Nil,
                "print" => TokenType::Print,
                _ => TokenType::Identifier(raw_text),
            };
            (Some(token_type), new_position)
        }
        _ => return Err(ScanError::UnexpectedCharacter(line, char)),
    };

    // If the token_type is None (e.g., comments), we skip returning a token.
    if let Some(token_type) = token_type {
        let token = Token {
            token_type,
            lexeme: std::str::from_utf8(&source.as_bytes()[position..next_position])
                .unwrap()
                .to_string(),
            line,
        };
        Ok((Some(token), next_position, line))
    } else {
        // No token to return (e.g., comments), just return the updated position.
        Ok((None, next_position, line))
    }
}
