use std::fmt;

#[derive(Debug)]
pub enum TokenType {
    // Single-character
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // Multi-character
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Literals
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords
    Var,
    Fun,
    Return,
    Class,
    This,
    Super,
    And,
    Or,
    If,
    Else,
    True,
    False,
    For,
    While,
    Nil,
    Print,

    // Misc
    Eof,
}

// Only required to match the test output requried by CodeCrafters
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // Single-character tokens
            TokenType::LeftParen => write!(f, "LEFT_PAREN"),
            TokenType::RightParen => write!(f, "RIGHT_PAREN"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE"),
            TokenType::Comma => write!(f, "COMMA"),
            TokenType::Dot => write!(f, "DOT"),
            TokenType::Minus => write!(f, "MINUS"),
            TokenType::Plus => write!(f, "PLUS"),
            TokenType::Semicolon => write!(f, "SEMICOLON"),
            TokenType::Slash => write!(f, "SLASH"),
            TokenType::Star => write!(f, "STAR"),

            // Multi-character tokens
            TokenType::Bang => write!(f, "BANG"),
            TokenType::BangEqual => write!(f, "BANG_EQUAL"),
            TokenType::Equal => write!(f, "EQUAL"),
            TokenType::EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenType::Less => write!(f, "LESS"),
            TokenType::LessEqual => write!(f, "LESS_EQUAL"),
            TokenType::Greater => write!(f, "GREATER"),
            TokenType::GreaterEqual => write!(f, "GREATER_EQUAL"),

            // Literals
            TokenType::Identifier(_) => write!(f, "IDENTIFIER"),
            TokenType::String(_) => write!(f, "STRING"),
            TokenType::Number(_) => write!(f, "NUMBER"),

            // Keywords
            TokenType::Var => write!(f, "VAR"),
            TokenType::Fun => write!(f, "FUN"),
            TokenType::Return => write!(f, "RETURN"),
            TokenType::Class => write!(f, "CLASS"),
            TokenType::This => write!(f, "THIS"),
            TokenType::Super => write!(f, "SUPER"),
            TokenType::And => write!(f, "AND"),
            TokenType::Or => write!(f, "OR"),
            TokenType::If => write!(f, "IF"),
            TokenType::Else => write!(f, "ELSE"),
            TokenType::True => write!(f, "TRUE"),
            TokenType::False => write!(f, "FALSE"),
            TokenType::For => write!(f, "FOR"),
            TokenType::While => write!(f, "WHILE"),
            TokenType::Nil => write!(f, "NIL"),
            TokenType::Print => write!(f, "PRINT"),

            // Misc
            TokenType::Eof => write!(f, "EOF"),
        }
    }
}
