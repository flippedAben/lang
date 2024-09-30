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
