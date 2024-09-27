use crate::token::Token;
use crate::token_type::TokenType;
use core::fmt;
use std::error::Error;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof(usize),
    UnmatchedParenthesis(usize),
    ExpectedExpression(usize, String),
    MissingPrintSemicolon(usize),
    MissingExprSemicolon(usize),
    MissingVarSemicolon(usize),
    InvalidAssignmentLValue(usize),
    ExpectBlockClosingBrace(usize),
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedEof(line) => {
                write!(f, "[line {}] Error at end", line)
            }
            ParseError::UnmatchedParenthesis(line) => {
                write!(f, "[line {}] Error: unmatched ')'", line)
            }
            ParseError::ExpectedExpression(line, lexeme) => {
                write!(
                    f,
                    "[line {}] Error at '{}': Expect expression.",
                    line, lexeme
                )
            }
            ParseError::MissingPrintSemicolon(line) => {
                write!(f, "[line {}] Expect ';' after value.", line)
            }
            ParseError::MissingExprSemicolon(line) => {
                write!(f, "[line {}] Expect ';' after expression.", line)
            }
            ParseError::MissingVarSemicolon(line) => {
                write!(f, "[line {}] Expect ';' after variable declaration.", line)
            }
            ParseError::InvalidAssignmentLValue(line) => {
                write!(f, "[line {}] Invalid assignment target.", line)
            }
            ParseError::ExpectBlockClosingBrace(line) => {
                write!(f, "[line {}] Expect '}}' after block.", line)
            }
        }
    }
}

#[derive(Debug)]
pub enum Operation {
    Not,
    Negate,
    Add,
    Subtract,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::Not => write!(f, "!"),
            Operation::Negate => write!(f, "-"),
            Operation::Add => write!(f, "+"),
            Operation::Subtract => write!(f, "-"),
            Operation::Divide => write!(f, "/"),
            Operation::Multiply => write!(f, "*"),
            Operation::Equal => write!(f, "=="),
            Operation::NotEqual => write!(f, "!="),
            Operation::Greater => write!(f, ">"),
            Operation::GreaterEqual => write!(f, ">="),
            Operation::Less => write!(f, "<"),
            Operation::LessEqual => write!(f, "<="),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Group(Box<Expr>),
    Unary(Operation, Box<Expr>),
    Binary(Operation, Box<Expr>, Box<Expr>),
    String(String),
    Number(f64),
    Boolean(bool),
    None,
    Variable(String),
    Assign(String, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Group(expr) => write!(f, "(group {})", expr),
            Expr::Unary(op, expr) => write!(f, "({} {})", op, expr),
            Expr::Binary(op, left, right) => write!(f, "({} {} {})", op, left, right),
            Expr::String(s) => write!(f, "{}", s),
            Expr::Number(x) => {
                if x.fract() == 0.0 {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Expr::Boolean(b) => write!(f, "{}", b),
            Expr::None => write!(f, "nil"),
            Expr::Variable(name) => write!(f, "{}", name),
            Expr::Assign(name, expr) => write!(f, "{} = {}", name, expr),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(String, Option<Expr>),
    Block(Vec<Stmt>),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "{}", expr),
            Stmt::Print(expr) => write!(f, "{}", expr),
            Stmt::Var(name, expr) => match expr {
                Some(expr) => write!(f, "{}: {}", name, expr),
                None => write!(f, "{}: null", name),
            },
            Stmt::Block(vec) => write!(f, "{:?}", vec),
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> (Vec<Stmt>, bool) {
    let mut statements = Vec::new();
    let mut position: usize = 0;
    let mut error = false;
    while position < tokens.len() {
        match tokens[position].token_type {
            TokenType::Eof => break,
            _ => match parse_decl(&tokens, position) {
                Ok((stmt, next_position)) => {
                    statements.push(stmt);
                    position = next_position;
                }
                Err(e) => {
                    error = true;
                    eprintln!("{}", e);
                    position = synchronize(&tokens, position + 1);
                }
            },
        }
    }
    (statements, error)
}

fn synchronize(tokens: &Vec<Token>, mut position: usize) -> usize {
    while position < tokens.len() {
        match tokens[position].token_type {
            TokenType::Eof
            | TokenType::Var
            | TokenType::If
            | TokenType::For
            | TokenType::While
            | TokenType::Class
            | TokenType::Fun
            | TokenType::Return
            | TokenType::Print => return position,
            TokenType::Semicolon => return position + 1,
            _ => position += 1,
        }
    }
    position
}

fn parse_decl(tokens: &Vec<Token>, position: usize) -> Result<(Stmt, usize), ParseError> {
    match tokens[position].token_type {
        TokenType::Var => {
            assert!(position + 1 < tokens.len());
            match &tokens[position + 1].token_type {
                TokenType::Identifier(name) => {
                    assert!(position + 2 < tokens.len());
                    match &tokens[position + 2].token_type {
                        TokenType::Equal => {
                            let (expr, next_position) = parse_expr(&tokens, position + 3)?;

                            match &tokens[next_position].token_type {
                                TokenType::Semicolon => {
                                    Ok((Stmt::Var(name.to_string(), Some(expr)), next_position + 1))
                                }
                                _ => {
                                    Err(ParseError::MissingVarSemicolon(tokens[next_position].line))
                                }
                            }
                        }
                        TokenType::Semicolon => {
                            Ok((Stmt::Var(name.to_string(), None), position + 3))
                        }
                        _ => panic!("not a valid next symbol for var. Should be = or ;"),
                    }
                }
                _ => panic!("Expect variable name"),
            }
        }
        _ => parse_stmt(tokens, position),
    }
}

fn parse_stmt(tokens: &Vec<Token>, position: usize) -> Result<(Stmt, usize), ParseError> {
    match tokens[position].token_type {
        TokenType::Print => {
            let (expr, next_position) = parse_expr(&tokens, position + 1)?;
            assert!(next_position < tokens.len());

            match tokens[next_position].token_type {
                TokenType::Semicolon => Ok((Stmt::Print(expr), next_position + 1)),
                _ => Err(ParseError::MissingPrintSemicolon(
                    tokens[next_position].line,
                )),
            }
        }
        TokenType::LeftBrace => {
            let mut next_position = position + 1;
            let mut statements = Vec::new();
            while next_position < tokens.len() {
                match tokens[next_position].token_type {
                    TokenType::RightBrace => {
                        return Ok((Stmt::Block(statements), next_position + 1));
                    }
                    _ => {
                        let (stmt, next_position_a) = parse_decl(tokens, next_position)?;
                        next_position = next_position_a;
                        statements.push(stmt);
                    }
                }
            }
            Err(ParseError::ExpectBlockClosingBrace(
                tokens.last().unwrap().line,
            ))
        }
        _ => {
            let (expr, next_position) = parse_expr(&tokens, position)?;
            assert!(next_position < tokens.len());

            match tokens[next_position].token_type {
                TokenType::Semicolon => Ok((Stmt::Expression(expr), next_position + 1)),
                _ => Err(ParseError::MissingExprSemicolon(tokens[next_position].line)),
            }
        }
    }
}

pub fn parse_expr(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    parse_assignment(&tokens, position)
}

fn parse_assignment(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    let (expr, position) = parse_equality(&tokens, position)?;

    match tokens[position].token_type {
        TokenType::Equal => match expr {
            Expr::Variable(name) => {
                let (right, next_position) = parse_assignment(tokens, position + 1)?;
                Ok((Expr::Assign(name, Box::new(right)), next_position))
            }
            _ => Err(ParseError::InvalidAssignmentLValue(tokens[position].line)),
        },
        _ => Ok((expr, position)),
    }
}

fn parse_equality(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    let (mut expr, mut position) = parse_comparison(&tokens, position)?;

    loop {
        assert!(position < tokens.len());

        match tokens[position].token_type {
            TokenType::EqualEqual | TokenType::BangEqual => {
                let (right, next_position) = parse_comparison(tokens, position + 1)?;
                let op = match tokens[position].token_type {
                    TokenType::EqualEqual => Operation::Equal,
                    TokenType::BangEqual => Operation::NotEqual,
                    _ => panic!("Should never happen"),
                };
                expr = Expr::Binary(op, Box::new(expr), Box::new(right));
                position = next_position;
            }
            _ => {
                break;
            }
        }
    }
    Ok((expr, position))
}

fn parse_comparison(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    let (mut expr, mut position) = parse_term(&tokens, position)?;

    loop {
        assert!(position < tokens.len());

        match tokens[position].token_type {
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => {
                let (right, next_position) = parse_term(tokens, position + 1)?;
                let op = match tokens[position].token_type {
                    TokenType::Greater => Operation::Greater,
                    TokenType::GreaterEqual => Operation::GreaterEqual,
                    TokenType::Less => Operation::Less,
                    TokenType::LessEqual => Operation::LessEqual,
                    _ => panic!("Should never happen"),
                };
                expr = Expr::Binary(op, Box::new(expr), Box::new(right));
                position = next_position;
            }
            _ => {
                break;
            }
        }
    }
    Ok((expr, position))
}

fn parse_term(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    let (mut expr, mut position) = parse_factor(&tokens, position)?;

    loop {
        assert!(position < tokens.len());

        match tokens[position].token_type {
            TokenType::Plus | TokenType::Minus => {
                let (right, next_position) = parse_factor(tokens, position + 1)?;
                let op = match tokens[position].token_type {
                    TokenType::Plus => Operation::Add,
                    TokenType::Minus => Operation::Subtract,
                    _ => panic!("Should never happen"),
                };
                expr = Expr::Binary(op, Box::new(expr), Box::new(right));
                position = next_position;
            }
            _ => {
                break;
            }
        }
    }
    Ok((expr, position))
}

fn parse_factor(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    let (mut expr, mut position) = parse_unary(&tokens, position)?;

    loop {
        assert!(position < tokens.len());

        match tokens[position].token_type {
            TokenType::Star | TokenType::Slash => {
                let (right, next_position) = parse_unary(tokens, position + 1)?;
                let op = match tokens[position].token_type {
                    TokenType::Star => Operation::Multiply,
                    TokenType::Slash => Operation::Divide,
                    _ => panic!("Should never happen"),
                };
                expr = Expr::Binary(op, Box::new(expr), Box::new(right));
                position = next_position;
            }
            _ => {
                break;
            }
        }
    }
    Ok((expr, position))
}

fn parse_unary(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    match tokens[position].token_type {
        TokenType::Bang | TokenType::Minus => {
            let (expr, next_position) = parse_unary(tokens, position + 1)?;
            let op = match tokens[position].token_type {
                TokenType::Bang => Operation::Not,
                TokenType::Minus => Operation::Negate,
                _ => panic!("Should never happen"),
            };
            Ok((Expr::Unary(op, Box::new(expr)), next_position))
        }
        _ => parse_primary(tokens, position),
    }
}

fn parse_primary(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    // println!("{}", tokens[position]);
    match &tokens[position].token_type {
        TokenType::String(x) => Ok((Expr::String(x.clone()), position + 1)),
        TokenType::Number(x) => Ok((Expr::Number(*x), position + 1)),
        TokenType::True => Ok((Expr::Boolean(true), position + 1)),
        TokenType::False => Ok((Expr::Boolean(false), position + 1)),
        TokenType::Nil => Ok((Expr::None, position + 1)),
        TokenType::LeftParen => {
            let (expr, next_position) = parse_expr(&tokens, position + 1)?;
            assert!(position < tokens.len());
            match tokens[next_position].token_type {
                TokenType::RightParen => Ok((Expr::Group(Box::new(expr)), next_position + 1)),
                _ => Err(ParseError::UnmatchedParenthesis(tokens[position].line)),
            }
        }
        TokenType::Identifier(name) => Ok((Expr::Variable(name.to_string()), position + 1)),
        TokenType::Eof => Err(ParseError::UnexpectedEof(tokens[position].line)),
        _ => Err(ParseError::ExpectedExpression(
            tokens[position].line,
            tokens[position].lexeme.clone(), // TODO: remove clone
        )),
    }
}
