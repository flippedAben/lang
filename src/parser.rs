use crate::token::Token;
use crate::token_type::TokenType;
use core::{fmt, panic};
use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof(usize),
    UnmatchedParenthesis(usize),
    InvalidPrimaryToken(usize, String),
    MissingExprSemicolon(usize),
    MissingLetSemicolon(usize),
    InvalidAssignmentLValue(usize),
    MissingBlockClosingBrace(usize),
    MissingWhileStartingBrace(usize),
    MissingIfStartingBrace(usize),
    MissingElseStartingBrace(usize),
    TooManyArgumentsInCall(usize),
    TooManyParametersInFnDecl(usize),
    MissingFnLeftBrace(usize),
    InvalidFnCallArg(usize),
    MissingReturnSemicolon(usize),
    InvalidGet(usize),
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
            ParseError::InvalidPrimaryToken(line, lexeme) => {
                write!(
                    f,
                    "[line {}] Error at '{}': invalid primary token.",
                    line, lexeme
                )
            }
            ParseError::MissingExprSemicolon(line) => {
                write!(f, "[line {}] Expect ';' after expression.", line)
            }
            ParseError::MissingLetSemicolon(line) => {
                write!(f, "[line {}] Expect ';' after variable declaration.", line)
            }
            ParseError::InvalidAssignmentLValue(line) => {
                write!(f, "[line {}] Invalid assignment target.", line)
            }
            ParseError::MissingBlockClosingBrace(line) => {
                write!(f, "[line {}] Expect '}}' after block.", line)
            }
            ParseError::MissingWhileStartingBrace(line) => {
                write!(f, "[line {}] Expect '{{' after 'while' keyword.", line)
            }
            ParseError::MissingIfStartingBrace(line) => {
                write!(f, "[line {}] Expect '{{' after 'if' keyword.", line)
            }
            ParseError::MissingElseStartingBrace(line) => {
                write!(f, "[line {}] Expect '{{' after 'else' keyword.", line)
            }
            ParseError::TooManyArgumentsInCall(line) => {
                write!(f, "[line {}] Expect <= 4 function call arguments.", line)
            }
            ParseError::TooManyParametersInFnDecl(line) => {
                write!(
                    f,
                    "[line {}] Expect <= 4 function declaration parameters.",
                    line
                )
            }
            ParseError::MissingFnLeftBrace(line) => {
                write!(
                    f,
                    "[line {}] Expect '{{' after function parameter declaration.",
                    line
                )
            }
            ParseError::InvalidFnCallArg(line) => {
                write!(
                    f,
                    "[line {}] Expect , or ) after argument of function call.",
                    line
                )
            }
            ParseError::MissingReturnSemicolon(line) => {
                write!(f, "[line {}] Expect ; at end of return statement.", line)
            }
            ParseError::InvalidGet(line) => {
                write!(f, "[line {}] Expect ; at end of return statement.", line)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Not,
    Negate,
    Add,
    Subtract,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Group(Box<Expr>),
    Unary(Operation, Box<Expr>),
    Binary(Operation, Box<Expr>, Box<Expr>),
    String(String),
    Number(f64),
    Boolean(bool),
    None,
    Variable(String, RefCell<Option<usize>>),
    Assign(String, Box<Expr>, RefCell<Option<usize>>),
    BinaryLogical(Operation, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Rc<Expr>, String),
    Set(Rc<Expr>, String, Rc<Expr>),
    Me(String, RefCell<Option<usize>>),
}

// TODO: should these be structs instead of enum?
#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Let(String, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Fn(String, Rc<Vec<String>>, Rc<Vec<Stmt>>),
    Return(Box<Expr>),
    Class(String, Rc<Vec<Stmt>>),
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, ParseError> {
    let mut statements = Vec::new();
    let mut position: usize = 0;
    let mut error: Option<ParseError> = None;
    while position < tokens.len() {
        match tokens[position].token_type {
            TokenType::Eof => break,
            _ => match parse_decl(&tokens, position) {
                Ok((stmt, next_position)) => {
                    statements.push(stmt);
                    position = next_position;
                }
                Err(e) => {
                    eprintln!("{}", e);
                    error = Some(e);
                    position = synchronize(&tokens, position + 1);
                }
            },
        }
    }
    match error {
        Some(error) => Err(error),
        None => Ok(statements),
    }
}

fn synchronize(tokens: &Vec<Token>, mut position: usize) -> usize {
    while position < tokens.len() {
        match tokens[position].token_type {
            TokenType::Eof
            | TokenType::Let
            | TokenType::If
            | TokenType::For
            | TokenType::While
            | TokenType::Class
            | TokenType::Fn
            | TokenType::Return => return position,
            TokenType::Semicolon => return position + 1,
            _ => position += 1,
        }
    }
    position
}

fn parse_decl(tokens: &Vec<Token>, position: usize) -> Result<(Stmt, usize), ParseError> {
    match tokens[position].token_type {
        TokenType::Let => {
            assert!(position + 1 < tokens.len());
            match &tokens[position + 1].token_type {
                TokenType::Identifier(name) => {
                    assert!(position + 2 < tokens.len());
                    match &tokens[position + 2].token_type {
                        TokenType::Equal => {
                            let (expr, next_position) = parse_expr(&tokens, position + 3)?;

                            match &tokens[next_position].token_type {
                                TokenType::Semicolon => {
                                    Ok((Stmt::Let(name.to_string(), Some(expr)), next_position + 1))
                                }
                                _ => {
                                    Err(ParseError::MissingLetSemicolon(tokens[next_position].line))
                                }
                            }
                        }
                        TokenType::Semicolon => {
                            Ok((Stmt::Let(name.to_string(), None), position + 3))
                        }
                        _ => panic!("not a valid next symbol for var. Should be = or ;"),
                    }
                }
                _ => panic!("Expect variable name"),
            }
        }
        TokenType::Fn => {
            assert!(position + 1 < tokens.len());
            match &tokens[position + 1].token_type {
                TokenType::Identifier(name) => match &tokens[position + 2].token_type {
                    TokenType::LeftParen => {
                        let mut parameters = Vec::new();
                        let mut position = position + 3;
                        loop {
                            match &tokens[position].token_type {
                                TokenType::RightParen => match &tokens[position + 1].token_type {
                                    TokenType::LeftBrace => {
                                        let (body, next_position) =
                                            parse_stmt(tokens, position + 1)?;
                                        if let Stmt::Block(body) = body {
                                            return Ok((
                                                Stmt::Fn(
                                                    name.to_string(),
                                                    Rc::new(parameters),
                                                    Rc::new(body),
                                                ),
                                                next_position,
                                            ));
                                        }
                                    }
                                    _ => {
                                        return Err(ParseError::MissingFnLeftBrace(position));
                                    }
                                },
                                TokenType::Identifier(parameter) => {
                                    parameters.push(parameter.to_string());
                                    if parameters.len() > 4 {
                                        return Err(ParseError::TooManyParametersInFnDecl(
                                            tokens[position + 1].line,
                                        ));
                                    }

                                    match &tokens[position + 1].token_type {
                                        TokenType::Comma => {
                                            position = position + 2;
                                        }
                                        TokenType::RightParen => {
                                            match &tokens[position + 2].token_type {
                                                TokenType::LeftBrace => {
                                                    let (body, next_position) =
                                                        parse_stmt(tokens, position + 2)?;
                                                    if let Stmt::Block(body) = body {
                                                        return Ok((
                                                            Stmt::Fn(
                                                                name.to_string(),
                                                                Rc::new(parameters),
                                                                Rc::new(body),
                                                            ),
                                                            next_position,
                                                        ));
                                                    }
                                                }
                                                _ => {
                                                    return Err(ParseError::MissingFnLeftBrace(
                                                        position,
                                                    ));
                                                }
                                            }
                                        }
                                        _ => panic!(
                                            "Expect only ',' or ')' after function parameter"
                                        ),
                                    }
                                }
                                _ => panic!("Expect only parameters or ')' after '{} ('", name),
                            }
                        }
                    }
                    _ => panic!("Expect '(' after function name: {}", name),
                },
                _ => panic!("Expect function name after 'fn' keyword"),
            }
        }
        TokenType::Class => {
            assert!(position + 1 < tokens.len());
            match &tokens[position + 1].token_type {
                TokenType::Identifier(name) => match &tokens[position + 2].token_type {
                    TokenType::LeftBrace => {
                        let mut methods = Vec::new();
                        let mut position = position + 3;
                        loop {
                            match &tokens[position].token_type {
                                TokenType::RightBrace => {
                                    return Ok((
                                        Stmt::Class(name.to_owned(), Rc::new(methods)),
                                        position + 1,
                                    ));
                                }
                                TokenType::Fn => {
                                    let (method, next_position) = parse_decl(tokens, position)?;
                                    methods.push(method);
                                    position = next_position;
                                }
                                _ => panic!("Only functions or '}}' allowed in class body"),
                            }
                        }
                    }
                    _ => panic!("Expected '{{' after 'class {}'", name),
                },
                _ => panic!("Expected name after 'class'"),
            }
        }
        _ => parse_stmt(tokens, position),
    }
}

fn parse_stmt(tokens: &Vec<Token>, position: usize) -> Result<(Stmt, usize), ParseError> {
    match tokens[position].token_type {
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
            Err(ParseError::MissingBlockClosingBrace(
                tokens.last().unwrap().line,
            ))
        }
        TokenType::If => {
            let (expr, position) = parse_expr(tokens, position + 1)?;

            match tokens[position].token_type {
                TokenType::LeftBrace => {
                    let (if_stmt, position) = parse_stmt(tokens, position)?;
                    match tokens[position].token_type {
                        TokenType::Else => match tokens[position + 1].token_type {
                            TokenType::LeftBrace => {
                                let (else_stmt, position) = parse_stmt(tokens, position + 1)?;
                                Ok((
                                    Stmt::If(expr, Box::new(if_stmt), Some(Box::new(else_stmt))),
                                    position,
                                ))
                            }
                            _ => Err(ParseError::MissingElseStartingBrace(tokens[position].line)),
                        },
                        _ => Ok((Stmt::If(expr, Box::new(if_stmt), None), position)),
                    }
                }
                _ => Err(ParseError::MissingIfStartingBrace(tokens[position].line)),
            }
        }
        TokenType::While => {
            let (expr, position) = parse_expr(tokens, position + 1)?;
            match tokens[position].token_type {
                TokenType::LeftBrace => {
                    let (stmt, position) = parse_stmt(tokens, position)?;
                    Ok((Stmt::While(expr, Box::new(stmt)), position))
                }
                _ => Err(ParseError::MissingWhileStartingBrace(tokens[position].line)),
            }
        }
        TokenType::Return => match tokens[position + 1].token_type {
            TokenType::Semicolon => Ok((Stmt::Return(Box::new(Expr::None)), position + 2)),
            _ => {
                let (expr, next_position) = parse_expr(tokens, position + 1)?;
                match tokens[next_position].token_type {
                    TokenType::Semicolon => Ok((Stmt::Return(Box::new(expr)), next_position + 1)),
                    _ => Err(ParseError::MissingReturnSemicolon(
                        tokens[next_position].line,
                    )),
                }
            }
        },
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
    let (expr, position) = parse_logical_or(&tokens, position)?;

    match tokens[position].token_type {
        TokenType::Equal => match expr {
            Expr::Variable(name, _) => {
                let (right, next_position) = parse_assignment(tokens, position + 1)?;
                Ok((
                    Expr::Assign(name, Box::new(right), RefCell::new(None)),
                    next_position,
                ))
            }
            Expr::Get(expr, name) => {
                let (right, next_position) = parse_assignment(tokens, position + 1)?;
                Ok((Expr::Set(expr, name, Rc::new(right)), next_position))
            }
            _ => Err(ParseError::InvalidAssignmentLValue(tokens[position].line)),
        },
        _ => Ok((expr, position)),
    }
}

fn parse_logical_or(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    let (mut expr, mut position) = parse_logical_and(&tokens, position)?;

    loop {
        assert!(position < tokens.len());

        match tokens[position].token_type {
            TokenType::Or => {
                let (right, next_position) = parse_logical_or(tokens, position + 1)?;
                expr = Expr::BinaryLogical(Operation::LogicalOr, Box::new(expr), Box::new(right));
                position = next_position;
            }
            _ => {
                break;
            }
        }
    }
    Ok((expr, position))
}

fn parse_logical_and(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    let (mut expr, mut position) = parse_equality(&tokens, position)?;

    loop {
        assert!(position < tokens.len());

        match tokens[position].token_type {
            TokenType::And => {
                let (right, next_position) = parse_logical_and(tokens, position + 1)?;
                expr = Expr::BinaryLogical(Operation::LogicalAnd, Box::new(expr), Box::new(right));
                position = next_position;
            }
            _ => {
                break;
            }
        }
    }
    Ok((expr, position))
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

    let mut prev_expr: Option<Expr> = None;
    loop {
        assert!(position < tokens.len());

        // syntactic sugar
        // a < b < c => a < b and b < c
        // (< (< a b) c) => (and (< a b) (< b c))
        match tokens[position].token_type {
            TokenType::Less | TokenType::LessEqual => match prev_expr {
                None => {
                    let token_type = &tokens[position].token_type;
                    let (right, next_position) = parse_term(tokens, position + 1)?;
                    let operation = match token_type {
                        TokenType::Less => Operation::Less,
                        TokenType::LessEqual => Operation::LessEqual,
                        _ => unreachable!(),
                    };
                    expr = Expr::Binary(operation, Box::new(expr), Box::new(right.clone()));

                    position = next_position;
                    prev_expr = Some(right);
                }
                Some(prev_right) => {
                    let token_type = &tokens[position].token_type;
                    let (right, next_position) = parse_term(tokens, position + 1)?;

                    let operation = match token_type {
                        TokenType::Less => Operation::Less,
                        TokenType::LessEqual => Operation::LessEqual,
                        _ => unreachable!(),
                    };
                    expr = Expr::BinaryLogical(
                        Operation::LogicalAnd,
                        Box::new(expr),
                        Box::new(Expr::Binary(
                            operation,
                            Box::new(prev_right),
                            Box::new(right.clone()),
                        )),
                    );

                    position = next_position;
                    prev_expr = Some(right);
                }
            },
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
        _ => parse_call(tokens, position),
    }
}

fn parse_call(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
    let (mut expr, mut position) = parse_primary(tokens, position)?;
    loop {
        match &tokens[position].token_type {
            TokenType::LeftParen => {
                let mut args = Vec::new();

                position = position + 1;
                loop {
                    match &tokens[position].token_type {
                        TokenType::RightParen => {
                            expr = Expr::Call(Box::new(expr), args);
                            return Ok((expr, position + 1));
                        }
                        _ => {
                            let (arg_expr, next_position) = parse_expr(tokens, position)?;
                            args.push(arg_expr);
                            position = next_position;
                            if args.len() > 4 {
                                return Err(ParseError::TooManyArgumentsInCall(
                                    tokens[position].line,
                                ));
                            }

                            match &tokens[position].token_type {
                                TokenType::RightParen => {
                                    expr = Expr::Call(Box::new(expr), args);
                                    return Ok((expr, position + 1));
                                }
                                TokenType::Comma => {
                                    position += 1;
                                }
                                _ => {
                                    return Err(ParseError::InvalidFnCallArg(tokens[position].line))
                                }
                            }
                        }
                    }
                }
            }
            TokenType::Dot => match &tokens[position + 1].token_type {
                TokenType::Identifier(name) => {
                    expr = Expr::Get(Rc::new(expr), name.to_owned());
                    position = position + 2;
                }
                _ => {
                    return Err(ParseError::InvalidGet(tokens[position].line));
                }
            },
            _ => return Ok((expr, position)),
        }
    }
}

fn parse_primary(tokens: &Vec<Token>, position: usize) -> Result<(Expr, usize), ParseError> {
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
        TokenType::Identifier(name) => Ok((
            Expr::Variable(name.to_string(), RefCell::new(None)),
            position + 1,
        )),
        TokenType::Me => Ok((
            Expr::Me(tokens[position].lexeme.to_string(), RefCell::new(None)),
            position + 1,
        )),
        TokenType::Eof => Err(ParseError::UnexpectedEof(tokens[position].line)),
        _ => Err(ParseError::InvalidPrimaryToken(
            tokens[position].line,
            tokens[position].lexeme.clone(), // TODO: remove clone
        )),
    }
}
