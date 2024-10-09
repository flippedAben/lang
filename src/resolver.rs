use std::{borrow::Borrow, cell::RefCell, collections::HashMap, error::Error, fmt, rc::Rc};

use crate::parser::{Expr, Stmt};

#[derive(Debug)]
pub enum ResolveError {
    Todo,
}

impl Error for ResolveError {}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ResolveError::Todo => write!(f, "todo"),
        }
    }
}

pub fn resolve(program: &Vec<Stmt>) -> Result<(), ResolveError> {
    let mut resolver = Resolver::new();
    for stmt in program.iter() {
        resolver.resolve_stmt(stmt);
    }
    Ok(())
}

struct Resolver {
    scopes: Vec<Rc<RefCell<HashMap<String, bool>>>>,
}

impl Resolver {
    fn new() -> Self {
        Resolver { scopes: Vec::new() }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), ResolveError> {
        match stmt {
            Stmt::Expression(expr) => self.resolve_expr(expr),
            Stmt::Let(name, expr) => {
                if let Some(scope) = self.scopes.last() {
                    scope.borrow_mut().insert(name.to_string(), false);
                }
                if let Some(expr) = expr {
                    self.resolve_expr(expr);
                }
                if let Some(scope) = self.scopes.last() {
                    scope.borrow_mut().insert(name.to_string(), true);
                }
                Ok(())
            }
            Stmt::Block(vec) => {
                self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
                for stmt in vec {
                    self.resolve_stmt(stmt);
                }
                self.scopes.pop();
                Ok(())
            }
            Stmt::If(expr, stmt, stmt1) => todo!(),
            Stmt::While(expr, stmt) => todo!(),
            Stmt::Fn(_, rc, rc1) => todo!(),
            Stmt::Return(expr) => todo!(),
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolveError> {
        match expr {
            Expr::Group(expr) => todo!(),
            Expr::Unary(operation, expr) => todo!(),
            Expr::Binary(operation, expr, expr1) => todo!(),
            Expr::String(_) => todo!(),
            Expr::Number(_) => todo!(),
            Expr::Boolean(_) => todo!(),
            Expr::None => todo!(),
            Expr::Variable(name) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(ready) = scope.borrow_mut().get(name) {
                        if !ready {
                            return Err(ResolveError::Todo);
                        }
                    }
                }
                Ok(())
            }
            Expr::Assign(_, expr) => todo!(),
            Expr::BinaryLogical(operation, expr, expr1) => todo!(),
            Expr::Call(expr, vec) => todo!(),
        }
    }
}
