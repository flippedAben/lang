use std::{cell::RefCell, collections::HashMap, error::Error, fmt, rc::Rc};

use crate::parser::{Expr, Stmt};

#[derive(Debug)]
pub enum ResolveError {
    Todo,
    UnresolvedVariableOrFn(String),
}

impl Error for ResolveError {}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ResolveError::Todo => write!(f, "todo"),
            // TODO: add more info
            ResolveError::UnresolvedVariableOrFn(name) => {
                write!(f, "Unresolved variable or function {}", name)
            }
        }
    }
}

pub fn resolve(program: &Vec<Stmt>) -> Result<(), ResolveError> {
    let mut resolver = Resolver::new();
    for stmt in program.iter() {
        resolver.resolve_stmt(stmt)?;
    }
    Ok(())
}

struct Resolver {
    scopes: Vec<Rc<RefCell<HashMap<String, bool>>>>,
}

impl Resolver {
    fn new() -> Self {
        Resolver {
            scopes: vec![Rc::new(RefCell::new(HashMap::from_iter([
                ("print".to_string(), true),
                ("clock".to_string(), true),
            ])))],
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), ResolveError> {
        match stmt {
            Stmt::Expression(expr) => self.resolve_expr(expr),
            Stmt::Let(name, expr) => {
                if let Some(scope) = self.scopes.last() {
                    scope.borrow_mut().insert(name.to_string(), false);
                }
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
                if let Some(scope) = self.scopes.last() {
                    scope.borrow_mut().insert(name.to_string(), true);
                }
                Ok(())
            }
            Stmt::Block(body) => {
                self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
                for stmt in body {
                    self.resolve_stmt(stmt)?;
                }
                self.scopes.pop();
                Ok(())
            }
            Stmt::If(condition, if_stmt, else_stmt) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(if_stmt)?;
                if let Some(else_stmt) = else_stmt {
                    self.resolve_stmt(else_stmt)?;
                }
                Ok(())
            }
            Stmt::While(condition, body) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;
                Ok(())
            }
            Stmt::Fn(name, parameters, body) => {
                if let Some(scope) = self.scopes.last() {
                    scope.borrow_mut().insert(name.to_string(), true);
                }

                self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
                for parameter in parameters.iter() {
                    if let Some(scope) = self.scopes.last() {
                        scope.borrow_mut().insert(parameter.to_string(), true);
                    }
                }

                for stmt in body.iter() {
                    self.resolve_stmt(stmt)?;
                }

                self.scopes.pop();
                Ok(())
            }
            Stmt::Return(expr) => {
                self.resolve_expr(expr)?;
                Ok(())
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolveError> {
        match expr {
            Expr::Group(expr) => self.resolve_expr(expr),
            Expr::Unary(_, expr) => self.resolve_expr(expr),
            Expr::Binary(_, left, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
                Ok(())
            }
            Expr::String(_) => Ok(()),
            Expr::Number(_) => Ok(()),
            Expr::Boolean(_) => Ok(()),
            Expr::None => Ok(()),
            Expr::Variable(name, semantic_depth) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(ready) = scope.borrow_mut().get(name) {
                        if !ready {
                            return Err(ResolveError::Todo);
                        }
                    }
                }
                *semantic_depth.borrow_mut() = Some(self.resolve_variable(name)?);
                Ok(())
            }
            Expr::Assign(name, expr, semantic_depth) => {
                self.resolve_expr(expr)?;
                *semantic_depth.borrow_mut() = Some(self.resolve_variable(name)?);
                Ok(())
            }
            Expr::BinaryLogical(_, left, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
                Ok(())
            }
            Expr::Call(callee, args) => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
        }
    }

    fn resolve_variable(&mut self, name: &String) -> Result<usize, ResolveError> {
        let mut distance: usize = 0;
        for scope in self.scopes.iter().rev() {
            if scope.borrow().contains_key(name) {
                return Ok(distance);
            }
            distance += 1;
        }
        Err(ResolveError::UnresolvedVariableOrFn(name.to_string()))
    }
}
