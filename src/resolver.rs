use std::{cell::RefCell, collections::HashMap, error::Error, fmt, rc::Rc};

use crate::parser::{Expr, Stmt};

#[derive(Debug)]
pub enum ResolveError {
    UnresolvedVariableOrFn(String),
    VariableInItsOwnInitializer(String),
    TopLevelReturn,
    // TODO: ReturnOutsideFunction
}

impl Error for ResolveError {}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // TODO: add more info
            ResolveError::UnresolvedVariableOrFn(name) => {
                write!(f, "Unresolved variable or function {}", name)
            }
            ResolveError::VariableInItsOwnInitializer(name) => {
                write!(
                    f,
                    "Variable in own initializer 'let {} = ... {}'",
                    name, name
                )
            }
            ResolveError::TopLevelReturn => {
                write!(f, "Cannot return from outside a function.",)
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

#[derive(Clone)]
enum FnType {
    None,
    Standalone,
    Method,
}

struct Resolver {
    scopes: Vec<Rc<RefCell<HashMap<String, bool>>>>,
    fn_type: FnType,
}

impl Resolver {
    fn new() -> Self {
        Resolver {
            scopes: vec![Rc::new(RefCell::new(HashMap::from_iter([
                ("print".to_string(), true),
                ("clock".to_string(), true),
            ])))],
            fn_type: FnType::None,
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

                self.resolve_function(parameters, body, FnType::Standalone)?;
                Ok(())
            }
            Stmt::Return(expr) => match self.fn_type {
                FnType::None => Err(ResolveError::TopLevelReturn),
                _ => {
                    self.resolve_expr(expr)?;
                    Ok(())
                }
            },
            Stmt::Class(name, methods) => {
                if let Some(scope) = self.scopes.last() {
                    scope.borrow_mut().insert(name.to_string(), true);
                }
                for method in methods.iter() {
                    match method {
                        Stmt::Fn(_, parameters, body) => {
                            self.resolve_function(parameters, body, FnType::Method)?;
                        }
                        _ => unreachable!("Parser guarantees this to be a function."),
                    }
                }
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
                            return Err(ResolveError::VariableInItsOwnInitializer(
                                name.to_string(),
                            ));
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
            Expr::Get(instance, _) => self.resolve_expr(instance),
            Expr::Set(instance, _, value) => {
                self.resolve_expr(instance)?;
                self.resolve_expr(value)?;
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
        // TODO: add line number
        Err(ResolveError::UnresolvedVariableOrFn(name.to_string()))
    }

    fn resolve_function(
        &mut self,
        parameters: &Rc<Vec<String>>,
        body: &Rc<Vec<Stmt>>,
        fn_type: FnType,
    ) -> Result<(), ResolveError> {
        let prev_fn_type = fn_type.clone();
        self.fn_type = fn_type;
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
        self.fn_type = prev_fn_type;
        Ok(())
    }
}
