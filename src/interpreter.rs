use crate::parser::{Expr, Operation, Stmt};
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    fmt::{self},
    rc::Rc,
};

#[derive(Debug)]
pub enum RuntimeError {
    ExpectNumberUnaryOperand,
    ExpectNumberBinaryOperand,
    DivisionByZero,
    ExpectNumberOrStringBinaryOperand,
    UndefinedVariable(String),
    CalledNoncallable,
    CallArityMismatch,
}

impl Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::ExpectNumberUnaryOperand => write!(f, "Operand must be a number."),
            RuntimeError::ExpectNumberBinaryOperand => write!(f, "Operands must be numbers."),
            RuntimeError::DivisionByZero => write!(f, "Dividing by zero is undefined."),
            RuntimeError::ExpectNumberOrStringBinaryOperand => {
                write!(f, "Operands must be two numbers or two strings.")
            }
            RuntimeError::UndefinedVariable(name) => {
                write!(f, "Undefined variable '{}'.", name)
            }
            RuntimeError::CalledNoncallable => {
                write!(f, "Not callable.")
            }
            RuntimeError::CallArityMismatch => {
                write!(
                    f,
                    "Function call argument count does not match declaratio parameter count."
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Boolean(bool),
    Number(f64),
    Function(String, Rc<Vec<String>>, Rc<Vec<Stmt>>),
    None,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::None => write!(f, "nil"),
            Value::Function(name, params, _) => write!(f, "<fn {}({:?})>", name, params),
        }
    }
}

impl Value {
    fn to_bool(&self) -> bool {
        match self {
            Value::Boolean(x) => *x,
            Value::None => false,
            _ => true,
        }
    }

    fn equals(self, other: Value) -> bool {
        match self {
            Value::String(x) => match other {
                Value::String(y) => x == y,
                _ => false,
            },
            Value::Boolean(x) => match other {
                Value::Boolean(y) => x == y,
                _ => false,
            },
            Value::Number(x) => match other {
                Value::Number(y) => x == y,
                _ => false,
            },
            Value::None => match other {
                Value::None => true,
                _ => false,
            },
            Value::Function(name, _, _) => match other {
                // TODO: check more than just name for equality?
                Value::Function(other_name, _, _) => name == other_name,
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    map: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            map: HashMap::new(),
            enclosing: None,
        }))
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.map.get(name) {
            return Some(v.clone());
        }
        if let Some(encloser) = &self.enclosing {
            return encloser.borrow().get(name);
        }
        None
    }

    fn try_set(&mut self, name: &str, value: Value) -> Option<Value> {
        if self.map.contains_key(name) {
            return self.map.insert(name.to_string(), value);
        } else {
            if let Some(encloser) = &self.enclosing {
                return encloser.borrow_mut().try_set(name, value);
            }
            None
        }
    }
}

pub fn interpret(program: Vec<Stmt>, out: &mut Option<String>) -> Result<(), RuntimeError> {
    // TODO: add native function: clock
    // TODO: add native function: print. Remove old one
    // define global environment with native functions (for this interpreter, native means written in Rust, not Lox)
    interpret_stmt_block(&program, Environment::new(), out)
}

pub fn interpret_stmt_block(
    program: &Vec<Stmt>,
    environment: Rc<RefCell<Environment>>,
    out: &mut Option<String>,
) -> Result<(), RuntimeError> {
    for stmt in program.iter() {
        interpret_stmt(stmt, environment.clone(), out)?
    }
    Ok(())
}

pub fn interpret_stmt(
    stmt: &Stmt,
    environment: Rc<RefCell<Environment>>,
    out: &mut Option<String>,
) -> Result<(), RuntimeError> {
    match stmt {
        Stmt::Expression(expr) => {
            interpret_expr(&expr, environment.clone(), out)?;
        }
        Stmt::Print(expr) => {
            let value = interpret_expr(&expr, environment.clone(), out)?;
            match out {
                Some(out) => {
                    out.push_str(&format!("{}\n", value));
                }
                None => println!("{}", value),
            }
        }
        Stmt::Let(name, expr) => match expr {
            Some(expr) => {
                let value = interpret_expr(&expr, environment.clone(), out)?;
                environment.borrow_mut().map.insert(name.to_string(), value);
            }
            None => {
                environment
                    .borrow_mut()
                    .map
                    .insert(name.to_string(), Value::None);
            }
        },
        Stmt::Block(program) => {
            let next_environment = Rc::new(RefCell::new(Environment {
                map: HashMap::new(),
                enclosing: Some(environment.clone()),
            }));
            interpret_stmt_block(program, next_environment, out)?
        }
        Stmt::If(expr, if_stmt, else_stmt) => {
            if interpret_expr(&expr, environment.clone(), out)?.to_bool() {
                interpret_stmt(if_stmt, environment.clone(), out)?
            } else {
                if let Some(else_stmt) = else_stmt {
                    interpret_stmt(else_stmt, environment.clone(), out)?
                }
            }
        }
        Stmt::While(expr, stmt) => {
            while interpret_expr(&expr, environment.clone(), out)?.to_bool() {
                interpret_stmt(stmt, environment.clone(), out)?
            }
        }
        Stmt::Fn(name, vec, stmt) => {
            environment.borrow_mut().map.insert(
                name.to_string(),
                Value::Function(name.to_string(), vec.clone(), stmt.clone()),
            );
        }
    }
    Ok(())
}

pub fn interpret_expr(
    expr: &Expr,
    environment: Rc<RefCell<Environment>>,
    out: &mut Option<String>,
) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Group(expr) => {
            let value = interpret_expr(expr, environment, out)?;
            Ok(value)
        }
        Expr::Unary(operation, expr) => {
            let value = interpret_expr(expr, environment, out)?;
            match operation {
                Operation::Not => Ok(Value::Boolean(!value.to_bool())),
                Operation::Negate => match value {
                    Value::Number(x) => Ok(Value::Number(-x)),
                    _ => Err(RuntimeError::ExpectNumberUnaryOperand),
                },
                _ => panic!("impossible"),
            }
        }
        Expr::Binary(operation, left_expr, right_expr) => {
            let left = interpret_expr(left_expr, environment.clone(), out)?;
            let right = interpret_expr(right_expr, environment, out)?;
            match operation {
                Operation::Add => match (left, right) {
                    (Value::String(x), Value::String(y)) => {
                        Ok(Value::String(format!("{}{}", x, y)))
                    }
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                    _ => Err(RuntimeError::ExpectNumberOrStringBinaryOperand),
                },
                Operation::Subtract => match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
                    _ => Err(RuntimeError::ExpectNumberBinaryOperand),
                },
                Operation::Divide => match (left, right) {
                    (Value::Number(x), Value::Number(y)) => {
                        if y == 0.0 {
                            Err(RuntimeError::DivisionByZero)
                        } else {
                            Ok(Value::Number(x / y))
                        }
                    }
                    _ => Err(RuntimeError::ExpectNumberBinaryOperand),
                },
                Operation::Multiply => match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
                    _ => Err(RuntimeError::ExpectNumberBinaryOperand),
                },
                Operation::Equal => Ok(Value::Boolean(left.equals(right))),
                Operation::NotEqual => Ok(Value::Boolean(!left.equals(right))),
                Operation::Less => match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(x < y)),
                    _ => Err(RuntimeError::ExpectNumberBinaryOperand),
                },
                _ => unimplemented!(),
            }
        }
        Expr::BinaryLogical(op, left_expr, right_expr) => {
            let left = interpret_expr(left_expr, environment.clone(), out)?;
            match op {
                Operation::LogicalOr => {
                    if left.to_bool() {
                        return Ok(left);
                    }
                }
                Operation::LogicalAnd => {
                    if !left.to_bool() {
                        return Ok(left);
                    }
                }
                _ => unreachable!(),
            }
            interpret_expr(right_expr, environment, out)
        }
        Expr::None => Ok(Value::None),
        Expr::String(x) => Ok(Value::String(x.to_string())),
        Expr::Number(x) => Ok(Value::Number(*x)),
        Expr::Boolean(x) => Ok(Value::Boolean(*x)),
        Expr::Variable(name) => match environment.borrow().get(&name) {
            Some(value) => Ok(value.clone()), // TODO: no clone
            None => Err(RuntimeError::UndefinedVariable(name.to_string())),
        },
        Expr::Assign(name, expr) => {
            let value = interpret_expr(expr, environment.clone(), out)?;
            match environment.borrow_mut().try_set(&name, value.clone()) {
                Some(_) => Ok(value),
                None => Err(RuntimeError::UndefinedVariable(name.to_string())),
            }
        }
        Expr::Call(expr, arg_exprs) => {
            // TODO: consider the native functions.
            let callee = interpret_expr(expr, environment.clone(), out)?;
            match callee {
                Value::Function(_, params, body) => {
                    if params.len() != arg_exprs.len() {
                        Err(RuntimeError::CallArityMismatch)
                    } else {
                        let mut args = Vec::new();
                        for arg_expr in arg_exprs {
                            args.push(interpret_expr(arg_expr, environment.clone(), out)?);
                        }

                        let fn_call_environment = Rc::new(RefCell::new(Environment {
                            map: HashMap::new(),
                            enclosing: Some(environment.clone()),
                        }));
                        for (param, arg) in params.iter().zip(args) {
                            fn_call_environment
                                .borrow_mut()
                                .map
                                .insert(param.to_string(), arg);
                        }

                        interpret_stmt_block(&body, fn_call_environment, out)?;

                        // TODO: return values
                        Ok(Value::None)
                    }
                }
                _ => Err(RuntimeError::CalledNoncallable),
            }
        }
    }
}
