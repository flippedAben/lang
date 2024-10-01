use crate::parser::{Expr, Operation, Stmt};
use std::{cell::RefCell, collections::HashMap, error::Error, fmt, rc::Rc};

#[derive(Debug)]
pub enum RuntimeError {
    ExpectNumberUnaryOperand,
    ExpectNumberBinaryOperand,
    DivisionByZero,
    ExpectNumberOrStringBinaryOperand,
    UndefinedVariable(String),
}

impl Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::ExpectNumberUnaryOperand => write!(f, "Operand must be a number."),
            RuntimeError::ExpectNumberBinaryOperand => write!(f, "Operands must be a numbers."),
            RuntimeError::DivisionByZero => write!(f, "Dividing by zero is undefined."),
            RuntimeError::ExpectNumberOrStringBinaryOperand => {
                write!(f, "Operands must be two numbers or two strings.")
            }
            RuntimeError::UndefinedVariable(name) => {
                write!(f, "Undefined variable '{}'.", name)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Boolean(bool),
    Number(f64),
    None,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::None => write!(f, "nil"),
        }
    }
}

impl Value {
    fn to_bool(self) -> bool {
        match self {
            Value::Boolean(x) => x,
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

pub fn interpret(program: Vec<Stmt>) -> Result<(), RuntimeError> {
    interpret_stmt(program, Environment::new())
}

pub fn interpret_stmt(
    program: Vec<Stmt>,
    environment: Rc<RefCell<Environment>>,
) -> Result<(), RuntimeError> {
    for stmt in program {
        match stmt {
            Stmt::Expression(expr) => {
                interpret_expr(expr, environment.clone())?;
            }
            Stmt::Print(expr) => {
                let value = interpret_expr(expr, environment.clone())?;
                println!("{}", value);
            }
            Stmt::Var(name, expr) => match expr {
                Some(expr) => {
                    let value = interpret_expr(expr, environment.clone())?;
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
                interpret_stmt(program, next_environment)?
            }
            Stmt::If(expr, if_stmt, else_stmt) => {
                if interpret_expr(expr, environment.clone())?.to_bool() {
                    interpret_stmt(vec![*if_stmt], environment.clone())? // TODO: shouldn't be a Vec<Stmt>
                } else {
                    if let Some(else_stmt) = else_stmt {
                        interpret_stmt(vec![*else_stmt], environment.clone())?
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn interpret_expr(
    expr: Expr,
    environment: Rc<RefCell<Environment>>,
) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Group(expr) => {
            let value = interpret_expr(*expr, environment)?;
            Ok(value)
        }
        Expr::Unary(operation, expr) => {
            let value = interpret_expr(*expr, environment)?;
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
            let left = interpret_expr(*left_expr, environment.clone())?;
            let right = interpret_expr(*right_expr, environment)?;
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
                Operation::Greater => match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(x > y)),
                    _ => Err(RuntimeError::ExpectNumberBinaryOperand),
                },
                Operation::GreaterEqual => match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(x >= y)),
                    _ => Err(RuntimeError::ExpectNumberBinaryOperand),
                },
                Operation::Less => match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(x < y)),
                    _ => Err(RuntimeError::ExpectNumberBinaryOperand),
                },
                Operation::LessEqual => match (left, right) {
                    (Value::Number(x), Value::Number(y)) => Ok(Value::Boolean(x <= y)),
                    _ => Err(RuntimeError::ExpectNumberBinaryOperand),
                },
                _ => panic!("impossible"),
            }
        }
        Expr::None => Ok(Value::None),
        Expr::String(x) => Ok(Value::String(x)),
        Expr::Number(x) => Ok(Value::Number(x)),
        Expr::Boolean(x) => Ok(Value::Boolean(x)),
        Expr::Variable(name) => match environment.borrow().get(&name) {
            Some(value) => Ok(value.clone()), // TODO: no clone
            None => Err(RuntimeError::UndefinedVariable(name.to_string())),
        },
        Expr::Assign(name, expr) => {
            let value = interpret_expr(*expr, environment.clone())?;
            match environment.borrow_mut().try_set(&name, value.clone()) {
                Some(_) => Ok(value),
                None => Err(RuntimeError::UndefinedVariable(name.to_string())),
            }
        }
    }
}
