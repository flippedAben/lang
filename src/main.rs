use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::ExitCode;

use interpreter::interpret;
use parser::parse;
use resolver::resolve;
use scanner::scan;

mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod token;
mod token_type;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} run <filename>", args[0]).unwrap();
        return ExitCode::from(2);
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap();
            run(file_contents, &mut None)
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            return ExitCode::from(2);
        }
    }
}

fn run(text: String, out: &mut Option<String>) -> ExitCode {
    match scan(text) {
        Ok(tokens) => match parse(tokens) {
            Ok(program) => match resolve(&program) {
                Ok(_) => match interpret(&program, out) {
                    Ok(_) => ExitCode::SUCCESS,
                    Err(e) => {
                        eprintln!("{}", e);
                        ExitCode::from(70)
                    }
                },
                Err(e) => {
                    eprintln!("{}", e);
                    ExitCode::from(70)
                }
            },
            Err(_) => ExitCode::from(65),
        },
        Err(_) => ExitCode::from(60),
    }
}

#[cfg(test)]
mod tests {
    use crate::run;

    #[test]
    fn block_scope_environment() {
        let mut out = Some(String::new());
        let program = r#"
            {
                let bar = "outer bar";
                let world = "outer world";
                {
                    bar = "modified bar";
                    let world = "inner world";
                    print(bar);
                    print(world);
                }
                print(bar);
                print(world);
            }
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"modified bar
inner world
modified bar
outer world
"#;
        if let Some(out) = out {
            assert_eq!(out, expected);
        }
    }

    #[test]
    fn if_else() {
        let mut out = Some(String::new());
        let program = r#"
            let x = 1;
            if x == 1 {
                print("x is 1");
            } else {
                print("x is not 1");
            }

            let y = 0;
            if y == 0 {
                print("y is 0");
            }
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"x is 1
y is 0
"#;
        if let Some(out) = out {
            assert_eq!(out, expected);
        }
    }
}
