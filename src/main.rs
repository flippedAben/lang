use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::ExitCode;

use interpreter::{interpret, interpret_expr, Environment};
use parser::{parse, parse_expr};
use scanner::scan;

mod interpreter;
mod parser;
mod scanner;
mod token;
mod token_type;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return ExitCode::from(2);
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap();
            let (tokens, return_code) = scan(file_contents);
            if return_code > 0 {
                return ExitCode::from(return_code);
            }
            let (program, has_error) = parse(tokens);
            if has_error {
                ExitCode::from(65)
            } else {
                match interpret(program) {
                    Ok(_) => ExitCode::SUCCESS,
                    Err(e) => {
                        eprintln!("{}", e);
                        ExitCode::from(70)
                    }
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return ExitCode::from(2);
        }
    }
}
