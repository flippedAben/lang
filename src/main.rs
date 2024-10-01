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
        writeln!(io::stderr(), "Usage: {} run <filename>", args[0]).unwrap();
        return ExitCode::from(2);
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap();
            match scan(file_contents) {
                Ok(tokens) => match parse(tokens) {
                    Ok(program) => {
                        println!("{:?}", program);
                        match interpret(program) {
                            Ok(_) => ExitCode::SUCCESS,
                            Err(e) => {
                                eprintln!("{}", e);
                                ExitCode::from(70)
                            }
                        }
                    }
                    Err(_) => ExitCode::from(65),
                },
                Err(_) => ExitCode::from(60),
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return ExitCode::from(2);
        }
    }
}
