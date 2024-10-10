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

    #[test]
    fn or_and() {
        let mut out = Some(String::new());
        let program = r#"
            print("hi" or 2);
            print(nil or "yes");
            print(nil or nil or "yes yes"); 
            print(nil and "never");
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"hi
yes
yes yes
nil
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    #[test]
    fn while_loop() {
        let mut out = Some(String::new());
        let program = r#"
            let a = 0;
            let b = 1;
            let temp;
    
            while a < 100 {
              temp = a + b;
              a = b;
              b = temp;
              print(a);
            }
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"1
1
2
3
5
8
13
21
34
55
89
144
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    #[test]
    fn less_than_sugar() {
        let mut out = Some(String::new());
        let program = r#"
            let a = 0;
            let b = 1;
            let c = 2;
            let d = 3;

            if a < b < c < d {
            print("a < b < c < d");
            }

            if d < b < c < a {
            print("FAIL: d < b < c < a");
            }

            if a <= b <= c <= c {
            print("a <= b <= c <= c");
            }
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"a < b < c < d
a <= b <= c <= c
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    #[test]
    fn function() {
        let mut out = Some(String::new());
        let program = r#"
            fn add_one(n) {
            let a = n + 1;
            print(a);
            }
            add_one(-1);

            fn count(n) {
            if 1 < n {
                count(n - 1);
            }
            print(n);
            }
            count(3);

            fn sum(a, b, c) {
            print(a + b + c);
            }
            sum(1, 2, 3);
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"0
1
2
3
6
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    #[test]
    fn function_return() {
        let mut out = Some(String::new());
        let program = r#"
            fn fib(n) {
                if n <= 1 {
                    return n;
                }
                return fib(n - 2) + fib(n - 1);
            }
            print(fib(7));
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"13
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    #[test]
    fn closures() {
        let mut out = Some(String::new());
        let program = r#"
            fn make_counter() {
                let i = 0;
                fn count() {
                    i = i + 1;
                    print(i);
                }

                return count;
            }

            let counter = make_counter();
            counter();
            counter();

            let counter_again = make_counter();
            counter_again();
            counter_again();
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"1
2
1
2
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    #[test]
    fn scopes_shadowing() {
        let mut out = Some(String::new());
        let program = r#"
            {
                let a = "outer";
                {
                    print(a);
                    let a = "inner";
                    print(a);
                }
                print(a);
                let a = "outer2";
                print(a);
            }
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"outer
inner
outer
outer2
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    #[test]
    fn closures_variables() {
        let mut out = Some(String::new());
        let program = r#"
            let a = "global";
            {
                fn show_a() {
                    print(a);
                }

                show_a();
                let a = "block";
                show_a();
            }
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"global
global
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    #[test]
    fn closures_assignments() {
        let mut out = Some(String::new());
        let program = r#"
            let a = 0;
            {
                fn inc_a() {
                    a = a + 1;
                    print(a);
                }

                inc_a();
                let a = 10;
                inc_a();
                print(a);
            }
        "#;
        run(program.to_string(), &mut out);
        let expected = r#"1
2
10
"#;
        assert_eq!(out, Some(expected.to_string()));
    }

    // TODO: finish test
    // #[test]
    // fn resolver_error_variable_in_its_own_initializer() {
    // }
}
