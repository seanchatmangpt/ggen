//! Simple CLI for cleanroom testing
//!
//! This CLI is designed to be tested by cleanroom framework
//! without requiring Docker containers or external dependencies.

use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: test_cli <command> [args...]");
        eprintln!("Commands:");
        eprintln!("  echo <message>    - Echo message back");
        eprintln!("  add <a> <b>       - Add two numbers");
        eprintln!("  version           - Show version");
        eprintln!("  help              - Show this help");
        process::exit(1);
    }

    let command = &args[1];

    match command.as_str() {
        "echo" => {
            if args.len() < 3 {
                eprintln!("Error: echo requires a message");
                process::exit(1);
            }
            let message = args[2..].join(" ");
            println!("{}", message);
        }

        "add" => {
            if args.len() < 4 {
                eprintln!("Error: add requires two numbers");
                process::exit(1);
            }
            match (args[2].parse::<i32>(), args[3].parse::<i32>()) {
                (Ok(a), Ok(b)) => println!("{}", a + b),
                _ => {
                    eprintln!("Error: arguments must be numbers");
                    process::exit(1);
                }
            }
        }

        "version" => {
            println!("test_cli version 1.0.0");
        }

        "help" => {
            println!("test_cli - Simple CLI for testing");
            println!();
            println!("Commands:");
            println!("  echo <message>    - Echo message back");
            println!("  add <a> <b>       - Add two numbers");
            println!("  version           - Show version");
            println!("  help              - Show this help");
        }

        _ => {
            eprintln!("Error: unknown command '{}'", command);
            eprintln!("Run 'test_cli help' for usage");
            process::exit(1);
        }
    }
}
