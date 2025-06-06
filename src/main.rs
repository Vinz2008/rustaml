#![allow(clippy::needless_return)]

use std::{fs, hint::black_box, path::PathBuf, process::ExitCode};

use clap::{Parser, Subcommand};

mod ast;
mod intepreter;
mod lexer;
mod print_error;

#[derive(Subcommand, Debug)]
enum Commands {
    /// intepret file
    Interpret {
        #[arg(value_name = "FILE")]
        filename: PathBuf,
    },
    /// compile file
    Compile {
        #[arg(value_name = "FILE")]
        filename: PathBuf,
    },
}

#[derive(Parser, Default, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

// TODO : use https://crates.io/crates/ariadne for printing errors

fn main() -> ExitCode {
    let args = Args::parse();

    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename } => {
            let content = fs::read_to_string(&filename).unwrap_or_else(|err| {
                panic!("Error when opening {} : {}", filename.display(), err)
            });
            // TODO : pass char slice to lex instead ?
            let tokens = lexer::lex(content.chars().collect());
            let ast = ast::parse(tokens);
            let ast = match ast {
                Ok(a) => a,
                Err(e) => return print_error::print_parser_error(e, &filename, 0..0, &content),
            };

            intepreter::interpret(ast)
        }
        Commands::Compile { filename } => {
            let content = fs::read_to_string(&filename).unwrap_or_else(|err| {
                panic!("Error when opening {} : {}", filename.display(), err)
            });
            let tokens = lexer::lex(content.chars().collect());
            let ast = ast::parse(tokens);
            let ast = match ast {
                Ok(a) => a,
                Err(e) => return print_error::print_parser_error(e, &filename, 0..0, &content),
            };
            black_box(ast); // TODO : only for linting, remove this after adding compiler
            todo!()
        }
    }
}
