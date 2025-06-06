#![allow(clippy::needless_return)]

use std::{fs, hint::black_box, path::{Path, PathBuf}, process::ExitCode};

use clap::{Parser, Subcommand};

use crate::ast::ASTNode;

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
    Check {
        #[arg(value_name = "FILE")]
        filename: PathBuf,
    }
}

#[derive(Parser, Default, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

// TODO : use https://crates.io/crates/ariadne for printing errors



// used for every command (used for code deduplication)
fn get_ast(filename : &Path) -> Result<ASTNode, ExitCode> {
    let content_bytes = fs::read(filename).unwrap_or_else(|err| {
            panic!("Error when opening {} : {}", filename.display(), err)
    });
    let content = content_bytes.iter().map(|b| *b as char).collect::<Vec<_>>();
    let tokens = lexer::lex(content);
    let ast = ast::parse(tokens);
    let ast = match ast {
            Ok(a) => a,
            Err(e) => return Err(print_error::print_parser_error(e, filename, 0..0, &String::from_utf8(content_bytes).unwrap())),
    };

    Ok(ast)

}

fn main() -> ExitCode {
    let args = Args::parse();

    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename } => {
            let ast = get_ast(&filename);
            let ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };

            intepreter::interpret(ast)
        }
        Commands::Compile { filename } => {
            let ast = get_ast(&filename);
            let ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };
            black_box(ast); // TODO : only for linting, remove this after adding compiler
            todo!()
        },

        Commands::Check { filename } => {
            let ast = get_ast(&filename);
            let _ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };

            ExitCode::SUCCESS
        },
    }
}
