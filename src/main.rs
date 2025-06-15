#![allow(clippy::needless_return)]
#![feature(debug_closure_helpers)]

use std::{fs, hint::black_box, path::{Path, PathBuf}, process::ExitCode};

use clap::{Parser, Subcommand};

use crate::{ast::{ASTNode, ASTPool, ASTRef}, string_intern::StrInterner};

mod ast;
mod intepreter;
mod lexer;
mod type_inference;
mod string_intern;
mod print_error;


// TODO : create lsp server

// TODO : replace clap with lexopt or pico-args ?

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
fn get_ast(filename : &Path, str_interner : &mut StrInterner, ast_pool : &mut ASTPool) -> Result<ASTRef, ExitCode> {
    let content_bytes = fs::read(filename).unwrap_or_else(|err| {
            panic!("Error when opening {} : {}", filename.display(), err)
    });
    let content = content_bytes.iter().map(|b| *b as char).collect::<Vec<_>>();
    let tokens = lexer::lex(content);
    let tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            let content = &String::from_utf8(content_bytes).unwrap();
            return Err(print_error::print_lexer_error(e, filename, content))
        },
    };

    let ast = ast::parse(tokens, str_interner, ast_pool);
    let ast = match ast {
        Ok(a) => a,
        Err(e) => {
            let content = &String::from_utf8(content_bytes).unwrap();
            return Err(print_error::print_parser_error(e, filename, content))
        },
    };

    Ok(ast)

}

fn main() -> ExitCode {
    let args = Args::parse();

    let mut str_interner = StrInterner::new();
    let mut ast_pool = ASTPool::new();

    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename } => {
            let ast = get_ast(&filename, &mut str_interner, &mut ast_pool);
            let ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };

            intepreter::interpret(ast, &mut str_interner,&mut ast_pool)
        }
        Commands::Compile { filename } => {
            let ast = get_ast(&filename, &mut str_interner, &mut ast_pool);
            let ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };
            black_box(ast); // TODO : only for linting, remove this after adding compiler
            todo!()
        },

        Commands::Check { filename } => {
            let ast = get_ast(&filename, &mut str_interner,&mut ast_pool);
            let _ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };

            ExitCode::SUCCESS
        },
    }
}
