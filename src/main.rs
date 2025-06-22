#![allow(clippy::needless_return)]
#![feature(debug_closure_helpers)]

use std::{fs, path::{Path, PathBuf}, process::ExitCode};

use clap::{Parser, Subcommand};
use rustc_hash::FxHashMap;


use crate::{ast::{ASTRef, Type}, rustaml::RustamlContext, string_intern::StringRef};

mod ast;
mod intepreter;
mod lexer;
mod type_inference;
mod string_intern;
mod print_error;
mod rustaml;
mod debug;

#[cfg(feature = "native")]
mod compiler;


#[cfg(feature = "native")]
use crate::compiler::compile;

// TODO : replace dbg calls for println (buffered print and use of stdout)

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

        #[arg(short = 'o', value_name = "FILE")]
        filename_out: PathBuf,

        #[arg(long, default_value_t = false)]
        should_keep_temp : bool,


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



// used for every command (used for code deduplication)
fn get_ast(filename : &Path, rustaml_context : &mut RustamlContext) -> Result<(ASTRef, FxHashMap<StringRef, Type>), ExitCode> {
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

    let ast_and_vars = ast::parse(tokens, rustaml_context);
    let (ast, vars) = match ast_and_vars {
        Ok(a_v) => a_v,
        Err(e) => {
            let content = &String::from_utf8(content_bytes).unwrap();
            return Err(print_error::print_parser_error(e, filename, content))
        },
    };

    Ok((ast, vars))

}

#[cfg(not(feature = "native"))]
pub fn compile(_ast : ASTRef, _vars: FxHashMap<StringRef, Type>, _rustaml_context: &RustamlContext, _filename : &Path, _filename_out : &Path, _should_keep_temp : bool) -> ExitCode {
    panic!("the compiler feature was not enabled");
}

fn main() -> ExitCode {
    let args = Args::parse();

    /*let mut str_interner = StrInterner::new();
    let mut ast_pool = ASTPool::new();*/

    let mut rustaml_context = RustamlContext::new();

    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename } => {
            let ast_and_vars = get_ast(&filename, &mut rustaml_context);
            let (ast, _vars) = match ast_and_vars {
                Ok(a_v) => a_v,
                Err(e) => return e,
            };

            intepreter::interpret(ast, &mut rustaml_context)
        }
        Commands::Compile { filename, filename_out, should_keep_temp } => {
            let ast_and_vars = get_ast(&filename, &mut rustaml_context);
            let (ast, vars) = match ast_and_vars {
                Ok(a_v) => a_v,
                Err(e) => return e,
            };
            compile(ast, vars, &rustaml_context, &filename, &filename_out, should_keep_temp)
        },

        Commands::Check { filename } => {
            let ast_and_vars = get_ast(&filename, &mut rustaml_context);
            let _ast_v = match ast_and_vars {
                Ok(a_v) => a_v,
                Err(e) => return e,
            };

            ExitCode::SUCCESS
        },
    }
}
