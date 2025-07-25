#![allow(clippy::needless_return)]
#![allow(clippy::box_collection)]
#![allow(clippy::let_and_return)]
#![feature(debug_closure_helpers)]

use std::{fs, path::{Path, PathBuf}, process::ExitCode};

use clap::{Parser, Subcommand};
use rustc_hash::FxHashMap;


use crate::{ast::{ASTRef, Type}, rustaml::RustamlContext, string_intern::StringRef};

#[cfg(feature = "human-panic")]
use human_panic::setup_panic;

mod ast;
mod interpreter;
mod lexer;
mod type_inference;
mod type_inference_debug;
mod string_intern;
mod print_error;
mod rustaml;
mod debug;
mod gc;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "native")] {
        mod compiler;
        mod compiler_utils;

        use crate::compiler::compile;
    }
}


// TODO : replace dbg calls for println (buffered print and use of stdout)

// TODO : create lsp server

// TODO : replace clap with lexopt or pico-args ?

#[derive(Subcommand, Debug)]
enum Commands {
    /// intepret file
    Interpret {
        #[arg(value_name = "FILE")]
        filename: PathBuf,

        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,
    },
    /// compile file
    Compile {
        #[arg(value_name = "FILE")]
        filename: PathBuf,

        #[arg(short = 'o', value_name = "FILE")]
        filename_out: Option<PathBuf>,

        #[arg(long, default_value_t = false)]
        keep_temp : bool,

        #[arg(short = 'O', value_parser = clap::value_parser!(u8).range(0..=3), num_args(0..=1))]
        optimization_level : Option<u8>,

        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,
    },
    Check {
        #[arg(value_name = "FILE")]
        filename: PathBuf,

        #[arg(long, default_value_t = false)]
        dump_inference : bool,


        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,
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
    let tokens = lexer::lex(content, rustaml_context.is_debug_print);
    let tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            let content = &String::from_utf8(content_bytes).unwrap();
            print_error::print_lexer_error(e, filename, content);
            return Err(ExitCode::FAILURE)
        },
    };

    let ast_and_vars = ast::parse(tokens, rustaml_context);
    let (ast, vars) = match ast_and_vars {
        Ok(a_v) => a_v,
        Err(e) => {
            let content = &String::from_utf8(content_bytes).unwrap();
            print_error::print_parser_error(e, filename, content);
            return Err(ExitCode::FAILURE);
        },
    };

    Ok((ast, vars))

}

#[cfg(not(feature = "native"))]
pub fn compile(_ast : ASTRef, _vars: FxHashMap<StringRef, Type>, _rustaml_context: &RustamlContext, _filename : &Path, _filename_out : Option<&Path>, _optimization_level : u8, _keep_temp : bool) -> ExitCode {
    panic!("the compiler feature was not enabled");
}

fn main() -> ExitCode {

    #[cfg(feature = "human-panic")]
    setup_panic!();


    let args = Args::parse();

    /*let mut str_interner = StrInterner::new();
    let mut ast_pool = ASTPool::new();*/

    let (dump_inference, debug_print) = match args.command {
        Some(Commands::Check { filename: _, dump_inference, debug_print }) => (dump_inference, debug_print),
        Some(Commands::Compile { filename: _, filename_out: _, keep_temp: _, optimization_level: _, debug_print }) => {
            (false, debug_print)
        },
        Some(Commands::Interpret { filename: _, debug_print }) => {
            (false, debug_print)
        },
        _ => (false, false),
    };

    let mut rustaml_context = RustamlContext::new(dump_inference, debug_print);

    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename, debug_print: _ } => {
            let ast_and_vars = get_ast(&filename, &mut rustaml_context);
            let (ast, _vars) = match ast_and_vars {
                Ok(a_v) => a_v,
                Err(e) => return e,
            };

            interpreter::interpret(ast, &mut rustaml_context)
        }
        Commands::Compile { filename, filename_out, keep_temp, optimization_level, debug_print: _ } => {
            let ast_and_vars = get_ast(&filename, &mut rustaml_context);
            let (ast, vars) = match ast_and_vars {
                Ok(a_v) => a_v,
                Err(e) => return e,
            };
            compile(ast, vars, &rustaml_context, &filename, filename_out.as_deref(), optimization_level.unwrap_or(0), keep_temp)
        },

        Commands::Check { filename, dump_inference: _, debug_print: _ } => {
            let ast_and_vars = get_ast(&filename, &mut rustaml_context);
            let _ast_v = match ast_and_vars {
                Ok(a_v) => a_v,
                Err(e) => return e,
            };

            if dump_inference {
                rustaml_context.dump_inference.borrow().dump(Path::new("infer.dump"), &rustaml_context).unwrap();
            }

            ExitCode::SUCCESS
        },
    }
}
