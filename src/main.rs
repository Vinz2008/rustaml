#![allow(clippy::needless_return)]
#![allow(clippy::box_collection)]
#![allow(clippy::let_and_return)]
#![feature(debug_closure_helpers)]

use std::{env::var, path::{Path, PathBuf}, process::ExitCode};

use clap::{Parser, Subcommand};
use debug_with_context::DebugWrapContext;


use crate::{rustaml::{get_ast, RustamlContext}};



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
    } else {
        use crate::ast::{ASTRef, Type};
        use rustc_hash::FxHashMap;
        use crate::string_intern::StringRef;
    }
}

cfg_if! {
    if #[cfg(feature = "repl")]{
        mod repl;
        use crate::repl::repl;
    } else {

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

        #[arg(long, default_value_t = true)]
        enable_gc : bool,

        // TODO : add a flag to build statically libgc

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
    },
    Repl {
        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,
    },
}

#[derive(Parser, Default, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[cfg(not(feature = "native"))]
pub fn compile(_ast : ASTRef, _vars: FxHashMap<StringRef, Type>, _rustaml_context: &RustamlContext, _filename : &Path, _filename_out : Option<&Path>, _optimization_level : u8, _keep_temp : bool, _enable_gc : bool) {
    panic!("the compiler feature was not enabled");
}

#[cfg(not(feature = "repl"))]
pub fn repl(_rustaml_context : &mut RustamlContext){
    panic!("the repl feature was not enabled");
}

fn main() -> ExitCode {

    #[cfg(feature = "human-panic")]
    setup_panic!();


    let args = Args::parse();

    let (dump_inference, debug_print) = match args.command {
        Some(Commands::Check { filename: _, dump_inference, debug_print }) => (dump_inference, debug_print),
        Some(Commands::Compile { filename: _, filename_out: _, keep_temp: _, optimization_level: _, enable_gc: _, debug_print }) => {
            (false, debug_print)
        },
        Some(Commands::Interpret { filename: _, debug_print }) => {
            (false, debug_print)
        },
        Some(Commands::Repl { debug_print }) => {
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
                Err(()) => return ExitCode::FAILURE,
            };

            interpreter::interpret(ast, &mut rustaml_context);
        }
        Commands::Compile { filename, filename_out, keep_temp, optimization_level, enable_gc, debug_print: _ } => {
            let ast_and_vars = get_ast(&filename, &mut rustaml_context);
            let (ast, vars) = match ast_and_vars {
                Ok(a_v) => a_v,
                Err(()) => return ExitCode::FAILURE,
            };

            debug_println!(debug_print, "var types = {:#?}", DebugWrapContext::new(&vars, &rustaml_context));

            compile(ast, vars, &rustaml_context, &filename, filename_out.as_deref(), optimization_level.unwrap_or(0), keep_temp, enable_gc);
        },

        Commands::Check { filename, dump_inference, debug_print: _ } => {
            let ast_and_vars = get_ast(&filename, &mut rustaml_context);
            let _ast_v = match ast_and_vars {
                Ok(a_v) => a_v,
                Err(()) => return ExitCode::FAILURE,
            };

            if dump_inference {
                rustaml_context.dump_inference.borrow().dump(Path::new("infer.dump"), &rustaml_context).unwrap();
            }

            
        },
        Commands::Repl { debug_print: _ } => {
            repl(&mut rustaml_context);
        }
    }
    ExitCode::SUCCESS
}
