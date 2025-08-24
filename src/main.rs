#![allow(clippy::needless_return)]
#![allow(clippy::box_collection)]
#![allow(clippy::let_and_return)]
#![feature(debug_closure_helpers)]

use std::{path::PathBuf, process::ExitCode};

use clap::{Parser, Subcommand};
use debug_with_context::DebugWrapContext;

#[cfg(not(feature = "native"))]
use crate::rustaml::FrontendOutput;

use crate::{rustaml::{frontend, RustamlContext}};
use crate::types_debug::dump_typed_ast;

#[cfg(not(feature = "native"))]
use std::path::Path;


#[cfg(feature = "human-panic")]
use human_panic::setup_panic;

mod ast;
mod interpreter;
mod lexer;
mod string_intern;
mod print_error;
mod rustaml;
mod debug;
mod gc;
mod types;
mod types_debug;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "native")] {
        mod compiler;
        mod compiler_utils;
        mod compiler_match;

        use crate::compiler::compile;
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

        // TODO : make this work 
        #[arg(long, default_value_t = false)]
        disable_gc : bool,

        #[arg(long, default_value_t = false)]
        enable_sanitizer : bool,

        // TODO : add a flag to build statically libgc

        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,
    },
    Check {
        #[arg(value_name = "FILE")]
        filename: PathBuf,

        #[arg(long, default_value_t = false)]
        dump_types : bool,


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
pub fn compile(_frontend_output : FrontendOutput, _rustaml_context: &mut RustamlContext, _filename : &Path, _filename_out : Option<&Path>, _optimization_level : u8, _keep_temp : bool, _disable_gc : bool, _enable_sanitizer : bool) {
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

    let debug_print = match args.command {
        Some(Commands::Check { filename: _, dump_types: _, debug_print }) => debug_print,
        Some(Commands::Compile { filename: _, filename_out: _, keep_temp: _, optimization_level: _, disable_gc: _, enable_sanitizer: _, debug_print }) => {
            debug_print
        },
        Some(Commands::Interpret { filename: _, debug_print }) => {
            debug_print
        },
        Some(Commands::Repl { debug_print }) => {
            debug_print
        },
        _ => false,
    };

    let mut rustaml_context = RustamlContext::new(debug_print);

    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename, debug_print: _ } => {

            let frontend_output = frontend(&filename, &mut rustaml_context);
            let frontend_output = match frontend_output {
                Ok(f) => f,
                Err(()) => return ExitCode::FAILURE,
            };

            // after removing this, remove pub at the ast_pool.1
            debug_println!(debug_print, "var types = {:#?}", DebugWrapContext::new(&rustaml_context.ast_pool.ast_node_types, &rustaml_context));

            interpreter::interpret(frontend_output.ast, &mut rustaml_context);
        }
        Commands::Compile { filename, filename_out, keep_temp, optimization_level, disable_gc, enable_sanitizer, debug_print: _ } => {

            // create a frontend fonction instead of get_ast ?
            let frontend_output = frontend(&filename, &mut rustaml_context);
            let frontend_output = match frontend_output {
                Ok(f) => f,
                Err(()) => return ExitCode::FAILURE,
            };

            // TODO : proper error printing
            //debug_println!(debug_print, "var types = {:#?}", DebugWrapContext::new(&vars, &rustaml_context));

            compile(frontend_output, &mut rustaml_context,  &filename, filename_out.as_deref(), optimization_level.unwrap_or(0), keep_temp, disable_gc, enable_sanitizer);
        },

        Commands::Check { filename, dump_types, debug_print: _ } => {
            let frontend_output = frontend(&filename, &mut rustaml_context);
            let frontend_output = match frontend_output {
                Ok(f) => f,
                Err(()) => return ExitCode::FAILURE,
            };

            if dump_types {
                dump_typed_ast(&rustaml_context, frontend_output.ast).unwrap();
            }

            
        },
        Commands::Repl { debug_print: _ } => {
            repl(&mut rustaml_context);
        }
    }
    ExitCode::SUCCESS
}
