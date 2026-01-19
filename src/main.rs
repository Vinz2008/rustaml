#![allow(clippy::needless_return)]
#![allow(clippy::let_and_return)]
#![feature(debug_closure_helpers)]

use std::{path::PathBuf, process::ExitCode};

use clap::{Parser, Subcommand};
use debug_with_context::DebugWrapContext;
use cfg_if::cfg_if;


cfg_if! {
    if #[cfg(feature = "native")] {
        use crate::compiler::OptionalArgs;
    } else {
        use crate::rustaml::FrontendOutput;
        pub(crate) struct OptionalArgs;
        impl OptionalArgs {
            pub(crate) fn new(_optimization_level : Option<u8>, _keep_temp : bool, _disable_gc : bool, _enable_sanitizer : bool, _enable_debuginfos : bool, _freestanding : bool, _lib_search_paths : Vec<String>) -> OptionalArgs {
                OptionalArgs
            }
        }
    }

}

use crate::profiler::ProfilerFormat;
use crate::rustaml::{frontend, RustamlContext};


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
mod print_warnings;
mod rustaml;
mod debug;
mod types;
mod types_debug;
mod mangle;
mod check;
mod profiler;



cfg_if! {
    if #[cfg(feature = "native")] {
        mod compiler;
        use crate::compiler::compile;
    }
}

cfg_if! {
    if #[cfg(feature = "repl")]{
        mod repl;
        use crate::repl::repl;
    }
}

cfg_if! {
    if #[cfg(feature = "dot-format")]{
        mod ast_dot;
        use crate::ast_dot::generate_ast_dot;
    } else {
        use crate::ast::ASTRef;
        use std::io;
        pub(crate) fn generate_ast_dot(_rustaml_context : &RustamlContext, _ast : ASTRef) -> io::Result<()>{
            Ok(())
        }
    }
}

cfg_if! {
    if #[cfg(feature = "cache")] {
        mod cache;
    } else {
    }
}


// TODO : replace dbg calls for println (buffered print and use of stdout)

// TODO : create lsp server

// TODO : replace clap with lexopt or pico-args ?

// TODO : add regex (in interpreter with regex crate and do custom implementation in std.c)

#[derive(Subcommand, Debug)]
enum Commands {
    /// intepret file
    Interpret {
        #[arg(value_name = "FILE")]
        filename: PathBuf,

        #[arg(long, default_value_t = false)]
        dump_jit_ir : bool,

        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,

        #[arg(long, default_value_t = false)]
        self_profile : bool,

        #[arg(long, value_enum, default_value_t = ProfilerFormat::Txt)]
        profile_format : ProfilerFormat,

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

        // TODO: why the sanitizer doesn't work ?
        #[arg(long, default_value_t = false)]
        enable_sanitizer : bool,

        #[arg(long, short = 'g', default_value_t = false)]
        enable_debuginfos : bool,

        #[arg(short = 'L', value_name = "DIR", num_args = 1, action = clap::ArgAction::Append)]
        lib_search_paths: Vec<String>,

        #[arg(long, default_value_t = false)]
        freestanding : bool,

        // TODO : add a flag to build statically libgc

        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,

        #[arg(long, default_value_t = false)]
        self_profile : bool,

        #[arg(long, value_enum, default_value_t = ProfilerFormat::Txt)]
        profile_format : ProfilerFormat,
    },
    Check {
        #[arg(value_name = "FILE")]
        filename: PathBuf,

        #[arg(long, default_value_t = false)]
        dump_types : bool,

        #[arg(long, default_value_t = false)]
        dump_dot : bool,


        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,
        
        #[arg(long, default_value_t = false)]
        self_profile : bool,

        #[arg(long, value_enum, default_value_t = ProfilerFormat::Txt)]
        profile_format : ProfilerFormat,
    },
    Repl {
        #[arg(long, short = 'd', default_value_t = false)]
        debug_print : bool,

        #[arg(long, default_value_t = false)]
        self_profile : bool,

        #[arg(long, value_enum, default_value_t = ProfilerFormat::Txt)]
        profile_format : ProfilerFormat,
    },
}

#[derive(Parser, Default, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[cfg(not(feature = "native"))]
pub(crate) fn compile(_frontend_output : FrontendOutput, _rustaml_context: &mut RustamlContext, _filename : &Path, _filename_out : Option<&Path>, _optional_args : OptionalArgs){
    panic!("the compiler feature was not enabled");
}

#[cfg(not(feature = "repl"))]
pub(crate) fn repl(_rustaml_context : &mut RustamlContext){
    panic!("the repl feature was not enabled");
}

fn main() -> ExitCode {

    #[cfg(feature = "human-panic")]
    setup_panic!();


    let args = Args::parse();

    let (debug_print, self_profile, profiler_format) = match args.command {
        Some(Commands::Check { filename: _, dump_types: _, dump_dot: _, debug_print, self_profile, profile_format }) => (debug_print, self_profile, profile_format),
        Some(Commands::Compile { filename: _, filename_out: _, keep_temp: _, optimization_level: _, disable_gc: _, enable_sanitizer: _, debug_print, self_profile, profile_format, enable_debuginfos: _, lib_search_paths: _, freestanding: _ }) => {
            (debug_print, self_profile, profile_format)
        },
        Some(Commands::Interpret { filename: _, dump_jit_ir: _, debug_print, self_profile, profile_format }) => {
            (debug_print, self_profile, profile_format)
        },
        Some(Commands::Repl { debug_print, self_profile, profile_format }) => {
            (debug_print, self_profile, profile_format)
        },
        _ => (false, false, ProfilerFormat::Txt),
    };

    let mut rustaml_context = RustamlContext::new(debug_print, self_profile);

    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename, dump_jit_ir, debug_print: _, self_profile: _, profile_format: _ } => {

            let frontend_output = frontend(&filename, &mut rustaml_context);
            let frontend_output = match frontend_output {
                Ok(f) => f,
                Err(()) => return ExitCode::FAILURE,
            };

            // after removing this, remove pub(crate)at the ast_pool.1
            debug_println!(debug_print, "var types = {:#?}", DebugWrapContext::new(&rustaml_context.ast_pool.ast_node_types, &rustaml_context));

            interpreter::interpret(frontend_output.ast, &mut rustaml_context, Some(frontend_output.type_infos), dump_jit_ir);
        }
        Commands::Compile { filename, filename_out, keep_temp, optimization_level, disable_gc, enable_sanitizer, debug_print: _, self_profile: _, profile_format: _, enable_debuginfos, lib_search_paths, freestanding } => {

            let frontend_output = frontend(&filename, &mut rustaml_context);
            let frontend_output = match frontend_output {
                Ok(f) => f,
                Err(()) => return ExitCode::FAILURE,
            };

            // TODO : proper error printing
            //debug_println!(debug_print, "var types = {:#?}", DebugWrapContext::new(&vars, &rustaml_context));
            let compile_argument = OptionalArgs::new(optimization_level, keep_temp, disable_gc, enable_sanitizer, enable_debuginfos, freestanding, lib_search_paths);
            compile(frontend_output, &mut rustaml_context,  &filename, filename_out.as_deref(), compile_argument);
        },

        Commands::Check { filename, dump_types, dump_dot, debug_print: _, self_profile: _, profile_format: _ } => {
            let frontend_output = frontend(&filename, &mut rustaml_context);
            let frontend_output = match frontend_output {
                Ok(f) => f,
                Err(()) => return ExitCode::FAILURE,
            };

            if dump_types {
                dump_typed_ast(&rustaml_context, frontend_output.ast).unwrap();
            }

            if dump_dot {
                generate_ast_dot(&rustaml_context, frontend_output.ast).unwrap();
            }

            
        },
        Commands::Repl { debug_print: _, self_profile: _, profile_format: _ } => {
            repl(&mut rustaml_context);
        }
    }

    rustaml_context.dump(profiler_format);

    ExitCode::SUCCESS
}
