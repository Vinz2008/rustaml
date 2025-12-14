#![allow(clippy::needless_return)]
#![allow(clippy::box_collection)]
#![allow(clippy::let_and_return)]
#![feature(debug_closure_helpers)]

use std::path::Path;

#[cfg(feature = "native")]
use std::process::ExitCode;

use cfg_if::cfg_if;

use crate::rustaml::RustamlContext;

pub mod rustaml;
mod ast;
mod interpreter;
mod lexer;
mod string_intern;
mod print_error;
mod print_warnings;
mod debug;
mod types;
mod types_debug;
mod mangle;
mod check;
mod profiler;

cfg_if! {
    if #[cfg(feature = "native")] {
        mod compiler;
    }
}

// make it not return ExitCode, just a empty error ?
// TODO : add self profiling ?
pub fn interpret_code(code : &str, filename : &Path, is_debug_print  : bool) -> Result<(), ()> {
    let content = code.chars().collect::<Vec<_>>();
    let mut rustaml_context = RustamlContext::new(is_debug_print, false);
    let tokens = lexer::lex(content, is_debug_print);
    let tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            print_error::print_lexer_error(e, filename, code);
            return Err(());
        },
    };

    let ast = ast::parse(tokens, &mut rustaml_context, filename.to_path_buf(), None);
    let ast = match ast {
        Ok(a) => a,
        Err(e) => {
            print_error::print_parser_error(e, filename, code);
            return Err(());
        }
    };
    interpreter::interpret(ast, &mut rustaml_context);
    Ok(())
}

// TODO : add compiler

#[cfg(feature = "native")]
pub fn compile(_code : &str, _filename : &Path) -> Result<String, ExitCode>{
    todo!()
}