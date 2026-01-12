#![allow(clippy::needless_return)]
#![allow(clippy::box_collection)]
#![allow(clippy::let_and_return)]
#![feature(debug_closure_helpers)]

use std::path::Path;

#[cfg(feature = "native")]
use std::process::ExitCode;

use cfg_if::cfg_if;

use crate::{check::check_ast, print_error::print_check_error, rustaml::{RustamlContext, get_ast_from_string}, types::resolve_and_typecheck};

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

cfg_if! {
    if #[cfg(feature = "cache")]{
        mod cache;
    }
}

// make it not return ExitCode, just a empty error ?
// TODO : add self profiling ?
pub fn interpret_code(code : &str, filename : &Path, is_debug_print : bool) -> Result<(), ()> {
    let content = code.chars().collect::<Vec<_>>();
    let mut rustaml_context = RustamlContext::new(is_debug_print, false);

    let ast = get_ast_from_string(&mut rustaml_context, content, Some(code), filename, None)?;

    if let Err(e) = resolve_and_typecheck(&mut rustaml_context, ast){
        print_error::print_type_error(e, filename, code);
        return Err(());
    }

    if let Err(check_error) = check_ast(&rustaml_context, filename, code, ast){
        print_check_error(check_error, filename, code);
        return Err(());
    }

    interpreter::interpret(ast, &mut rustaml_context);
    Ok(())
}

// TODO : add compiler

#[cfg(feature = "native")]
pub(crate) fn compile(_code : &str, _filename : &Path) -> Result<String, ExitCode>{
    todo!()
}