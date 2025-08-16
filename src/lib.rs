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
pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod string_intern;
pub mod print_error;
pub mod debug;
pub mod gc;
pub mod types;
pub mod types_debug;

cfg_if! {
    if #[cfg(feature = "native")] {
        pub mod compiler;
        pub mod compiler_utils;
    }
}


// make it not return ExitCode, just a empty error ?
pub fn interpret_code(code : &str, filename : &Path, is_debug_print  : bool) -> Result<(), ()> {
    let mut rustaml_context = RustamlContext::new(is_debug_print);
    let content = code.chars().collect::<Vec<_>>();
    let tokens = lexer::lex(content, is_debug_print);
    let tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            print_error::print_lexer_error(e, filename, code);
            return Err(());
        },
    };

    /*let ast_and_vars = ast::parse(tokens, &mut rustaml_context);
    let (ast, _vars) = match ast_and_vars {
        Ok(a_v) => a_v,
        Err(e) => {
            print_error::print_parser_error(e, filename, code);
            return Err(());
        },
    };*/

    let ast = ast::parse(tokens, &mut rustaml_context);
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