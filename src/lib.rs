#![feature(debug_closure_helpers)]

use std::{path::Path, process::ExitCode};

use crate::rustaml::RustamlContext;

pub mod rustaml;
pub mod ast;
pub mod intepreter;
pub mod lexer;
pub mod type_inference;
pub mod string_intern;
pub mod print_error;
pub mod debug;

#[cfg(feature = "native")]
pub mod compiler;

pub fn interpret_code(code : &str, filename : &Path) -> Result<(), ExitCode> {
    let mut rustaml_context = RustamlContext::new();
    let content = code.chars().collect::<Vec<_>>();
    let tokens = lexer::lex(content);
    let tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            return Err(print_error::print_lexer_error(e, filename, code))
        },
    };

    let ast = ast::parse(tokens, &mut rustaml_context);
    let ast = match ast {
        Ok(a) => a,
        Err(e) => {
            return Err(print_error::print_parser_error(e, filename, code))
        },
    };
    let ex = intepreter::interpret(ast, &mut rustaml_context);
    if ex != ExitCode::SUCCESS {
        return Err(ex);
    }
    Ok(())
}

// TODO : add compiler