use std::path::Path;

use debug_with_context::DebugWrapContext;

use crate::debug_println;
use crate::rustaml::{get_ast_from_string, RustamlContext};

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::interpreter::interpret_with_val;

fn interpret_repl_code(rustaml_context : &mut RustamlContext, code : String){
    println!("input string : {}", &code);
    let code_chars = code.chars().collect();
    let ast = match get_ast_from_string(rustaml_context, code_chars, None, Path::new("")) {
        Ok(a) => a,
        Err(()) => return,
    };
    let val = interpret_with_val(ast, rustaml_context);

    debug_println!(rustaml_context.is_debug_print, "debug val : {:?}", DebugWrapContext::new(&val, rustaml_context));

    println!("val : {}", val.display(rustaml_context));
}

// TODO : make ctr-c work in long run function
pub fn repl(rustaml_context : &mut RustamlContext){
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => interpret_repl_code(rustaml_context, line),
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                panic!("Error : {}:?", err);
            }
        } 
    }
}