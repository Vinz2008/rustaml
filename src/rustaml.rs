use rustc_hash::FxHashMap;

use crate::{ast::{self, ASTPool, ASTRef, Type}, interpreter::ListPool, lexer, print_error, string_intern::{StrInterner, StringRef}, type_inference_debug::DumpInfer};
use std::{cell::RefCell, fs, path::Path};

pub struct RustamlContext {
    pub str_interner : StrInterner,
    pub ast_pool : ASTPool,
    pub list_node_pool : ListPool,

    pub dump_inference : RefCell<DumpInfer>,

    pub is_debug_print : bool,
}

impl RustamlContext {
    pub fn new(dump_inference : bool, is_debug_print : bool) -> RustamlContext {
        RustamlContext { 
            str_interner: StrInterner::new(), 
            ast_pool: ASTPool::new(), 
            list_node_pool: ListPool::new(), 
            dump_inference: RefCell::new(DumpInfer::new(dump_inference)),
            is_debug_print,
        }
    }
}

pub fn get_ast_from_string(rustaml_context : &mut RustamlContext, content : Vec<char>, content_str: Option<String>, filename : &Path) -> Result<(ASTRef, FxHashMap<StringRef, Type>), ()> {
    
    let content_str = match content_str {
        Some(c) => c,
        None => content.iter().collect(),
    };

    let tokens = lexer::lex(content, rustaml_context.is_debug_print);
    let tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            print_error::print_lexer_error(e, filename, &content_str);
            return Err(())
        },
    };

    let ast_and_vars = ast::parse(tokens, rustaml_context);
    let (ast, vars) = match ast_and_vars {
        Ok(a_v) => a_v,
        Err(e) => {
            print_error::print_parser_error(e, filename, &content_str);
            return Err(());
        },
    };

    Ok((ast, vars))
}

// used for every command (used for code deduplication)
pub fn get_ast(filename : &Path, rustaml_context : &mut RustamlContext) -> Result<(ASTRef, FxHashMap<StringRef, Type>), ()> {
    let content_bytes = fs::read(filename).unwrap_or_else(|err| {
            panic!("Error when opening {} : {}", filename.display(), err)
    });
    let content = String::from_utf8(content_bytes).unwrap_or_else(|err| {
        panic!("Invalid UTF-8 in {}: {}", filename.display(), err);
    });
    let content_chars = content.chars().collect::<Vec<_>>();
    
    get_ast_from_string(rustaml_context, content_chars, Some(content), filename)
}