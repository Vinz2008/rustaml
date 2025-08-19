use cfg_if::cfg_if;

use crate::{ast::{self, ASTPool, ASTRef}, interpreter::ListPool, lexer, print_error, string_intern::StrInterner, types::{resolve_and_typecheck, TypeInfos}};
use std::{fs, path::Path};

pub struct RustamlContext {
    pub str_interner : StrInterner,
    pub ast_pool : ASTPool,
    pub list_node_pool : ListPool,

    pub is_debug_print : bool,
}

impl RustamlContext {
    pub fn new(is_debug_print : bool) -> RustamlContext {
        RustamlContext { 
            str_interner: StrInterner::new(), 
            ast_pool: ASTPool::new(), 
            list_node_pool: ListPool::new(), 
            is_debug_print,
        }
    }
}

pub fn get_ast_from_string(rustaml_context : &mut RustamlContext, content : Vec<char>, content_str: Option<&str>, filename : &Path) -> Result<ASTRef, ()> /*Result<(ASTRef, FxHashMap<StringRef, Type>), ()>*/ {
    
    let content_str = match content_str {
        Some(c) => c,
        None => &content.iter().collect::<String>(),
    };

    let tokens = lexer::lex(content, rustaml_context.is_debug_print);
    let tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            print_error::print_lexer_error(e, filename, content_str);
            return Err(())
        },
    };

    let ast = ast::parse(tokens, rustaml_context);

    let ast = match ast {
        Ok(a) => a,
        Err(e) => {
            print_error::print_parser_error(e, filename, &content_str);
            return Err(());
        }
    };

    Ok(ast)
}


pub struct FrontendOutput {
    pub ast : ASTRef,
    pub type_infos : TypeInfos,
}

// used for every command (used for code deduplication)
pub fn frontend(filename : &Path, rustaml_context : &mut RustamlContext) -> Result<FrontendOutput, ()> {
    let content_bytes = fs::read(filename).unwrap_or_else(|err| {
            panic!("Error when opening {} : {}", filename.display(), err)
    });
    let content = String::from_utf8(content_bytes).unwrap_or_else(|err| {
        panic!("Invalid UTF-8 in {}: {}", filename.display(), err);
    });

    let content_chars = content.chars().collect::<Vec<_>>();
    
    let ast = get_ast_from_string(rustaml_context, content_chars, Some(&content), filename)?;

    let type_infos = match resolve_and_typecheck(rustaml_context, ast) {
        Ok(t) => t,
        Err(e) => { 
            print_error::print_type_error(e, filename, &content);
            return Err(());
        }
    };

    Ok(FrontendOutput {
        ast,
        type_infos,
    })
}


cfg_if! {
    if #[cfg(feature = "stack-expand")]{
        const RED_ZONE : usize = 100 * 1024; // 100KB
        const STACK_PER_RECURSION : usize = 1024 * 1024; // 1MB
        #[inline]
        pub fn ensure_stack<R>(f : impl FnOnce() -> R) -> R {

            #[cfg(feature = "stack-expand-test-print")]
            {
                let enough_space = match stacker::remaining_stack() {
                    Some(remaining) => remaining >= RED_ZONE,
                    None => false,
                };
                if !enough_space {
                    println!("STACK EXPAND");
                }
                
            }
            
            stacker::maybe_grow(RED_ZONE, STACK_PER_RECURSION, f)
        }
    } else {
        #[inline(always)]
        pub fn ensure_stack<R>(f : impl FnOnce() -> R) -> R {
            f()
        }
    }
}