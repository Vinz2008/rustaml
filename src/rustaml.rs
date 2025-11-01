use cfg_if::cfg_if;
use levenshtein::levenshtein;
use rustc_hash::FxHashSet;

use crate::{ast::{self, ASTPool, ASTRef, PatternPool}, check::check_ast, interpreter::ListPool, lexer, print_error::{self, print_check_error}, string_intern::StrInterner, types::{TypeInfos, resolve_and_typecheck}};
use std::{fs, path::{Path, PathBuf}};

cfg_if! {
    if #[cfg(feature = "native")] {
        use crate::compiler::debuginfo::ContentLoc;
    } else {
        #[derive(Clone)]
        pub struct ContentLoc;

        impl ContentLoc {
            fn new(_v : &[char]) -> ContentLoc { ContentLoc }
        }
    }
}

// TODO : remove the clone and recursively in every types in it after removing the clone it types_debug
#[derive(Clone)]
pub struct RustamlContext {
    pub str_interner : StrInterner,
    pub ast_pool : ASTPool,
    pub pattern_pool : PatternPool,
    pub list_node_pool : ListPool,

    pub is_debug_print : bool,
    pub content : Option<ContentLoc>,
}

impl RustamlContext {
    pub fn new(is_debug_print : bool) -> RustamlContext {
        RustamlContext { 
            str_interner: StrInterner::new(), 
            ast_pool: ASTPool::new(),
            pattern_pool: PatternPool::new(),
            list_node_pool: ListPool::new(),
            is_debug_print,
            content: None,
        }
    }

    pub fn set_content_chars(&mut self, content_chars : &[char]){
        self.content = Some(ContentLoc::new(content_chars));
    }
}

pub fn nearest_string<'a>(searched_str : &str, strings : impl IntoIterator<Item = &'a str>, default : Option<&'a str>) -> Option<&'a str> {
    let mut min_distance = usize::MAX;
    let mut nearest = default;

    for current_s in strings {
        let current_distance = levenshtein(searched_str, current_s);
        if current_distance < min_distance {
            min_distance = current_distance;
            nearest = Some(current_s);
        }
    }

    nearest
}

pub fn get_ast_from_string(rustaml_context : &mut RustamlContext, content : Vec<char>, content_str: Option<&str>, filename : &Path, already_added_filenames : Option<&mut FxHashSet<PathBuf>>) -> Result<ASTRef, ()> {
    
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

    let ast = ast::parse(tokens, rustaml_context, filename.to_path_buf(), already_added_filenames);

    let ast = match ast {
        Ok(a) => a,
        Err(e) => {
            print_error::print_parser_error(e, filename, content_str);
            return Err(());
        }
    };

    Ok(ast)
}


pub struct FrontendOutput {
    pub ast : ASTRef,
    pub type_infos : TypeInfos,
}

pub fn read_file(filename : &Path) -> String {
    let content_bytes = fs::read(filename).unwrap_or_else(|err| {
            panic!("Error when opening {} : {}", filename.display(), err)
    });
    let content = String::from_utf8(content_bytes).unwrap_or_else(|err| {
        panic!("Invalid UTF-8 in {}: {}", filename.display(), err);
    });

    content
}

// used for every command (used for code deduplication)
pub fn frontend(filename : &Path, rustaml_context : &mut RustamlContext) -> Result<FrontendOutput, ()> {
    let content = read_file(filename);

    let content_chars = content.chars().collect::<Vec<_>>();

    rustaml_context.set_content_chars(&content_chars); // do only do this when compiling (it is only needed for debuginfos so only do when it is activated ?)
    
    let ast = get_ast_from_string(rustaml_context, content_chars, Some(&content), filename, None)?;

    let type_infos = match resolve_and_typecheck(rustaml_context, ast) {
        Ok(t) => t,
        Err(e) => { 
            print_error::print_type_error(e, filename, &content);
            return Err(());
        }
    };

    if let Err(check_error) = check_ast(rustaml_context, &type_infos, filename, &content, ast) {
        print_check_error(check_error, filename, &content);
        return Err(());
    }

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