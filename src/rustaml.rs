use crate::{ast::ASTPool, interpreter::ListPool, string_intern::StrInterner, type_inference::DumpInfer};
use std::cell::RefCell;

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