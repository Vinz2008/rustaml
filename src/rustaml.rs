use crate::{ast::ASTPool, intepreter::ListPool, string_intern::StrInterner};

pub struct RustamlContext {
    pub str_interner : StrInterner,
    pub ast_pool : ASTPool,
    pub list_node_pool : ListPool,
}

impl RustamlContext {
    pub fn new() -> RustamlContext {
        RustamlContext { str_interner: StrInterner::new(), ast_pool: ASTPool::new(), list_node_pool: ListPool::new() }
    }
}