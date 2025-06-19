use crate::{ast::ASTPool, string_intern::StrInterner};

pub struct RustamlContext {
    pub str_interner : StrInterner,
    pub ast_pool : ASTPool,
}

impl RustamlContext {
    pub fn new(str_interner : StrInterner, ast_pool : ASTPool) -> RustamlContext {
        RustamlContext { str_interner, ast_pool }
    }
}