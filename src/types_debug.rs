use std::{fmt, fs::File, io::{self, Write}};

use debug_with_context::{DebugWithContext, DebugWrapContext};


use crate::{ast::{ASTNode, ASTRef, PatternRef, Type}, rustaml::RustamlContext, string_intern::StringRef};

pub(crate) struct PrintTypedContext {
    rustaml_context : RustamlContext,
}

struct AstWithType<'a> {
    node_type : &'a Type,
    ast_node : &'a ASTNode,
}


impl<'a> DebugWithContext<PrintTypedContext> for AstWithType<'a> {
    fn fmt_with_context(&self, f: &mut fmt::Formatter<'_>, context: &PrintTypedContext) -> fmt::Result {
        f.debug_struct("AstWithType").field("node_type", &self.node_type).field_with("ast_node", |fmt| self.ast_node.fmt_with_context(fmt, context)).finish()
    }
}

impl DebugWithContext<PrintTypedContext> for ASTRef {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, context : &PrintTypedContext) -> std::fmt::Result {
        
        let ast_with_type = AstWithType {
            ast_node: self.get(&context.rustaml_context.ast_pool),
            node_type: self.get_type(&context.rustaml_context.ast_pool),
        };
        ast_with_type.fmt_with_context(f, context)
    }
}

impl DebugWithContext<PrintTypedContext> for PatternRef {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, context: &PrintTypedContext) -> std::fmt::Result {
        self.get(&context.rustaml_context.pattern_pool).fmt_with_context(f, context)
    }
}

impl DebugWithContext<PrintTypedContext> for StringRef {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, context : &PrintTypedContext) -> fmt::Result {
        write!(f, "{}", context.rustaml_context.str_interner.lookup(*self))
    }
}

// need to prevent not used warning because only called in main.rs but need the struct PrintTypedContext for derives
#[allow(dead_code)]
pub(crate) fn dump_typed_ast(rustaml_context : &RustamlContext, ast : ASTRef) -> io::Result<()> {
    let context = PrintTypedContext {
        rustaml_context: rustaml_context.clone(), // TODO : remove this clone ?
    };
    let mut file = File::create("types.dump")?;
    writeln!(&mut file, "ast_typed : {:#?}", DebugWrapContext::new(&ast, &context))?;

    Ok(())
}