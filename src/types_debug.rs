use std::{fmt, fs::File, io::{self, Write}};

use debug_with_context::{DebugWithContext, DebugWrapContext};


use crate::{ast::{ASTNode, ASTRef, PatternRef, Type}, rustaml::RustamlContext, string_intern::StringRef};

pub  struct PrintTypedContext {
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

/*

// TODO : make derive generic for contexts and fix the part with lifetimes parameters
impl<'a> DebugWithContext<PrintTypedContext<'a>> for ASTNode {
        fn fmt_with_context(
            &self,
            f: &mut ::std::fmt::Formatter,
            context: &PrintTypedContext<'a>,
        ) -> ::std::fmt::Result {
            match self {
                Self::TopLevel { nodes } => {
                    f.debug_struct("TopLevel")
                        .field_with(
                            "nodes",
                            |fmt| { nodes.fmt_with_context(fmt, context) },
                        )
                        .finish()
                }
                Self::FunctionDefinition { name, args, body, return_type } => {
                    f.debug_struct("FunctionDefinition")
                        .field_with(
                            "name",
                            |fmt| { name.fmt_with_context(fmt, context) },
                        )
                        .field_with(
                            "args",
                            |fmt| { args.fmt_with_context(fmt, context.rustaml_context) },
                        )
                        .field_with(
                            "body",
                            |fmt| { body.fmt_with_context(fmt, context) },
                        )
                        .field_with(
                            "return_type",
                            |fmt| { return_type.fmt_with_context(fmt, context) },
                        )
                        .finish()
                }
                Self::VarDecl { name, val, body, var_type } => {
                    f.debug_struct("VarDecl")
                        .field_with(
                            "name",
                            |fmt| { name.fmt_with_context(fmt, context) },
                        )
                        .field_with("val", |fmt| { val.fmt_with_context(fmt, context) })
                        .field_with(#[debug_context(RustamlContext)]
                            "body",
                            |fmt| { body.fmt_with_context(fmt, context) },
                        )
                        .field_with(
                            "var_type",
                            |fmt| { var_type.fmt_with_context(fmt, context) },
                        )
                        .finish()
                }
                Self::VarUse { name } => {
                    f.debug_struct("VarUse")
                        .field_with(
                            "name",
                            |fmt| { name.fmt_with_context(fmt, context) },
                        )
                        .finish()
                }
                Self::IfExpr { cond_expr, then_body, else_body } => {
                    f.debug_struct("IfExpr")
                        .field_with(
                            "cond_expr",
                            |fmt| { cond_expr.fmt_with_context(fmt, context) },
                        )
                        .field_with(
                            "then_body",
                            |fmt| { then_body.fmt_with_context(fmt, context) },
                        )
                        .field_with(
                            "else_body",
                            |fmt| { else_body.fmt_with_context(fmt, context) },
                        )
                        .finish()
                }
                Self::MatchExpr { matched_expr, patterns } => {
                    f.debug_struct("MatchExpr")
                        .field_with(
                            "matched_expr",
                            |fmt| { matched_expr.fmt_with_context(fmt, context) },
                        )
                        .field_with(
                            "patterns",
                            |fmt| { patterns.fmt_with_context(fmt, context) },
                        )
                        .finish()
                }
                Self::Integer { nb } => {
                    f.debug_struct("Integer")
                        .field_with("nb", |fmt| { nb.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::Float { nb } => {
                    f.debug_struct("Float")
                        .field_with("nb", |fmt| { nb.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::String { str } => {
                    f.debug_struct("String")
                        .field_with("str", |fmt| { str.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::List { list } => {
                    f.debug_struct("List")
                        .field_with(
                            "list",
                            |fmt| { list.fmt_with_context(fmt, context) },
                        )
                        .finish()
                }
                Self::Boolean { b } => {
                    f.debug_struct("Boolean")
                        .field_with("b", |fmt| { b.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::BinaryOp { op, lhs, rhs } => {
                    f.debug_struct("BinaryOp")
                        .field_with("op", |fmt| { op.fmt_with_context(fmt, context.rustaml_context) })
                        .field_with("lhs", |fmt| { lhs.fmt_with_context(fmt, context) })
                        .field_with("rhs", |fmt| { rhs.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::FunctionCall { name, args } => {
                    f.debug_struct("FunctionCall")
                        .field_with(
                            "name",
                            |fmt| { name.fmt_with_context(fmt, context) },
                        )
                        .field_with(
                            "args",
                            |fmt| { args.fmt_with_context(fmt, context) },
                        )
                        .finish()
                }
                Self::Unit => f.write_fmt(format_args!("Unit")),
            }
        }
    }

    impl<'a> DebugWithContext<PrintTypedContext<'a>> for Pattern {
        fn fmt_with_context(
            &self,
            f: &mut ::std::fmt::Formatter,
            context: &PrintTypedContext,
        ) -> ::std::fmt::Result {
            match self {
                Self::VarName(arg0) => {
                    f.debug_tuple("VarName")
                        .field_with(|fmt| { arg0.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::Integer(arg0) => {
                    f.debug_tuple("Integer")
                        .field_with(|fmt| { arg0.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::Float(arg0) => {
                    f.debug_tuple("Float")
                        .field_with(|fmt| { arg0.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::Range(arg0, arg1, arg2) => {
                    f.debug_tuple("Range")
                        .field_with(|fmt| { arg0.fmt_with_context(fmt, context) })
                        .field_with(|fmt| { arg1.fmt_with_context(fmt, context) })
                        .field_with(|fmt| { arg2.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::String(arg0) => {
                    f.debug_tuple("String")
                        .field_with(|fmt| { arg0.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::List(arg0) => {
                    f.debug_tuple("List")
                        .field_with(|fmt| { arg0.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::ListDestructure(arg0, arg1) => {
                    f.debug_tuple("ListDestructure")
                        .field_with(|fmt| { arg0.fmt_with_context(fmt, context) })
                        .field_with(|fmt| { arg1.fmt_with_context(fmt, context) })
                        .finish()
                }
                Self::Underscore => f.write_fmt(format_args!("Underscore")),
            }
        }
    }*/

pub fn dump_typed_ast(rustaml_context : &RustamlContext, ast : ASTRef) -> io::Result<()> {
    let context = PrintTypedContext {
        rustaml_context: rustaml_context.clone(), // TODO : remove this clone ?
    };
    let mut file = File::create("types.dump")?;
    writeln!(&mut file, "ast_typed : {:#?}", DebugWrapContext::new(&ast, &context))?;

    Ok(())
}