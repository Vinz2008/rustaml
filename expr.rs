#![feature(prelude_import)]
#![allow(clippy::needless_return)]
#![feature(debug_closure_helpers)]
#[prelude_import]
use std::prelude::rust_2024::*;
#[macro_use]
extern crate std;
use std::{
    fs, hint::black_box, path::{Path, PathBuf},
    process::ExitCode,
};
use clap::{Parser, Subcommand};
use crate::{ast::ASTNode, string_intern::StrInterner};
mod ast {
    use std::ops::Range;
    use std::fmt::Debug;
    use rustc_hash::FxHashMap;
    use enum_tags::{Tag, TaggedEnum};
    use crate::{
        dbg_intern, lexer::{Operator, Token, TokenData, TokenDataTag},
        string_intern::{DebugWithInterner, StrInterner, StringRef},
        type_inference::{infer_var_type, TypeInferenceErr},
    };
    pub struct Arg {
        pub name: StringRef,
        arg_type: Type,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Arg {
        #[inline]
        fn clone(&self) -> Arg {
            Arg {
                name: ::core::clone::Clone::clone(&self.name),
                arg_type: ::core::clone::Clone::clone(&self.arg_type),
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Arg {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Arg {
        #[inline]
        fn eq(&self, other: &Arg) -> bool {
            self.name == other.name && self.arg_type == other.arg_type
        }
    }
    impl DebugWithInterner for Arg {
        fn fmt_with_interner(
            &self,
            f: &mut std::fmt::Formatter,
            interner: &StrInterner,
        ) -> std::fmt::Result {
            f.debug_struct("Arg")
                .field_with("name", |fmt| { self.name.fmt_with_interner(fmt, interner) })
                .field("arg_type", &self.arg_type)
                .finish()
        }
    }
    pub enum Pattern {
        VarName(StringRef),
        Integer(i64),
        Float(f64),
        Range(i64, i64, bool),
        String(StringRef),
        List(Vec<Pattern>),
        ListDestructure(StringRef, StringRef),
        Underscore,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Pattern {
        #[inline]
        fn clone(&self) -> Pattern {
            match self {
                Pattern::VarName(__self_0) => {
                    Pattern::VarName(::core::clone::Clone::clone(__self_0))
                }
                Pattern::Integer(__self_0) => {
                    Pattern::Integer(::core::clone::Clone::clone(__self_0))
                }
                Pattern::Float(__self_0) => {
                    Pattern::Float(::core::clone::Clone::clone(__self_0))
                }
                Pattern::Range(__self_0, __self_1, __self_2) => {
                    Pattern::Range(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                        ::core::clone::Clone::clone(__self_2),
                    )
                }
                Pattern::String(__self_0) => {
                    Pattern::String(::core::clone::Clone::clone(__self_0))
                }
                Pattern::List(__self_0) => {
                    Pattern::List(::core::clone::Clone::clone(__self_0))
                }
                Pattern::ListDestructure(__self_0, __self_1) => {
                    Pattern::ListDestructure(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
                Pattern::Underscore => Pattern::Underscore,
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Pattern {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Pattern {
        #[inline]
        fn eq(&self, other: &Pattern) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (Pattern::VarName(__self_0), Pattern::VarName(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (Pattern::Integer(__self_0), Pattern::Integer(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (Pattern::Float(__self_0), Pattern::Float(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (
                        Pattern::Range(__self_0, __self_1, __self_2),
                        Pattern::Range(__arg1_0, __arg1_1, __arg1_2),
                    ) => {
                        __self_0 == __arg1_0 && __self_1 == __arg1_1
                            && __self_2 == __arg1_2
                    }
                    (Pattern::String(__self_0), Pattern::String(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (Pattern::List(__self_0), Pattern::List(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (
                        Pattern::ListDestructure(__self_0, __self_1),
                        Pattern::ListDestructure(__arg1_0, __arg1_1),
                    ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                    _ => true,
                }
        }
    }
    impl DebugWithInterner for Pattern {
        fn fmt_with_interner(
            &self,
            f: &mut std::fmt::Formatter,
            interner: &StrInterner,
        ) -> std::fmt::Result {
            match self {
                Self::VarName(arg0) => {
                    f.debug_tuple("VarName").field(&arg0.get_str(interner)).finish()
                }
                Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
                Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
                Self::Range(arg0, arg1, arg2) => {
                    f.debug_tuple("Range").field(arg0).field(arg1).field(arg2).finish()
                }
                Self::String(arg0) => {
                    f.debug_tuple("String").field(&arg0.get_str(interner)).finish()
                }
                Self::List(arg0) => {
                    f.debug_tuple("List")
                        .field_with(|fmt| arg0.fmt_with_interner(fmt, interner))
                        .finish()
                }
                Self::ListDestructure(arg0, arg1) => {
                    f.debug_tuple("ListDestructure")
                        .field(&arg0.get_str(interner))
                        .field(&arg1.get_str(interner))
                        .finish()
                }
                Self::Underscore => f.write_fmt(format_args!("Underscore")),
            }
        }
    }
    pub enum ASTNode {
        TopLevel { nodes: Vec<ASTNode> },
        FunctionDefinition {
            name: StringRef,
            args: Vec<Arg>,
            body: Box<ASTNode>,
            return_type: Type,
        },
        VarDecl { name: StringRef, val: Box<ASTNode>, body: Option<Box<ASTNode>> },
        VarUse { name: StringRef },
        IfExpr {
            cond_expr: Box<ASTNode>,
            then_body: Box<ASTNode>,
            else_body: Box<ASTNode>,
        },
        MatchExpr { matched_expr: Box<ASTNode>, patterns: Vec<(Pattern, ASTNode)> },
        Integer { nb: i64 },
        Float { nb: f64 },
        String { str: StringRef },
        List { list: Vec<ASTNode> },
        Boolean { b: bool },
        BinaryOp { op: Operator, lhs: Box<ASTNode>, rhs: Box<ASTNode> },
        FunctionCall { name: StringRef, args: Vec<ASTNode> },
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ASTNode {
        #[inline]
        fn clone(&self) -> ASTNode {
            match self {
                ASTNode::TopLevel { nodes: __self_0 } => {
                    ASTNode::TopLevel {
                        nodes: ::core::clone::Clone::clone(__self_0),
                    }
                }
                ASTNode::FunctionDefinition {
                    name: __self_0,
                    args: __self_1,
                    body: __self_2,
                    return_type: __self_3,
                } => {
                    ASTNode::FunctionDefinition {
                        name: ::core::clone::Clone::clone(__self_0),
                        args: ::core::clone::Clone::clone(__self_1),
                        body: ::core::clone::Clone::clone(__self_2),
                        return_type: ::core::clone::Clone::clone(__self_3),
                    }
                }
                ASTNode::VarDecl { name: __self_0, val: __self_1, body: __self_2 } => {
                    ASTNode::VarDecl {
                        name: ::core::clone::Clone::clone(__self_0),
                        val: ::core::clone::Clone::clone(__self_1),
                        body: ::core::clone::Clone::clone(__self_2),
                    }
                }
                ASTNode::VarUse { name: __self_0 } => {
                    ASTNode::VarUse {
                        name: ::core::clone::Clone::clone(__self_0),
                    }
                }
                ASTNode::IfExpr {
                    cond_expr: __self_0,
                    then_body: __self_1,
                    else_body: __self_2,
                } => {
                    ASTNode::IfExpr {
                        cond_expr: ::core::clone::Clone::clone(__self_0),
                        then_body: ::core::clone::Clone::clone(__self_1),
                        else_body: ::core::clone::Clone::clone(__self_2),
                    }
                }
                ASTNode::MatchExpr { matched_expr: __self_0, patterns: __self_1 } => {
                    ASTNode::MatchExpr {
                        matched_expr: ::core::clone::Clone::clone(__self_0),
                        patterns: ::core::clone::Clone::clone(__self_1),
                    }
                }
                ASTNode::Integer { nb: __self_0 } => {
                    ASTNode::Integer {
                        nb: ::core::clone::Clone::clone(__self_0),
                    }
                }
                ASTNode::Float { nb: __self_0 } => {
                    ASTNode::Float {
                        nb: ::core::clone::Clone::clone(__self_0),
                    }
                }
                ASTNode::String { str: __self_0 } => {
                    ASTNode::String {
                        str: ::core::clone::Clone::clone(__self_0),
                    }
                }
                ASTNode::List { list: __self_0 } => {
                    ASTNode::List {
                        list: ::core::clone::Clone::clone(__self_0),
                    }
                }
                ASTNode::Boolean { b: __self_0 } => {
                    ASTNode::Boolean {
                        b: ::core::clone::Clone::clone(__self_0),
                    }
                }
                ASTNode::BinaryOp { op: __self_0, lhs: __self_1, rhs: __self_2 } => {
                    ASTNode::BinaryOp {
                        op: ::core::clone::Clone::clone(__self_0),
                        lhs: ::core::clone::Clone::clone(__self_1),
                        rhs: ::core::clone::Clone::clone(__self_2),
                    }
                }
                ASTNode::FunctionCall { name: __self_0, args: __self_1 } => {
                    ASTNode::FunctionCall {
                        name: ::core::clone::Clone::clone(__self_0),
                        args: ::core::clone::Clone::clone(__self_1),
                    }
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for ASTNode {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for ASTNode {
        #[inline]
        fn eq(&self, other: &ASTNode) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (
                        ASTNode::TopLevel { nodes: __self_0 },
                        ASTNode::TopLevel { nodes: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    (
                        ASTNode::FunctionDefinition {
                            name: __self_0,
                            args: __self_1,
                            body: __self_2,
                            return_type: __self_3,
                        },
                        ASTNode::FunctionDefinition {
                            name: __arg1_0,
                            args: __arg1_1,
                            body: __arg1_2,
                            return_type: __arg1_3,
                        },
                    ) => {
                        __self_0 == __arg1_0 && __self_1 == __arg1_1
                            && __self_2 == __arg1_2 && __self_3 == __arg1_3
                    }
                    (
                        ASTNode::VarDecl {
                            name: __self_0,
                            val: __self_1,
                            body: __self_2,
                        },
                        ASTNode::VarDecl {
                            name: __arg1_0,
                            val: __arg1_1,
                            body: __arg1_2,
                        },
                    ) => {
                        __self_0 == __arg1_0 && __self_1 == __arg1_1
                            && __self_2 == __arg1_2
                    }
                    (
                        ASTNode::VarUse { name: __self_0 },
                        ASTNode::VarUse { name: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    (
                        ASTNode::IfExpr {
                            cond_expr: __self_0,
                            then_body: __self_1,
                            else_body: __self_2,
                        },
                        ASTNode::IfExpr {
                            cond_expr: __arg1_0,
                            then_body: __arg1_1,
                            else_body: __arg1_2,
                        },
                    ) => {
                        __self_0 == __arg1_0 && __self_1 == __arg1_1
                            && __self_2 == __arg1_2
                    }
                    (
                        ASTNode::MatchExpr {
                            matched_expr: __self_0,
                            patterns: __self_1,
                        },
                        ASTNode::MatchExpr { matched_expr: __arg1_0, patterns: __arg1_1 },
                    ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                    (
                        ASTNode::Integer { nb: __self_0 },
                        ASTNode::Integer { nb: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    (
                        ASTNode::Float { nb: __self_0 },
                        ASTNode::Float { nb: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    (
                        ASTNode::String { str: __self_0 },
                        ASTNode::String { str: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    (
                        ASTNode::List { list: __self_0 },
                        ASTNode::List { list: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    (
                        ASTNode::Boolean { b: __self_0 },
                        ASTNode::Boolean { b: __arg1_0 },
                    ) => __self_0 == __arg1_0,
                    (
                        ASTNode::BinaryOp { op: __self_0, lhs: __self_1, rhs: __self_2 },
                        ASTNode::BinaryOp { op: __arg1_0, lhs: __arg1_1, rhs: __arg1_2 },
                    ) => {
                        __self_0 == __arg1_0 && __self_1 == __arg1_1
                            && __self_2 == __arg1_2
                    }
                    (
                        ASTNode::FunctionCall { name: __self_0, args: __self_1 },
                        ASTNode::FunctionCall { name: __arg1_0, args: __arg1_1 },
                    ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
        }
    }
    impl DebugWithInterner for ASTNode {
        fn fmt_with_interner(
            &self,
            f: &mut std::fmt::Formatter,
            interner: &StrInterner,
        ) -> std::fmt::Result {
            match self {
                Self::TopLevel { nodes } => {
                    f.debug_struct("TopLevel")
                        .field_with(
                            "nodes",
                            |fmt| nodes.fmt_with_interner(fmt, interner),
                        )
                        .finish()
                }
                Self::FunctionDefinition { name, args, body, return_type } => {
                    f.debug_struct("FunctionDefinition")
                        .field("name", &name.get_str(interner))
                        .field_with("args", |fmt| args.fmt_with_interner(fmt, interner))
                        .field_with("body", |fmt| body.fmt_with_interner(fmt, interner))
                        .field("return_type", return_type)
                        .finish()
                }
                Self::VarDecl { name, val, body } => {
                    f.debug_struct("VarDecl")
                        .field("name", &name.get_str(interner))
                        .field_with("val", |fmt| val.fmt_with_interner(fmt, interner))
                        .field_with("body", |fmt| body.fmt_with_interner(fmt, interner))
                        .finish()
                }
                Self::VarUse { name } => {
                    f.debug_struct("VarUse")
                        .field("name", &name.get_str(interner))
                        .finish()
                }
                Self::IfExpr { cond_expr, then_body, else_body } => {
                    f.debug_struct("IfExpr")
                        .field_with(
                            "cond_expr",
                            |fmt| cond_expr.fmt_with_interner(fmt, interner),
                        )
                        .field_with(
                            "then_body",
                            |fmt| then_body.fmt_with_interner(fmt, interner),
                        )
                        .field_with(
                            "else_body",
                            |fmt| else_body.fmt_with_interner(fmt, interner),
                        )
                        .finish()
                }
                Self::MatchExpr { matched_expr, patterns } => {
                    f.debug_struct("MatchExpr")
                        .field_with(
                            "matched_expr",
                            |fmt| matched_expr.fmt_with_interner(fmt, interner),
                        )
                        .field_with(
                            "patterns",
                            |fmt| patterns.fmt_with_interner(fmt, interner),
                        )
                        .finish()
                }
                Self::Integer { nb } => {
                    f.debug_struct("Integer").field("nb", nb).finish()
                }
                Self::Float { nb } => f.debug_struct("Float").field("nb", nb).finish(),
                Self::String { str } => {
                    f.debug_struct("String")
                        .field("str", &str.get_str(interner))
                        .finish()
                }
                Self::List { list } => {
                    f.debug_struct("List")
                        .field_with("list", |fmt| list.fmt_with_interner(fmt, interner))
                        .finish()
                }
                Self::Boolean { b } => f.debug_struct("Boolean").field("b", b).finish(),
                Self::BinaryOp { op, lhs, rhs } => {
                    f.debug_struct("BinaryOp")
                        .field("op", op)
                        .field_with("lhs", |fmt| lhs.fmt_with_interner(fmt, interner))
                        .field_with("rhs", |fmt| rhs.fmt_with_interner(fmt, interner))
                        .finish()
                }
                Self::FunctionCall { name, args } => {
                    f.debug_struct("FunctionCall")
                        .field("name", &name.get_str(interner))
                        .field_with("args", |fmt| args.fmt_with_interner(fmt, interner))
                        .finish()
                }
            }
        }
    }
    pub enum Type {
        Integer,
        Float,
        Bool,
        Function(Vec<Type>, Box<Type>),
        Str,
        List(Box<Type>),
        Any,
        Unit,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Type {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Type::Integer => ::core::fmt::Formatter::write_str(f, "Integer"),
                Type::Float => ::core::fmt::Formatter::write_str(f, "Float"),
                Type::Bool => ::core::fmt::Formatter::write_str(f, "Bool"),
                Type::Function(__self_0, __self_1) => {
                    ::core::fmt::Formatter::debug_tuple_field2_finish(
                        f,
                        "Function",
                        __self_0,
                        &__self_1,
                    )
                }
                Type::Str => ::core::fmt::Formatter::write_str(f, "Str"),
                Type::List(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "List",
                        &__self_0,
                    )
                }
                Type::Any => ::core::fmt::Formatter::write_str(f, "Any"),
                Type::Unit => ::core::fmt::Formatter::write_str(f, "Unit"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Type {
        #[inline]
        fn clone(&self) -> Type {
            match self {
                Type::Integer => Type::Integer,
                Type::Float => Type::Float,
                Type::Bool => Type::Bool,
                Type::Function(__self_0, __self_1) => {
                    Type::Function(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
                Type::Str => Type::Str,
                Type::List(__self_0) => Type::List(::core::clone::Clone::clone(__self_0)),
                Type::Any => Type::Any,
                Type::Unit => Type::Unit,
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Type {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Type {
        #[inline]
        fn eq(&self, other: &Type) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (
                        Type::Function(__self_0, __self_1),
                        Type::Function(__arg1_0, __arg1_1),
                    ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                    (Type::List(__self_0), Type::List(__arg1_0)) => __self_0 == __arg1_0,
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for Type {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<Vec<Type>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Type>>;
            let _: ::core::cmp::AssertParamIsEq<Box<Type>>;
        }
    }
    impl ASTNode {
        pub fn get_type(&self, parser: &Parser) -> Type {
            match self {
                ASTNode::Boolean { b: _ } => Type::Bool,
                ASTNode::Integer { nb: _ } => Type::Integer,
                ASTNode::Float { nb: _ } => Type::Float,
                ASTNode::String { str: _ } => Type::Str,
                ASTNode::List { list } => {
                    let elem_type = match list.first() {
                        Some(f) => f.get_type(parser),
                        None => Type::Any,
                    };
                    Type::List(Box::new(elem_type))
                }
                ASTNode::BinaryOp { op, lhs: _, rhs: _ } => op.get_type(),
                ASTNode::VarDecl { name: _, val: _, body: _ } => Type::Unit,
                ASTNode::FunctionCall { name, args: _ } => {
                    parser.vars.get(name).unwrap().clone()
                }
                ASTNode::VarUse { name } => {
                    match parser.vars.get(name) {
                        Some(t) => t.clone(),
                        None => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!(
                                        "Unknown var {0}",
                                        name.get_str(&parser.str_interner),
                                    ),
                                ),
                            );
                        }
                    }
                }
                ASTNode::IfExpr { cond_expr: _, then_body, else_body: _ } => {
                    then_body.get_type(parser)
                }
                ASTNode::MatchExpr { matched_expr: _, patterns } => {
                    patterns.first().unwrap().1.get_type(parser)
                }
                ASTNode::TopLevel { nodes: _ } => Type::Unit,
                ASTNode::FunctionDefinition {
                    name: _,
                    args: _,
                    body: _,
                    return_type: _,
                } => Type::Unit,
            }
        }
    }
    fn init_precedences() -> FxHashMap<Operator, (i32, Associativity)> {
        let mut p = FxHashMap::default();
        p.insert(Operator::IsEqual, (10, Associativity::Left));
        p.insert(Operator::Superior, (10, Associativity::Left));
        p.insert(Operator::Inferior, (10, Associativity::Left));
        p.insert(Operator::SuperiorOrEqual, (10, Associativity::Left));
        p.insert(Operator::InferiorOrEqual, (10, Associativity::Left));
        p.insert(Operator::Plus, (20, Associativity::Left));
        p.insert(Operator::Minus, (20, Associativity::Left));
        p.insert(Operator::Mult, (30, Associativity::Left));
        p.insert(Operator::Div, (30, Associativity::Left));
        p.insert(Operator::StrAppend, (5, Associativity::Right));
        p.insert(Operator::ListAppend, (6, Associativity::Right));
        p
    }
    pub enum Associativity {
        Left,
        Right,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Associativity {
        #[inline]
        fn clone(&self) -> Associativity {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Associativity {}
    pub struct Parser<'intern> {
        tokens: Vec<Token>,
        pos: usize,
        pub vars: FxHashMap<StringRef, Type>,
        precedences: FxHashMap<Operator, (i32, Associativity)>,
        pub str_interner: &'intern mut StrInterner,
    }
    pub enum ParserErrData {
        UnexpectedEOF,
        UnexpectedTok { tok: TokenData },
        WrongTok { expected_tok: TokenDataTag, got_tok: TokenData },
        TypeInferenceErr { arg_name: String },
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ParserErrData {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ParserErrData::UnexpectedEOF => {
                    ::core::fmt::Formatter::write_str(f, "UnexpectedEOF")
                }
                ParserErrData::UnexpectedTok { tok: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "UnexpectedTok",
                        "tok",
                        &__self_0,
                    )
                }
                ParserErrData::WrongTok { expected_tok: __self_0, got_tok: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "WrongTok",
                        "expected_tok",
                        __self_0,
                        "got_tok",
                        &__self_1,
                    )
                }
                ParserErrData::TypeInferenceErr { arg_name: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "TypeInferenceErr",
                        "arg_name",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub enum ParserErrDataTag {
        UnexpectedEOF,
        UnexpectedTok,
        WrongTok,
        TypeInferenceErr,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParserErrDataTag {
        #[inline]
        fn clone(&self) -> ParserErrDataTag {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for ParserErrDataTag {}
    #[automatically_derived]
    impl ::core::fmt::Debug for ParserErrDataTag {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    ParserErrDataTag::UnexpectedEOF => "UnexpectedEOF",
                    ParserErrDataTag::UnexpectedTok => "UnexpectedTok",
                    ParserErrDataTag::WrongTok => "WrongTok",
                    ParserErrDataTag::TypeInferenceErr => "TypeInferenceErr",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for ParserErrDataTag {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for ParserErrDataTag {
        #[inline]
        fn eq(&self, other: &ParserErrDataTag) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for ParserErrDataTag {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    impl ::enum_tags_traits::TaggedEnum for ParserErrData {
        type Tag = ParserErrDataTag;
        fn tag(&self) -> Self::Tag {
            match *self {
                Self::UnexpectedEOF => Self::Tag::UnexpectedEOF,
                Self::UnexpectedTok { tok: _ } => Self::Tag::UnexpectedTok,
                Self::WrongTok { expected_tok: _, got_tok: _ } => Self::Tag::WrongTok,
                Self::TypeInferenceErr { arg_name: _ } => Self::Tag::TypeInferenceErr,
            }
        }
    }
    pub struct ParserErr {
        pub parser_err_data: Box<ParserErrData>,
        pub range: Range<usize>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ParserErr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "ParserErr",
                "parser_err_data",
                &self.parser_err_data,
                "range",
                &&self.range,
            )
        }
    }
    impl ParserErr {
        pub fn new(parser_err_data: ParserErrData, range: Range<usize>) -> ParserErr {
            ParserErr {
                parser_err_data: Box::new(parser_err_data),
                range,
            }
        }
    }
    impl From<TypeInferenceErr> for ParserErr {
        fn from(err: TypeInferenceErr) -> Self {
            ParserErr::new(
                ParserErrData::TypeInferenceErr {
                    arg_name: *err.arg_name,
                },
                err.range,
            )
        }
    }
    impl Parser<'_> {
        fn has_tokens_left(&self) -> bool {
            self.pos + 1 < self.tokens.len()
        }
        fn eat_tok(
            &mut self,
            token_type: Option<TokenDataTag>,
        ) -> Result<Token, ParserErr> {
            if self.pos >= self.tokens.len() {
                let pos = self.tokens.len() - 1;
                return Err(ParserErr::new(ParserErrData::UnexpectedEOF, pos..pos));
            }
            if let Some(tok_type) = token_type
                && self.tokens[self.pos].tok_data.tag() != tok_type
            {
                return Err(
                    ParserErr::new(
                        ParserErrData::WrongTok {
                            expected_tok: tok_type,
                            got_tok: self.tokens[self.pos].tok_data.clone(),
                        },
                        self.tokens[self.pos].range.clone(),
                    ),
                );
            }
            let current_tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Ok(current_tok)
        }
        fn current_tok(&self) -> Option<&Token> {
            self.tokens.get(self.pos)
        }
        fn current_tok_data(&self) -> Option<&TokenData> {
            self.current_tok().map(|t| &t.tok_data)
        }
    }
    fn parse_integer(nb: i64) -> ASTNode {
        ASTNode::Integer { nb }
    }
    fn parse_float(nb: f64) -> ASTNode {
        ASTNode::Float { nb }
    }
    fn parse_string(parser: &mut Parser, buf: Vec<char>) -> ASTNode {
        ASTNode::String {
            str: parser.str_interner.intern(&buf.iter().collect::<String>()),
        }
    }
    fn parse_annotation_simple(parser: &mut Parser) -> Result<Type, ParserErr> {
        let tok = parser.eat_tok(None)?;
        match &tok.tok_data {
            TokenData::Identifier(b) => {
                let type_annot = match b.iter().collect::<String>().as_str() {
                    "int" => Type::Integer,
                    "bool" => Type::Bool,
                    "float" => Type::Float,
                    "str" => Type::Str,
                    _ => {
                        ::core::panicking::panic_fmt(format_args!("Unknown type"));
                    }
                };
                Ok(type_annot)
            }
            TokenData::ParenOpen => {
                parser.eat_tok(Some(TokenDataTag::ParenClose))?;
                Ok(Type::Unit)
            }
            _ => {
                Err(
                    ParserErr::new(
                        ParserErrData::UnexpectedTok {
                            tok: tok.tok_data,
                        },
                        tok.range.clone(),
                    ),
                )
            }
        }
    }
    fn parse_type_annotation(parser: &mut Parser) -> Result<Type, ParserErr> {
        parser.eat_tok(Some(TokenDataTag::Colon))?;
        let simple_type = parse_annotation_simple(parser)?;
        let type_parsed = match parser.current_tok_data() {
            Some(TokenData::Arrow) => {
                {
                    ::std::io::_print(format_args!("function type\n"));
                };
                let mut function_type_parts = <[_]>::into_vec(
                    ::alloc::boxed::box_new([simple_type]),
                );
                match parser.current_tok_data() {
                    tmp => {
                        {
                            ::std::io::_eprint(
                                format_args!(
                                    "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                    "src/ast.rs",
                                    330u32,
                                    13u32,
                                    "parser.current_tok_data()",
                                    &tmp,
                                ),
                            );
                        };
                        tmp
                    }
                };
                while let Some(t) = parser.current_tok_data()
                    && match t {
                        TokenData::Arrow => true,
                        _ => false,
                    }
                {
                    parser.eat_tok(Some(TokenDataTag::Arrow))?;
                    let function_type_part = parse_annotation_simple(parser)?;
                    function_type_parts.push(function_type_part);
                }
                let return_type = function_type_parts.pop();
                let return_type = match return_type {
                    Some(t) if !function_type_parts.is_empty() => t,
                    _ => {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "ERROR : missing type in function type annotation, found return type of {0:?} and args of {1:?}",
                                return_type,
                                function_type_parts,
                            ),
                        );
                    }
                };
                Type::Function(function_type_parts, Box::new(return_type))
            }
            _ => simple_type,
        };
        Ok(type_parsed)
    }
    fn parse_let(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
        let name_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;
        let name = match name_tok.tok_data {
            TokenData::Identifier(s) => {
                parser.str_interner.intern(&s.iter().collect::<String>())
            }
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        };
        let node = if match parser.current_tok_data() {
            Some(TokenData::Identifier(_)) => true,
            _ => false,
        } {
            let mut arg_names = Vec::new();
            let mut arg_ranges = Vec::new();
            while match parser.current_tok_data() {
                Some(TokenData::Identifier(_)) => true,
                _ => false,
            } {
                let arg_identifier = parser
                    .eat_tok(Some(TokenDataTag::Identifier))
                    .unwrap();
                let arg_name = match arg_identifier.tok_data {
                    TokenData::Identifier(s) => s.iter().collect::<String>(),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                arg_ranges.push(arg_identifier.range);
                arg_names.push(arg_name);
            }
            let function_type: Type = match parser.current_tok_data() {
                Some(TokenData::Colon) => parse_type_annotation(parser)?,
                Some(_) | None => {
                    Type::Function(
                        ::alloc::vec::from_elem(Type::Any, arg_names.len()),
                        Box::new(Type::Any),
                    )
                }
            };
            let (arg_types, mut return_type) = match function_type {
                Type::Function(a, r) => (a, r),
                _ => {
                    ::core::panicking::panic_fmt(
                        format_args!("Expected a function type"),
                    );
                }
            };
            parser
                .vars
                .insert(name, Type::Function(arg_types.clone(), return_type.clone()));
            let mut args = arg_names
                .into_iter()
                .zip(arg_types.clone())
                .map(|x| Arg {
                    name: parser.str_interner.intern(&x.0),
                    arg_type: x.1,
                })
                .collect::<Vec<Arg>>();
            for Arg { name, arg_type } in &args {
                parser.vars.insert(name.clone(), arg_type.clone());
            }
            let equal_tok = parser.eat_tok(Some(TokenDataTag::Op));
            match equal_tok.map(|t| t.tok_data) {
                Ok(TokenData::Op(Operator::Equal)) => {}
                Ok(t) => {
                    ::core::panicking::panic_fmt(
                        format_args!("expected equal in let expr, got {0:?}", t),
                    );
                }
                Err(e) => {
                    ::core::panicking::panic_fmt(
                        format_args!("Error when expecting equal in let expr : {0:?}", e),
                    );
                }
            };
            let body = parse_node(parser)?;
            if match return_type.as_ref() {
                Type::Any => true,
                _ => false,
            } {
                let body_type = body.get_type(parser);
                return_type = Box::new(body_type);
            }
            for (arg, arg_range) in args.iter_mut().zip(arg_ranges) {
                if match arg.arg_type {
                    Type::Any => true,
                    _ => false,
                } {
                    arg.arg_type = infer_var_type(parser, arg.name, &body, &arg_range)?;
                    parser.vars.insert(arg.name.clone(), arg.arg_type.clone());
                }
            }
            for Arg { name, arg_type: _ } in &args {
                parser.vars.remove(name);
            }
            parser
                .vars
                .insert(name, Type::Function(arg_types.clone(), return_type.clone()));
            ASTNode::FunctionDefinition {
                name,
                args,
                body: Box::new(body),
                return_type: *return_type,
            }
        } else {
            let mut var_type = match parser.current_tok_data() {
                Some(TokenData::Colon) => Some(parse_type_annotation(parser)?),
                Some(_) | None => None,
            };
            let tok = parser.eat_tok(Some(TokenDataTag::Op))?;
            match &tok.tok_data {
                TokenData::Op(Operator::Equal) => {}
                _ => {
                    return Err(
                        ParserErr::new(
                            ParserErrData::UnexpectedTok {
                                tok: tok.tok_data,
                            },
                            tok.range,
                        ),
                    );
                }
            };
            let val_node = parse_node(parser)?;
            if var_type.is_none() {
                var_type = Some(val_node.get_type(parser));
            }
            parser.vars.insert(name.clone(), var_type.unwrap());
            let body = match parser.current_tok_data() {
                Some(TokenData::In) => {
                    parser.eat_tok(Some(TokenDataTag::In))?;
                    Some(Box::new(parse_node(parser)?))
                }
                _ => None,
            };
            if body.is_some() {
                parser.vars.remove(&name);
            }
            ASTNode::VarDecl {
                name,
                val: Box::new(val_node),
                body,
            }
        };
        if let Some(TokenData::EndOfExpr) = parser.current_tok_data() {
            parser.eat_tok(Some(TokenDataTag::EndOfExpr)).unwrap();
        }
        Ok(node)
    }
    fn parse_function_call(
        parser: &mut Parser,
        function_name: StringRef,
    ) -> Result<ASTNode, ParserErr> {
        let mut args = Vec::new();
        fn function_call_parse_continue(tok_data: Option<&TokenData>) -> bool {
            !match tok_data {
                Some(TokenData::EndOfExpr)
                | Some(TokenData::Op(_))
                | Some(TokenData::Else)
                | Some(TokenData::In) => true,
                _ => false,
            }
        }
        while parser.has_tokens_left()
            && function_call_parse_continue(parser.current_tok_data())
        {
            let arg = parse_primary(parser)?;
            args.push(arg);
        }
        Ok(ASTNode::FunctionCall {
            name: function_name,
            args,
        })
    }
    fn parse_identifier_expr(
        parser: &mut Parser,
        identifier_buf: Vec<char>,
    ) -> Result<ASTNode, ParserErr> {
        let identifier = parser
            .str_interner
            .intern(&identifier_buf.iter().collect::<String>());
        let is_function = match parser.vars.get(&identifier) {
            Some(t) => {
                match t {
                    Type::Function(_, _) => true,
                    _ => false,
                }
            }
            None => {
                ::core::panicking::panic_fmt(
                    format_args!(
                        "ERROR : unknown identifier {0}",
                        identifier.get_str(&mut parser.str_interner),
                    ),
                );
            }
        };
        if is_function {
            parse_function_call(parser, identifier)
        } else {
            Ok(ASTNode::VarUse {
                name: identifier,
            })
        }
    }
    fn parse_if(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
        let cond_expr = parse_node(parser)?;
        match cond_expr.get_type(parser) {
            Type::Bool => {}
            t => {
                ::core::panicking::panic_fmt(
                    format_args!(
                        "Error in type checking : {0:?} type passed in if expr",
                        t,
                    ),
                );
            }
        }
        parser.eat_tok(Some(TokenDataTag::Then))?;
        let then_body = parse_node(parser)?;
        parser.eat_tok(Some(TokenDataTag::Else))?;
        let else_body = parse_node(parser)?;
        Ok(ASTNode::IfExpr {
            cond_expr: Box::new(cond_expr),
            then_body: Box::new(then_body),
            else_body: Box::new(else_body),
        })
    }
    fn parse_list_form<T, F>(
        parser: &mut Parser,
        parse_elem_fun: F,
    ) -> Result<Vec<T>, ParserErr>
    where
        F: Fn(&mut Parser) -> Result<T, ParserErr>,
    {
        let mut iter_nb = 0;
        let mut elems = Vec::new();
        while !match parser.current_tok_data() {
            Some(TokenData::ArrayClose) => true,
            _ => false,
        } {
            if iter_nb != 0 {
                parser.eat_tok(Some(TokenDataTag::Comma))?;
            }
            let elem_expr = parse_elem_fun(parser)?;
            elems.push(elem_expr);
            iter_nb += 1;
        }
        parser.eat_tok(Some(TokenDataTag::ArrayClose))?;
        Ok(elems)
    }
    fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParserErr> {
        let pattern_tok = parser.eat_tok(None)?;
        let pattern = match pattern_tok.tok_data {
            TokenData::Identifier(buf) => {
                let s = buf.iter().collect::<String>();
                match s.as_str() {
                    "_" => Pattern::Underscore,
                    s_ref => Pattern::VarName(parser.str_interner.intern(s_ref)),
                }
            }
            TokenData::Integer(nb) => {
                if match parser.current_tok_data() {
                    Some(TokenData::Range) | Some(TokenData::Op(Operator::Equal)) => true,
                    _ => false,
                } {
                    let inclusivity = match parser.current_tok_data() {
                        Some(TokenData::Op(Operator::Equal)) => true,
                        _ => false,
                    };
                    parser.eat_tok(None)?;
                    if inclusivity {
                        parser.eat_tok(Some(TokenDataTag::Range))?;
                    }
                    let end_tok = parser.eat_tok(Some(TokenDataTag::Integer))?;
                    let end_nb = match end_tok.tok_data {
                        TokenData::Integer(end) => end,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    };
                    Pattern::Range(nb, end_nb, inclusivity)
                } else {
                    Pattern::Integer(nb)
                }
            }
            TokenData::Float(nb) => Pattern::Float(nb),
            TokenData::String(s) => {
                Pattern::String(
                    parser.str_interner.intern(&s.iter().collect::<String>()),
                )
            }
            TokenData::ArrayOpen => {
                let elems = parse_list_form(parser, parse_pattern)?;
                Pattern::List(elems)
            }
            t => {
                return Err(
                    ParserErr::new(
                        ParserErrData::UnexpectedTok {
                            tok: t,
                        },
                        pattern_tok.range,
                    ),
                );
            }
        };
        Ok(pattern)
    }
    fn parse_match(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
        let matched_expr = parse_primary(parser)?;
        parser.eat_tok(Some(TokenDataTag::With))?;
        let mut patterns = Vec::new();
        while parser.current_tok().is_some()
            && match parser.current_tok_data().unwrap() {
                TokenData::Pipe => true,
                _ => false,
            }
        {
            parser.eat_tok(Some(TokenDataTag::Pipe))?;
            let pattern = parse_pattern(parser)?;
            parser.eat_tok(Some(TokenDataTag::Arrow))?;
            let pattern_expr = parse_primary(parser)?;
            patterns.push((pattern, pattern_expr));
        }
        Ok(ASTNode::MatchExpr {
            matched_expr: Box::new(matched_expr),
            patterns,
        })
    }
    fn parse_parenthesis(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
        let expr = parse_node(parser)?;
        match &expr {
            tmp => {
                {
                    ::std::io::_eprint(
                        format_args!(
                            "[{0}:{1}:{2}] {3} = {4:#?}\n",
                            "src/ast.rs",
                            635u32,
                            5u32,
                            "&expr",
                            crate::string_intern::DebugWrapInterner::new(
                                &tmp,
                                &parser.str_interner,
                            ),
                        ),
                    );
                };
                tmp
            }
        };
        parser.eat_tok(Some(TokenDataTag::ParenClose))?;
        Ok(expr)
    }
    fn parse_static_list(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
        let elems = parse_list_form(parser, parse_node)?;
        Ok(ASTNode::List { list: elems })
    }
    fn parse_primary(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
        let tok = parser.eat_tok(None).unwrap();
        let node = match tok.tok_data {
            TokenData::Let => parse_let(parser),
            TokenData::If => parse_if(parser),
            TokenData::Match => parse_match(parser),
            TokenData::Integer(nb) => Ok(parse_integer(nb)),
            TokenData::Float(nb) => Ok(parse_float(nb)),
            TokenData::String(buf) => Ok(parse_string(parser, buf)),
            TokenData::Identifier(buf) => parse_identifier_expr(parser, buf),
            TokenData::True => Ok(ASTNode::Boolean { b: true }),
            TokenData::False => Ok(ASTNode::Boolean { b: false }),
            TokenData::ParenOpen => parse_parenthesis(parser),
            TokenData::ArrayOpen => parse_static_list(parser),
            t => {
                Err(
                    ParserErr::new(
                        ParserErrData::UnexpectedTok {
                            tok: t,
                        },
                        tok.range,
                    ),
                )
            }
        };
        return node;
    }
    fn parse_binary_rec(
        parser: &mut Parser,
        lhs: ASTNode,
        min_precedence: i32,
    ) -> Result<ASTNode, ParserErr> {
        let mut lhs = lhs;
        while parser.has_tokens_left() {
            let current_tok_data = parser.current_tok_data();
            let operator = match current_tok_data {
                Some(TokenData::Op(op)) => *op,
                Some(_) | None => break,
            };
            let (first_precedence, _) = *parser.precedences.get(&operator).unwrap();
            if first_precedence < min_precedence {
                break;
            }
            parser.eat_tok(Some(TokenDataTag::Op)).unwrap();
            let mut rhs = parse_primary(parser)?;
            while parser.has_tokens_left() {
                let current_tok_data = parser.current_tok_data();
                let new_operator = match current_tok_data {
                    Some(TokenData::Op(op)) => op,
                    Some(_) | None => break,
                };
                let (precedence, associativity) = *parser
                    .precedences
                    .get(new_operator)
                    .unwrap();
                if precedence < first_precedence
                    || (precedence == first_precedence
                        && match associativity {
                            Associativity::Left => true,
                            _ => false,
                        })
                {
                    break;
                }
                let new_precedence = match associativity {
                    Associativity::Left => precedence + 1,
                    Associativity::Right => precedence,
                };
                rhs = parse_binary_rec(parser, rhs, new_precedence)?;
            }
            lhs = ASTNode::BinaryOp {
                op: operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }
    fn parse_binary(parser: &mut Parser, lhs: ASTNode) -> Result<ASTNode, ParserErr> {
        parse_binary_rec(parser, lhs, 0)
    }
    fn parse_node(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
        let lhs = parse_primary(parser)?;
        let ret_expr = parse_binary(parser, lhs)?;
        Ok(ret_expr)
    }
    fn parse_top_level_node(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
        let mut nodes: Vec<ASTNode> = Vec::new();
        while parser.has_tokens_left() {
            nodes.push(parse_node(parser)?);
        }
        Ok(ASTNode::TopLevel { nodes })
    }
    pub fn parse(
        tokens: Vec<Token>,
        str_interner: &mut StrInterner,
    ) -> Result<ASTNode, ParserErr> {
        let mut parser = Parser {
            tokens,
            pos: 0,
            vars: FxHashMap::default(),
            precedences: init_precedences(),
            str_interner: str_interner,
        };
        let root_node = parse_top_level_node(&mut parser)?;
        match &root_node {
            tmp => {
                {
                    ::std::io::_eprint(
                        format_args!(
                            "[{0}:{1}:{2}] {3} = {4:#?}\n",
                            "src/ast.rs",
                            750u32,
                            5u32,
                            "&root_node",
                            crate::string_intern::DebugWrapInterner::new(
                                &tmp,
                                str_interner,
                            ),
                        ),
                    );
                };
                tmp
            }
        };
        Ok(root_node)
    }
}
mod intepreter {
    use rustc_hash::FxHashMap;
    use std::{cmp::Ordering, process::ExitCode};
    use std::fmt;
    use crate::dbg_intern;
    use crate::string_intern::{StrInterner, StringRef, DebugWithInterner};
    use crate::{
        ast::{ASTNode, Type, Pattern},
        lexer::Operator,
    };
    enum List {
        None,
        Node(Val, Box<List>),
    }
    #[automatically_derived]
    impl ::core::clone::Clone for List {
        #[inline]
        fn clone(&self) -> List {
            match self {
                List::None => List::None,
                List::Node(__self_0, __self_1) => {
                    List::Node(
                        ::core::clone::Clone::clone(__self_0),
                        ::core::clone::Clone::clone(__self_1),
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for List {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for List {
        #[inline]
        fn eq(&self, other: &List) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (List::Node(__self_0, __self_1), List::Node(__arg1_0, __arg1_1)) => {
                        __self_0 == __arg1_0 && __self_1 == __arg1_1
                    }
                    _ => true,
                }
        }
    }
    impl List {
        fn new(context: &mut InterpretContext, v: &Vec<ASTNode>) -> List {
            let mut l = List::None;
            for e in v {
                let val = interpret_node(context, e);
                l.append(val);
            }
            l
        }
        fn append(&mut self, val: Val) {
            let mut current: &mut List = self;
            while let List::Node(_, next) = current {
                current = next.as_mut();
            }
            *current = List::Node(val, Box::new(List::None));
        }
        fn iter(self: &List) -> ListIter<'_> {
            ListIter { current: self }
        }
        fn len(&self) -> usize {
            let mut count = 0;
            let mut current: &List = self;
            while let List::Node(_, next) = current {
                current = next.as_ref();
                count += 1;
            }
            count
        }
        fn empty(&self) -> bool {
            match self {
                List::None => true,
                List::Node(_, _) => false,
            }
        }
    }
    struct ListIter<'a> {
        current: &'a List,
    }
    impl<'a> Iterator for ListIter<'a> {
        type Item = &'a Val;
        fn next(&mut self) -> Option<Self::Item> {
            match self.current {
                List::None => None,
                List::Node(v, next) => {
                    let current = v;
                    self.current = next;
                    Some(current)
                }
            }
        }
    }
    impl DebugWithInterner for List {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            let mut current: &List = self;
            let mut iter_nb = 0;
            while let List::Node(v, next) = current {
                if iter_nb != 0 {
                    f.write_fmt(format_args!(", "))?;
                }
                v.fmt_with_interner(f, interner)?;
                current = next.as_ref();
                iter_nb += 1;
            }
            Ok(())
        }
    }
    enum Val {
        Integer(i64),
        Float(f64),
        Bool(bool),
        String(StringRef),
        List(Box<List>),
        Unit,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Val {
        #[inline]
        fn clone(&self) -> Val {
            match self {
                Val::Integer(__self_0) => {
                    Val::Integer(::core::clone::Clone::clone(__self_0))
                }
                Val::Float(__self_0) => Val::Float(::core::clone::Clone::clone(__self_0)),
                Val::Bool(__self_0) => Val::Bool(::core::clone::Clone::clone(__self_0)),
                Val::String(__self_0) => {
                    Val::String(::core::clone::Clone::clone(__self_0))
                }
                Val::List(__self_0) => Val::List(::core::clone::Clone::clone(__self_0)),
                Val::Unit => Val::Unit,
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Val {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Val {
        #[inline]
        fn eq(&self, other: &Val) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (Val::Integer(__self_0), Val::Integer(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (Val::Float(__self_0), Val::Float(__arg1_0)) => __self_0 == __arg1_0,
                    (Val::Bool(__self_0), Val::Bool(__arg1_0)) => __self_0 == __arg1_0,
                    (Val::String(__self_0), Val::String(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (Val::List(__self_0), Val::List(__arg1_0)) => __self_0 == __arg1_0,
                    _ => true,
                }
        }
    }
    impl DebugWithInterner for Val {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            match self {
                Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
                Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
                Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
                Self::String(arg0) => {
                    f.debug_tuple("String")
                        .field_with(|fmt| arg0.fmt_with_interner(fmt, interner))
                        .finish()
                }
                Self::List(arg0) => {
                    f.debug_tuple("List")
                        .field_with(|fmt| arg0.fmt_with_interner(fmt, interner))
                        .finish()
                }
                Self::Unit => f.write_fmt(format_args!("Unit")),
            }
        }
    }
    impl PartialOrd for Val {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            match (self, other) {
                (Val::Integer(nb_self), Val::Integer(nb_other)) => {
                    Some(nb_self.cmp(nb_other))
                }
                (Val::Float(nb_self), Val::Float(nb_other)) => {
                    nb_self.partial_cmp(nb_other)
                }
                (Val::String(str_self), Val::String(str_other)) => {
                    str_self.partial_cmp(str_other)
                }
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
    }
    impl Val {
        fn get_type(&self) -> Type {
            match self {
                Val::Integer(_) => Type::Integer,
                Val::Float(_) => Type::Float,
                Val::Bool(_) => Type::Bool,
                Val::String(_) => Type::Str,
                Val::List(l) => {
                    let elem_type = match l.as_ref() {
                        List::Node(v, _next) => v.get_type(),
                        List::None => Type::Any,
                    };
                    Type::List(Box::new(elem_type))
                }
                Val::Unit => Type::Unit,
            }
        }
    }
    struct FunctionDef {
        name: StringRef,
        args: Vec<StringRef>,
        body: Box<ASTNode>,
        return_type: Type,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for FunctionDef {
        #[inline]
        fn clone(&self) -> FunctionDef {
            FunctionDef {
                name: ::core::clone::Clone::clone(&self.name),
                args: ::core::clone::Clone::clone(&self.args),
                body: ::core::clone::Clone::clone(&self.body),
                return_type: ::core::clone::Clone::clone(&self.return_type),
            }
        }
    }
    impl DebugWithInterner for FunctionDef {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            f.debug_struct("FunctionDef")
                .field("name", &self.name.get_str(interner))
                .field_with("args", |fmt| self.args.fmt_with_interner(fmt, interner))
                .field_with("body", |fmt| self.body.fmt_with_interner(fmt, interner))
                .field("return_type", &self.return_type)
                .finish()
        }
    }
    struct InterpretContext<'intern> {
        functions: FxHashMap<StringRef, FunctionDef>,
        vars: FxHashMap<StringRef, Val>,
        pub str_interner: &'intern mut StrInterner,
    }
    impl<'intern> DebugWithInterner for InterpretContext<'intern> {
        #[inline]
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            str_interner: &StrInterner,
        ) -> fmt::Result {
            f.debug_struct("InterpretContext")
                .field_with(
                    "functions",
                    |fmt| self.functions.fmt_with_interner(fmt, str_interner),
                )
                .field_with("vars", |fmt| self.vars.fmt_with_interner(fmt, str_interner))
                .finish()
        }
    }
    fn interpret_binop_nb(op: Operator, lhs_val: Val, rhs_val: Val) -> Val {
        let lhs_nb = match lhs_val {
            Val::Integer(nb) => nb,
            _ => {
                ::core::panicking::panic_fmt(
                    format_args!("Expected number in left-side of binary operation"),
                );
            }
        };
        let rhs_nb = match rhs_val {
            Val::Integer(nb) => nb,
            _ => {
                ::core::panicking::panic_fmt(
                    format_args!("Expected number in right-side of binary operation"),
                );
            }
        };
        let res_nb = match op {
            Operator::Plus => lhs_nb + rhs_nb,
            Operator::Minus => lhs_nb - rhs_nb,
            Operator::Mult => lhs_nb * rhs_nb,
            Operator::Div => lhs_nb / rhs_nb,
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        };
        Val::Integer(res_nb)
    }
    fn interpret_binop_bool(op: Operator, lhs_val: Val, rhs_val: Val) -> Val {
        let lhs_val_type = lhs_val.get_type();
        let rhs_val_type = rhs_val.get_type();
        if rhs_val.get_type() != lhs_val.get_type() {
            {
                ::core::panicking::panic_fmt(
                    format_args!(
                        "Not the same types around operators (lhs : {0:?}, rhs : {1:?})",
                        lhs_val_type,
                        rhs_val_type,
                    ),
                );
            }
        }
        match op {
            Operator::IsEqual => Val::Bool(lhs_val == rhs_val),
            Operator::SuperiorOrEqual => Val::Bool(lhs_val >= rhs_val),
            Operator::InferiorOrEqual => Val::Bool(lhs_val <= rhs_val),
            Operator::Superior => Val::Bool(lhs_val > rhs_val),
            Operator::Inferior => Val::Bool(lhs_val < rhs_val),
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        }
    }
    fn interpret_binop_str(
        context: &mut InterpretContext,
        op: Operator,
        lhs_val: Val,
        rhs_val: Val,
    ) -> Val {
        let lhs_str = match lhs_val {
            Val::String(s) => s,
            _ => {
                ::core::panicking::panic_fmt(
                    format_args!("Expected string in left-side of binary operation"),
                );
            }
        };
        let rhs_str = match rhs_val {
            Val::String(s) => s,
            _ => {
                ::core::panicking::panic_fmt(
                    format_args!("Expected string in right-side of binary operation"),
                );
            }
        };
        match op {
            Operator::StrAppend => {
                Val::String(lhs_str.add(rhs_str, context.str_interner))
            }
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        }
    }
    fn interpret_binop_list(op: Operator, lhs_val: Val, rhs_val: Val) -> Val {
        let rhs_type = rhs_val.get_type();
        let rhs_list = match rhs_val {
            Val::List(l) => *l,
            _ => {
                ::core::panicking::panic_fmt(
                    format_args!("Expected list in right-side of binary operation"),
                );
            }
        };
        let rhs_elem_type = match rhs_type {
            Type::List(e_t) => *e_t,
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        };
        (
            match lhs_val.get_type() {
                tmp => {
                    {
                        ::std::io::_eprint(
                            format_args!(
                                "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                "src/intepreter.rs",
                                278u32,
                                5u32,
                                "lhs_val.get_type()",
                                &tmp,
                            ),
                        );
                    };
                    tmp
                }
            },
            match &rhs_elem_type {
                tmp => {
                    {
                        ::std::io::_eprint(
                            format_args!(
                                "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                "src/intepreter.rs",
                                278u32,
                                5u32,
                                "&rhs_elem_type",
                                &tmp,
                            ),
                        );
                    };
                    tmp
                }
            },
        );
        if !rhs_list.empty() && lhs_val.get_type() != rhs_elem_type {
            {
                ::core::panicking::panic_fmt(
                    format_args!(
                        "Trying to add to an array of a type an element of another type",
                    ),
                );
            };
        }
        match op {
            Operator::ListAppend => {
                Val::List(Box::new(List::Node(lhs_val, Box::new(rhs_list))))
            }
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        }
    }
    fn interpret_binop(
        context: &mut InterpretContext,
        op: Operator,
        lhs: &ASTNode,
        rhs: &ASTNode,
    ) -> Val {
        let lhs_val = interpret_node(context, lhs);
        let rhs_val = interpret_node(context, rhs);
        match op.get_type() {
            Type::Integer => interpret_binop_nb(op, lhs_val, rhs_val),
            Type::Bool => interpret_binop_bool(op, lhs_val, rhs_val),
            Type::Str => interpret_binop_str(context, op, lhs_val, rhs_val),
            Type::List(_) => interpret_binop_list(op, lhs_val, rhs_val),
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        }
    }
    fn interpret_function_call(
        context: &mut InterpretContext,
        name: StringRef,
        args: Vec<ASTNode>,
    ) -> Val {
        let func_def = context.functions.get(&name).unwrap().clone();
        if args.len() != func_def.args.len() {
            {
                ::core::panicking::panic_fmt(
                    format_args!(
                        "Invalid args number in function call, expected {0}, got {1}",
                        func_def.args.len(),
                        args.len(),
                    ),
                );
            };
        }
        let args_val = args
            .into_iter()
            .map(|e| interpret_node(context, &e))
            .collect::<Vec<_>>();
        let mut old_vals: Vec<(StringRef, Val)> = Vec::new();
        for (arg_name, arg_val) in func_def.args.iter().zip(&args_val) {
            if let Some(old_val) = context.vars.get(arg_name) {
                old_vals.push((*arg_name, old_val.clone()));
            }
            context.vars.insert(arg_name.clone(), arg_val.clone());
        }
        let res_val = interpret_node(context, &func_def.body);
        for arg_name in &func_def.args {
            context.vars.remove(arg_name);
        }
        for (old_name, old_val) in old_vals {
            context.vars.insert(old_name, old_val);
        }
        res_val
    }
    fn interpret_if_expr(
        context: &mut InterpretContext,
        cond_expr: &ASTNode,
        then_body: &ASTNode,
        else_body: &ASTNode,
    ) -> Val {
        let cond_expr_val = match interpret_node(context, cond_expr) {
            Val::Bool(b) => b,
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        };
        if cond_expr_val {
            interpret_node(context, then_body)
        } else {
            interpret_node(context, else_body)
        }
    }
    fn interpret_match_pattern(matched_val: &Val, pattern: &Pattern) -> bool {
        match pattern {
            Pattern::VarName(_) | Pattern::Underscore => true,
            Pattern::Integer(nb) => {
                match matched_val {
                    Val::Integer(matched_nb) => *nb == *matched_nb,
                    _ => {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "matching an expression that is not an integer with an integer pattern",
                            ),
                        );
                    }
                }
            }
            Pattern::Float(nb) => {
                match matched_val {
                    Val::Float(matched_nb) => *nb == *matched_nb,
                    _ => {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "matching an expression that is not a float with a float pattern",
                            ),
                        );
                    }
                }
            }
            Pattern::Range(start, end, inclusivity) => {
                match matched_val {
                    Val::Integer(matched_nb) => {
                        if *inclusivity {
                            *start <= *matched_nb && matched_nb <= end
                        } else {
                            *start < *matched_nb && matched_nb < end
                        }
                    }
                    _ => {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "matching an expression that is not an integer with an range integer pattern",
                            ),
                        );
                    }
                }
            }
            Pattern::String(s) => {
                match matched_val {
                    Val::String(matched_str) => s == matched_str,
                    _ => {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "matching an expression that is not an integer with an integer pattern",
                            ),
                        );
                    }
                }
            }
            Pattern::List(l) => {
                let matched_expr_list = match matched_val {
                    Val::List(l) => l,
                    _ => {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "matching an expression that is not a list with a list pattern",
                            ),
                        );
                    }
                };
                if l.is_empty()
                    && match matched_expr_list.as_ref() {
                        List::None => true,
                        _ => false,
                    }
                {
                    return true;
                }
                let mut pattern_matched_nb = 0;
                for (p, v) in l.iter().zip(matched_expr_list.iter()) {
                    if !interpret_match_pattern(v, p) {
                        return false;
                    }
                    pattern_matched_nb += 1;
                }
                if pattern_matched_nb == l.len()
                    && pattern_matched_nb == matched_expr_list.len()
                {
                    return true;
                }
                false
            }
            Pattern::ListDestructure(_, _) => {
                ::core::panicking::panic("not yet implemented")
            }
        }
    }
    fn interpret_match(
        context: &mut InterpretContext,
        matched_expr: &ASTNode,
        patterns: &[(Pattern, ASTNode)],
    ) -> Val {
        let matched_expr_val = interpret_node(context, matched_expr);
        for (pattern, pattern_expr) in patterns {
            if interpret_match_pattern(&matched_expr_val, pattern) {
                match pattern {
                    Pattern::VarName(s) => {
                        context.vars.insert(s.clone(), matched_expr_val.clone());
                    }
                    _ => {}
                }
                let res_val = interpret_node(context, pattern_expr);
                match pattern {
                    Pattern::VarName(s) => {
                        context.vars.remove(s);
                    }
                    _ => {}
                }
                return res_val;
            }
        }
        {
            ::core::panicking::panic_fmt(
                format_args!(
                    "No pattern was matched in match expressions (not exhaustive match)",
                ),
            );
        }
    }
    fn interpret_node(context: &mut InterpretContext, ast: &ASTNode) -> Val {
        match ast {
            ASTNode::TopLevel { nodes } => {
                for node in nodes {
                    interpret_node(context, node);
                }
                Val::Unit
            }
            ASTNode::FunctionDefinition { name, args, body, return_type } => {
                let func_def = FunctionDef {
                    name: *name,
                    args: args.iter().map(|arg| arg.name.clone()).collect(),
                    body: body.clone(),
                    return_type: return_type.clone(),
                };
                context.functions.insert(name.clone(), func_def);
                Val::Unit
            }
            ASTNode::Float { nb } => Val::Float(*nb),
            ASTNode::Integer { nb } => Val::Integer(*nb),
            ASTNode::Boolean { b } => Val::Bool(*b),
            ASTNode::VarDecl { name, val, body } => {
                let val_node = interpret_node(context, val.as_ref());
                context.vars.insert(name.clone(), val_node);
                match body {
                    Some(b) => {
                        let body_val = interpret_node(context, b.as_ref());
                        context.vars.remove(name);
                        body_val
                    }
                    None => Val::Unit,
                }
            }
            ASTNode::VarUse { name } => {
                context
                    .vars
                    .get(name)
                    .unwrap_or_else(|| {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "BUG interpreter : unknown var {0}",
                                name.get_str(context.str_interner),
                            ),
                        );
                    })
                    .clone()
            }
            ASTNode::BinaryOp { op, lhs, rhs } => {
                interpret_binop(context, *op, lhs.as_ref(), rhs.as_ref())
            }
            ASTNode::FunctionCall { name, args } => {
                interpret_function_call(context, *name, args.clone())
            }
            ASTNode::IfExpr { cond_expr, then_body, else_body } => {
                interpret_if_expr(context, cond_expr, then_body, else_body)
            }
            ASTNode::MatchExpr { matched_expr, patterns } => {
                interpret_match(context, matched_expr.as_ref(), patterns.as_slice())
            }
            ASTNode::String { str } => Val::String(*str),
            ASTNode::List { list } => Val::List(Box::new(List::new(context, list))),
        }
    }
    pub fn interpret(ast: ASTNode, str_interner: &mut StrInterner) -> ExitCode {
        let mut context = InterpretContext {
            vars: FxHashMap::default(),
            functions: FxHashMap::default(),
            str_interner,
        };
        interpret_node(&mut context, &ast);
        match context {
            tmp => {
                {
                    ::std::io::_eprint(
                        format_args!(
                            "[{0}:{1}:{2}] {3} = {4:#?}\n",
                            "src/intepreter.rs",
                            569u32,
                            5u32,
                            "context",
                            crate::string_intern::DebugWrapInterner::new(
                                &tmp,
                                str_interner,
                            ),
                        ),
                    );
                };
                tmp
            }
        };
        ExitCode::SUCCESS
    }
}
mod lexer {
    use std::ops::Range;
    use crate::ast::Type;
    use enum_tags::Tag;
    pub enum Operator {
        Plus,
        Minus,
        Mult,
        Div,
        Equal,
        IsEqual,
        SuperiorOrEqual,
        InferiorOrEqual,
        Superior,
        Inferior,
        StrAppend,
        ListAppend,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Operator {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    Operator::Plus => "Plus",
                    Operator::Minus => "Minus",
                    Operator::Mult => "Mult",
                    Operator::Div => "Div",
                    Operator::Equal => "Equal",
                    Operator::IsEqual => "IsEqual",
                    Operator::SuperiorOrEqual => "SuperiorOrEqual",
                    Operator::InferiorOrEqual => "InferiorOrEqual",
                    Operator::Superior => "Superior",
                    Operator::Inferior => "Inferior",
                    Operator::StrAppend => "StrAppend",
                    Operator::ListAppend => "ListAppend",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Operator {
        #[inline]
        fn clone(&self) -> Operator {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Operator {}
    #[automatically_derived]
    impl ::core::cmp::Eq for Operator {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    #[automatically_derived]
    impl ::core::hash::Hash for Operator {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_discr, state)
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Operator {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Operator {
        #[inline]
        fn eq(&self, other: &Operator) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    impl Operator {
        pub const OPERATORS: &'static [&'static str] = &[
            "+",
            "-",
            "*",
            "/",
            "=",
            "==",
            ">=",
            "<=",
        ];
        pub fn get_type(&self) -> Type {
            match self {
                Self::StrAppend => Type::Str,
                Self::ListAppend => Type::List(Box::new(Type::Any)),
                Self::IsEqual
                | Self::SuperiorOrEqual
                | Self::InferiorOrEqual
                | Self::Superior
                | Self::Inferior => Type::Bool,
                _ => Type::Integer,
            }
        }
        pub fn get_operand_type(
            &self,
            is_left: bool,
            other_operand_type: &Type,
        ) -> Type {
            match self {
                Self::IsEqual => other_operand_type.clone(),
                Self::StrAppend => Type::Str,
                Self::ListAppend => {
                    if is_left {
                        Type::Any
                    } else {
                        Type::List(Box::new(other_operand_type.clone()))
                    }
                }
                Self::Equal => {
                    ::core::panicking::panic("internal error: entered unreachable code")
                }
                _ => Type::Integer,
            }
        }
        fn is_char_op(c: char) -> bool {
            match c {
                '+' | '-' | '*' | '/' | '=' | '<' | '>' | '^' | ':' => true,
                _ => false,
            }
        }
        pub fn str_to_op(s: &str, range: &Range<usize>) -> Result<Operator, LexerErr> {
            let op = match s {
                "+" => Operator::Plus,
                "-" => Operator::Minus,
                "*" => Operator::Mult,
                "/" => Operator::Div,
                "=" => Operator::Equal,
                "==" => Operator::IsEqual,
                ">=" => Operator::SuperiorOrEqual,
                "<=" => Operator::InferiorOrEqual,
                ">" => Operator::Superior,
                "<" => Operator::Inferior,
                "^" => Operator::StrAppend,
                "::" => Operator::ListAppend,
                _ => {
                    return Err(
                        LexerErr::new(
                            LexerErrData::InvalidOp(Box::new(s.to_owned())),
                            range.clone(),
                        ),
                    );
                }
            };
            Ok(op)
        }
    }
    pub enum TokenData {
        Identifier(Vec<char>),
        String(Vec<char>),
        Op(Operator),
        Integer(i64),
        Float(f64),
        Let,
        If,
        Then,
        Else,
        Match,
        With,
        In,
        True,
        False,
        ParenOpen,
        ParenClose,
        ArrayOpen,
        ArrayClose,
        Colon,
        Comma,
        Arrow,
        Pipe,
        EndOfExpr,
        Range,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TokenData {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                TokenData::Identifier(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Identifier",
                        &__self_0,
                    )
                }
                TokenData::String(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "String",
                        &__self_0,
                    )
                }
                TokenData::Op(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Op", &__self_0)
                }
                TokenData::Integer(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Integer",
                        &__self_0,
                    )
                }
                TokenData::Float(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Float",
                        &__self_0,
                    )
                }
                TokenData::Let => ::core::fmt::Formatter::write_str(f, "Let"),
                TokenData::If => ::core::fmt::Formatter::write_str(f, "If"),
                TokenData::Then => ::core::fmt::Formatter::write_str(f, "Then"),
                TokenData::Else => ::core::fmt::Formatter::write_str(f, "Else"),
                TokenData::Match => ::core::fmt::Formatter::write_str(f, "Match"),
                TokenData::With => ::core::fmt::Formatter::write_str(f, "With"),
                TokenData::In => ::core::fmt::Formatter::write_str(f, "In"),
                TokenData::True => ::core::fmt::Formatter::write_str(f, "True"),
                TokenData::False => ::core::fmt::Formatter::write_str(f, "False"),
                TokenData::ParenOpen => ::core::fmt::Formatter::write_str(f, "ParenOpen"),
                TokenData::ParenClose => {
                    ::core::fmt::Formatter::write_str(f, "ParenClose")
                }
                TokenData::ArrayOpen => ::core::fmt::Formatter::write_str(f, "ArrayOpen"),
                TokenData::ArrayClose => {
                    ::core::fmt::Formatter::write_str(f, "ArrayClose")
                }
                TokenData::Colon => ::core::fmt::Formatter::write_str(f, "Colon"),
                TokenData::Comma => ::core::fmt::Formatter::write_str(f, "Comma"),
                TokenData::Arrow => ::core::fmt::Formatter::write_str(f, "Arrow"),
                TokenData::Pipe => ::core::fmt::Formatter::write_str(f, "Pipe"),
                TokenData::EndOfExpr => ::core::fmt::Formatter::write_str(f, "EndOfExpr"),
                TokenData::Range => ::core::fmt::Formatter::write_str(f, "Range"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TokenData {
        #[inline]
        fn clone(&self) -> TokenData {
            match self {
                TokenData::Identifier(__self_0) => {
                    TokenData::Identifier(::core::clone::Clone::clone(__self_0))
                }
                TokenData::String(__self_0) => {
                    TokenData::String(::core::clone::Clone::clone(__self_0))
                }
                TokenData::Op(__self_0) => {
                    TokenData::Op(::core::clone::Clone::clone(__self_0))
                }
                TokenData::Integer(__self_0) => {
                    TokenData::Integer(::core::clone::Clone::clone(__self_0))
                }
                TokenData::Float(__self_0) => {
                    TokenData::Float(::core::clone::Clone::clone(__self_0))
                }
                TokenData::Let => TokenData::Let,
                TokenData::If => TokenData::If,
                TokenData::Then => TokenData::Then,
                TokenData::Else => TokenData::Else,
                TokenData::Match => TokenData::Match,
                TokenData::With => TokenData::With,
                TokenData::In => TokenData::In,
                TokenData::True => TokenData::True,
                TokenData::False => TokenData::False,
                TokenData::ParenOpen => TokenData::ParenOpen,
                TokenData::ParenClose => TokenData::ParenClose,
                TokenData::ArrayOpen => TokenData::ArrayOpen,
                TokenData::ArrayClose => TokenData::ArrayClose,
                TokenData::Colon => TokenData::Colon,
                TokenData::Comma => TokenData::Comma,
                TokenData::Arrow => TokenData::Arrow,
                TokenData::Pipe => TokenData::Pipe,
                TokenData::EndOfExpr => TokenData::EndOfExpr,
                TokenData::Range => TokenData::Range,
            }
        }
    }
    pub enum TokenDataTag {
        Identifier,
        String,
        Op,
        Integer,
        Float,
        Let,
        If,
        Then,
        Else,
        Match,
        With,
        In,
        True,
        False,
        ParenOpen,
        ParenClose,
        ArrayOpen,
        ArrayClose,
        Colon,
        Comma,
        Arrow,
        Pipe,
        EndOfExpr,
        Range,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TokenDataTag {
        #[inline]
        fn clone(&self) -> TokenDataTag {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for TokenDataTag {}
    #[automatically_derived]
    impl ::core::fmt::Debug for TokenDataTag {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    TokenDataTag::Identifier => "Identifier",
                    TokenDataTag::String => "String",
                    TokenDataTag::Op => "Op",
                    TokenDataTag::Integer => "Integer",
                    TokenDataTag::Float => "Float",
                    TokenDataTag::Let => "Let",
                    TokenDataTag::If => "If",
                    TokenDataTag::Then => "Then",
                    TokenDataTag::Else => "Else",
                    TokenDataTag::Match => "Match",
                    TokenDataTag::With => "With",
                    TokenDataTag::In => "In",
                    TokenDataTag::True => "True",
                    TokenDataTag::False => "False",
                    TokenDataTag::ParenOpen => "ParenOpen",
                    TokenDataTag::ParenClose => "ParenClose",
                    TokenDataTag::ArrayOpen => "ArrayOpen",
                    TokenDataTag::ArrayClose => "ArrayClose",
                    TokenDataTag::Colon => "Colon",
                    TokenDataTag::Comma => "Comma",
                    TokenDataTag::Arrow => "Arrow",
                    TokenDataTag::Pipe => "Pipe",
                    TokenDataTag::EndOfExpr => "EndOfExpr",
                    TokenDataTag::Range => "Range",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for TokenDataTag {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for TokenDataTag {
        #[inline]
        fn eq(&self, other: &TokenDataTag) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for TokenDataTag {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    impl ::enum_tags_traits::TaggedEnum for TokenData {
        type Tag = TokenDataTag;
        fn tag(&self) -> Self::Tag {
            match *self {
                Self::Identifier(_) => Self::Tag::Identifier,
                Self::String(_) => Self::Tag::String,
                Self::Op(_) => Self::Tag::Op,
                Self::Integer(_) => Self::Tag::Integer,
                Self::Float(_) => Self::Tag::Float,
                Self::Let => Self::Tag::Let,
                Self::If => Self::Tag::If,
                Self::Then => Self::Tag::Then,
                Self::Else => Self::Tag::Else,
                Self::Match => Self::Tag::Match,
                Self::With => Self::Tag::With,
                Self::In => Self::Tag::In,
                Self::True => Self::Tag::True,
                Self::False => Self::Tag::False,
                Self::ParenOpen => Self::Tag::ParenOpen,
                Self::ParenClose => Self::Tag::ParenClose,
                Self::ArrayOpen => Self::Tag::ArrayOpen,
                Self::ArrayClose => Self::Tag::ArrayClose,
                Self::Colon => Self::Tag::Colon,
                Self::Comma => Self::Tag::Comma,
                Self::Arrow => Self::Tag::Arrow,
                Self::Pipe => Self::Tag::Pipe,
                Self::EndOfExpr => Self::Tag::EndOfExpr,
                Self::Range => Self::Tag::Range,
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for TokenData {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for TokenData {
        #[inline]
        fn eq(&self, other: &TokenData) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (
                        TokenData::Identifier(__self_0),
                        TokenData::Identifier(__arg1_0),
                    ) => __self_0 == __arg1_0,
                    (TokenData::String(__self_0), TokenData::String(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (TokenData::Op(__self_0), TokenData::Op(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (TokenData::Integer(__self_0), TokenData::Integer(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (TokenData::Float(__self_0), TokenData::Float(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    _ => true,
                }
        }
    }
    pub struct Token {
        pub tok_data: TokenData,
        pub range: Range<usize>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Token {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Token",
                "tok_data",
                &self.tok_data,
                "range",
                &&self.range,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Token {
        #[inline]
        fn clone(&self) -> Token {
            Token {
                tok_data: ::core::clone::Clone::clone(&self.tok_data),
                range: ::core::clone::Clone::clone(&self.range),
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Token {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Token {
        #[inline]
        fn eq(&self, other: &Token) -> bool {
            self.tok_data == other.tok_data && self.range == other.range
        }
    }
    impl Token {
        pub fn new(tok_data: TokenData, range: Range<usize>) -> Token {
            Token { tok_data, range }
        }
    }
    struct Lexer {
        content: Vec<char>,
        pos: usize,
    }
    pub enum LexerErrData {
        NumberParsingFailure(Box<Vec<char>>),
        InvalidOp(Box<String>),
        UnexpectedChar(char),
        UnexpectedEOF,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for LexerErrData {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                LexerErrData::NumberParsingFailure(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "NumberParsingFailure",
                        &__self_0,
                    )
                }
                LexerErrData::InvalidOp(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "InvalidOp",
                        &__self_0,
                    )
                }
                LexerErrData::UnexpectedChar(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "UnexpectedChar",
                        &__self_0,
                    )
                }
                LexerErrData::UnexpectedEOF => {
                    ::core::fmt::Formatter::write_str(f, "UnexpectedEOF")
                }
            }
        }
    }
    pub enum LexerErrDataTag {
        NumberParsingFailure,
        InvalidOp,
        UnexpectedChar,
        UnexpectedEOF,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for LexerErrDataTag {
        #[inline]
        fn clone(&self) -> LexerErrDataTag {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for LexerErrDataTag {}
    #[automatically_derived]
    impl ::core::fmt::Debug for LexerErrDataTag {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    LexerErrDataTag::NumberParsingFailure => "NumberParsingFailure",
                    LexerErrDataTag::InvalidOp => "InvalidOp",
                    LexerErrDataTag::UnexpectedChar => "UnexpectedChar",
                    LexerErrDataTag::UnexpectedEOF => "UnexpectedEOF",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for LexerErrDataTag {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for LexerErrDataTag {
        #[inline]
        fn eq(&self, other: &LexerErrDataTag) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for LexerErrDataTag {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    impl ::enum_tags_traits::TaggedEnum for LexerErrData {
        type Tag = LexerErrDataTag;
        fn tag(&self) -> Self::Tag {
            match *self {
                Self::NumberParsingFailure(_) => Self::Tag::NumberParsingFailure,
                Self::InvalidOp(_) => Self::Tag::InvalidOp,
                Self::UnexpectedChar(_) => Self::Tag::UnexpectedChar,
                Self::UnexpectedEOF => Self::Tag::UnexpectedEOF,
            }
        }
    }
    pub struct LexerErr {
        pub lexer_err_data: Box<LexerErrData>,
        pub range: Range<usize>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for LexerErr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "LexerErr",
                "lexer_err_data",
                &self.lexer_err_data,
                "range",
                &&self.range,
            )
        }
    }
    impl LexerErr {
        fn new(lexer_err_data: LexerErrData, range: Range<usize>) -> LexerErr {
            LexerErr {
                lexer_err_data: Box::new(lexer_err_data),
                range,
            }
        }
    }
    impl Lexer {
        fn has_char_left(&self) -> bool {
            self.pos < self.content.len()
        }
        pub fn peek(&self) -> Option<char> {
            if self.pos + 1 >= self.content.len() {
                return None;
            }
            Some(self.content[self.pos + 1])
        }
        pub fn current_char(&self) -> Option<char> {
            if self.pos >= self.content.len() {
                return None;
            }
            Some(self.content[self.pos])
        }
        pub fn read_char(&mut self) -> Option<char> {
            if !self.has_char_left() {
                return None;
            }
            let c = self.content[self.pos];
            self.pos += 1;
            Some(c)
        }
        pub fn go_back_pos(&mut self, count: usize) {
            self.pos -= count;
        }
    }
    fn lex_nb(lexer: &mut Lexer) -> Result<Token, LexerErr> {
        fn continue_nb(c: char, is_float: &mut bool, is_range: &mut bool) -> bool {
            match c {
                '0'..='9' => true,
                '.' => {
                    if *is_float {
                        *is_range = true;
                    }
                    *is_float = true;
                    true
                }
                _ => false,
            }
        }
        let mut is_float = false;
        let mut is_range = false;
        let start_pos = lexer.pos - 1;
        while lexer.pos < lexer.content.len()
            && continue_nb(lexer.current_char().unwrap(), &mut is_float, &mut is_range)
        {
            if is_range {
                lexer.go_back_pos(1);
                is_float = lexer.content[start_pos..lexer.pos].contains(&'.');
                break;
            }
            lexer.read_char();
        }
        let buf = lexer.content[start_pos..lexer.pos].to_vec();
        let range = start_pos..lexer.pos;
        let str = buf.iter().collect::<String>();
        if is_float {
            let nb = str::parse::<f64>(str.as_str());
            let nb = match nb {
                Ok(n) => n,
                Err(_) => {
                    return Err(
                        LexerErr::new(
                            LexerErrData::NumberParsingFailure(Box::new(buf)),
                            range,
                        ),
                    );
                }
            };
            Ok(Token::new(TokenData::Float(nb), range))
        } else {
            let nb = str::parse::<i64>(str.as_str());
            let nb = match nb {
                Ok(n) => n,
                Err(_) => {
                    return Err(
                        LexerErr::new(
                            LexerErrData::NumberParsingFailure(Box::new(buf)),
                            range,
                        ),
                    );
                }
            };
            Ok(Token::new(TokenData::Integer(nb), range))
        }
    }
    fn lex_alphabetic(lexer: &mut Lexer) -> Token {
        fn continue_alphabetic(c: char) -> bool {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
                _ => false,
            }
        }
        let start_pos = lexer.pos - 1;
        while lexer.pos < lexer.content.len()
            && continue_alphabetic(lexer.current_char().unwrap())
        {
            lexer.read_char();
        }
        let buf = lexer.content[start_pos..lexer.pos].to_vec();
        let range = start_pos..lexer.pos;
        let tok_data = match buf.iter().collect::<String>().as_str() {
            "let" => TokenData::Let,
            "if" => TokenData::If,
            "match" => TokenData::Match,
            "with" => TokenData::With,
            "then" => TokenData::Then,
            "else" => TokenData::Else,
            "in" => TokenData::In,
            "true" => TokenData::True,
            "false" => TokenData::False,
            _ => TokenData::Identifier(buf),
        };
        Token::new(tok_data, range)
    }
    fn handle_comment(lexer: &mut Lexer) {
        while let Some(c) = lexer.read_char() && c != '\n' {}
    }
    fn lex_op(lexer: &mut Lexer) -> Result<Option<Token>, LexerErr> {
        fn continue_op(c: char) -> bool {
            Operator::is_char_op(c)
        }
        let start_pos = lexer.pos - 1;
        while lexer.pos < lexer.content.len()
            && continue_op(lexer.current_char().unwrap())
        {
            lexer.read_char();
        }
        let buf = lexer.content[start_pos..lexer.pos].to_vec();
        let range = start_pos..lexer.pos;
        let op_str = buf.iter().collect::<String>();
        let tok_data = match op_str.as_str() {
            "->" => TokenData::Arrow,
            "//" => {
                handle_comment(lexer);
                return Ok(None);
            }
            ":" => TokenData::Colon,
            _ => TokenData::Op(Operator::str_to_op(&op_str, &range)?),
        };
        Ok(Some(Token::new(tok_data, range)))
    }
    fn lex_string(lexer: &mut Lexer) -> Result<Token, LexerErr> {
        let start_pos = lexer.pos - 1;
        let mut buf = Vec::new();
        while let Some(c) = lexer.current_char() && c != '\"' {
            let c = lexer.read_char().unwrap();
            buf.push(c);
        }
        let range = start_pos..lexer.pos + 1;
        match lexer.read_char() {
            Some('\"') => {}
            Some(_) => {
                ::core::panicking::panic("internal error: entered unreachable code")
            }
            None => {
                return Err(
                    LexerErr::new(LexerErrData::UnexpectedEOF, lexer.pos..lexer.pos),
                );
            }
        }
        Ok(Token::new(TokenData::String(buf), range))
    }
    pub fn lex(content: Vec<char>) -> Result<Vec<Token>, LexerErr> {
        let mut lexer = Lexer { content, pos: 0 };
        let mut tokens = ::alloc::vec::Vec::new();
        while let Some(c) = lexer.read_char() {
            let range = lexer.pos - 1..lexer.pos - 1;
            let tok: Option<Token> = match c {
                ' ' | '\t' | '\n' => None,
                '(' => Some(Token::new(TokenData::ParenOpen, range)),
                ')' => Some(Token::new(TokenData::ParenClose, range)),
                '[' => Some(Token::new(TokenData::ArrayOpen, range)),
                ']' => Some(Token::new(TokenData::ArrayClose, range)),
                ',' => Some(Token::new(TokenData::Comma, range)),
                '.' => {
                    match lexer.read_char() {
                        Some('.') => {
                            Some(
                                Token::new(TokenData::Range, lexer.pos - 2..lexer.pos - 1),
                            )
                        }
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!("Not complete \"..\" token"),
                            );
                        }
                    }
                }
                ';' => {
                    match lexer.read_char() {
                        Some(';') => {
                            Some(
                                Token::new(
                                    TokenData::EndOfExpr,
                                    lexer.pos - 2..lexer.pos - 1,
                                ),
                            )
                        }
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!("Not complete \";;\" token"),
                            );
                        }
                    }
                }
                '|' => Some(Token::new(TokenData::Pipe, range)),
                '\"' => Some(lex_string(&mut lexer)?),
                op_char if Operator::is_char_op(op_char) => lex_op(&mut lexer)?,
                '0'..='9' => Some(lex_nb(&mut lexer)?),
                'a'..='z' | 'A'..='Z' | '_' => Some(lex_alphabetic(&mut lexer)),
                c => return Err(LexerErr::new(LexerErrData::UnexpectedChar(c), range)),
            };
            if let Some(t) = tok {
                tokens.push(t);
            }
        }
        match &tokens {
            tmp => {
                {
                    ::std::io::_eprint(
                        format_args!(
                            "[{0}:{1}:{2}] {3} = {4:#?}\n",
                            "src/lexer.rs",
                            380u32,
                            5u32,
                            "&tokens",
                            &tmp,
                        ),
                    );
                };
                tmp
            }
        };
        Ok(tokens)
    }
}
mod type_inference {
    use std::ops::Range;
    use crate::{
        ast::{ASTNode, Parser, Pattern, Type},
        dbg_intern, string_intern::StringRef,
    };
    fn infer_var_type_pattern(
        parser: &Parser,
        pattern: &Pattern,
        body: &ASTNode,
        range: &Range<usize>,
    ) -> Option<Type> {
        match pattern {
            Pattern::Float(_) => Some(Type::Float),
            Pattern::Integer(_) | Pattern::Range(_, _, _) => Some(Type::Integer),
            Pattern::String(_) => Some(Type::Str),
            Pattern::List(l) => {
                let elem_type = match l.first() {
                    Some(first) => {
                        match infer_var_type_pattern(parser, first, body, range) {
                            Some(t) => t,
                            None => Type::Any,
                        }
                    }
                    None => Type::Any,
                };
                Some(Type::List(Box::new(elem_type)))
            }
            Pattern::ListDestructure(_head_name, _tail_name) => {
                Some(Type::List(Box::new(Type::Any)))
            }
            Pattern::Underscore => None,
            Pattern::VarName(var_name) => _infer_var_type(parser, *var_name, body, range),
        }
    }
    pub struct TypeInferenceErr {
        pub arg_name: Box<String>,
        pub range: Range<usize>,
    }
    impl TypeInferenceErr {
        fn new(arg_name: String, range: Range<usize>) -> TypeInferenceErr {
            TypeInferenceErr {
                arg_name: Box::new(arg_name),
                range,
            }
        }
    }
    pub fn _infer_var_type(
        parser: &Parser,
        var_name: StringRef,
        node: &ASTNode,
        range: &Range<usize>,
    ) -> Option<Type> {
        match node {
            ASTNode::TopLevel { nodes } => {
                for n in nodes {
                    let type_inferred = _infer_var_type(parser, var_name, n, range);
                    if type_inferred.is_some() {
                        return type_inferred;
                    }
                }
                None
            }
            ASTNode::FunctionDefinition { name, args: _, body, return_type: _ } => {
                _infer_var_type(parser, *name, body, range)
            }
            ASTNode::VarDecl { name, val, body } => {
                let val_type_inferred = _infer_var_type(
                    parser,
                    *name,
                    val.as_ref(),
                    range,
                );
                if val_type_inferred.is_some() {
                    return val_type_inferred;
                }
                if let Some(b) = body {
                    return _infer_var_type(parser, *name, b, range);
                }
                None
            }
            ASTNode::VarUse { name: _ } => None,
            ASTNode::IfExpr { cond_expr, then_body, else_body } => {
                let cond_type_inferred = _infer_var_type(
                    parser,
                    var_name,
                    cond_expr,
                    range,
                );
                if cond_type_inferred.is_some() {
                    return cond_type_inferred;
                }
                let then_type_inferred = _infer_var_type(
                    parser,
                    var_name,
                    then_body,
                    range,
                );
                if then_type_inferred.is_some() {
                    return then_type_inferred;
                }
                return _infer_var_type(parser, var_name, else_body, range);
            }
            ASTNode::MatchExpr { matched_expr, patterns } => {
                let matched_expr_type_inferred = _infer_var_type(
                    parser,
                    var_name,
                    matched_expr,
                    range,
                );
                if matched_expr_type_inferred.is_some() {
                    return matched_expr_type_inferred;
                }
                if match matched_expr.as_ref() {
                    ASTNode::VarUse {
                        name: var_use_name,
                    } if *var_use_name == var_name => true,
                    _ => false,
                } {
                    for pattern in patterns {
                        let pattern_type_inferred = infer_var_type_pattern(
                            parser,
                            &pattern.0,
                            &pattern.1,
                            range,
                        );
                        if pattern_type_inferred.is_some() {
                            return pattern_type_inferred;
                        }
                    }
                }
                for pattern in patterns {
                    let pattern_body_type_inferred = _infer_var_type(
                        parser,
                        var_name,
                        &pattern.1,
                        range,
                    );
                    if pattern_body_type_inferred.is_some() {
                        return pattern_body_type_inferred;
                    }
                }
                None
            }
            ASTNode::Integer { nb: _ } => None,
            ASTNode::Float { nb: _ } => None,
            ASTNode::String { str: _ } => None,
            ASTNode::Boolean { b: _ } => None,
            ASTNode::List { list: _ } => None,
            ASTNode::BinaryOp { op, lhs, rhs } => {
                let is_left_var = match lhs.as_ref() {
                    ASTNode::VarUse { name } => *name == var_name,
                    _ => false,
                };
                let is_right_var = match rhs.as_ref() {
                    ASTNode::VarUse { name } => *name == var_name,
                    _ => false,
                };
                (
                    match is_left_var {
                        tmp => {
                            {
                                ::std::io::_eprint(
                                    format_args!(
                                        "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                        "src/type_inference.rs",
                                        134u32,
                                        13u32,
                                        "is_left_var",
                                        &tmp,
                                    ),
                                );
                            };
                            tmp
                        }
                    },
                    match is_right_var {
                        tmp => {
                            {
                                ::std::io::_eprint(
                                    format_args!(
                                        "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                        "src/type_inference.rs",
                                        134u32,
                                        13u32,
                                        "is_right_var",
                                        &tmp,
                                    ),
                                );
                            };
                            tmp
                        }
                    },
                );
                if is_left_var || is_right_var {
                    let other_operand_type = if is_left_var {
                        rhs.get_type(parser)
                    } else {
                        lhs.get_type(parser)
                    };
                    let operand_type = op
                        .get_operand_type(is_left_var, &other_operand_type);
                    (
                        match &operand_type {
                            tmp => {
                                {
                                    ::std::io::_eprint(
                                        format_args!(
                                            "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                            "src/type_inference.rs",
                                            143u32,
                                            17u32,
                                            "&operand_type",
                                            &tmp,
                                        ),
                                    );
                                };
                                tmp
                            }
                        },
                        match &other_operand_type {
                            tmp => {
                                {
                                    ::std::io::_eprint(
                                        format_args!(
                                            "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                            "src/type_inference.rs",
                                            143u32,
                                            17u32,
                                            "&other_operand_type",
                                            &tmp,
                                        ),
                                    );
                                };
                                tmp
                            }
                        },
                    );
                    match var_name {
                        tmp => {
                            {
                                ::std::io::_eprint(
                                    format_args!(
                                        "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                        "src/type_inference.rs",
                                        144u32,
                                        17u32,
                                        "var_name",
                                        crate::string_intern::DebugWrapInterner::new(
                                            &tmp,
                                            parser.str_interner,
                                        ),
                                    ),
                                );
                            };
                            tmp
                        }
                    };
                    match node {
                        tmp => {
                            {
                                ::std::io::_eprint(
                                    format_args!(
                                        "[{0}:{1}:{2}] {3} = {4:#?}\n",
                                        "src/type_inference.rs",
                                        145u32,
                                        17u32,
                                        "node",
                                        crate::string_intern::DebugWrapInterner::new(
                                            &tmp,
                                            parser.str_interner,
                                        ),
                                    ),
                                );
                            };
                            tmp
                        }
                    };
                    Some(operand_type)
                } else {
                    let lhs_inferred = _infer_var_type(
                        parser,
                        var_name,
                        lhs.as_ref(),
                        range,
                    );
                    if lhs_inferred.is_some() {
                        return lhs_inferred;
                    }
                    let rhs_inferred = _infer_var_type(
                        parser,
                        var_name,
                        rhs.as_ref(),
                        range,
                    );
                    if rhs_inferred.is_some() {
                        return rhs_inferred;
                    }
                    None
                }
            }
            ASTNode::FunctionCall { name: function_name, args } => {
                match parser.vars.get(function_name) {
                    Some(Type::Function(a, _)) => {
                        for (arg, arg_type) in args.iter().zip(a) {
                            match arg {
                                ASTNode::VarUse { name } if *name == var_name => {
                                    return Some(arg_type.clone());
                                }
                                _ => {
                                    let inferred_arg_type = _infer_var_type(
                                        parser,
                                        var_name,
                                        arg,
                                        range,
                                    );
                                    if inferred_arg_type.is_some() {
                                        return inferred_arg_type;
                                    }
                                }
                            }
                        }
                        None
                    }
                    _ => None,
                }
            }
        }
    }
    pub fn infer_var_type(
        parser: &Parser,
        var_name: StringRef,
        node: &ASTNode,
        range: &Range<usize>,
    ) -> Result<Type, TypeInferenceErr> {
        match _infer_var_type(parser, var_name, node, range) {
            Some(t) => Ok(t),
            None => {
                Err(
                    TypeInferenceErr::new(
                        var_name.get_str(parser.str_interner).to_owned(),
                        range.clone(),
                    ),
                )
            }
        }
    }
}
mod string_intern {
    use std::fmt::{self, Debug};
    use rustc_hash::FxHashMap;
    pub struct StrInterner {
        map: FxHashMap<String, u32>,
        strs: Vec<String>,
    }
    pub struct StringRef(u32);
    #[automatically_derived]
    impl ::core::clone::Clone for StringRef {
        #[inline]
        fn clone(&self) -> StringRef {
            let _: ::core::clone::AssertParamIsClone<u32>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for StringRef {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for StringRef {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for StringRef {
        #[inline]
        fn eq(&self, other: &StringRef) -> bool {
            self.0 == other.0
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for StringRef {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<u32>;
        }
    }
    #[automatically_derived]
    impl ::core::cmp::PartialOrd for StringRef {
        #[inline]
        fn partial_cmp(
            &self,
            other: &StringRef,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for StringRef {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            ::core::hash::Hash::hash(&self.0, state)
        }
    }
    impl StringRef {
        pub fn get_str(self, str_interner: &StrInterner) -> &str {
            str_interner.lookup(self)
        }
        pub fn add(self, rhs: StringRef, str_interner: &mut StrInterner) -> StringRef {
            let new_str = str_interner.lookup(self).to_owned()
                + str_interner.lookup(rhs);
            str_interner.intern(&new_str)
        }
    }
    pub trait DebugWithInterner {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result;
    }
    pub struct DebugWrapInterner<'a, T> {
        value: &'a T,
        interner: &'a StrInterner,
    }
    impl<'a, T> DebugWrapInterner<'a, T> {
        pub fn new(value: &'a T, interner: &'a StrInterner) -> DebugWrapInterner<'a, T> {
            DebugWrapInterner {
                value,
                interner,
            }
        }
    }
    impl<'a, T> Debug for DebugWrapInterner<'a, T>
    where
        T: DebugWithInterner,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.value.fmt_with_interner(f, self.interner)
        }
    }
    impl<'a, T> DebugWithInterner for &'a T
    where
        T: DebugWithInterner,
    {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            (*self).fmt_with_interner(f, interner)
        }
    }
    impl DebugWithInterner for StringRef {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            f.write_fmt(format_args!("{0}", interner.lookup(*self)))
        }
    }
    impl<T1, T2> DebugWithInterner for (T1, T2)
    where
        T1: DebugWithInterner,
        T2: DebugWithInterner,
    {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            f.debug_tuple("")
                .field_with(|fmt| self.0.fmt_with_interner(fmt, interner))
                .field_with(|fmt| self.1.fmt_with_interner(fmt, interner))
                .finish()
        }
    }
    impl<T> DebugWithInterner for Vec<T>
    where
        T: DebugWithInterner,
    {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            f.debug_list()
                .entries(
                    self
                        .iter()
                        .map(|item| DebugWrapInterner {
                            value: item,
                            interner,
                        }),
                )
                .finish()
        }
    }
    impl<T> DebugWithInterner for Option<Box<T>>
    where
        T: DebugWithInterner,
    {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            match self {
                Some(s) => s.fmt_with_interner(f, interner),
                None => None::<()>.fmt(f),
            }
        }
    }
    impl<K, V> DebugWithInterner for FxHashMap<K, V>
    where
        K: DebugWithInterner,
        V: DebugWithInterner,
    {
        fn fmt_with_interner(
            &self,
            f: &mut fmt::Formatter,
            interner: &StrInterner,
        ) -> fmt::Result {
            f.debug_map()
                .entries(
                    self
                        .iter()
                        .map(|(k, v)| (
                            DebugWrapInterner::new(k, interner),
                            DebugWrapInterner::new(v, interner),
                        )),
                )
                .finish()
        }
    }
    impl StrInterner {
        pub fn new() -> StrInterner {
            StrInterner {
                map: FxHashMap::default(),
                strs: Vec::new(),
            }
        }
        pub fn intern(&mut self, name: &str) -> StringRef {
            if let Some(idx) = self.map.get(name) {
                return StringRef(*idx);
            }
            let idx = self.strs.len() as u32;
            self.map.insert(name.to_owned(), idx);
            self.strs.push(name.to_owned());
            StringRef(idx)
        }
        pub fn lookup(&self, idx: StringRef) -> &str {
            self.strs[idx.0 as usize].as_str()
        }
    }
}
mod print_error {
    use std::{ops::Range, path::Path, process::ExitCode};
    use levenshtein::levenshtein;
    use crate::{
        ast::{ParserErr, ParserErrData},
        lexer::{LexerErr, Operator, TokenData, TokenDataTag},
    };
    use crate::lexer::LexerErrData;
    use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
    use enum_tags::TaggedEnum;
    const PARSER_ERROR_OFFSET: u32 = 100;
    fn print_number_parsing_failure(
        error_nb: u8,
        range: Range<usize>,
        filename: &str,
        content: &str,
        buf: Vec<char>,
    ) {
        Report::build(ReportKind::Error, (filename, range))
            .with_code(error_nb)
            .with_message(
                ::alloc::__export::must_use({
                    ::alloc::fmt::format(
                        format_args!("Failure when parsing number \"{0:?}\"", buf),
                    )
                }),
            )
            .finish()
            .print((filename, Source::from(content)))
            .unwrap();
    }
    fn nearest_op(s: &str) -> &'static str {
        let mut min_distance = usize::MAX;
        let mut nearest = Operator::OPERATORS[0];
        for op in Operator::OPERATORS {
            let distance = levenshtein(s, op);
            if distance < min_distance {
                min_distance = distance;
                nearest = op;
            }
        }
        nearest
    }
    fn print_invalid_op_error(
        error_nb: u8,
        range: Range<usize>,
        filename: &str,
        content: &str,
        op: String,
    ) {
        let mut colors = ColorGenerator::new();
        let a = colors.next();
        Report::build(ReportKind::Error, (filename, range.clone()))
            .with_code(error_nb)
            .with_message(
                ::alloc::__export::must_use({
                    ::alloc::fmt::format(format_args!("Invalid operator \"{0}\"", op))
                }),
            )
            .with_label(
                Label::new((filename, range.clone()))
                    .with_message("This operator doesn't exist")
                    .with_color(a),
            )
            .with_note(
                ::alloc::__export::must_use({
                    ::alloc::fmt::format(
                        format_args!("maybe you meant {0}", nearest_op(&op)),
                    )
                }),
            )
            .finish()
            .print((filename, Source::from(content)))
            .unwrap();
    }
    fn print_unexpected_char_error(
        error_nb: u8,
        range: Range<usize>,
        filename: &str,
        content: &str,
        c: char,
    ) {
        let mut colors = ColorGenerator::new();
        let a = colors.next();
        Report::build(ReportKind::Error, (filename, range.clone()))
            .with_code(error_nb)
            .with_message(
                ::alloc::__export::must_use({
                    ::alloc::fmt::format(format_args!("Unexpected char \'{0}\'", c))
                }),
            )
            .with_label(
                Label::new((filename, range.clone()))
                    .with_message("This character shouldn't be there")
                    .with_color(a),
            )
            .finish()
            .print((filename, Source::from(content)))
            .unwrap();
    }
    pub fn print_lexer_error(
        lexer_error: LexerErr,
        filename: &Path,
        content: &str,
    ) -> ExitCode {
        let error_nb = lexer_error.lexer_err_data.tag() as u8;
        let range = lexer_error.range;
        let filename_str = filename.to_str().unwrap();
        match *lexer_error.lexer_err_data {
            LexerErrData::NumberParsingFailure(b) => {
                print_number_parsing_failure(error_nb, range, filename_str, content, *b)
            }
            LexerErrData::InvalidOp(s) => {
                print_invalid_op_error(error_nb, range, filename_str, content, *s)
            }
            LexerErrData::UnexpectedEOF => {
                print_unexpected_eof_error(error_nb as u32, range, filename_str, content)
            }
            LexerErrData::UnexpectedChar(c) => {
                print_unexpected_char_error(error_nb, range, filename_str, content, c)
            }
        }
        ExitCode::FAILURE
    }
    fn print_unexpected_eof_error(
        error_nb: u32,
        range: Range<usize>,
        filename: &str,
        content: &str,
    ) {
        let mut colors = ColorGenerator::new();
        let a = colors.next();
        Report::build(ReportKind::Error, (filename, range.clone()))
            .with_code(error_nb)
            .with_message("Unexpected end of file")
            .with_label(
                Label::new((filename, range.clone()))
                    .with_message("There is here a EOF that should not be here")
                    .with_color(a),
            )
            .finish()
            .print((filename, Source::from(content)))
            .unwrap();
    }
    fn print_wrong_tok_error(
        error_nb: u32,
        range: Range<usize>,
        filename: &str,
        content: &str,
        expected_tok: TokenDataTag,
        got_tok: TokenData,
    ) {
        let mut colors = ColorGenerator::new();
        let a = colors.next();
        Report::build(ReportKind::Error, (filename, range.clone()))
            .with_code(error_nb)
            .with_message(
                ::alloc::__export::must_use({
                    ::alloc::fmt::format(
                        format_args!(
                            "Wrong Token : expected {0:?} but got {1:?}",
                            expected_tok,
                            got_tok,
                        ),
                    )
                }),
            )
            .with_label(
                Label::new((filename, range.clone()))
                    .with_message("This is the wrong token")
                    .with_color(a),
            )
            .finish()
            .print((filename, Source::from(content)))
            .unwrap();
    }
    fn print_unexpected_tok_error(
        error_nb: u32,
        range: Range<usize>,
        filename: &str,
        content: &str,
        tok: TokenData,
    ) {
        let mut colors = ColorGenerator::new();
        let a = colors.next();
        Report::build(ReportKind::Error, (filename, range.clone()))
            .with_code(error_nb)
            .with_message(
                ::alloc::__export::must_use({
                    ::alloc::fmt::format(format_args!("Unexpected Token {0:?}", tok))
                }),
            )
            .with_label(
                Label::new((filename, range.clone()))
                    .with_message("This is the wrong token")
                    .with_color(a),
            )
            .finish()
            .print((filename, Source::from(content)))
            .unwrap();
    }
    fn print_type_inference_error(
        error_nb: u32,
        range: Range<usize>,
        filename: &str,
        content: &str,
        arg_name: &str,
    ) {
        let mut colors = ColorGenerator::new();
        let a = colors.next();
        Report::build(ReportKind::Error, (filename, range.clone()))
            .with_code(error_nb)
            .with_message(
                ::alloc::__export::must_use({
                    ::alloc::fmt::format(
                        format_args!(
                            "Type inference error with the argument {0:?}",
                            arg_name,
                        ),
                    )
                }),
            )
            .with_label(
                Label::new((filename, range.clone()))
                    .with_message(
                        "This argument's type couldn't be deduced from the body of the function",
                    )
                    .with_color(a),
            )
            .with_note(
                ::alloc::__export::must_use({
                    ::alloc::fmt::format(
                        format_args!(
                            "Either add a type annotation, or change the body to disambiguate",
                        ),
                    )
                }),
            )
            .finish()
            .print((filename, Source::from(content)))
            .unwrap();
    }
    pub fn print_parser_error(
        parser_error: ParserErr,
        filename: &Path,
        content: &str,
    ) -> ExitCode {
        let error_nb = PARSER_ERROR_OFFSET + parser_error.parser_err_data.tag() as u32;
        let range = parser_error.range;
        let filename_str = filename.to_str().unwrap();
        match *parser_error.parser_err_data {
            ParserErrData::UnexpectedEOF => {
                print_unexpected_eof_error(error_nb, range, filename_str, content)
            }
            ParserErrData::WrongTok { expected_tok, got_tok } => {
                print_wrong_tok_error(
                    error_nb,
                    range,
                    filename_str,
                    content,
                    expected_tok,
                    got_tok,
                )
            }
            ParserErrData::UnexpectedTok { tok } => {
                print_unexpected_tok_error(error_nb, range, filename_str, content, tok)
            }
            ParserErrData::TypeInferenceErr { arg_name } => {
                print_type_inference_error(
                    error_nb,
                    range,
                    filename_str,
                    content,
                    &arg_name,
                )
            }
        };
        ExitCode::FAILURE
    }
}
enum Commands {
    /// intepret file
    Interpret { #[arg(value_name = "FILE")] filename: PathBuf },
    /// compile file
    Compile { #[arg(value_name = "FILE")] filename: PathBuf },
    Check { #[arg(value_name = "FILE")] filename: PathBuf },
}
#[allow(
    dead_code,
    unreachable_code,
    unused_variables,
    unused_braces,
    unused_qualifications,
)]
#[allow(
    clippy::style,
    clippy::complexity,
    clippy::pedantic,
    clippy::restriction,
    clippy::perf,
    clippy::deprecated,
    clippy::nursery,
    clippy::cargo,
    clippy::suspicious_else_formatting,
    clippy::almost_swapped,
    clippy::redundant_locals,
)]
#[automatically_derived]
impl clap::FromArgMatches for Commands {
    fn from_arg_matches(
        __clap_arg_matches: &clap::ArgMatches,
    ) -> ::std::result::Result<Self, clap::Error> {
        Self::from_arg_matches_mut(&mut __clap_arg_matches.clone())
    }
    fn from_arg_matches_mut(
        __clap_arg_matches: &mut clap::ArgMatches,
    ) -> ::std::result::Result<Self, clap::Error> {
        #![allow(deprecated)]
        if let Some((__clap_name, mut __clap_arg_sub_matches)) = __clap_arg_matches
            .remove_subcommand()
        {
            let __clap_arg_matches = &mut __clap_arg_sub_matches;
            if __clap_name == "interpret" && !__clap_arg_matches.contains_id("") {
                return ::std::result::Result::Ok(Self::Interpret {
                    filename: __clap_arg_matches
                        .remove_one::<PathBuf>("filename")
                        .ok_or_else(|| clap::Error::raw(
                            clap::error::ErrorKind::MissingRequiredArgument,
                            "The following required argument was not provided: filename",
                        ))?,
                });
            }
            if __clap_name == "compile" && !__clap_arg_matches.contains_id("") {
                return ::std::result::Result::Ok(Self::Compile {
                    filename: __clap_arg_matches
                        .remove_one::<PathBuf>("filename")
                        .ok_or_else(|| clap::Error::raw(
                            clap::error::ErrorKind::MissingRequiredArgument,
                            "The following required argument was not provided: filename",
                        ))?,
                });
            }
            if __clap_name == "check" && !__clap_arg_matches.contains_id("") {
                return ::std::result::Result::Ok(Self::Check {
                    filename: __clap_arg_matches
                        .remove_one::<PathBuf>("filename")
                        .ok_or_else(|| clap::Error::raw(
                            clap::error::ErrorKind::MissingRequiredArgument,
                            "The following required argument was not provided: filename",
                        ))?,
                });
            }
            ::std::result::Result::Err(
                clap::Error::raw(
                    clap::error::ErrorKind::InvalidSubcommand,
                    ::alloc::__export::must_use({
                        ::alloc::fmt::format(
                            format_args!(
                                "The subcommand \'{0}\' wasn\'t recognized",
                                __clap_name,
                            ),
                        )
                    }),
                ),
            )
        } else {
            ::std::result::Result::Err(
                clap::Error::raw(
                    clap::error::ErrorKind::MissingSubcommand,
                    "A subcommand is required but one was not provided.",
                ),
            )
        }
    }
    fn update_from_arg_matches(
        &mut self,
        __clap_arg_matches: &clap::ArgMatches,
    ) -> ::std::result::Result<(), clap::Error> {
        self.update_from_arg_matches_mut(&mut __clap_arg_matches.clone())
    }
    fn update_from_arg_matches_mut<'b>(
        &mut self,
        __clap_arg_matches: &mut clap::ArgMatches,
    ) -> ::std::result::Result<(), clap::Error> {
        #![allow(deprecated)]
        if let Some(__clap_name) = __clap_arg_matches.subcommand_name() {
            match self {
                Self::Interpret { filename } if "interpret" == __clap_name => {
                    let (_, mut __clap_arg_sub_matches) = __clap_arg_matches
                        .remove_subcommand()
                        .unwrap();
                    let __clap_arg_matches = &mut __clap_arg_sub_matches;
                    {
                        if __clap_arg_matches.contains_id("filename") {
                            *filename = __clap_arg_matches
                                .remove_one::<PathBuf>("filename")
                                .ok_or_else(|| clap::Error::raw(
                                    clap::error::ErrorKind::MissingRequiredArgument,
                                    "The following required argument was not provided: filename",
                                ))?;
                        }
                    }
                }
                Self::Compile { filename } if "compile" == __clap_name => {
                    let (_, mut __clap_arg_sub_matches) = __clap_arg_matches
                        .remove_subcommand()
                        .unwrap();
                    let __clap_arg_matches = &mut __clap_arg_sub_matches;
                    {
                        if __clap_arg_matches.contains_id("filename") {
                            *filename = __clap_arg_matches
                                .remove_one::<PathBuf>("filename")
                                .ok_or_else(|| clap::Error::raw(
                                    clap::error::ErrorKind::MissingRequiredArgument,
                                    "The following required argument was not provided: filename",
                                ))?;
                        }
                    }
                }
                Self::Check { filename } if "check" == __clap_name => {
                    let (_, mut __clap_arg_sub_matches) = __clap_arg_matches
                        .remove_subcommand()
                        .unwrap();
                    let __clap_arg_matches = &mut __clap_arg_sub_matches;
                    {
                        if __clap_arg_matches.contains_id("filename") {
                            *filename = __clap_arg_matches
                                .remove_one::<PathBuf>("filename")
                                .ok_or_else(|| clap::Error::raw(
                                    clap::error::ErrorKind::MissingRequiredArgument,
                                    "The following required argument was not provided: filename",
                                ))?;
                        }
                    }
                }
                s => {
                    *s = <Self as clap::FromArgMatches>::from_arg_matches_mut(
                        __clap_arg_matches,
                    )?;
                }
            }
        }
        ::std::result::Result::Ok(())
    }
}
#[allow(
    dead_code,
    unreachable_code,
    unused_variables,
    unused_braces,
    unused_qualifications,
)]
#[allow(
    clippy::style,
    clippy::complexity,
    clippy::pedantic,
    clippy::restriction,
    clippy::perf,
    clippy::deprecated,
    clippy::nursery,
    clippy::cargo,
    clippy::suspicious_else_formatting,
    clippy::almost_swapped,
    clippy::redundant_locals,
)]
#[automatically_derived]
impl clap::Subcommand for Commands {
    fn augment_subcommands<'b>(__clap_app: clap::Command) -> clap::Command {
        let __clap_app = __clap_app;
        let __clap_app = __clap_app
            .subcommand({
                let __clap_subcommand = clap::Command::new("interpret");
                {
                    let __clap_subcommand = __clap_subcommand
                        .group(
                            clap::ArgGroup::new("Interpret")
                                .multiple(true)
                                .args({
                                    let members: [clap::Id; 1usize] = [
                                        clap::Id::from("filename"),
                                    ];
                                    members
                                }),
                        );
                    let __clap_subcommand = __clap_subcommand
                        .arg({
                            #[allow(deprecated)]
                            let arg = clap::Arg::new("filename")
                                .value_name("FILENAME")
                                .required(true && clap::ArgAction::Set.takes_values())
                                .value_parser({
                                    use ::clap_builder::builder::impl_prelude::*;
                                    let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                        PathBuf,
                                    >::new();
                                    (&&&&&&auto).value_parser()
                                })
                                .action(clap::ArgAction::Set);
                            let arg = arg.value_name("FILE");
                            let arg = arg;
                            arg
                        });
                    __clap_subcommand.about("intepret file").long_about(None)
                }
            });
        let __clap_app = __clap_app
            .subcommand({
                let __clap_subcommand = clap::Command::new("compile");
                {
                    let __clap_subcommand = __clap_subcommand
                        .group(
                            clap::ArgGroup::new("Compile")
                                .multiple(true)
                                .args({
                                    let members: [clap::Id; 1usize] = [
                                        clap::Id::from("filename"),
                                    ];
                                    members
                                }),
                        );
                    let __clap_subcommand = __clap_subcommand
                        .arg({
                            #[allow(deprecated)]
                            let arg = clap::Arg::new("filename")
                                .value_name("FILENAME")
                                .required(true && clap::ArgAction::Set.takes_values())
                                .value_parser({
                                    use ::clap_builder::builder::impl_prelude::*;
                                    let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                        PathBuf,
                                    >::new();
                                    (&&&&&&auto).value_parser()
                                })
                                .action(clap::ArgAction::Set);
                            let arg = arg.value_name("FILE");
                            let arg = arg;
                            arg
                        });
                    __clap_subcommand.about("compile file").long_about(None)
                }
            });
        let __clap_app = __clap_app
            .subcommand({
                let __clap_subcommand = clap::Command::new("check");
                {
                    let __clap_subcommand = __clap_subcommand
                        .group(
                            clap::ArgGroup::new("Check")
                                .multiple(true)
                                .args({
                                    let members: [clap::Id; 1usize] = [
                                        clap::Id::from("filename"),
                                    ];
                                    members
                                }),
                        );
                    let __clap_subcommand = __clap_subcommand
                        .arg({
                            #[allow(deprecated)]
                            let arg = clap::Arg::new("filename")
                                .value_name("FILENAME")
                                .required(true && clap::ArgAction::Set.takes_values())
                                .value_parser({
                                    use ::clap_builder::builder::impl_prelude::*;
                                    let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                        PathBuf,
                                    >::new();
                                    (&&&&&&auto).value_parser()
                                })
                                .action(clap::ArgAction::Set);
                            let arg = arg.value_name("FILE");
                            let arg = arg;
                            arg
                        });
                    __clap_subcommand
                }
            });
        __clap_app
    }
    fn augment_subcommands_for_update<'b>(__clap_app: clap::Command) -> clap::Command {
        let __clap_app = __clap_app;
        let __clap_app = __clap_app
            .subcommand({
                let __clap_subcommand = clap::Command::new("interpret");
                {
                    let __clap_subcommand = __clap_subcommand
                        .group(
                            clap::ArgGroup::new("Interpret")
                                .multiple(true)
                                .args({
                                    let members: [clap::Id; 1usize] = [
                                        clap::Id::from("filename"),
                                    ];
                                    members
                                }),
                        );
                    let __clap_subcommand = __clap_subcommand
                        .arg({
                            #[allow(deprecated)]
                            let arg = clap::Arg::new("filename")
                                .value_name("FILENAME")
                                .required(true && clap::ArgAction::Set.takes_values())
                                .value_parser({
                                    use ::clap_builder::builder::impl_prelude::*;
                                    let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                        PathBuf,
                                    >::new();
                                    (&&&&&&auto).value_parser()
                                })
                                .action(clap::ArgAction::Set);
                            let arg = arg.value_name("FILE");
                            let arg = arg.required(false);
                            arg
                        });
                    __clap_subcommand.about("intepret file").long_about(None)
                }
            });
        let __clap_app = __clap_app
            .subcommand({
                let __clap_subcommand = clap::Command::new("compile");
                {
                    let __clap_subcommand = __clap_subcommand
                        .group(
                            clap::ArgGroup::new("Compile")
                                .multiple(true)
                                .args({
                                    let members: [clap::Id; 1usize] = [
                                        clap::Id::from("filename"),
                                    ];
                                    members
                                }),
                        );
                    let __clap_subcommand = __clap_subcommand
                        .arg({
                            #[allow(deprecated)]
                            let arg = clap::Arg::new("filename")
                                .value_name("FILENAME")
                                .required(true && clap::ArgAction::Set.takes_values())
                                .value_parser({
                                    use ::clap_builder::builder::impl_prelude::*;
                                    let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                        PathBuf,
                                    >::new();
                                    (&&&&&&auto).value_parser()
                                })
                                .action(clap::ArgAction::Set);
                            let arg = arg.value_name("FILE");
                            let arg = arg.required(false);
                            arg
                        });
                    __clap_subcommand.about("compile file").long_about(None)
                }
            });
        let __clap_app = __clap_app
            .subcommand({
                let __clap_subcommand = clap::Command::new("check");
                {
                    let __clap_subcommand = __clap_subcommand
                        .group(
                            clap::ArgGroup::new("Check")
                                .multiple(true)
                                .args({
                                    let members: [clap::Id; 1usize] = [
                                        clap::Id::from("filename"),
                                    ];
                                    members
                                }),
                        );
                    let __clap_subcommand = __clap_subcommand
                        .arg({
                            #[allow(deprecated)]
                            let arg = clap::Arg::new("filename")
                                .value_name("FILENAME")
                                .required(true && clap::ArgAction::Set.takes_values())
                                .value_parser({
                                    use ::clap_builder::builder::impl_prelude::*;
                                    let auto = ::clap_builder::builder::_infer_ValueParser_for::<
                                        PathBuf,
                                    >::new();
                                    (&&&&&&auto).value_parser()
                                })
                                .action(clap::ArgAction::Set);
                            let arg = arg.value_name("FILE");
                            let arg = arg.required(false);
                            arg
                        });
                    __clap_subcommand
                }
            });
        __clap_app
    }
    fn has_subcommand(__clap_name: &str) -> bool {
        if "interpret" == __clap_name {
            return true;
        }
        if "compile" == __clap_name {
            return true;
        }
        if "check" == __clap_name {
            return true;
        }
        false
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Commands {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match self {
            Commands::Interpret { filename: __self_0 } => {
                ::core::fmt::Formatter::debug_struct_field1_finish(
                    f,
                    "Interpret",
                    "filename",
                    &__self_0,
                )
            }
            Commands::Compile { filename: __self_0 } => {
                ::core::fmt::Formatter::debug_struct_field1_finish(
                    f,
                    "Compile",
                    "filename",
                    &__self_0,
                )
            }
            Commands::Check { filename: __self_0 } => {
                ::core::fmt::Formatter::debug_struct_field1_finish(
                    f,
                    "Check",
                    "filename",
                    &__self_0,
                )
            }
        }
    }
}
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}
#[automatically_derived]
#[allow(unused_qualifications, clippy::redundant_locals)]
impl clap::Parser for Args {}
#[allow(
    dead_code,
    unreachable_code,
    unused_variables,
    unused_braces,
    unused_qualifications,
)]
#[allow(
    clippy::style,
    clippy::complexity,
    clippy::pedantic,
    clippy::restriction,
    clippy::perf,
    clippy::deprecated,
    clippy::nursery,
    clippy::cargo,
    clippy::suspicious_else_formatting,
    clippy::almost_swapped,
    clippy::redundant_locals,
)]
#[automatically_derived]
impl clap::CommandFactory for Args {
    fn command<'b>() -> clap::Command {
        let __clap_app = clap::Command::new("rustaml");
        <Self as clap::Args>::augment_args(__clap_app)
    }
    fn command_for_update<'b>() -> clap::Command {
        let __clap_app = clap::Command::new("rustaml");
        <Self as clap::Args>::augment_args_for_update(__clap_app)
    }
}
#[allow(
    dead_code,
    unreachable_code,
    unused_variables,
    unused_braces,
    unused_qualifications,
)]
#[allow(
    clippy::style,
    clippy::complexity,
    clippy::pedantic,
    clippy::restriction,
    clippy::perf,
    clippy::deprecated,
    clippy::nursery,
    clippy::cargo,
    clippy::suspicious_else_formatting,
    clippy::almost_swapped,
    clippy::redundant_locals,
)]
#[automatically_derived]
impl clap::FromArgMatches for Args {
    fn from_arg_matches(
        __clap_arg_matches: &clap::ArgMatches,
    ) -> ::std::result::Result<Self, clap::Error> {
        Self::from_arg_matches_mut(&mut __clap_arg_matches.clone())
    }
    fn from_arg_matches_mut(
        __clap_arg_matches: &mut clap::ArgMatches,
    ) -> ::std::result::Result<Self, clap::Error> {
        #![allow(deprecated)]
        let v = Args {
            command: {
                if __clap_arg_matches
                    .subcommand_name()
                    .map(<Commands as clap::Subcommand>::has_subcommand)
                    .unwrap_or(false)
                {
                    Some(
                        <Commands as clap::FromArgMatches>::from_arg_matches_mut(
                            __clap_arg_matches,
                        )?,
                    )
                } else {
                    None
                }
            },
        };
        ::std::result::Result::Ok(v)
    }
    fn update_from_arg_matches(
        &mut self,
        __clap_arg_matches: &clap::ArgMatches,
    ) -> ::std::result::Result<(), clap::Error> {
        self.update_from_arg_matches_mut(&mut __clap_arg_matches.clone())
    }
    fn update_from_arg_matches_mut(
        &mut self,
        __clap_arg_matches: &mut clap::ArgMatches,
    ) -> ::std::result::Result<(), clap::Error> {
        #![allow(deprecated)]
        {
            #[allow(non_snake_case)]
            let command = &mut self.command;
            if let Some(command) = command.as_mut() {
                <Commands as clap::FromArgMatches>::update_from_arg_matches_mut(
                    command,
                    __clap_arg_matches,
                )?;
            } else {
                *command = Some(
                    <Commands as clap::FromArgMatches>::from_arg_matches_mut(
                        __clap_arg_matches,
                    )?,
                );
            }
        }
        ::std::result::Result::Ok(())
    }
}
#[allow(
    dead_code,
    unreachable_code,
    unused_variables,
    unused_braces,
    unused_qualifications,
)]
#[allow(
    clippy::style,
    clippy::complexity,
    clippy::pedantic,
    clippy::restriction,
    clippy::perf,
    clippy::deprecated,
    clippy::nursery,
    clippy::cargo,
    clippy::suspicious_else_formatting,
    clippy::almost_swapped,
    clippy::redundant_locals,
)]
#[automatically_derived]
impl clap::Args for Args {
    fn group_id() -> Option<clap::Id> {
        Some(clap::Id::from("Args"))
    }
    fn augment_args<'b>(__clap_app: clap::Command) -> clap::Command {
        {
            let __clap_app = __clap_app
                .group(
                    clap::ArgGroup::new("Args")
                        .multiple(true)
                        .args({
                            let members: [clap::Id; 0usize] = [];
                            members
                        }),
                );
            let __clap_app = <Commands as clap::Subcommand>::augment_subcommands(
                __clap_app,
            );
            let __clap_app = __clap_app;
            __clap_app.version("0.1.0").long_about(None)
        }
    }
    fn augment_args_for_update<'b>(__clap_app: clap::Command) -> clap::Command {
        {
            let __clap_app = __clap_app
                .group(
                    clap::ArgGroup::new("Args")
                        .multiple(true)
                        .args({
                            let members: [clap::Id; 0usize] = [];
                            members
                        }),
                );
            let __clap_app = <Commands as clap::Subcommand>::augment_subcommands(
                __clap_app,
            );
            let __clap_app = __clap_app
                .subcommand_required(false)
                .arg_required_else_help(false);
            __clap_app.version("0.1.0").long_about(None)
        }
    }
}
#[automatically_derived]
impl ::core::default::Default for Args {
    #[inline]
    fn default() -> Args {
        Args {
            command: ::core::default::Default::default(),
        }
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for Args {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field1_finish(
            f,
            "Args",
            "command",
            &&self.command,
        )
    }
}
fn get_ast(
    filename: &Path,
    str_interner: &mut StrInterner,
) -> Result<ASTNode, ExitCode> {
    let content_bytes = fs::read(filename)
        .unwrap_or_else(|err| {
            {
                ::core::panicking::panic_fmt(
                    format_args!("Error when opening {0} : {1}", filename.display(), err),
                );
            }
        });
    let content = content_bytes.iter().map(|b| *b as char).collect::<Vec<_>>();
    let tokens = lexer::lex(content);
    let tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            let content = &String::from_utf8(content_bytes).unwrap();
            return Err(print_error::print_lexer_error(e, filename, content));
        }
    };
    let ast = ast::parse(tokens, str_interner);
    let ast = match ast {
        Ok(a) => a,
        Err(e) => {
            let content = &String::from_utf8(content_bytes).unwrap();
            return Err(print_error::print_parser_error(e, filename, content));
        }
    };
    Ok(ast)
}
fn main() -> ExitCode {
    let args = Args::parse();
    let mut str_interner = StrInterner::new();
    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename } => {
            let ast = get_ast(&filename, &mut str_interner);
            let ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };
            intepreter::interpret(ast, &mut str_interner)
        }
        Commands::Compile { filename } => {
            let ast = get_ast(&filename, &mut str_interner);
            let ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };
            black_box(ast);
            ::core::panicking::panic("not yet implemented")
        }
        Commands::Check { filename } => {
            let ast = get_ast(&filename, &mut str_interner);
            let _ast = match ast {
                Ok(a) => a,
                Err(e) => return e,
            };
            ExitCode::SUCCESS
        }
    }
}
