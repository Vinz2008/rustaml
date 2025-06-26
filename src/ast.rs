use std::ops::Range;
use std::fmt::Debug;

use rustc_hash::FxHashMap;

use enum_tags::{Tag, TaggedEnum};

use crate::{debug::{DebugWithContext, DebugWrapContext}, lexer::{Operator, Token, TokenData, TokenDataTag}, rustaml::RustamlContext, string_intern::StringRef, type_inference::{infer_var_type, TypeInferenceErr}};

#[derive(Clone, PartialEq)]
pub struct Arg {
    pub name : StringRef,
    pub arg_type : Type,
}



impl DebugWithContext for Arg {    
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, rustaml_context : &RustamlContext) -> std::fmt::Result {
        f.debug_struct("Arg")
        .field_with("name",  |fmt| {
            self.name.fmt_with_context(fmt, rustaml_context)
        })
        .field("arg_type", &self.arg_type).finish()
    }
}

// TODO : create a pattern pool ?

// TODO : add a guard clauses (create struct with an enum and guard clauses)

#[derive(Clone, PartialEq,)]
pub enum Pattern {
    VarName(StringRef), // | x pattern
    Integer(i64), // | 2
    Float(f64), // | 2.6
    Range(i64, i64, bool), // bool is for the inclusivity | 0..1
    String(StringRef), // | "test"
    List(Vec<Pattern>), // | [1, 2, 3] // TODO : replace vec with Box<[Pattern]
    ListDestructure(StringRef, Box<Pattern>), // head name then tail name TODO : refactor to be recursive so you can have e::e2::l
    Underscore,
}

impl DebugWithContext for Pattern {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, rustaml_context: &RustamlContext) -> std::fmt::Result {
        match self {
            Self::VarName(arg0) => f.debug_tuple("VarName").field(&arg0.get_str(&rustaml_context.str_interner)).finish(),
            Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::Range(arg0, arg1, arg2) => f.debug_tuple("Range").field(arg0).field(arg1).field(arg2).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(&arg0.get_str(&rustaml_context.str_interner)).finish(),
            Self::List(arg0) => f.debug_tuple("List").field_with(|fmt| arg0.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::ListDestructure(arg0, arg1) => f.debug_tuple("ListDestructure").field(&arg0.get_str(&rustaml_context.str_interner)).field_with(|fmt| arg1.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::Underscore => write!(f, "Underscore"),
        }
    }
}

pub struct ASTPool(Vec<ASTNode>);

impl ASTPool {
    pub fn new() -> ASTPool {
        ASTPool(Vec::new())
    }
    pub fn get(&self, expr : ASTRef) -> &ASTNode {
        &self.0[expr.0 as usize]
    }

    fn push(&mut self, node : ASTNode) -> ASTRef {
        let idx = self.0.len();
        self.0.push(node);
        ASTRef(idx.try_into().expect("too many ast nodes in the pool"))
    }
}


#[derive(Clone, Copy, PartialEq)]
pub struct ASTRef(u32);

impl ASTRef {
    pub fn get(self, ast_pool : &ASTPool) -> &ASTNode {
        ast_pool.get(self)
    }
}

impl DebugWithContext for ASTRef {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, rustaml_context : &RustamlContext) -> std::fmt::Result {
        self.get(&rustaml_context.ast_pool).fmt_with_context(f, rustaml_context)
    }
}


#[derive(Clone, PartialEq)]
pub enum ASTNode {
    TopLevel {
        nodes: Vec<ASTRef>,
    },
    FunctionDefinition {
        name : StringRef,
        args : Vec<Arg>,
        body : ASTRef,
        return_type : Type,
    },
    VarDecl {
        name: StringRef,
        val: ASTRef,
        body : Option<ASTRef>,
    },
    VarUse {
        name : StringRef,
    },
    IfExpr {
        cond_expr : ASTRef,
        then_body : ASTRef,
        else_body : ASTRef,
    },
    MatchExpr {
        matched_expr : ASTRef,
        patterns : Vec<(Pattern, ASTRef)>,
    },
    Integer {
        nb: i64,
    },
    Float {
        nb: f64,
    },
    String {
        str : StringRef
    },
    List {
        list : Vec<ASTRef>,
    },
    Boolean {
        b : bool,
    },
    BinaryOp {
        op: Operator,
        lhs: ASTRef,
        rhs: ASTRef,
    },
    // TODO : UnaryOp
    FunctionCall {
        name : StringRef,
        args : Vec<ASTRef>,
    }
}

impl DebugWithContext for ASTNode {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, rustaml_context : &RustamlContext) -> std::fmt::Result {
        match self {
            Self::TopLevel { nodes } => f.debug_struct("TopLevel").field_with("nodes", |fmt| nodes.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::FunctionDefinition { name, args, body, return_type } => f.debug_struct("FunctionDefinition").field("name", &name.get_str(&rustaml_context.str_interner)).field_with("args", |fmt| args.fmt_with_context(fmt, rustaml_context)).field_with("body", |fmt| body.get(&rustaml_context.ast_pool).fmt_with_context(fmt, rustaml_context)).field("return_type", return_type).finish(),
            Self::VarDecl { name, val, body } => f.debug_struct("VarDecl").field("name", &name.get_str(&rustaml_context.str_interner)).field_with("val", |fmt| val.get(&rustaml_context.ast_pool).fmt_with_context(fmt, rustaml_context)).field_with("body", |fmt| body.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::VarUse { name } => f.debug_struct("VarUse").field("name", &name.get_str(&rustaml_context.str_interner)).finish(),
            Self::IfExpr { cond_expr, then_body, else_body } => f.debug_struct("IfExpr").field_with("cond_expr", |fmt|  cond_expr.get(&rustaml_context.ast_pool).fmt_with_context(fmt, rustaml_context)).field_with("then_body", |fmt| then_body.fmt_with_context(fmt, rustaml_context)).field_with("else_body", |fmt| else_body.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::MatchExpr { matched_expr, patterns } => f.debug_struct("MatchExpr").field_with("matched_expr", |fmt| matched_expr.get(&rustaml_context.ast_pool).fmt_with_context(fmt, rustaml_context)).field_with("patterns", |fmt| patterns.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::Integer { nb } => f.debug_struct("Integer").field("nb", nb).finish(),
            Self::Float { nb } => f.debug_struct("Float").field("nb", nb).finish(),
            Self::String { str } => f.debug_struct("String").field("str", &str.get_str(&rustaml_context.str_interner)).finish(),
            Self::List { list } => f.debug_struct("List").field_with("list", |fmt| list.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::Boolean { b } => f.debug_struct("Boolean").field("b", b).finish(),
            Self::BinaryOp { op, lhs, rhs } => f.debug_struct("BinaryOp").field("op", op).field_with("lhs", |fmt| lhs.fmt_with_context(fmt, rustaml_context)).field_with("rhs", |fmt| rhs.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::FunctionCall { name, args } => f.debug_struct("FunctionCall").field("name", &name.get_str(&rustaml_context.str_interner)).field_with("args", |fmt| args.fmt_with_context(fmt, rustaml_context)).finish(),
        }
    }
}


// TODO : add a type pool to remove boxes (test performance ?)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    Float,
    Bool,
    Function(Vec<Type>, Box<Type>),
    Str,
    List(Box<Type>),
    // TODO : add a number to any (to have 'a, 'b, etc)
    Any, // equivalent to 'a
    Unit,
}

impl DebugWithContext for Type {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, _rustaml_context: &RustamlContext) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ASTNode {
    pub fn get_type(&self, rustaml_context : &RustamlContext, vars: &FxHashMap<StringRef, Type>) -> Type {
        match self {
            ASTNode::Boolean { b: _ } => Type::Bool,
            ASTNode::Integer { nb: _ } => Type::Integer,
            ASTNode::Float { nb : _ } => Type::Float,
            ASTNode::String { str: _ } => Type::Str,
            ASTNode::List { list } => { 
                let elem_type = match list.first() {
                    Some(f) => f.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars),
                    None => Type::Any,
                };
                Type::List(Box::new(elem_type)) 
            },
            ASTNode::BinaryOp { op, lhs: _, rhs: _ } => op.get_type(),
            ASTNode::VarDecl { name: _, val: _, body: _ } => Type::Unit, // TODO
            ASTNode::FunctionCall { name, args: _ } => { 
                let func_type = vars.get(name).unwrap();
                match func_type {
                    Type::Function(_args, ret) => ret.as_ref().clone(),
                    _ => panic!("Trying to call a function"),
                } 
            },
            ASTNode::VarUse { name} => match vars.get(name){
                Some(t) => t.clone(),
                None => unreachable!("Unknown var {}", name.get_str(&rustaml_context.str_interner))
             },
            ASTNode::IfExpr { cond_expr: _, then_body, else_body : _ } => then_body.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars), // no need for typechecking the two branches because it is done when constructing the IfExpr
            ASTNode::MatchExpr { matched_expr: _, patterns } => patterns.first().unwrap().1.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars),
            ASTNode::TopLevel { nodes: _ } => Type::Unit,
            ASTNode::FunctionDefinition { name: _, args: _, body: _, return_type: _ } => Type::Unit,
        }
    }
}


fn init_precedences() -> FxHashMap<Operator, (i32, Associativity)> {
    let mut p = FxHashMap::default();
    // TODO : reserve map size ?
    // see https://ocaml.org/manual/5.3/expr.html#ss%3Aprecedence-and-associativity for precedence ?
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


#[derive(Clone, Copy)]
pub enum Associativity {
    Left, // most operators
    Right, // ::
}

pub struct Parser<'context> {
    tokens: Vec<Token>,
    pos: usize,
    // optional types because of type inference of return values of functions that need to be inserted for recursive functions (TODO ?)
    pub vars : FxHashMap<StringRef, Type>, // include functions (which are just vars with function types)
    precedences : FxHashMap<Operator, (i32, Associativity)>,
    pub rustaml_context : &'context mut RustamlContext,
}

#[derive(Debug, Tag)]
pub enum ParserErrData {
    UnexpectedEOF,
    UnexpectedTok {
        tok : TokenData,
    },
    WrongTok {
        expected_tok : TokenDataTag,
        got_tok : TokenData,
    },
    UnknownTypeAnnotation {

    },
    TypeInferenceErr {
        arg_name: String,
    }
}


#[derive(Debug)]
pub struct ParserErr {
    pub parser_err_data : Box<ParserErrData>,
    pub range : Range<usize>,
}

impl ParserErr {
    pub fn new(parser_err_data : ParserErrData, range : Range<usize>) -> ParserErr {
        ParserErr { parser_err_data: Box::new(parser_err_data), range }
    }
}

impl From<TypeInferenceErr> for ParserErr {
    fn from(err: TypeInferenceErr) -> Self {
        ParserErr::new(
            ParserErrData::TypeInferenceErr { arg_name: *err.arg_name },
            err.range,
        )
    }
}

impl Parser<'_> {
    fn has_tokens_left(&self) -> bool {
        self.pos + 1 < self.tokens.len()
    }

    fn eat_tok(&mut self, token_type: Option<TokenDataTag>) -> Result<Token, ParserErr> {
        if self.pos >= self.tokens.len() {
            let pos = self.tokens.len()-1;
            return Err(ParserErr::new(ParserErrData::UnexpectedEOF, pos..pos));
        }

        if let Some(tok_type) = token_type && self.tokens[self.pos].tok_data.tag() != tok_type {
            return Err(ParserErr::new(ParserErrData::WrongTok {
                expected_tok: tok_type,
                got_tok: self.tokens[self.pos].tok_data.clone(),
            }, self.tokens[self.pos].range.clone()));
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

fn parse_integer(parser: &mut Parser, nb: i64) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::Integer { nb })
}

fn parse_float(parser: &mut Parser, nb: f64) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::Float { nb })
}

fn parse_string(parser: &mut Parser, buf : Vec<char>) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::String { str: parser.rustaml_context.str_interner.intern(&buf.iter().collect::<String>()) })
}



fn parse_annotation_simple(parser: &mut Parser) -> Result<Type, ParserErr> {
    let tok = parser.eat_tok(None)?;
    //dbg!(&tok);
    match &tok.tok_data {
        TokenData::Identifier(b) => {
            let type_annot = match b.iter().collect::<String>().as_str() {
                "int" => Type::Integer,
                "bool" => Type::Bool,
                "float" => Type::Float,
                "str" => Type::Str,
                "list" => {
                    parser.eat_tok(Some(TokenDataTag::ArrayOpen))?;
                    let elem_type = parse_annotation_simple(parser)?; // TODO : replace with parse_annotation to have access to more complicated types ?
                    parser.eat_tok(Some(TokenDataTag::ArrayClose))?;
                    Type::List(Box::new(elem_type))
                },
                _ => return Err(ParserErr::new(ParserErrData::UnknownTypeAnnotation {  }, tok.range.clone())),
            };
            Ok(type_annot)
        },
        TokenData::ParenOpen => {
            parser.eat_tok(Some(TokenDataTag::ParenClose))?;
            Ok(Type::Unit)
        },
        _ => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: tok.tok_data }, tok.range.clone())),
    }
}

// TODO : add type system (Hindley–Milner ?) (and move type checking to after building the AST ?)
fn parse_type_annotation(parser: &mut Parser) -> Result<Type, ParserErr> {
    parser.eat_tok(Some(TokenDataTag::Colon))?;
    
    let simple_type = parse_annotation_simple(parser)?;

    let type_parsed = match parser.current_tok_data() {
        Some(TokenData::Arrow) => {
            // only simple types can be returned or passed to functions, need to refator this code to support cases like (int -> int) -> (int -> int)
            let mut function_type_parts = vec![simple_type];
            println!("parser.current_tok_data() = {:#?}", parser.current_tok_data());
            //dbg!(parser.current_tok_data());
            while let Some(t) = parser.current_tok_data() && matches!(t, TokenData::Arrow) {
                parser.eat_tok(Some(TokenDataTag::Arrow))?;
                let function_type_part = parse_annotation_simple(parser)?;
                function_type_parts.push(function_type_part);
                //dbg!((parser.current_tok_data(), matches!(parser.current_tok_data(), Some(TokenData::Arrow))));
            }
            let return_type = function_type_parts.pop();
            let return_type = match return_type {
                Some(t) if !function_type_parts.is_empty() => t,
                _ => panic!("ERROR : missing type in function type annotation, found return type of {:?} and args of {:?}", return_type, function_type_parts), // TODO : better error handling
            };

            Type::Function(function_type_parts, Box::new(return_type))
        },
        _ => simple_type,
    };
    
    Ok(type_parsed)
}

fn parse_let(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let name_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;
    let name = match name_tok.tok_data {
        TokenData::Identifier(s) => parser.rustaml_context.str_interner.intern(&s.iter().collect::<String>()),
        _ => unreachable!(),
    };
    let node = if matches!(parser.current_tok_data(), Some(TokenData::Identifier(_))) {
        // function definition
        let mut arg_names = Vec::new();
        let mut arg_ranges = Vec::new();
        while matches!(parser.current_tok_data(), Some(TokenData::Identifier(_))) {
            let arg_identifier = parser.eat_tok(Some(TokenDataTag::Identifier)).unwrap();
            let arg_name = match arg_identifier.tok_data {
                TokenData::Identifier(s) => s.iter().collect::<String>(),
                _ => unreachable!(),
            };

            arg_ranges.push(arg_identifier.range);

            arg_names.push(arg_name);
        }

        //let arg_types = vec![Type::Number; args.len()];

        let function_type: Type = match parser.current_tok_data() {
            Some(TokenData::Colon) => parse_type_annotation(parser)?,
            Some(_) | None => {
                Type::Function(vec![Type::Any; arg_names.len()], Box::new(Type::Any))
                /*let mut arg_types = Vec::new();
                for arg_name in &arg_names {
                    arg_types.push(infer_var_type(parser, arg_name, node));
                }
                Type::Function(arg_types, None)*/
            }
        };
        let (arg_types, mut return_type) = match function_type {
            Type::Function(a, r) => (a, r),
            _ => panic!("Expected a function type"), // TODO 
        };

    
        parser.vars.insert(name,  Type::Function(arg_types.clone(), return_type.clone()));

        let mut args = arg_names.into_iter().zip(arg_types.clone()).map(|x| Arg { name: parser.rustaml_context.str_interner.intern(&x.0), arg_type: x.1 }).collect::<Vec<Arg>>();

        for Arg { name, arg_type} in &args {
            parser.vars.insert(*name, arg_type.clone());
        }

        let equal_tok = parser.eat_tok(Some(TokenDataTag::Op));

        match equal_tok.map(|t| t.tok_data) {
            Ok(TokenData::Op(Operator::Equal)) => {},
            Ok(t) => panic!("expected equal in let expr, got {:?}", t),
            Err(e) => panic!("Error when expecting equal in let expr : {:?}", e),
        };

        let body = parse_node(parser)?;

        if matches!(return_type.as_ref(), Type::Any){
            let body_type = body.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars);
            return_type = Box::new(body_type);
        }

        for (arg, arg_range) in args.iter_mut().zip(arg_ranges) {
            if matches!(arg.arg_type, Type::Any){
                arg.arg_type = infer_var_type(parser.rustaml_context,  &parser.vars, arg.name, body, &arg_range)?;
                parser.vars.insert(arg.name, arg.arg_type.clone());
            }
        }

        for Arg {name, arg_type: _} in &args {
            parser.vars.remove(name);
        }


        parser.vars.insert(name,  Type::Function(arg_types.clone(), return_type.clone())); // reinsertion with real types
        
        ASTNode::FunctionDefinition { 
            name, 
            args, 
            body,
            return_type: *return_type,
        }
    } else {
        let mut var_type = match parser.current_tok_data() {
            Some(TokenData::Colon) => Some(parse_type_annotation(parser)?),
            Some(_) | None => None,
        };


        let tok = parser.eat_tok(Some(TokenDataTag::Op))?;
        match &tok.tok_data {
            TokenData::Op(Operator::Equal) => {},
            _ => return Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: tok.tok_data }, tok.range)),
        };

        let val_node = parse_node(parser)?;
        if var_type.is_none() {
            var_type = Some(val_node.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars))
        }

        parser.vars.insert(name, var_type.unwrap());

        let body = match parser.current_tok_data() {
            Some(TokenData::In) => {
                parser.eat_tok(Some(TokenDataTag::In))?;
                Some(parse_node(parser)?)
            },
            _ => None,
        };

        if body.is_some() {
            // if has body, is a local variable
            parser.vars.remove(&name);
        }

        ASTNode::VarDecl {
            name,
            val: val_node,
            body,
        }
    };

    if let Some(TokenData::EndOfExpr) = parser.current_tok_data() {
        parser.eat_tok(Some(TokenDataTag::EndOfExpr)).unwrap();
    }

    Ok(parser.rustaml_context.ast_pool.push(node))
    
}

// for parsing operators https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html



fn parse_function_call(parser: &mut Parser, function_name : StringRef) -> Result<ASTRef, ParserErr> {
    let mut args = Vec::new();

    fn function_call_parse_continue(tok_data : Option<&TokenData>) -> bool {
        !matches!(tok_data, Some(TokenData::EndOfExpr) | Some(TokenData::Op(_)) | Some(TokenData::Else) | Some(TokenData::In))
    }

    while parser.has_tokens_left() && function_call_parse_continue(parser.current_tok_data()) {
        let arg = parse_primary(parser)?; // TODO : replace with parse_node ? (fix problems with stack overflow -> less recursion ? implement tail call optimization ?)
        args.push(arg);
    }
    //dbg!(&args);
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::FunctionCall { 
        name: function_name, 
        args,
    }))
}

fn parse_identifier_expr(parser: &mut Parser, identifier_buf : Vec<char>) -> Result<ASTRef, ParserErr> {
    let identifier = parser.rustaml_context.str_interner.intern(&identifier_buf.iter().collect::<String>());
    let is_function = match parser.vars.get(&identifier) {
        Some(t) => matches!(t, Type::Function(_, _)),
        None => false, // no error because there are variables that are created in match that are not accounted for in vars (TODO !!!)
        //None => panic!("ERROR : unknown identifier {}", identifier.get_str(&mut parser.rustaml_context.str_interner)),
    };
    if is_function {
        parse_function_call(parser, identifier)
    } else {
        // var use
        Ok(parser.rustaml_context.ast_pool.push(ASTNode::VarUse { name: identifier }))
    }
    
}

fn parse_if(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let cond_expr = parse_node(parser)?;

    match cond_expr.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars) {
        Type::Bool => {},
        t => panic!("Error in type checking : {:?} type passed in if expr", t), // TODO : return a result instead
    }

    parser.eat_tok(Some(TokenDataTag::Then))?;

    let then_body = parse_node(parser)?;

    parser.eat_tok(Some(TokenDataTag::Else))?;

    let else_body = parse_node(parser)?;
    
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::IfExpr { 
        cond_expr, 
        then_body, 
        else_body 
    }))
}


// parse the form a, b, c] (it doesn't pass the '[') , helper function to deduplicate code between the exprs and patterns 
fn parse_list_form<T, F>(parser: &mut Parser, parse_elem_fun : F ) -> Result<Vec<T>, ParserErr>
where F: Fn(&mut Parser) -> Result<T, ParserErr>
{
    let mut iter_nb = 0;
    let mut elems = Vec::new();
    while !matches!(parser.current_tok_data(), Some(TokenData::ArrayClose)){
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

fn parse_pattern(parser : &mut Parser) -> Result<Pattern, ParserErr> {
    let pattern_tok = parser.eat_tok(None)?;

    let pattern = match pattern_tok.tok_data {
        TokenData::Identifier(buf) => {
            if matches!(parser.current_tok_data(), Some(TokenData::Op(Operator::ListAppend))){
                parser.eat_tok(Some(TokenDataTag::Op))?;

                let head = buf.iter().collect::<String>();
                let tail_pattern = parse_pattern(parser)?;
                Pattern::ListDestructure(parser.rustaml_context.str_interner.intern(&head), Box::new(tail_pattern))
            } else {
                let s = buf.iter().collect::<String>();
                match s.as_str() {
                    "_" => Pattern::Underscore,
                    s_ref => Pattern::VarName(parser.rustaml_context.str_interner.intern(s_ref)),
                }
            }
            
        },
        TokenData::Integer(nb) => { 
            if matches!(parser.current_tok_data(), Some(TokenData::Range) | Some(TokenData::Op(Operator::Equal))) {
                let inclusivity = matches!(parser.current_tok_data(), Some(TokenData::Op(Operator::Equal)));
                parser.eat_tok(None)?;
                if inclusivity {
                    parser.eat_tok(Some(TokenDataTag::Range))?;
                }
                let end_tok = parser.eat_tok(Some(TokenDataTag::Integer))?;
                let end_nb = match end_tok.tok_data {
                    TokenData::Integer(end) => end,
                    _ => unreachable!(),
                };
                Pattern::Range(nb, end_nb, inclusivity)

            } else {
                Pattern::Integer(nb)
            } 
        },
        TokenData::Float(nb) => Pattern::Float(nb),
        TokenData::String(s) => Pattern::String(parser.rustaml_context.str_interner.intern(&s.iter().collect::<String>())),
        TokenData::ArrayOpen => {
            let elems = parse_list_form(parser, parse_pattern)?;

            Pattern::List(elems)
        },
        t => return Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, pattern_tok.range)),
    };

    Ok(pattern)

}

fn parse_match(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let matched_expr = parse_primary(parser)?;
    parser.eat_tok(Some(TokenDataTag::With))?;
    let mut patterns = Vec::new();

    if !matches!(parser.current_tok_data(), Some(TokenData::Pipe)){
        let err = match parser.current_tok() {
            Some(t) => ParserErr::new(ParserErrData::WrongTok { expected_tok: TokenDataTag::Pipe, got_tok: t.tok_data.clone() }, parser.current_tok().unwrap().range.clone()),
            None => ParserErr::new(ParserErrData::UnexpectedEOF, parser.tokens.len()-1..parser.tokens.len()-1),
        };
        return Err(err)
    }

    while parser.current_tok().is_some() && matches!(parser.current_tok_data().unwrap(), TokenData::Pipe) {
        parser.eat_tok(Some(TokenDataTag::Pipe))?;
        let pattern = parse_pattern(parser)?;
        //dbg!(&pattern);
        parser.eat_tok(Some(TokenDataTag::Arrow))?;
        // TODO : add vars from match pattern ? (will need a function that from a pattern and the type of the matched pattern will return the names and the types of the vars)
        let pattern_expr = parse_node(parser)?;
        patterns.push((pattern, pattern_expr));
    }

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::MatchExpr { 
        matched_expr, 
        patterns, 
    }))
}


// TODO : fix parsing parenthesis in parenthesis ex : (fib_list (i-1))
fn parse_parenthesis(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let expr = parse_node(parser)?;
    println!("expr = {:#?}", DebugWrapContext::new(&expr, parser.rustaml_context));
    //dbg_intern!(&expr, &parser.rustaml_context);
    parser.eat_tok(Some(TokenDataTag::ParenClose))?;
    Ok(expr)
}

fn parse_static_list(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let elems = parse_list_form(parser, parse_node)?;
    
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::List { list: elems }))
}

fn parse_primary(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let tok = parser.eat_tok(None).unwrap();
    let node = match tok.tok_data {
        TokenData::Let => parse_let(parser),
        TokenData::If => parse_if(parser),
        TokenData::Match => parse_match(parser),
        TokenData::Integer(nb) => Ok(parse_integer(parser, nb)),
        TokenData::Float(nb) => Ok(parse_float(parser, nb)),
        TokenData::String(buf) => Ok(parse_string(parser, buf)),
        TokenData::Identifier(buf) => parse_identifier_expr(parser, buf),
        TokenData::True => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: true })),
        TokenData::False => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: false })),
        TokenData::ParenOpen => parse_parenthesis(parser), // TODO : move this to the start of parse_node and make it unreachable! ? (because each time there are parenthesis, parse_node -> parse_primary -> parse_node is added to the call stack) 
        TokenData::ArrayOpen => parse_static_list(parser),
        t => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, tok.range))
    };

    return node;
}

fn parse_binary_rec(parser: &mut Parser, lhs: ASTRef, min_precedence: i32) -> Result<ASTRef, ParserErr> {
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
            let new_operator =  match current_tok_data {
                Some(TokenData::Op(op)) => op,
                Some(_) | None => break,
            };
            let (precedence, associativity) = *parser.precedences.get(new_operator).unwrap();
            /*if precedence <= first_precedence {
                break;
            }*/

            if precedence < first_precedence || (precedence == first_precedence && matches!(associativity, Associativity::Left)){
                break;
            }

            /*let new_precedence = if precedence > first_precedence {
                first_precedence + 1
            } else {
                first_precedence
            };*/

            let new_precedence = match associativity {
                Associativity::Left => precedence + 1,
                Associativity::Right => precedence,
            };

            rhs = parse_binary_rec(parser, rhs, new_precedence)?;
        }

        lhs = parser.rustaml_context.ast_pool.push(ASTNode::BinaryOp {
            op: operator,
            lhs,
            rhs,
        });
    }

    Ok(lhs)
}

fn parse_binary(parser: &mut Parser, lhs: ASTRef) -> Result<ASTRef, ParserErr> {
    parse_binary_rec(parser, lhs, 0)
}

fn parse_node(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    // TODO : problem with precedence 
    // for example match e with | a -> 1 :: a ;; becomes (match ... 1) :: a and not match ... -> (1 :: a) 
    /*let lhs = match parser.current_tok_data() {
        Some(TokenData::Match) => parse_match(parser)?,
        Some(TokenData::If) => parse_if(parser)?,
        Some(TokenData::Let) => parse_let(parser)?,
        _ => parse_primary(parser)?,
    };*/
    let lhs = parse_primary(parser)?;
    let ret_expr = parse_binary(parser, lhs)?;
    Ok(ret_expr)
}

fn parse_top_level_node(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let mut nodes: Vec<ASTRef> = Vec::new();
    while parser.has_tokens_left() {
        nodes.push(parse_node(parser)?);
    }
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::TopLevel { nodes }))
}

pub fn parse(tokens: Vec<Token>, rustaml_context : &mut RustamlContext) -> Result<(ASTRef, FxHashMap<StringRef, Type>), ParserErr> {
    let (root_node, vars) = { 
        let mut parser = Parser { 
            tokens, 
            pos: 0,
            vars: FxHashMap::default(),
            precedences: init_precedences(),
            rustaml_context,
        };
        let root_node = parse_top_level_node(&mut parser)?;
        (root_node, parser.vars)
    };

    println!("root_node = {:#?}", DebugWrapContext::new(&root_node, rustaml_context));
    Ok((root_node, vars))
}


#[cfg(test)]
mod tests {
    /*use crate::lexer::TokenData;

    use super::*;

    #[test]
    fn parser_simple() {
        // TODO
        let input = vec![TokenData::Let, TokenData::Identifier(vec!['a']), TokenData::Op(Operator::Equal), TokenData::Integer(2), TokenData::EndOfExpr].into_iter().map(|t| Token::new(t, 0..0)).collect();
        let result = parse(input).unwrap();
        let expected =  ASTNode::VarDecl { name: "a".to_string(), val: Box::new(ASTNode::Integer { nb: 2 }), body: None };
        let expected_toplevel = ASTNode::TopLevel { nodes: vec![expected] };
        assert_eq!(result,  expected_toplevel);
    }*/
}