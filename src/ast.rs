use std::path::PathBuf;
use std::{ops::Range, path::Path};
use std::fmt::{Debug, Display, Write};

use rustc_hash::{FxHashMap, FxHashSet};

use enum_tags::{Tag, TaggedEnum};
use pathbuf::pathbuf;

use crate::rustaml::read_file;
use crate::{debug_println, lexer::{Operator, Token, TokenData, TokenDataTag}, rustaml::{get_ast_from_string, RustamlContext}, string_intern::StringRef, types_debug::PrintTypedContext};
use debug_with_context::{DebugWithContext, DebugWrapContext};

// TODO : add a guard clauses (create struct with an enum and guard clauses)

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
#[debug_context(PrintTypedContext)]
pub enum Pattern {
    // TODO : should the VarName and SumTypeVariant be differentiated here in the AST or after ?
    VarName(StringRef), // | x pattern
    SumTypeVariant(StringRef), // | Test1 pattern // TODO : put also the index of the variant to optimize ?
    Integer(i64), // | 2
    Float(f64), // | 2.6
    Bool(bool), // | true
    Range(i64, i64, bool), // bool is for the inclusivity | 0..1
    String(StringRef), // | "test"
    List(Box<[PatternRef]>), // | [1, 2, 3]
    ListDestructure(StringRef, PatternRef), // head name then tail name TODO : refactor to be recursive so you can have e::e2::l
    Underscore,
}

// TODO : add a warning for recursive functions without base case (ex : just a function call directly)

#[derive(Default, Clone)]
pub struct PatternPool {
    pattern_vec : Vec<Pattern>,
    pattern_ranges : Vec<Range<usize>>,
}

impl PatternPool {
    pub fn new () -> PatternPool {
        PatternPool::default()
    }

    pub fn get(&self, pat : PatternRef) -> &Pattern {
        &self.pattern_vec[pat.0 as usize]
    }

    pub fn get_range(&self, pat : PatternRef) -> Range<usize> {
        self.pattern_ranges[pat.0 as usize].clone()
    }

    pub fn push(&mut self, pat : Pattern, range : Range<usize>) -> PatternRef {
        let idx = self.pattern_vec.len();
        self.pattern_vec.push(pat);
        self.pattern_ranges.push(range);
        PatternRef(idx.try_into().unwrap())
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct PatternRef(u32);

impl PatternRef {
    #[inline]
    pub fn get(self, pattern_pool : &PatternPool) -> &Pattern {
        pattern_pool.get(self)
    }

    #[inline]
    pub fn get_range(self, pattern_pool : &PatternPool) -> Range<usize> {
        pattern_pool.get_range(self)
    }
}

impl DebugWithContext<RustamlContext> for PatternRef {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, context: &RustamlContext) -> std::fmt::Result {
        self.get(&context.pattern_pool).fmt_with_context(f, context)
    }
}


#[derive(Default, Clone)]
pub struct ASTPool {
    ast_pool_vec : Vec<ASTNode>,
    pub ast_node_types : Vec<Type>,
    ast_node_ranges : Vec<Range<usize>>, // TODO : replace these ranges with FilePlace (or smth like that) with Range and the filename (that is interned ?)
}

impl ASTPool {
    pub fn new() -> ASTPool {
        ASTPool::default()
    }

    pub fn get(&self, expr : ASTRef) -> &ASTNode {
        &self.ast_pool_vec[expr.0 as usize]
    }

    /*pub fn get_mut(&mut self, expr : ASTRef) -> &mut ASTNode {
        &mut self.ast_pool_vec[expr.0 as usize]
    }*/

    pub fn get_type(&self, expr : ASTRef) -> &Type {
        &self.ast_node_types[expr.0 as usize]
    }

    pub fn set_type(&mut self, expr : ASTRef, t: Type) {
        self.ast_node_types[expr.0 as usize] = t;
    }

    pub fn get_range(&self, node : ASTRef) -> Range<usize> {
        self.ast_node_ranges[node.0 as usize].clone()
    }

    pub fn push(&mut self, node : ASTNode, range : Range<usize>) -> ASTRef {
        self.push_with_type(node, Type::Any, range)
    }

    #[inline]
    pub fn push_with_type(&mut self, node: ASTNode, t : Type, range : Range<usize>) -> ASTRef {
        let idx = self.ast_pool_vec.len();
        self.ast_pool_vec.push(node);
        self.ast_node_types.push(t);
        self.ast_node_ranges.push(range);
        ASTRef(idx.try_into().expect("too many ast nodes in the pool"))
    }
}


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ASTRef(u32);

impl ASTRef {
    #[inline]
    pub fn get(self, ast_pool : &ASTPool) -> &ASTNode {
        ast_pool.get(self)
    }

    /*pub fn get_mut(self, ast_pool : &mut ASTPool) -> &mut ASTNode {
        ast_pool.get_mut(self)
    }*/

    #[inline]
    pub fn get_type(self, ast_pool : &ASTPool) -> &Type {
        ast_pool.get_type(self)
    }

    #[inline]
    pub fn get_range(self, ast_pool : &ASTPool) -> Range<usize> {
        ast_pool.get_range(self)
    }

    #[inline]
    pub fn set_type(self, ast_pool : &mut ASTPool, t: Type){
        ast_pool.set_type(self, t);
    }
}

impl DebugWithContext<RustamlContext> for ASTRef {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, rustaml_context : &RustamlContext) -> std::fmt::Result {
        self.get(&rustaml_context.ast_pool).fmt_with_context(f, rustaml_context)
    }
}


#[derive(Clone, Copy, PartialEq, DebugWithContext)]
pub enum ExternLang {
    C,
    Cpp,
}

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
#[debug_context(PrintTypedContext)]
pub enum ASTNode {
    TopLevel {
        nodes: Box<[ASTRef]>,
    },
    FunctionDefinition {
        name : StringRef,
        args : Box<[StringRef]>,
        body : ASTRef,
        type_annotation : Option<Type>,
    },
    AnonFunc {
        args : Box<[StringRef]>,
        body : ASTRef,
        type_annotation : Option<Type>,
    },
    TypeAlias {
        name : StringRef,
        type_alias : Type,
    },
    ExternFunc {
        name : StringRef,
        type_annotation : Type,
        lang : ExternLang,
        so_str : Option<StringRef>,
    },
    VarDecl {
        name: StringRef,
        val: ASTRef,
        body : Option<ASTRef>,
        var_type : Option<Type>,
    },
    VarUse {
        name : StringRef,
    },
    Variant {
        name : StringRef,
        arg : Option<ASTRef>,
    },
    IfExpr {
        cond_expr : ASTRef,
        then_body : ASTRef,
        else_body : ASTRef,
    },
    MatchExpr {
        matched_expr : ASTRef,
        patterns : Box<[(PatternRef, ASTRef)]>,
    },
    Integer {
        nb: i128, // put i128 to hold i64::Min without the sign
    },
    Float {
        nb: f64,
    },
    String {
        str : StringRef
    },
    List {
        list : Box<[ASTRef]>,
    },
    Boolean {
        b : bool,
    },
    BinaryOp {
        op: Operator,
        lhs: ASTRef,
        rhs: ASTRef,
    },
    UnaryOp {
        op: Operator,
        expr: ASTRef,
    },
    FunctionCall {
        callee : ASTRef,
        args : Box<[ASTRef]>,
    },
    Cast {
        to_type : Type,
        expr : ASTRef,
    },
    Unit,
}


// TODO : add alias for these (be able to use float, double, int, long in code)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    I64,
    U64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub name: Box<str>, // TODO : should it be this (probably not StringRef to not need context to print types ? or should it ?)
    pub associated_type : Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SumType {
    pub name : Option<Box<str>>,
    pub variants : Box<[Variant]>,
}

// TODO : add a type pool to remove boxes (test performance ? normally should be useful for lowering the type size, it would become only 64 bit and we could make it Copy, but we wouldn't use it everywhere there is Type like for other types, just in refence in the type to other types to lower the size while only indexing in the vector when it is really needed)
// THE PROBLEM : would need to make the type system only have functions with only one args, but could do it by returning type of function types, which could even help for currying
#[derive(Debug, Clone, PartialEq, Eq, Hash, Tag)]
pub enum Type {
    Integer,
    Float,
    Bool,
    Function(Box<[Type]>, Box<Type>, bool), // the bool is if the function is variadic
    Str,
    List(Box<Type>),
    Any, // not already resolved type during type checking
    Generic(u32),
    CType(CType),
    SumType(SumType), // box it ? (benchmark to see)
    // TODO : record/product type
    Unit,
    Never,
}

// TODO : replace with macro (add a flag to say that the type just implements debug)
impl <C> DebugWithContext<C> for Type {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, _context: &C) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn best_display_sum_type(){

}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Integer => f.write_str("int"),
            Type::Float => f.write_str("float"),
            Type::Bool => f.write_str("bool"),
            Type::Str => f.write_str("str"),
            Type::List(e) => {
                f.write_str(&format!("list[{}]", e.as_ref()))
            },
            Type::Unit => f.write_str("()"),
            Type::Never => f.write_char('!'),
            Type::SumType(sum_type) => {
                if let Some(s) = sum_type.name.as_deref() {
                    f.write_str(&s)
                } else {
                    todo!() // TODO
                }
            },
            Type::Any => f.write_str("Any"), // TODO
            Type::Generic(_g_idx) => panic!("Can't print generic type"), // TODO ?
            Type::CType(_) => todo!(), // TODO
            Type::Function(_, _, _) => unreachable!(),
        }
        
    }
}

impl Type {
    pub fn contains_generic(&self) -> bool {
        match self {
            Type::Generic(_) => true,
            Type::List(l) => l.contains_generic(),
            Type::Function(args, ret, _) => ret.contains_generic() || args.iter().any(|e| e.contains_generic()),
            _ => false,
        }
    }
}


fn init_precedences() -> FxHashMap<Operator, (i32, Associativity)> {

    // see https://ocaml.org/manual/5.3/expr.html#ss%3Aprecedence-and-associativity for precedence ?

    FxHashMap::from_iter([
        // TODO : should the and and or be Right or Left Associativity ?
        (Operator::StrAppend, (5, Associativity::Right)),
        (Operator::And, (5, Associativity::Right)),
        (Operator::Or, (5, Associativity::Right)),
        (Operator::ListAppend, (6, Associativity::Right)),
        (Operator::ListMerge, (6, Associativity::Left)),
        (Operator::IsEqual, (10, Associativity::Left)),
        (Operator::IsNotEqual, (10, Associativity::Left)),
        (Operator::Superior, (10, Associativity::Left)),
        (Operator::Inferior, (10, Associativity::Left)),
        (Operator::SuperiorOrEqual, (10, Associativity::Left)),
        (Operator::InferiorOrEqual, (10, Associativity::Left)),
        (Operator::Plus, (20, Associativity::Left)),
        (Operator::Minus, (20, Associativity::Left)),
        (Operator::PlusFloat, (20, Associativity::Left)),
        (Operator::MinusFloat, (20, Associativity::Left)),
        (Operator::Mult, (30, Associativity::Left)),
        (Operator::Div, (30, Associativity::Left)),
        (Operator::Rem, (30, Associativity::Left)),
        (Operator::MultFloat, (30, Associativity::Left)),
        (Operator::DivFloat, (30, Associativity::Left)),
        (Operator::RemFloat, (30, Associativity::Left)),
    ])
}


#[derive(Clone, Copy)]
pub enum Associativity {
    Left, // most operators
    Right, // ::
}

pub struct Parser<'context> {
    tokens: Box<[Token]>,
    pos: usize,
    precedences : FxHashMap<Operator, (i32, Associativity)>,
    filename : PathBuf,
    pub rustaml_context : &'context mut RustamlContext,
    imported_files : &'context mut FxHashSet<PathBuf>,
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
        type_str : String
    },
    NotFunctionTypeInAnnotationLet {
        function_name: String
    },
    ImportError,
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

impl Parser<'_> {
    fn has_tokens_left(&self) -> bool {
        // TODO : this was added to make it work in the case of a single tok in a repl (can we make this cleaner ?)
        if self.pos == 0 && self.tokens.len() == 1 {
            return true;
        }
        self.pos + 1 < self.tokens.len()
    }

    fn eat_tok(&mut self, token_type: Option<TokenDataTag>) -> Result<Token, ParserErr> {
        if self.pos >= self.tokens.len() {
            let last_token_range_end = self.tokens.last().unwrap().range.end;
            return Err(ParserErr::new(ParserErrData::UnexpectedEOF, last_token_range_end..last_token_range_end));
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

fn parse_integer(parser: &mut Parser, nb: i128, range : Range<usize>) -> ASTRef {
    //let nb = nb.try_into().expect("Number is more than 64 bits
    parser.rustaml_context.ast_pool.push(ASTNode::Integer { nb }, range)
}

fn parse_float(parser: &mut Parser, nb: f64, range : Range<usize>) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::Float { nb }, range)
}

fn parse_string(parser: &mut Parser, buf : &[char], range : Range<usize>) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::String { str: parser.rustaml_context.str_interner.intern_compiler(&buf.iter().collect::<String>()) }, range)
}



// the usize is the range end
fn parse_annotation_simple(parser: &mut Parser) -> Result<(Type, usize), ParserErr> {
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
                    let (elem_type, _) = parse_annotation_simple(parser)?; // TODO : replace with parse_annotation to have access to more complicated types ?
                    let array_close_tok = parser.eat_tok(Some(TokenDataTag::ArrayClose))?;
                    let type_annot = Type::List(Box::new(elem_type));
                    return Ok((type_annot, array_close_tok.range.end));
                },
                "c_type" => {
                    parser.eat_tok(Some(TokenDataTag::Dot))?;
                    let c_type_name_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;
                    let c_type_name = match c_type_name_tok.tok_data {
                        TokenData::Identifier(i) => i.iter().collect::<String>(),
                        _ => unreachable!(),
                    };

                    let c_type = match c_type_name.as_str() {
                        // TODO : where do you put char ?
                        "u8" | "uchar" => CType::U8,
                        "i8" | "char" => CType::I8,
                        "u16" | "ushort" => CType::U16,
                        "i16" | "short" => CType::I16,
                        "u32" | "uint" => CType::U32,
                        "i32" | "int" => CType::I32,
                        "u64" | "size_t" => CType::U64,
                        "i64" | "long" => CType::I64,
                        "f32" | "float" => CType::F32,
                        "f64" | "double" => CType::F64,
                        _ => panic!("Invalid c type"),
                    };

                    return Ok((Type::CType(c_type) , c_type_name_tok.range.end))
                }
                s => {
                    if parser.rustaml_context.str_interner.is_str_present(s){ 
                        let s = parser.rustaml_context.str_interner.intern_compiler(s);
                        if let Some(t) = parser.rustaml_context.type_aliases.get(&s) {
                            return Ok((t.clone(), tok.range.end));
                        }
                    }
                    return Err(ParserErr::new(ParserErrData::UnknownTypeAnnotation { type_str: s.to_owned() }, tok.range.clone()));
                }
            };
            Ok((type_annot, tok.range.end))
        },
        TokenData::ParenOpen => {
            if matches!(parser.current_tok_data(), Some(TokenData::ParenClose)){
                let paren_close_tok = parser.eat_tok(None)?;
                Ok((Type::Unit, paren_close_tok.range.end))
            } else {
                let inner_type_annotation = parse_type_annotation(parser)?.0;
                let paren_close_tok = parser.eat_tok(Some(TokenDataTag::ParenClose))?;
                Ok((inner_type_annotation, paren_close_tok.range.end))
            }
            
        },
        TokenData::Apostrophe => {
            let identifier_generic = parser.eat_tok(Some(TokenDataTag::Identifier))?;
            let type_generic = match identifier_generic.tok_data {
                TokenData::Identifier(i) => i.iter().collect::<String>(),
                _ => unreachable!(),
            };
            assert!(type_generic.len() == 1); // TODO : better error handling
            let first_c = type_generic.chars().next().unwrap();
            assert!(first_c.is_alphabetic());
            let gen_nb = (first_c as u32) - ('a' as u32);
            Ok((Type::Generic(gen_nb), identifier_generic.range.end))
        },
        _ => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: tok.tok_data }, tok.range.clone())),
    }
}

fn parse_sum_type(parser: &mut Parser) -> Result<(Type, usize), ParserErr> {
    let mut variants = Vec::new();
    let mut end_range = parser.pos;
    while parser.current_tok_data().is_some() && matches!(parser.current_tok_data().unwrap(), TokenData::Pipe){
        parser.eat_tok(Some(TokenDataTag::Pipe))?;
        let variant_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;
        end_range = variant_tok.range.end;
        let variant_name = match variant_tok.tok_data {
            TokenData::Identifier(s) => s.iter().collect::<Box<str>>(),
            _ => unreachable!(),
        };

        variants.push(Variant { 
            name: variant_name, 
            associated_type: None // TODO 
        });
    }

    let sum_type = SumType { 
        name: None,
        variants: variants.into_boxed_slice() 
    };
    Ok((Type::SumType(sum_type), end_range))
}

fn parse_type_annotation(parser: &mut Parser) -> Result<(Type, usize), ParserErr> {
    // TODO : should it be annotation simple ? (is it a good idea to declare a sum type in a annotation type for ex as a return or arg type ?)
    if let Some(current_tok) = parser.current_tok_data() && matches!(current_tok, TokenData::Pipe) {
        return parse_sum_type(parser);
    }

    let (simple_type, simple_end_range) = parse_annotation_simple(parser)?;

    let (type_parsed, type_end_range) = match parser.current_tok_data() {
        Some(TokenData::Arrow) => {
            // only simple types can be returned or passed to functions, need to refactor this code to support cases like (int -> int) -> (int -> int)
            let mut function_type_parts = vec![simple_type];
            debug_println!(parser.rustaml_context.is_debug_print, "parser.current_tok_data() = {:#?}", parser.current_tok_data());
            //dbg!(parser.current_tok_data());
            let mut end_range = simple_end_range;
            while let Some(t) = parser.current_tok_data() && matches!(t, TokenData::Arrow) {
                parser.eat_tok(Some(TokenDataTag::Arrow))?;
                let (function_type_part, function_range_end) = parse_annotation_simple(parser)?;
                end_range = function_range_end;
                function_type_parts.push(function_type_part);
                //dbg!((parser.current_tok_data(), matches!(parser.current_tok_data(), Some(TokenData::Arrow))));
            }
            let return_type = function_type_parts.pop();
            let return_type = match return_type {
                Some(t) if !function_type_parts.is_empty() => t,
                _ => panic!("ERROR : missing type in function type annotation, found return type of {:?} and args of {:?}", return_type, function_type_parts), // TODO : better error handling
            };

            let type_annot = Type::Function(function_type_parts.into_boxed_slice(), Box::new(return_type), false);

            (type_annot, end_range)
        },
        _ => (simple_type, simple_end_range),
    };
    
    Ok((type_parsed, type_end_range))
}

fn parse_type_alias(parser : &mut Parser) -> Result<ASTRef, ParserErr> {
    let name_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;
    let name_str = match name_tok.tok_data {
        TokenData::Identifier(s) => s.iter().collect::<String>(),
        _ => unreachable!(),
    };

    let name = parser.rustaml_context.str_interner.intern_compiler(&name_str);

    parser.eat_tok(Some(TokenDataTag::Equal))?;

    let (mut t, mut type_end_range) = parse_type_annotation(parser)?;

    if let Some(TokenData::EndOfExpr) = parser.current_tok_data() {
        let eof_tok = parser.eat_tok(Some(TokenDataTag::EndOfExpr)).unwrap();
        type_end_range = eof_tok.range.end;
    }

    match &mut t {
        Type::SumType(s) => {
            s.name = Some(name_str.clone().into_boxed_str());
        },
        _ => {},
    }

    parser.rustaml_context.type_aliases.insert(name, t.clone());
    
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::TypeAlias { 
        name, 
        type_alias: t, 
    }, name_tok.range.start..type_end_range))
}

fn parse_let(parser: &mut Parser, let_range_start : usize) -> Result<ASTRef, ParserErr> {
    let name_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;
    let name = match name_tok.tok_data {
        TokenData::Identifier(s) => parser.rustaml_context.str_interner.intern_compiler(&s.iter().collect::<String>()),
        _ => unreachable!(),
    };
    let mut end_range;
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

        let function_type_range_start = arg_ranges.last().unwrap().end;

        let function_type = match parser.current_tok_data() {
            Some(TokenData::Colon) => {
                parser.eat_tok(Some(TokenDataTag::Colon))?;
                Some(parse_type_annotation(parser)?.0)
            },
            Some(_) | None => {
                None
            }
        };

        let function_type_range_end = parser.current_tok().unwrap().range.start - 1;  // TODO ? (fix this by also returning a end_range in parse_type_annotation)

        let function_type_range = function_type_range_start..function_type_range_end;
    
        match function_type {
            Some(t) if !matches!(t, Type::Function(_, _, _)) => return Err(ParserErr::new(ParserErrData::NotFunctionTypeInAnnotationLet { function_name: name.get_str(&parser.rustaml_context.str_interner).to_owned() }, function_type_range)), 
            _ => {}
        }

        let arg_names = arg_names.iter().map(|e| parser.rustaml_context.str_interner.intern_compiler(e)).collect();

        parser.eat_tok(Some(TokenDataTag::Equal))?;

        let body = parse_node(parser)?;

        end_range = body.get_range(&parser.rustaml_context.ast_pool).end;
        
        ASTNode::FunctionDefinition { 
            name, 
            args: arg_names, 
            body,
            type_annotation: function_type,
        }
    } else {
        let var_type = match parser.current_tok_data() {
            Some(TokenData::Colon) => {
                parser.eat_tok(Some(TokenDataTag::Colon))?;
                Some(parse_type_annotation(parser)?.0)
            },
            Some(_) | None => None,
        };


        parser.eat_tok(Some(TokenDataTag::Equal))?;

        let val_node = parse_node(parser)?;

        
        let (body, end_range_expr) = match parser.current_tok_data() {
            Some(TokenData::In) => {
                parser.eat_tok(Some(TokenDataTag::In))?;
                let body = parse_node(parser)?;
                (Some(body), body.get_range(&parser.rustaml_context.ast_pool).end)
            },
            _ => {
                (None, val_node.get_range(&parser.rustaml_context.ast_pool).end)
            },
        };

        end_range = end_range_expr;

        ASTNode::VarDecl {
            name,
            val: val_node,
            var_type,
            body,
        }
    };

    if let Some(TokenData::EndOfExpr) = parser.current_tok_data() {
        let eof_tok = parser.eat_tok(Some(TokenDataTag::EndOfExpr)).unwrap();
        end_range = eof_tok.range.end;
    }

    Ok(parser.rustaml_context.ast_pool.push(node, let_range_start..end_range))
    
}

// for parsing operators https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn is_function_arg_start(tok_data : Option<&TokenData>) -> bool {
    match tok_data {
        Some(t) => 
            matches!(t, TokenData::Identifier(_) | TokenData::Integer(_) | TokenData::Float(_) | TokenData::String(_) | TokenData::ParenOpen | TokenData::ArrayOpen | TokenData::True | TokenData::False),
        None => false,
    }
}

fn parse_function_arg(parser : &mut Parser) -> Result<ASTRef, ParserErr> {
    let tok = parser.eat_tok(None).unwrap();
    let tok_range = tok.range;
    let node = match tok.tok_data {
        TokenData::Integer(nb) => Ok(parse_integer(parser, nb, tok_range)),
        TokenData::Float(nb) => Ok(parse_float(parser, nb, tok_range)),
        TokenData::String(buf) => Ok(parse_string(parser, &buf, tok_range)),
        TokenData::Identifier(buf) => parse_identifier_expr(parser, &buf, tok_range),
        TokenData::True => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: true }, tok_range)),
        TokenData::False => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: false }, tok_range)),
        TokenData::ParenOpen => parse_parenthesis(parser, tok_range.start), // TODO : move this to the start of parse_node and make it unreachable! ? (because each time there are parenthesis, parse_node -> parse_primary -> parse_node is added to the call stack) 
        TokenData::ArrayOpen => parse_static_list(parser, tok_range.start),
        t => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, tok_range))
    };

    return node;
}

fn parse_function_call(parser: &mut Parser, mut callee : ASTRef) -> Result<ASTRef, ParserErr> {

    loop {
        if !is_function_arg_start(parser.current_tok_data()){
            break;
        }

        let mut args = Vec::new();
        let mut last_arg = None;


        while parser.has_tokens_left() && is_function_arg_start(parser.current_tok_data()) {
            let arg= parse_function_arg(parser)?;
            last_arg = Some(arg);
            args.push(arg);
        }

        // TODO : is it needed ?
        if args.is_empty(){
            break;
        }

        let range = callee.get_range(&parser.rustaml_context.ast_pool).start..last_arg.unwrap().get_range(&parser.rustaml_context.ast_pool).end;
        callee = parser.rustaml_context.ast_pool.push(ASTNode::FunctionCall { callee, args: args.into_boxed_slice() }, range);
    }

    Ok(callee)
}

fn parse_identifier_expr(parser: &mut Parser, identifier_buf : &[char], identifier_range : Range<usize>) -> Result<ASTRef, ParserErr> {
    let s = identifier_buf.iter().collect::<String>();
    let identifier = parser.rustaml_context.str_interner.intern_compiler(&s);
    for (_name, t) in &parser.rustaml_context.type_aliases {
        match t {
            Type::SumType(sum_type) => {
                for v in &sum_type.variants {
                    // accelerate this ? TODO : make it use StringRef instead ?
                    if v.name.as_ref() == s.as_str() {
                        let variant_ast = ASTNode::Variant { 
                            name: identifier, 
                            arg: None, // TODO
                        };
                        return Ok(parser.rustaml_context.ast_pool.push(variant_ast, identifier_range))
                    }
                }
            }
            _ => {},
        }
    }


    Ok(parser.rustaml_context.ast_pool.push(ASTNode::VarUse { name: identifier }, identifier_range))
    
}

fn parse_if(parser: &mut Parser, if_range_start : usize) -> Result<ASTRef, ParserErr> {
    let cond_expr = parse_node(parser)?;

    parser.eat_tok(Some(TokenDataTag::Then))?;

    let then_body = parse_node(parser)?;

    parser.eat_tok(Some(TokenDataTag::Else))?;

    let else_body = parse_node(parser)?;
    
    let end_range = else_body.get_range(&parser.rustaml_context.ast_pool).end;

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::IfExpr { 
        cond_expr, 
        then_body, 
        else_body 
    }, if_range_start..end_range))
}


// parse the form a, b, c] (it doesn't pass the '[') , helper function to deduplicate code between the exprs and patterns
// second ret (usize) is the end range
fn parse_list_form<T, F>(parser: &mut Parser, parse_elem_fun : F ) -> Result<(Vec<T>, usize), ParserErr>
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

    let array_close_tok = parser.eat_tok(Some(TokenDataTag::ArrayClose))?;

    Ok((elems, array_close_tok.range.end))
}

fn parse_integer_pattern(parser : &mut Parser, token : &Token) -> Result<(i64, usize), ParserErr> {
     match token.tok_data {
        TokenData::Integer(nb) => Ok((nb.try_into().unwrap(), token.range.end)),
        TokenData::Op(Operator::Minus) => {
            let int_tok = parser.eat_tok(Some(TokenDataTag::Integer))?;
            match int_tok.tok_data {
                TokenData::Integer(nb) => {
                    let nb = (-nb).try_into().unwrap();
                    Ok((nb, int_tok.range.end))
                },
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn is_a_variant(parser : &mut Parser, name : &str) -> bool {
    for (_n, t) in &parser.rustaml_context.type_aliases {
        match t {
            Type::SumType(s) => {
                for v in &s.variants {
                    if v.name.as_ref() == name {
                        return true;
                    }
                }
            },
            _ => {}
        }
    }
    return false;
}

fn parse_pattern(parser : &mut Parser) -> Result<PatternRef, ParserErr> {
    let pattern_tok = parser.eat_tok(None)?;
    let pattern_first_tok_range = pattern_tok.range.clone();

    let (pattern, range) = match pattern_tok.tok_data {
        TokenData::Identifier(buf) => {
            let p = if matches!(parser.current_tok_data(), Some(TokenData::Op(Operator::ListAppend))){
                parser.eat_tok(Some(TokenDataTag::Op))?;

                let head = buf.iter().collect::<String>();
                let tail_pattern = parse_pattern(parser)?;
                Pattern::ListDestructure(parser.rustaml_context.str_interner.intern_compiler(&head), tail_pattern)
            } else {
                let s = buf.iter().collect::<String>();
                match s.as_str() {
                    "_" => Pattern::Underscore,
                    s_ref => {
                        let interned_str = parser.rustaml_context.str_interner.intern_compiler(s_ref);
                        if is_a_variant(parser, s_ref) {
                            Pattern::SumTypeVariant(interned_str)
                        } else {
                            Pattern::VarName(interned_str)
                        }
                    },
                }
            };
            (p, pattern_first_tok_range)
        },
        TokenData::Integer(_) | TokenData::Op(Operator::Minus) => { 
           let nb = parse_integer_pattern(parser, &pattern_tok)?.0;
            
            if let Some(&TokenData::Range(inclusivity)) = parser.current_tok_data() {
                parser.eat_tok(None)?;
                let end_tok = parser.eat_tok(None)?;
                let (end_nb, end_tok_range_end) = parse_integer_pattern(parser, &end_tok)?;
                /*let end_tok = parser.eat_tok(Some(TokenDataTag::Integer))?;
                let end_nb = match end_tok.tok_data {
                    TokenData::Integer(end) => end,
                    _ => unreachable!(),
                };*/
                (Pattern::Range(nb, end_nb, inclusivity), pattern_first_tok_range.start..end_tok_range_end)

            } else {
                (Pattern::Integer(nb), pattern_first_tok_range)
            } 
        },
        TokenData::Float(nb) => (Pattern::Float(nb), pattern_first_tok_range),
        TokenData::True => (Pattern::Bool(true) , pattern_first_tok_range),
        TokenData::False => (Pattern::Bool(false), pattern_first_tok_range),
        TokenData::String(s) => (Pattern::String(parser.rustaml_context.str_interner.intern_compiler(&s.iter().collect::<String>())), pattern_first_tok_range),
        TokenData::ArrayOpen => {
            let (elems, range_end) = parse_list_form(parser, parse_pattern)?;

            (Pattern::List(elems.into_boxed_slice()), pattern_first_tok_range.start..range_end)
        },
        t => return Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, pattern_first_tok_range)),
    };

    Ok(parser.rustaml_context.pattern_pool.push(pattern, range))

}

fn parse_match(parser: &mut Parser, match_range_start : usize) -> Result<ASTRef, ParserErr> {
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

    let mut last_pattern_expr = None;

    while parser.current_tok().is_some() && matches!(parser.current_tok_data().unwrap(), TokenData::Pipe) {
        parser.eat_tok(Some(TokenDataTag::Pipe))?;
        let pattern = parse_pattern(parser)?;
        //dbg!(&pattern);
        parser.eat_tok(Some(TokenDataTag::Arrow))?;
        let pattern_expr = parse_node(parser)?;
        patterns.push((pattern, pattern_expr));
        last_pattern_expr = Some(pattern_expr);
    }

    let end_range = match last_pattern_expr {
        Some(e) => e.get_range(&parser.rustaml_context.ast_pool).end,
        None => unreachable!(),
    };


    Ok(parser.rustaml_context.ast_pool.push(ASTNode::MatchExpr { 
        matched_expr, 
        patterns: patterns.into_boxed_slice(), 
    }, match_range_start..end_range))
}


fn parse_parenthesis(parser: &mut Parser, open_paren_range_start : usize) -> Result<ASTRef, ParserErr> {
    if let Some(t) = parser.current_tok_data() && matches!(t, TokenData::ParenClose){
        debug_println!(parser.rustaml_context.is_debug_print, "FOUND UNIT");
        let paren_close_tok = parser.eat_tok(Some(TokenDataTag::ParenClose))?;
        return Ok(parser.rustaml_context.ast_pool.push(ASTNode::Unit, open_paren_range_start..paren_close_tok.range.end));
    }
    debug_println!(parser.rustaml_context.is_debug_print, "START OF PARENTHESE");
    let expr: ASTRef = parse_node(parser)?;
    debug_println!(parser.rustaml_context.is_debug_print, "expr = {:#?}", DebugWrapContext::new(&expr, parser.rustaml_context));
    //dbg_intern!(&expr, &parser.rustaml_context);
    parser.eat_tok(Some(TokenDataTag::ParenClose))?;
    debug_println!(parser.rustaml_context.is_debug_print, "EAT END OF PARENTHESE");
    Ok(expr)
}

fn parse_static_list(parser: &mut Parser, array_open_start : usize) -> Result<ASTRef, ParserErr> {
    let range_start = array_open_start;
    let (elems, range_end) = parse_list_form(parser, parse_node)?;
    
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::List { list: elems.into_boxed_slice() }, range_start..range_end))
}

fn parse_anonymous_function(parser: &mut Parser, function_range_start : usize) -> Result<ASTRef, ParserErr> {
    let mut arg_names = Vec::new();
    let mut arg_ranges = Vec::new();
    while matches!(parser.current_tok_data(), Some(TokenData::Identifier(_))) {
        let arg_identifier = parser.eat_tok(Some(TokenDataTag::Identifier)).unwrap();
        let arg_name = match arg_identifier.tok_data {
            TokenData::Identifier(s) => s.iter().collect::<String>(),
            _ => unreachable!(),
        };

        arg_ranges.push(arg_identifier.range);

        arg_names.push(parser.rustaml_context.str_interner.intern_compiler(&arg_name));
    }

    let function_type = match parser.current_tok_data() {
        Some(TokenData::Colon) => {
            parser.eat_tok(Some(TokenDataTag::Colon))?;
            parser.eat_tok(Some(TokenDataTag::ParenOpen))?;
            let fun_type = Some(parse_type_annotation(parser)?.0);
            parser.eat_tok(Some(TokenDataTag::ParenClose))?;
            fun_type
        },
        Some(_) | None => {
            None
        }
    };



    parser.eat_tok(Some(TokenDataTag::Arrow))?;

    let body = parse_node(parser)?;

    let end_range = body.get_range(&parser.rustaml_context.ast_pool).end;
    
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::AnonFunc { 
        args: arg_names.into_boxed_slice(), 
        body, 
        type_annotation: function_type, 
    }, function_range_start..end_range))
}

const UNARY_OPS : [Operator; 2] = [Operator::Minus, Operator::Not];

fn parse_unary_op(parser: &mut Parser, op : Operator, unary_range_start : usize) -> Result<ASTRef, ParserErr> {
    if !UNARY_OPS.contains(&op){
        panic!("Unknown unary op : {:?}", op); // TODO : better error handling
    }

    let expr = parse_primary(parser)?;

    let range_end = expr.get_range(&parser.rustaml_context.ast_pool).end;

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::UnaryOp { op, expr }, unary_range_start..range_end))

}

fn parse_extern_func(parser: &mut Parser, extern_range_start : usize) -> Result<ASTRef, ParserErr> {
    let str_lang_tok = parser.eat_tok(Some(TokenDataTag::String))?;
    let s = match str_lang_tok.tok_data {
        TokenData::String(s) => s.into_iter().collect::<String>(),
        _ => unreachable!(),
    };

    let extern_lang = match s.as_str() {
        "C" => ExternLang::C,
        "C++" | "Cpp" => ExternLang::Cpp,
        _ => panic!("Unknown lang in extern function {}", s), // TODO : better error handling
    };

    parser.eat_tok(Some(TokenDataTag::Function))?;

    let func_name_ident_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;

    let func_name = match func_name_ident_tok.tok_data {
        TokenData::Identifier(i) => i.into_iter().collect::<String>(),
        _ => unreachable!(),
    };

    parser.eat_tok(Some(TokenDataTag::Colon))?;
    let (type_annotation, mut end_range) = parse_type_annotation(parser)?;

    let mut so_str = None;


    if let Some(TokenData::String(_)) = parser.current_tok_data(){
        let so_str_tok = parser.eat_tok(None)?;
        so_str = match so_str_tok.tok_data {
            TokenData::String(s) => {
                let s = parser.rustaml_context.str_interner.intern_compiler(&s.iter().collect::<String>());
                Some(s)
            },
            _ => unreachable!(),
        };
        end_range = so_str_tok.range.end;
    } 

    if let Some(TokenData::EndOfExpr) = parser.current_tok_data() {
        let eof_tok = parser.eat_tok(Some(TokenDataTag::EndOfExpr)).unwrap();
        end_range = eof_tok.range.end;
    }

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::ExternFunc { 
        name: parser.rustaml_context.str_interner.intern_compiler(&func_name), 
        type_annotation, 
        lang: extern_lang,
        so_str,
    }, extern_range_start..end_range))
}

fn parse_cast(parser : &mut Parser, cast_range_start : usize) -> Result<ASTRef, ParserErr> {
    let (cast_type, _) = parse_type_annotation(parser)?;
    let expr = parse_node(parser)?;
    let end_range = expr.get_range(&parser.rustaml_context.ast_pool).end;
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::Cast { 
        to_type: cast_type, 
        expr, 
    }, cast_range_start..end_range))
}

fn parse_primary(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let tok = parser.eat_tok(None).unwrap();
    let tok_range = tok.range.clone();
    let node = match tok.tok_data {
        TokenData::Let => parse_let(parser, tok_range.start),
        TokenData::If => parse_if(parser, tok_range.start),
        TokenData::Match => parse_match(parser, tok_range.start),
        TokenData::Integer(nb) => Ok(parse_integer(parser, nb, tok_range)),
        TokenData::Float(nb) => Ok(parse_float(parser, nb, tok_range)),
        TokenData::String(buf) => Ok(parse_string(parser, &buf, tok_range)),
        TokenData::Identifier(buf) => parse_identifier_expr(parser, &buf, tok_range),
        TokenData::Function => parse_anonymous_function(parser, tok_range.start),
        TokenData::True => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: true }, tok_range)),
        TokenData::False => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: false }, tok_range)),
        TokenData::ParenOpen => parse_parenthesis(parser, tok_range.start), // TODO : move this to the start of parse_node and make it unreachable! ? (because each time there are parenthesis, parse_node -> parse_primary -> parse_node is added to the call stack) 
        TokenData::ArrayOpen => parse_static_list(parser, tok_range.start),
        TokenData::Op(op) => parse_unary_op(parser, op, tok_range.start),
        TokenData::Extern => parse_extern_func(parser, tok_range.start),
        TokenData::Type => parse_type_alias(parser),
        TokenData::Cast => parse_cast(parser, tok_range.start),
        //t => panic!("t: {:?}", t),
        t => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, tok.range))
    };

    return node;
}

fn parse_binary_rec(parser: &mut Parser, lhs: ASTRef, min_precedence: i32) -> Result<ASTRef, ParserErr> {
    let mut lhs = lhs;


    while parser.has_tokens_left() {
        let current_tok_data = parser.current_tok_data();
        let operator = match current_tok_data {
            Some(&TokenData::Op(op)) => op,
            Some(_) | None => break,
        };
        let (first_precedence, _) = *parser.precedences.get(&operator).unwrap();
        if first_precedence < min_precedence {
            break;
        }
        parser.eat_tok(Some(TokenDataTag::Op)).unwrap();
        let mut rhs = parse_application(parser)?; // will parse primary and application

        while parser.has_tokens_left() {
            let current_tok_data = parser.current_tok_data();
            let new_operator =  match current_tok_data {
                Some(TokenData::Op(op)) => op,
                Some(_) | None => break,
            };
            let (precedence, associativity) = *parser.precedences.get(new_operator).unwrap();

            if precedence < first_precedence || (precedence == first_precedence && matches!(associativity, Associativity::Left)){
                break;
            }

            let new_precedence = match associativity {
                Associativity::Left => precedence + 1,
                Associativity::Right => precedence,
            };

            rhs = parse_binary_rec(parser, rhs, new_precedence)?;
        }

        let range = lhs.get_range(&parser.rustaml_context.ast_pool).start..rhs.get_range(&parser.rustaml_context.ast_pool).end;
        lhs = parser.rustaml_context.ast_pool.push(ASTNode::BinaryOp {
            op: operator,
            lhs,
            rhs,
        }, range);
    }

    Ok(lhs)
}

fn parse_binary(parser: &mut Parser, lhs: ASTRef) -> Result<ASTRef, ParserErr> {
    parse_binary_rec(parser, lhs, 0)
}

// TODO : add a way to namespace (let a = import "..." ? import "..." as a ?) and then namespace the functions and vars ?
// TODO : make it possible to parse (and codegen ?) with multiple threads (probably need to replace the top level returned from get_ast_string to a Imported ast node)
// if file already imported, return None
fn parse_import(parser : &mut Parser, import_tok_range : Range<usize>) -> Result<Option<ASTRef>, ParserErr> {
    let import_filename_tok = parser.eat_tok(Some(TokenDataTag::String))?;

    let import_range = import_tok_range.start..import_filename_tok.range.end;

    let import_filename = match import_filename_tok.tok_data {
        TokenData::String(s) => s.iter().collect::<String>(),
        _ => unreachable!(),
    };


    let import_filename_path = Path::new(&import_filename);

    // for example : test/a.rml becomes test/
    let filename_path = parser.filename.parent().unwrap_or(Path::new(""));
    let import_path = match pathbuf![filename_path, import_filename_path].canonicalize(){
        Ok(p) => p,
        Err(e) => panic!("Error when opening file for imports : {}", e), // TODO : either : replace these with an other type of error to not interfere with human-panic or remove human-panic
    };

    //dbg!(&parser.imported_files);
    //println!("TRY TO IMPORT {}", import_path.display());

    if parser.imported_files.contains(&import_path){
        //println!("ALREADY IMPORTED {}", import_path.display());
        return Ok(None);
    }

    parser.imported_files.insert(import_path.clone());

    let content_str = read_file(&import_path);

    let content_chars = content_str.chars().collect();
    
    let ast = match get_ast_from_string(parser.rustaml_context, content_chars, Some(&content_str), &import_path, Some(parser.imported_files)) {
        Ok(a) => a,
        Err(()) => return Err(ParserErr::new(ParserErrData::ImportError, import_range)),
    };

   Ok(Some(ast))
}

fn parse_application(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let lhs = parse_primary(parser)?;
    let applied = parse_function_call(parser, lhs)?;
    Ok(applied)
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

    let applied = parse_application(parser)?;
    let ret_expr = parse_binary(parser, applied)?;
    Ok(ret_expr)
}

fn parse_top_level_node(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let mut nodes: Vec<ASTRef> = Vec::new();

    // TODO : make it be in the first loop to have imports anywhere
    // TODO : make this multithreaded ?
    while parser.has_tokens_left() && matches!(parser.current_tok_data(), Some(TokenData::Import)) {
        let import_tok = parser.eat_tok(None)?;
        let import = parse_import(parser, import_tok.range)?;
        if let Some(import) = import {
            nodes.push(import);
        }
        
    }

    while parser.has_tokens_left() {
        nodes.push(parse_node(parser)?);
    }
    let range = match (nodes.first(), nodes.last()) {
        (Some(first), Some(last)) => first.get_range(&parser.rustaml_context.ast_pool).start..last.get_range(&parser.rustaml_context.ast_pool).end,
        _ => 0..0, // 0..0 because there is no expressions
    };

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::TopLevel { nodes: nodes.into_boxed_slice() }, range))
}

pub fn parse(tokens: Vec<Token>, rustaml_context : &mut RustamlContext, filename : PathBuf, already_added_filenames : Option<&mut FxHashSet<PathBuf>>) -> Result<ASTRef, ParserErr> {

    let filename_complete_path = filename.canonicalize().unwrap_or_default().clone();
    let imported_files = if let Some(already_added_filenames) = already_added_filenames {
        already_added_filenames.insert(filename_complete_path);
        already_added_filenames
    } else {
        &mut FxHashSet::from_iter([filename_complete_path])
    };

    let root_node = { 
        let mut parser = Parser { 
            tokens: tokens.into_boxed_slice(), 
            pos: 0,
            precedences: init_precedences(),
            imported_files,
            filename,
            rustaml_context,
        };
        let root_node = parse_top_level_node(&mut parser)?;
        root_node
    };

    debug_println!(rustaml_context.is_debug_print, "root_node = {:#?}", DebugWrapContext::new(&root_node, rustaml_context));
    //debug_println!(rustaml_context.is_debug_print, "nodes ranges: {:?}", rustaml_context.ast_pool.ast_node_ranges);
    
    Ok(root_node)
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