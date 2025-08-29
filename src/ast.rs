use std::path::PathBuf;
use std::{ops::Range, path::Path};
use std::fmt::{Debug, Display, Write};

use rustc_hash::{FxHashMap, FxHashSet};

use enum_tags::{Tag, TaggedEnum};
use pathbuf::pathbuf;

use crate::rustaml::read_file;
use crate::{debug_println, lexer::{Operator, Token, TokenData, TokenDataTag}, rustaml::{get_ast_from_string, RustamlContext}, string_intern::StringRef, types_debug::PrintTypedContext};
use debug_with_context::{DebugWithContext, DebugWrapContext};

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
#[debug_context(PrintTypedContext)]
pub struct Arg {
    pub name : StringRef,
    pub arg_type : Type,
}

// TODO : add a guard clauses (create struct with an enum and guard clauses)

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
#[debug_context(PrintTypedContext)]
pub enum Pattern {
    VarName(StringRef), // | x pattern
    Integer(i64), // | 2
    Float(f64), // | 2.6
    Bool(bool), // | true
    Range(i64, i64, bool), // bool is for the inclusivity | 0..1
    String(StringRef), // | "test"
    List(Vec<PatternRef>), // | [1, 2, 3] // TODO : replace vec with Box<[Pattern]>
    ListDestructure(StringRef, PatternRef), // head name then tail name TODO : refactor to be recursive so you can have e::e2::l
    Underscore,
}

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


#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
#[debug_context(PrintTypedContext)]
pub enum ASTNode {
    TopLevel {
        nodes: Vec<ASTRef>,
    },
    // TODO : replace with just a type with the function type in an optional and only StringRefs in args ?
    FunctionDefinition {
        name : StringRef,
        args : Vec<Arg>,
        body : ASTRef,
        return_type : Type,
    },
    AnonFunc {
        args : Vec<StringRef>,
        body : ASTRef,
        type_annotation : Option<Type>,
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
    IfExpr {
        cond_expr : ASTRef,
        then_body : ASTRef,
        else_body : ASTRef,
    },
    MatchExpr {
        matched_expr : ASTRef,
        patterns : Vec<(PatternRef, ASTRef)>,
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
        callee : ASTRef,
        args : Vec<ASTRef>,
    },
    Unit,
}

// TODO : add a type pool to remove boxes (test performance ? normally should be useful for lowering the type size, it would become only 64 bit and we could make it Copy, but we wouldn't use it everywhere there is Type like for other types, just in refence in the type to other types to lower the size while only indexing in the vector when it is really needed)
// THE PROBLEM : would need to make the type system only have functions with only one args, but could do it by returning type of function types, which could even help for currying
#[derive(Debug, Clone, PartialEq, Eq, Hash, Tag)]
pub enum Type {
    Integer,
    Float,
    Bool,
    Function(Vec<Type>, Box<Type>, bool), // the bool is if the function is variadic
    Str,
    List(Box<Type>),
    // TODO : add a number to any (to have 'a, 'b, etc)
    // TODO: or remove Any ?
    Any, // equivalent to 'a
    Unit,
    Never,
}

// TODO : replace with macro (add a flag to say that the type just implements debug)
impl <C> DebugWithContext<C> for Type {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, _context: &C) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
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
            Type::Any => f.write_str("Any"), // TODO
            Type::Function(_, _, _) => unreachable!(),
        }
        
    }
}

// TODO : add @ operator to append lists


fn init_precedences() -> FxHashMap<Operator, (i32, Associativity)> {

    // see https://ocaml.org/manual/5.3/expr.html#ss%3Aprecedence-and-associativity for precedence ?

    FxHashMap::from_iter([
        // TODO : should the and and or be Right or Left Associativity ?
        (Operator::And, (5, Associativity::Right)),
        (Operator::Or, (5, Associativity::Right)),
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
        (Operator::MultFloat, (30, Associativity::Left)),
        (Operator::DivFloat, (30, Associativity::Left)),
        (Operator::StrAppend, (5, Associativity::Right)),
        (Operator::ListAppend, (6, Associativity::Right)),
    ])
}


#[derive(Clone, Copy)]
pub enum Associativity {
    Left, // most operators
    Right, // ::
}

pub struct Parser<'context> {
    tokens: Vec<Token>,
    pos: usize,
    // TODO : replace vars with global vars, and add in each function, a local var table
    // pub vars : FxHashMap<StringRef, Type>, // include functions (which are just vars with function types)
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

fn parse_integer(parser: &mut Parser, nb: i64, range : Range<usize>) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::Integer { nb }, range)
}

fn parse_float(parser: &mut Parser, nb: f64, range : Range<usize>) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::Float { nb }, range)
}

fn parse_string(parser: &mut Parser, buf : Vec<char>, range : Range<usize>) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::String { str: parser.rustaml_context.str_interner.intern_compiler(&buf.iter().collect::<String>()) }, range)
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
                s => return Err(ParserErr::new(ParserErrData::UnknownTypeAnnotation { type_str: s.to_owned() }, tok.range.clone())),
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

fn parse_type_annotation(parser: &mut Parser) -> Result<Type, ParserErr> {
    parser.eat_tok(Some(TokenDataTag::Colon))?;
    
    let simple_type = parse_annotation_simple(parser)?;

    let type_parsed = match parser.current_tok_data() {
        Some(TokenData::Arrow) => {
            // only simple types can be returned or passed to functions, need to refactor this code to support cases like (int -> int) -> (int -> int)
            let mut function_type_parts = vec![simple_type];
            debug_println!(parser.rustaml_context.is_debug_print, "parser.current_tok_data() = {:#?}", parser.current_tok_data());
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

            Type::Function(function_type_parts, Box::new(return_type), false)
        },
        _ => simple_type,
    };
    
    Ok(type_parsed)
}

fn parse_let(parser: &mut Parser, let_range : Range<usize>) -> Result<ASTRef, ParserErr> {
    let start_range = let_range.start;
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


        //let function_type_range_start = parser.current_tok().unwrap().range.start;
        let function_type_range_start = arg_ranges.last().unwrap().end;

        let function_type = match parser.current_tok_data() {
            Some(TokenData::Colon) => parse_type_annotation(parser)?,
            Some(_) | None => {
                Type::Function(vec![Type::Any; arg_names.len()], Box::new(Type::Any), false)
            }
        };

        let function_type_range_end = parser.current_tok().unwrap().range.start - 1;  // TODO ? (verify if good)

        let function_type_range = function_type_range_start..function_type_range_end;
        
        let (arg_types, return_type, _) = match function_type {
            Type::Function(a, r, v) => (a, r, v),
            _ => return Err(ParserErr::new(ParserErrData::NotFunctionTypeInAnnotationLet { function_name: name.get_str(&parser.rustaml_context.str_interner).to_owned() }, function_type_range)), 
        };

        let args = arg_names.into_iter().zip(arg_types.clone()).map(|x| Arg { name: parser.rustaml_context.str_interner.intern_compiler(&x.0), arg_type: x.1 }).collect::<Vec<Arg>>();


        parser.eat_tok(Some(TokenDataTag::Equal))?;

        let body = parse_node(parser)?;

        end_range = body.get_range(&parser.rustaml_context.ast_pool).end;
        
        ASTNode::FunctionDefinition { 
            name, 
            args, 
            body,
            return_type: *return_type,
        }
    } else {
        let var_type = match parser.current_tok_data() {
            Some(TokenData::Colon) => Some(parse_type_annotation(parser)?),
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

    Ok(parser.rustaml_context.ast_pool.push(node, start_range..end_range))
    
}

// for parsing operators https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn is_function_arg_start(tok_data : Option<&TokenData>) -> bool {
    match tok_data {
        Some(t) => 
            matches!(t, TokenData::Identifier(_) | TokenData::Integer(_) | TokenData::Float(_) | TokenData::String(_) | TokenData::ParenOpen | TokenData::ArrayOpen | TokenData::True | TokenData::False),
        None => false,
    }
}

fn parse_var_use_in_function(parser : &mut Parser , identifier_buf : Vec<char>, tok_range : Range<usize>) -> Result<ASTRef, ParserErr> {
    let identifier = parser.rustaml_context.str_interner.intern_compiler(&identifier_buf.iter().collect::<String>());
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::VarUse { name: identifier }, tok_range))

}

fn parse_function_arg(parser : &mut Parser) -> Result<ASTRef, ParserErr> {
    let tok = parser.eat_tok(None).unwrap();
    let tok_range = tok.range.clone();
    let node = match tok.tok_data {
        TokenData::Integer(nb) => Ok(parse_integer(parser, nb, tok_range)),
        TokenData::Float(nb) => Ok(parse_float(parser, nb, tok_range)),
        TokenData::String(buf) => Ok(parse_string(parser, buf, tok_range)),
        TokenData::Identifier(buf) => parse_var_use_in_function(parser, buf, tok_range),
        TokenData::True => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: true }, tok_range)),
        TokenData::False => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: false }, tok_range)),
        TokenData::ParenOpen => parse_parenthesis(parser, tok_range), // TODO : move this to the start of parse_node and make it unreachable! ? (because each time there are parenthesis, parse_node -> parse_primary -> parse_node is added to the call stack) 
        TokenData::ArrayOpen => parse_static_list(parser, tok_range.start),
        t => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, tok_range))
    };

    return node;
}

// parse what could be a function call
// TODO : make this work for any expression for the function
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
        callee = parser.rustaml_context.ast_pool.push(ASTNode::FunctionCall { callee, args }, range);
    }

    Ok(callee)
    /*let start_range = first_tok_start;
    let mut args = Vec::new();

    let mut last_arg = None;

    while parser.has_tokens_left() && is_function_arg_start(parser.current_tok_data()) {
        //let arg = parse_primary(parser)?;
        let arg= parse_function_arg(parser)?;
        last_arg = Some(arg);

        args.push(arg);
    }

    let end_range = last_arg.unwrap().get_range(&parser.rustaml_context.ast_pool).end;

    let ast_node = if args.is_empty(){
        ASTNode::VarUse { 
            name
        }
    } else {
        ASTNode::FunctionCall { 
            callee, 
            args,
        }
    };
    
    //dbg!(&args);
    Ok(parser.rustaml_context.ast_pool.push(ast_node, start_range..end_range))*/
}

fn parse_identifier_expr(parser: &mut Parser, identifier_buf : Vec<char>, identifier_range : Range<usize>) -> Result<ASTRef, ParserErr> {
    let identifier = parser.rustaml_context.str_interner.intern_compiler(&identifier_buf.iter().collect::<String>());

    /*let is_function = is_function_arg_start(parser.current_tok_data());
    if is_function {
        parse_function_call(parser, identifier, identifier_range.start)
    } else {
        // var use
        Ok(parser.rustaml_context.ast_pool.push(ASTNode::VarUse { name: identifier }, identifier_range))
    }*/

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::VarUse { name: identifier }, identifier_range))
    
}

fn parse_if(parser: &mut Parser, if_range : Range<usize>) -> Result<ASTRef, ParserErr> {
    let start_range = if_range.start;
    let cond_expr = parse_node(parser)?;

    parser.eat_tok(Some(TokenDataTag::Then))?;

    let then_body = parse_node(parser)?;

    parser.eat_tok(Some(TokenDataTag::Else))?;

    let else_body = parse_node(parser)?;
    
    let end_range = parser.current_tok().unwrap().range.start;

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::IfExpr { 
        cond_expr, 
        then_body, 
        else_body 
    }, start_range..end_range))
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

fn parse_pattern(parser : &mut Parser) -> Result<PatternRef, ParserErr> {
    let pattern_tok = parser.eat_tok(None)?;
    let pattern_first_tok_range = pattern_tok.range;

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
                    s_ref => Pattern::VarName(parser.rustaml_context.str_interner.intern_compiler(s_ref)),
                }
            };
            (p, pattern_first_tok_range)
        },
        TokenData::Integer(nb) => { 
            if let Some(&TokenData::Range(inclusivity)) = parser.current_tok_data() {
                parser.eat_tok(None)?;
                let end_tok = parser.eat_tok(Some(TokenDataTag::Integer))?;
                let end_nb = match end_tok.tok_data {
                    TokenData::Integer(end) => end,
                    _ => unreachable!(),
                };
                (Pattern::Range(nb, end_nb, inclusivity), pattern_first_tok_range.start..end_tok.range.end)

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

            (Pattern::List(elems), pattern_first_tok_range.start..range_end)
        },
        t => return Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, pattern_first_tok_range)),
    };

    Ok(parser.rustaml_context.pattern_pool.push(pattern, range))

}

fn parse_match(parser: &mut Parser, match_range : Range<usize>) -> Result<ASTRef, ParserErr> {
    let start_range = match_range.start;
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
        patterns, 
    }, start_range..end_range))
}


fn parse_parenthesis(parser: &mut Parser, open_paren_range : Range<usize>) -> Result<ASTRef, ParserErr> {
    if let Some(t) = parser.current_tok_data() && matches!(t, TokenData::ParenClose){
        debug_println!(parser.rustaml_context.is_debug_print, "FOUND UNIT");
        let paren_close_tok = parser.eat_tok(Some(TokenDataTag::ParenClose))?;
        return Ok(parser.rustaml_context.ast_pool.push(ASTNode::Unit, open_paren_range.start..paren_close_tok.range.end));
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
    
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::List { list: elems }, range_start..range_end))
}

fn parse_anonymous_function(parser: &mut Parser, function_range : Range<usize>) -> Result<ASTRef, ParserErr> {
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
        Some(TokenData::Colon) => Some(parse_type_annotation(parser)?),
        Some(_) | None => {
            None
        }
    };



    parser.eat_tok(Some(TokenDataTag::Arrow))?;

    let body = parse_node(parser)?;

    let end_range = body.get_range(&parser.rustaml_context.ast_pool).end;
    
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::AnonFunc { 
        args: arg_names, 
        body, 
        type_annotation: function_type, 
    }, function_range.start..end_range))
}

fn parse_primary(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let tok = parser.eat_tok(None).unwrap();
    let tok_range = tok.range.clone();
    // TODO : pass tok_range.start instead of tok_range in these functions
    let node = match tok.tok_data {
        TokenData::Let => parse_let(parser, tok_range),
        TokenData::If => parse_if(parser, tok_range),
        TokenData::Match => parse_match(parser, tok_range),
        TokenData::Integer(nb) => Ok(parse_integer(parser, nb, tok_range)),
        TokenData::Float(nb) => Ok(parse_float(parser, nb, tok_range)),
        TokenData::String(buf) => Ok(parse_string(parser, buf, tok_range)),
        TokenData::Identifier(buf) => parse_identifier_expr(parser, buf, tok_range),
        TokenData::Function => parse_anonymous_function(parser, tok_range),
        TokenData::True => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: true }, tok_range)),
        TokenData::False => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: false }, tok_range)),
        TokenData::ParenOpen => parse_parenthesis(parser, tok_range), // TODO : move this to the start of parse_node and make it unreachable! ? (because each time there are parenthesis, parse_node -> parse_primary -> parse_node is added to the call stack) 
        TokenData::ArrayOpen => parse_static_list(parser, tok_range.start),
        //t => panic!("t : {:?}", t),
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
fn parse_import(parser : &mut Parser) -> Result<Option<ASTRef>, ParserErr> {
    let import_filename_tok = parser.eat_tok(Some(TokenDataTag::String))?;
    let import_filename = match import_filename_tok.tok_data {
        TokenData::String(s) => s.iter().collect::<String>(),
        _ => unreachable!(),
    };


    let import_filename_path = Path::new(&import_filename);

    // for example : test/a.rml becomes test/
    let filename_path = parser.filename.parent().unwrap_or(Path::new(""));
    let import_path = pathbuf![filename_path, import_filename_path].canonicalize().unwrap(); // TODO : replace the unwrap with a panic!(filename not found)

    //dbg!(&parser.imported_files);
    //println!("TRY TO IMPORT {}", import_path.display());

    if parser.imported_files.contains(&import_path){
        //println!("ALREADY IMPORTED {}", import_path.display());
        return Ok(None);
    }

    parser.imported_files.insert(import_path.clone());

    // TODO : open file
    let content_str = read_file(&import_path);

    let content_chars = content_str.chars().collect();
    
    let ast = match get_ast_from_string(parser.rustaml_context, content_chars, Some(&content_str), &import_path, Some(parser.imported_files)) {
        Ok(a) => a,
        Err(()) => return Err(ParserErr::new(ParserErrData::ImportError, 0..0)), // TODO
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
        parser.eat_tok(None)?;
        let import = parse_import(parser)?;
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

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::TopLevel { nodes }, range))
}

pub fn parse(tokens: Vec<Token>, rustaml_context : &mut RustamlContext, filename : PathBuf, already_added_filenames : Option<&mut FxHashSet<PathBuf>>) -> Result<ASTRef, ParserErr> /*Result<(ASTRef, FxHashMap<StringRef, Type>), ParserErr>*/ {
    //let mut imported_files = FxHashSet::from_iter([filename.clone()]);
    let filename_complete_path = filename.canonicalize().unwrap().clone();
    let imported_files = if let Some(already_added_filenames) = already_added_filenames {
        //imported_files.extend(already_added_filenames);
        already_added_filenames.insert(filename_complete_path);
        already_added_filenames
    } else {
        &mut FxHashSet::from_iter([filename_complete_path])
    };

    let root_node = { 
        let mut parser = Parser { 
            tokens, 
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