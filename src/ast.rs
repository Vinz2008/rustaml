use std::ops::Range;
use std::fmt::Debug;

use rustc_hash::FxHashMap;

use enum_tags::{Tag, TaggedEnum};

use crate::{debug_println, lexer::{Operator, Token, TokenData, TokenDataTag}, rustaml::RustamlContext, string_intern::StringRef};
use debug_with_context::{DebugWithContext, DebugWrapContext};

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub struct Arg {
    pub name : StringRef,
    pub arg_type : Type,
}

// TODO : add imports

// TODO : create a pattern pool ?

// TODO : add a guard clauses (create struct with an enum and guard clauses)

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub enum Pattern {
    VarName(StringRef), // | x pattern
    Integer(i64), // | 2
    Float(f64), // | 2.6
    Range(i64, i64, bool), // bool is for the inclusivity | 0..1
    String(StringRef), // | "test"
    List(Vec<Pattern>), // | [1, 2, 3] // TODO : replace vec with Box<[Pattern]>
    ListDestructure(StringRef, Box<Pattern>), // head name then tail name TODO : refactor to be recursive so you can have e::e2::l
    Underscore,
}

pub struct ASTPool(Vec<ASTNode>, pub Vec<Type>);

impl ASTPool {
    pub fn new() -> ASTPool {
        ASTPool(Vec::new(), Vec::new())
    }
    pub fn get(&self, expr : ASTRef) -> &ASTNode {
        &self.0[expr.0 as usize]
    }

    pub fn get_mut(&mut self, expr : ASTRef) -> &mut ASTNode {
        &mut self.0[expr.0 as usize]
    }

    pub fn get_type(&self, expr : ASTRef) -> &Type {
        &self.1[expr.0 as usize]
    }

    pub fn set_type(&mut self, expr : ASTRef, t: Type) {
        self.1[expr.0 as usize] = t;
    }

    pub fn push(&mut self, node : ASTNode) -> ASTRef {
        self.push_with_type(node, Type::Any)
    }

    #[inline]
    pub fn push_with_type(&mut self, node: ASTNode, t : Type) -> ASTRef {
        let idx = self.0.len();
        self.0.push(node);
        self.1.push(t);
        ASTRef(idx.try_into().expect("too many ast nodes in the pool"))
    }
}


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ASTRef(u32);

impl ASTRef {
    pub fn get(self, ast_pool : &ASTPool) -> &ASTNode {
        ast_pool.get(self)
    }

    pub fn get_mut(self, ast_pool : &mut ASTPool) -> &mut ASTNode {
        ast_pool.get_mut(self)
    }

    pub fn get_type(self, ast_pool : &ASTPool) -> &Type {
        ast_pool.get_type(self)
    }

    pub fn set_type(self, ast_pool : &mut ASTPool, t: Type){
        ast_pool.set_type(self, t);
    }
}

impl DebugWithContext<RustamlContext> for ASTRef {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, rustaml_context : &RustamlContext) -> std::fmt::Result {
        self.get(&rustaml_context.ast_pool).fmt_with_context(f, rustaml_context)
    }
}


// TODO : add a range for astNodes to simplify error messages later
#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub enum ASTNode {
    TopLevel {
        nodes: Vec<ASTRef>,
    },
    // TODO : replace with just a type with the function type ?
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
    },
    Unit,
}


// TODO : add a type pool to remove boxes (test performance ? normally should be useful for lowering the type size, it would become only 64 bit and we could make it Copy, but we wouldn't use it everywhere there is Type like for other types, just in refence in the type to other types to lower the size while only indexing in the vector when it is really needed)
// THE PROBLEM : would need to make the type system only have functions with only one args, but could do it by returning type of function types, which could even help for currying
#[derive(Debug, Clone, PartialEq, Eq, Tag)]
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
impl DebugWithContext<RustamlContext> for Type {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, _rustaml_context: &RustamlContext) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ASTNode {
    // TODO : return a GetTypeErr instead of a ParserErr that can be coerced to other error types
    pub fn get_type(&self, rustaml_context : &RustamlContext, vars: &FxHashMap<StringRef, Type>) -> Result<Type, ParserErr> {
        let t = match self {
            ASTNode::Boolean { b: _ } => Type::Bool,
            ASTNode::Integer { nb: _ } => Type::Integer,
            ASTNode::Float { nb : _ } => Type::Float,
            ASTNode::String { str: _ } => Type::Str,
            ASTNode::List { list } => { 
                let elem_type = match list.first() {
                    Some(f) => f.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)?,
                    None => Type::Any,
                };
                Type::List(Box::new(elem_type)) 
            },
            ASTNode::BinaryOp { op, lhs: _, rhs: _ } => op.get_type(None),
            ASTNode::VarDecl { name: _, val: _, body, var_type: _ } => {
                if let Some(b) = body {
                    b.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)?
                } else {
                    Type::Unit
                }
                
            },
            ASTNode::FunctionCall { name, args: _ } => { 
                let func_type = vars.get(name).unwrap();
                match func_type {
                    Type::Function(_, ret, _) => ret.as_ref().clone(),
                    _ => panic!("Trying to call something that is not a function"),
                } 
            },
            ASTNode::VarUse { name} => match vars.get(name){
                Some(t) => t.clone(),
                None => return Err(ParserErr::new(ParserErrData::UnknownVar { name: name.get_str(&rustaml_context.str_interner).to_owned() }, 0..0)), // TODO
             },
            ASTNode::IfExpr { cond_expr: _, then_body, else_body } => {
                let then_type = then_body.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)?;
                let else_type = else_body.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)?;
                match (then_type, else_type){
                    (Type::Never, t) | (t, Type::Never) => t,
                    (t, t2) if t.tag() == t2.tag() => t,
                    (t, t2) => panic!("Typechecking error in if : if -> {:?}, else -> {:?}", t, t2), // TODO : return a type checking error instead that can coerce to a parserErr
                }
            }, 
            ASTNode::MatchExpr { matched_expr: _, patterns } => patterns.first().unwrap().1.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)?,
            ASTNode::TopLevel { nodes: _ } => Type::Unit,
            ASTNode::FunctionDefinition { name: _, args: _, body: _, return_type: _ } => Type::Unit,
            ASTNode::Unit => Type::Unit,
        };

        Ok(t)
    }
}


fn init_precedences() -> FxHashMap<Operator, (i32, Associativity)> {

    // see https://ocaml.org/manual/5.3/expr.html#ss%3Aprecedence-and-associativity for precedence ?

    FxHashMap::from_iter([
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
    UnknownVar {
        name : String,
    },
    UnknownTypeAnnotation {
        type_str : String
    },
    TypeInferenceErr {
        arg_name: String,
    },
    NotFunctionTypeInAnnotationLet {
        function_name: String
    },
    WrongNumberOfArgs {
        function_name : String,
        expected_nb : usize,
        got_nb : usize,
    },
    WrongArgType {
        function_name: String,
        // TODO : add an arg name ?
        expected_type: Type,
        got_type: Type,
    },
    MismatchedBinOpType {
        op : String,
        expected_type: Type,
        got_type: Type,
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

fn parse_integer(parser: &mut Parser, nb: i64) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::Integer { nb })
}

fn parse_float(parser: &mut Parser, nb: f64) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::Float { nb })
}

fn parse_string(parser: &mut Parser, buf : Vec<char>) -> ASTRef {
    parser.rustaml_context.ast_pool.push(ASTNode::String { str: parser.rustaml_context.str_interner.intern_compiler(&buf.iter().collect::<String>()) })
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

// TODO : add type system (Hindley–Milner ?) (and move type checking to after building the AST ?)
fn parse_type_annotation(parser: &mut Parser) -> Result<Type, ParserErr> {
    parser.eat_tok(Some(TokenDataTag::Colon))?;
    
    let simple_type = parse_annotation_simple(parser)?;

    let type_parsed = match parser.current_tok_data() {
        Some(TokenData::Arrow) => {
            // only simple types can be returned or passed to functions, need to refator this code to support cases like (int -> int) -> (int -> int)
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

fn parse_let(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let name_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;
    let name = match name_tok.tok_data {
        TokenData::Identifier(s) => parser.rustaml_context.str_interner.intern_compiler(&s.iter().collect::<String>()),
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

        let function_type_range_start = parser.current_tok().unwrap().range.start;

        let function_type: Type = match parser.current_tok_data() {
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

    
        //parser.vars.insert(name,  Type::Function(arg_types.clone(), return_type.clone(), is_variadic));

        let args = arg_names.into_iter().zip(arg_types.clone()).map(|x| Arg { name: parser.rustaml_context.str_interner.intern_compiler(&x.0), arg_type: x.1 }).collect::<Vec<Arg>>();


        //let mut old_arg_types = Vec::new();

        /*for Arg { name, arg_type} in &args {
            let old_type = parser.vars.insert(*name, arg_type.clone());
            if let Some(t) = old_type {
                old_arg_types.push((*name, t));
            }
        }*/

        let equal_tok = parser.eat_tok(Some(TokenDataTag::Op));

        match equal_tok.map(|t| t.tok_data) {
            Ok(TokenData::Op(Operator::Equal)) => {},
            Ok(t) => panic!("expected equal in let expr, got {:?}", t),
            Err(e) => panic!("Error when expecting equal in let expr : {:?}", e),
        };

        let body = parse_node(parser)?;

        /*if matches!(return_type.as_ref(), Type::Any){
            let body_type = body.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars)?;
            return_type = Box::new(body_type);
        }

        for (arg, arg_range) in args.iter_mut().zip(arg_ranges) {
            if matches!(arg.arg_type, Type::Any){
                arg.arg_type = infer_var_type(parser.rustaml_context,  &parser.vars, arg.name, body, &arg_range)?;
                //parser.vars.insert(arg.name, arg.arg_type.clone());
            }
        }*/

        /*for Arg {name, arg_type: _} in &args {
            parser.vars.remove(name);
        }

        for (name, t) in old_arg_types {
            parser.vars.insert(name, t);
        }


        parser.vars.insert(name,  Type::Function(arg_types.clone(), return_type.clone(), false)); // reinsertion with real types*/
        
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


        let tok = parser.eat_tok(Some(TokenDataTag::Op))?;
        match &tok.tok_data {
            TokenData::Op(Operator::Equal) => {},
            _ => return Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: tok.tok_data }, tok.range)),
        };

        let val_node = parse_node(parser)?;
        /*if var_type.is_none() {
            var_type = Some(val_node.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars)?);
        }*/

        /*if name.get_str(&parser.rustaml_context.str_interner) != "_" {
            parser.vars.insert(name, var_type.unwrap());
        }*/
        
        let body = match parser.current_tok_data() {
            Some(TokenData::In) => {
                parser.eat_tok(Some(TokenDataTag::In))?;
                Some(parse_node(parser)?)
            },
            _ => None,
        };

        /*if body.is_some() {
            // if has body, is a local variable
            parser.vars.remove(&name);
        }*/

        ASTNode::VarDecl {
            name,
            val: val_node,
            var_type,
            body,
        }
    };

    if let Some(TokenData::EndOfExpr) = parser.current_tok_data() {
        parser.eat_tok(Some(TokenDataTag::EndOfExpr)).unwrap();
    }

    Ok(parser.rustaml_context.ast_pool.push(node))
    
}

// for parsing operators https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn is_start_of_expr(tok_data : Option<&TokenData>) -> bool {
    let is_end_of_expr = match tok_data {
        Some(t) => matches!(t, TokenData::EndOfExpr | TokenData::Op(_) | TokenData::Else | TokenData::In | TokenData::ParenClose | TokenData::With),
        None => false,
    } ;
    !is_end_of_expr
}


fn is_function_arg_start(tok_data : Option<&TokenData>) -> bool {
    match tok_data {
        Some(t) => 
            matches!(t, TokenData::Identifier(_) | TokenData::Integer(_) | TokenData::Float(_) | TokenData::String(_) | TokenData::ParenOpen | TokenData::ArrayOpen | TokenData::True | TokenData::False),
        None => false,
    }
}

fn parse_var_use_in_function(parser : &mut Parser , identifier_buf : Vec<char>, first_tok_start: usize) -> Result<ASTRef, ParserErr> {
    let identifier = parser.rustaml_context.str_interner.intern_compiler(&identifier_buf.iter().collect::<String>());
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::VarUse { name: identifier }))

}

fn parse_function_arg(parser : &mut Parser) -> Result<ASTRef, ParserErr> {
    let tok = parser.eat_tok(None).unwrap();
    let first_tok_start = tok.range.start;
    let node = match tok.tok_data {
        TokenData::Integer(nb) => Ok(parse_integer(parser, nb)),
        TokenData::Float(nb) => Ok(parse_float(parser, nb)),
        TokenData::String(buf) => Ok(parse_string(parser, buf)),
        TokenData::Identifier(buf) => parse_var_use_in_function(parser, buf, first_tok_start),
        TokenData::True => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: true })),
        TokenData::False => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: false })),
        TokenData::ParenOpen => parse_parenthesis(parser), // TODO : move this to the start of parse_node and make it unreachable! ? (because each time there are parenthesis, parse_node -> parse_primary -> parse_node is added to the call stack) 
        TokenData::ArrayOpen => parse_static_list(parser),
        t => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, tok.range))
    };

    return node;
}

// parse what could be a function call
// TODO : make this work for any expression for the function
fn parse_function_call(parser: &mut Parser, name : StringRef, first_tok_start: usize) -> Result<ASTRef, ParserErr> {
    let mut args = Vec::new();

    //let mut end_last_arg = first_tok_start;
    //let mut arg_ranges=  Vec::new();
    while parser.has_tokens_left() && is_function_arg_start(parser.current_tok_data()) {
        //let arg_range_start = parser.current_tok().unwrap().range.start;
        //let arg = parse_primary(parser)?; // TODO : replace with parse_node ? (fix problems with stack overflow -> less recursion ? implement tail call optimization ?)
        //let arg_range_end = parser.current_tok().unwrap().range.end-1;
        //end_last_arg = parser.current_tok().unwrap().range.end-1;
        //arg_ranges.push(arg_range_start..arg_range_end);
        /*let arg = match parser.current_tok_data() {
            Some(TokenData::Identifier(buf)) => {
                // If the arg itself is an identifier, treat it as a var use, not a call
                let name = parser.rustaml_context.str_interner.intern_compiler(&buf.iter().collect::<String>());
                parser.eat_tok(None)?;
                parser.rustaml_context.ast_pool.push(ASTNode::VarUse { name })
            }
            _ => parse_primary(parser)?,
        };*/
        //let arg = parse_primary(parser)?;
        let arg= parse_function_arg(parser)?;

        args.push(arg);
    }

    let ast_node = if args.is_empty(){
        ASTNode::VarUse { 
            name
        }
    } else {
        ASTNode::FunctionCall { 
            name, 
            args,
        }
    };
    
    /*let (args_type, is_variadic)  = match parser.vars.get(&function_name).unwrap() {
        Type::Function(args_type, _, is_variadic) => (args_type, *is_variadic),
        _ => unreachable!(),
    };*/

    //let function_call_range = first_tok_start..arg_ranges.last().unwrap().end;

    /*if !is_variadic && args.len() != args_type.len(){
        let function_name = function_name.get_str(&parser.rustaml_context.str_interner).to_owned();
        return Err(ParserErr::new(ParserErrData::WrongNumberOfArgs { function_name, expected_nb: args_type.len(), got_nb: args.len() }, function_call_range))
    }*/

    // TODO add an error for when the function is variadic but args.len() < args_type.len() 

    /*for (idx, arg_type) in args_type.iter().enumerate(){
        let arg_given_type = args[idx].get(&parser.rustaml_context.ast_pool).get_type(&parser.rustaml_context, &parser.vars)?;
        if !matches!(arg_given_type, Type::Any) && !matches!(arg_type, Type::Any) && &arg_given_type != arg_type {
            let function_name = function_name.get_str(&parser.rustaml_context.str_interner).to_owned();
            return Err(ParserErr::new(ParserErrData::WrongArgType { function_name, expected_type: arg_type.clone(), got_type: arg_given_type } , arg_ranges[idx].clone()))
        }
    }*/

    //dbg!(&args);
    Ok(parser.rustaml_context.ast_pool.push(ast_node))
}

fn parse_identifier_expr(parser: &mut Parser, identifier_buf : Vec<char>, first_tok_start: usize) -> Result<ASTRef, ParserErr> {
    let identifier = parser.rustaml_context.str_interner.intern_compiler(&identifier_buf.iter().collect::<String>());
    /*let is_function = match parser.vars.get(&identifier) {
        Some(t) => matches!(t, Type::Function(_, _, _)),
        None => false, // no error because there are variables that are created in match that are not accounted for in vars (TODO !!!)
        //None => panic!("ERROR : unknown identifier {}", identifier.get_str(&mut parser.rustaml_context.str_interner)),
    };*/

    let is_function = is_function_arg_start(parser.current_tok_data());
    if is_function {
        parse_function_call(parser, identifier, first_tok_start)
    } else {
        // var use
        Ok(parser.rustaml_context.ast_pool.push(ASTNode::VarUse { name: identifier }))
    }
    
}

fn parse_if(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let cond_expr = parse_node(parser)?;

    /*match cond_expr.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars)? {
        Type::Bool => {},
        t => panic!("Error in type checking : {:?} type passed in if expr", t), // TODO : return a result instead
    }*/

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


// TODO : handle old values of these vars
/*fn add_vars_from_pattern(parser : &mut Parser, pattern : &Pattern, val_type : &Type){
    match pattern {
        Pattern::VarName(name) => { parser.vars.insert(*name, val_type.clone()); },
        Pattern::ListDestructure(e, l) => {
            let element_type = match &val_type {
                Type::List(e_type) => e_type.as_ref(),
                Type::Any => &Type::Any, // only a workaround, use the inference (TODO ?) 
                _ => panic!("Expected a list type"),
            };
            parser.vars.insert(*e, element_type.clone());
            add_vars_from_pattern(parser, l.as_ref(), val_type);
        },
        _ => {} 
    }
}*/

/*fn remove_vars_from_pattern(parser : &mut Parser, pattern : &Pattern){
    match pattern {
        Pattern::VarName(name) => { parser.vars.remove(name); },
        Pattern::ListDestructure(e, l) => {
            parser.vars.remove(e);
            remove_vars_from_pattern(parser, l.as_ref());
        },
        _ => {} 
    }
}*/



fn parse_pattern(parser : &mut Parser) -> Result<Pattern, ParserErr> {
    let pattern_tok = parser.eat_tok(None)?;

    let pattern = match pattern_tok.tok_data {
        TokenData::Identifier(buf) => {
            if matches!(parser.current_tok_data(), Some(TokenData::Op(Operator::ListAppend))){
                parser.eat_tok(Some(TokenDataTag::Op))?;

                let head = buf.iter().collect::<String>();
                let tail_pattern = parse_pattern(parser)?;
                Pattern::ListDestructure(parser.rustaml_context.str_interner.intern_compiler(&head), Box::new(tail_pattern))
            } else {
                let s = buf.iter().collect::<String>();
                match s.as_str() {
                    "_" => Pattern::Underscore,
                    s_ref => Pattern::VarName(parser.rustaml_context.str_interner.intern_compiler(s_ref)),
                }
            }
            
        },
        TokenData::Integer(nb) => { 
            if let Some(&TokenData::Range(inclusivity)) = parser.current_tok_data() {
                parser.eat_tok(None)?;
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
        TokenData::String(s) => Pattern::String(parser.rustaml_context.str_interner.intern_compiler(&s.iter().collect::<String>())),
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

    // TODO : maybe infer this type ?
    //let val_type = matched_expr.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars)?;

    while parser.current_tok().is_some() && matches!(parser.current_tok_data().unwrap(), TokenData::Pipe) {
        parser.eat_tok(Some(TokenDataTag::Pipe))?;
        let pattern = parse_pattern(parser)?;
        //dbg!(&pattern);
        parser.eat_tok(Some(TokenDataTag::Arrow))?;
        // TODO : add vars from match pattern ? (will need a function that from a pattern and the type of the matched pattern will return the names and the types of the vars)
        //add_vars_from_pattern(parser, &pattern, &val_type);
        let pattern_expr = parse_node(parser)?;
        //remove_vars_from_pattern(parser, &pattern);
        patterns.push((pattern, pattern_expr));
    }

    Ok(parser.rustaml_context.ast_pool.push(ASTNode::MatchExpr { 
        matched_expr, 
        patterns, 
    }))
}


// TODO : fix parsing parenthesis in parenthesis ex : (fib_list (i-1))
fn parse_parenthesis(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    if let Some(t) = parser.current_tok_data() && matches!(t, TokenData::ParenClose){
        debug_println!(parser.rustaml_context.is_debug_print, "FOUND UNIT");
        parser.eat_tok(Some(TokenDataTag::ParenClose))?;
        return Ok(parser.rustaml_context.ast_pool.push(ASTNode::Unit));
    }
    debug_println!(parser.rustaml_context.is_debug_print, "START OF PARENTHESE");
    let expr: ASTRef = parse_node(parser)?;
    debug_println!(parser.rustaml_context.is_debug_print, "expr = {:#?}", DebugWrapContext::new(&expr, parser.rustaml_context));
    //dbg_intern!(&expr, &parser.rustaml_context);
    parser.eat_tok(Some(TokenDataTag::ParenClose))?;
    debug_println!(parser.rustaml_context.is_debug_print, "EAT END OF PARENTHESE");
    Ok(expr)
}

fn parse_static_list(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let elems = parse_list_form(parser, parse_node)?;
    
    Ok(parser.rustaml_context.ast_pool.push(ASTNode::List { list: elems }))
}

fn parse_primary(parser: &mut Parser) -> Result<ASTRef, ParserErr> {
    let tok = parser.eat_tok(None).unwrap();
    let first_tok_start = tok.range.start;
    let node = match tok.tok_data {
        TokenData::Let => parse_let(parser),
        TokenData::If => parse_if(parser),
        TokenData::Match => parse_match(parser),
        TokenData::Integer(nb) => Ok(parse_integer(parser, nb)),
        TokenData::Float(nb) => Ok(parse_float(parser, nb)),
        TokenData::String(buf) => Ok(parse_string(parser, buf)),
        TokenData::Identifier(buf) => parse_identifier_expr(parser, buf, first_tok_start),
        TokenData::True => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: true })),
        TokenData::False => Ok(parser.rustaml_context.ast_pool.push(ASTNode::Boolean { b: false })),
        TokenData::ParenOpen => parse_parenthesis(parser), // TODO : move this to the start of parse_node and make it unreachable! ? (because each time there are parenthesis, parse_node -> parse_primary -> parse_node is added to the call stack) 
        TokenData::ArrayOpen => parse_static_list(parser),
        t => panic!("t : {:?}", t),
        t => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, tok.range))
    };

    return node;
}

/*fn ensure_type_is_binop(op : Operator, t : Type, is_type : Type, range : Range<usize>) -> Result<(), ParserErr>{
    if t == is_type {
        Ok(())
    } else {
        Err(ParserErr::new(ParserErrData::MismatchedBinOpType 
            { op: format!("{:?}", op), expected_type: is_type, got_type: t }, range))
    }
    
}

// TODO : move this after having created the whole AST to resolve types ?
fn typecheck_binop(op : Operator, lhs_type : Type, rhs_type : Type, lhs_range : Range<usize>, rhs_range : Range<usize>) -> Result<(), ParserErr>{
    let op_type = op.get_type(None);
    match &op_type {
        Type::Float | Type::Integer => {
            ensure_type_is_binop(op, lhs_type, op_type.clone(), lhs_range)?;
            ensure_type_is_binop(op, rhs_type, op_type, rhs_range)?;
        },
        _ => {
            // TODO : for more complicated like like lists, etc
        }
    }
    Ok(())
}*/

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
        let mut rhs = parse_primary(parser)?;

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

        //let lhs_type = lhs.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars)?;
        //let rhs_type = rhs.get(&parser.rustaml_context.ast_pool).get_type(parser.rustaml_context, &parser.vars)?;
        //typecheck_binop(operator, lhs_type, rhs_type, 0..0, 0..0)?; // TODO
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

// TODO : remove this
/*fn init_std_functions(rustaml_context : &mut RustamlContext) -> FxHashMap<StringRef, Type> {
    let mut i = |s| rustaml_context.str_interner.intern_compiler(s);
    FxHashMap::from_iter([
        (i("print"), Type::Function(vec![Type::Any], Box::new(Type::Unit), false)),
        (i("rand"), Type::Function(vec![Type::Unit], Box::new(Type::Integer), false)),
        // TODO : add a rand_f ? or make the rand function generic with its return ?
        (i("format"), Type::Function(vec![Type::Str], Box::new(Type::Str), true)),
    ])
}*/

pub fn parse(tokens: Vec<Token>, rustaml_context : &mut RustamlContext) -> Result<ASTRef, ParserErr> /*Result<(ASTRef, FxHashMap<StringRef, Type>), ParserErr>*/ {
    let /*(root_node, vars)*/ root_node = { 
        //let vars = init_std_functions(rustaml_context);
        let mut parser = Parser { 
            tokens, 
            pos: 0,
            //vars,
            precedences: init_precedences(),
            rustaml_context,
        };
        let root_node = parse_top_level_node(&mut parser)?;
        //(root_node, parser.vars)
        root_node
    };

    debug_println!(rustaml_context.is_debug_print, "root_node = {:#?}", DebugWrapContext::new(&root_node, rustaml_context));
    //Ok((root_node, vars))
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