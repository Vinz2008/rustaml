use std::ops::Range;

use rustc_hash::FxHashMap;

use enum_tags::{Tag, TaggedEnum};

use crate::{lexer::{Operator, Token, TokenData, TokenDataTag}, type_inference::{infer_var_type, TypeInferenceErr}};

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub name : String,
    arg_type : Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    VarName(String), // | x pattern
    Integer(i64), // | 2
    Float(f64), // | 2.6
    Range(i64, i64, bool), // bool is for the inclusivity | 0..1
    String(String), // | "test"
    List(Vec<Pattern>), // | [1, 2, 3]
    ListDestructure(String, String), // head name then tail name TODO : refactor to be recursive so you can have e::e2::l
    Underscore,
}


// TODO : flatten AST nodes (https://www.cs.cornell.edu/~asampson/blog/flattening.html)
#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    TopLevel {
        nodes: Vec<ASTNode>,
    },
    FunctionDefinition {
        name : String,
        args : Vec<Arg>,
        body : Box<ASTNode>,
        return_type : Type,
    },
    VarDecl {
        // TODO : intern all strings and replace these by refs to strings ?
        name: String,
        val: Box<ASTNode>,
        body : Option<Box<ASTNode>>,
    },
    VarUse {
        name : String,
    },
    IfExpr {
        cond_expr : Box<ASTNode>,
        then_body : Box<ASTNode>,
        else_body : Box<ASTNode>,
    },
    MatchExpr {
        matched_expr : Box<ASTNode>,
        patterns : Vec<(Pattern, ASTNode)>
    },
    Integer {
        nb: i64,
    },
    Float {
        nb: f64,
    },
    String {
        str : String
    },
    List {
        list : Vec<ASTNode>,
    },
    Boolean {
        b : bool,
    },
    BinaryOp {
        op: Operator,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
    // TODO : UnaryOp
    FunctionCall {
        name : String,
        args : Vec<ASTNode>,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    Float,
    Bool,
    Function(Vec<Type>, Box<Type>),
    Str,
    List(Box<Type>),
    Any, // equivalent to 'a
    Unit,
}

impl ASTNode {
    pub fn get_type(&self, parser: &Parser) -> Type {
        match self {
            ASTNode::Boolean { b: _ } => Type::Bool,
            ASTNode::Integer { nb: _ } => Type::Integer,
            ASTNode::Float { nb : _ } => Type::Float,
            ASTNode::String { str: _ } => Type::Str,
            ASTNode::List { list } => Type::List(Box::new(list.first().unwrap().get_type(parser))),
            ASTNode::BinaryOp { op, lhs: _, rhs: _ } => op.get_type(), // TODO
            ASTNode::VarDecl { name: _, val: _, body: _ } => Type::Unit, // TODO
            ASTNode::FunctionCall { name, args: _ } => parser.vars.get(name).unwrap().clone(), // need to create a hashmap for function types, in parser context ?
            ASTNode::VarUse { name} => match parser.vars.get(name){
                Some(t) => t.clone(),
                None => panic!("expected var {}", &name)
             },
            ASTNode::IfExpr { cond_expr: _, then_body, else_body : _ } => then_body.get_type(parser), // no need for typechecking the two branches because it is done when constructing the IfExpr
            ASTNode::MatchExpr { matched_expr: _, patterns } => patterns.first().unwrap().1.get_type(parser),
            ASTNode::TopLevel { nodes: _ } => Type::Unit,
            ASTNode::FunctionDefinition { name: _, args: _, body: _, return_type: _ } => Type::Unit,
        }
    }
}


fn init_precedences() -> FxHashMap<Operator, i32> {
    let mut p = FxHashMap::default();
    // TODO : reserve map size ?
    p.insert(Operator::IsEqual, 10);
    p.insert(Operator::Superior, 10);
    p.insert(Operator::Inferior, 10);
    p.insert(Operator::SuperiorOrEqual, 10);
    p.insert(Operator::InferiorOrEqual, 10);
    p.insert(Operator::Plus, 20);
    p.insert(Operator::Minus, 20);
    p.insert(Operator::Mult, 30);
    p.insert(Operator::Div, 30);
    p.insert(Operator::StrAppend, 30); // TODO : what precedence for this ?
    p
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    // optional types because of type inference of return values of functions that need to be inserted for recursive functions (TODO ?)
    pub vars : FxHashMap<String, Type>, // include functions (which are just vars with function types)
    precedences : FxHashMap<Operator, i32>,
}

#[derive(Debug, Tag)]
pub enum ParserErrData {
    UnexpectedEOF,
    UnexpectedTok {
        tok : TokenData,
    },
    WrongTok {
        // TODO : put more infos ? (entire token ? range ?)
        expected_tok : TokenDataTag,
        got_tok : TokenData,
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

impl Parser {
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

fn parse_integer(nb: i64) -> ASTNode {
    ASTNode::Integer { nb }
}

fn parse_float(nb: f64) -> ASTNode {
    ASTNode::Float { nb }
}

fn parse_string(buf : Vec<char>) -> ASTNode {
    ASTNode::String { str: buf.iter().collect() }
}


fn parse_annotation_simple(parser: &mut Parser) -> Result<Type, ParserErr> {
    let tok = parser.eat_tok(None)?;
    dbg!(&tok);
    match &tok.tok_data {
        TokenData::Identifier(b) => {
            let type_annot = match b.iter().collect::<String>().as_str() {
                "int" => Type::Integer,
                "bool" => Type::Bool,
                "float" => Type::Float,
                "str" => Type::Str,
                _ => panic!("Unknown type"),
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
            println!("function type");
            // only simple types can be returned or passed to functions, need to refator this code to support cases like (int -> int) -> (int -> int)
            let mut function_type_parts = vec![simple_type];
            dbg!(parser.current_tok_data());
            while let Some(t) = parser.current_tok_data() && matches!(t, TokenData::Arrow) {
                println!("PARSE FUNCTION TYPE PART");
                parser.eat_tok(Some(TokenDataTag::Arrow))?;
                let function_type_part = parse_annotation_simple(parser)?;
                function_type_parts.push(function_type_part);
                dbg!((parser.current_tok_data(), matches!(parser.current_tok_data(), Some(TokenData::Arrow))));
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

fn parse_let(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
    // TODO : pass error handling (by making all parse functions return results, than handling in the main function)
    let name_tok = parser.eat_tok(Some(TokenDataTag::Identifier))?;
    let name = match name_tok.tok_data {
        TokenData::Identifier(s) => s.iter().collect::<String>(),
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

    
        parser.vars.insert(name.clone(),  Type::Function(arg_types.clone(), return_type.clone()));

        let mut args = arg_names.iter().zip(arg_types.clone()).map(|x| Arg { name: x.0.clone(), arg_type: x.1 }).collect::<Vec<Arg>>();

        for Arg { name, arg_type} in &args {
            parser.vars.insert(name.clone(), arg_type.clone());
        }

        let equal_tok = parser.eat_tok(Some(TokenDataTag::Op));

        match equal_tok.map(|t| t.tok_data) {
            Ok(TokenData::Op(Operator::Equal)) => {},
            Ok(t) => panic!("expected equal in let expr, got {:?}", t),
            Err(e) => panic!("Error when expecting equal in let expr : {:?}", e),
        };

        let body = parse_node(parser)?;

        if matches!(return_type.as_ref(), Type::Any){
            let body_type = body.get_type(parser);
            return_type = Box::new(body_type);
        }
        

        for Arg {name, arg_type: _} in &args {
            parser.vars.remove(name);
        }

        for (arg, arg_range) in args.iter_mut().zip(arg_ranges) {
            if matches!(arg.arg_type, Type::Any){
                arg.arg_type = infer_var_type(parser, &arg.name, &body, &arg_range)?;
            }
        }


        parser.vars.insert(name.clone(),  Type::Function(arg_types.clone(), return_type.clone())); // reinsertion with real types
        
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
            TokenData::Op(Operator::Equal) => {},
            _ => return Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: tok.tok_data }, tok.range)),
        };

        let val_node = parse_node(parser)?;
        if var_type.is_none() {
            var_type = Some(val_node.get_type(parser))
        }

        parser.vars.insert(name.clone(), var_type.unwrap());

        let body = match parser.current_tok_data() {
            Some(TokenData::In) => {
                parser.eat_tok(Some(TokenDataTag::In))?;
                Some(Box::new(parse_node(parser)?))
            },
            _ => None,
        };

        if body.is_some() {
            // if has body, is a local variable
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

// for parsing operators https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html



fn parse_function_call(parser: &mut Parser, function_name : String) -> Result<ASTNode, ParserErr> {
    let mut args = Vec::new();

    fn function_call_parse_continue(tok_data : Option<&TokenData>) -> bool {
        !matches!(tok_data, Some(TokenData::EndOfExpr) | Some(TokenData::Op(_)) | Some(TokenData::Else) | Some(TokenData::In))
    }

    while parser.has_tokens_left() && function_call_parse_continue(parser.current_tok_data()) {
        let arg = parse_primary(parser)?;
        args.push(arg);
    }
    dbg!(&args);
    Ok(ASTNode::FunctionCall { 
        name: function_name, 
        args,
    })
}

fn parse_identifier_expr(parser: &mut Parser, identifier_buf : Vec<char>) -> Result<ASTNode, ParserErr> {
    let identifier = identifier_buf.iter().collect();
    let is_function = match parser.vars.get(&identifier) {
        Some(t) => matches!(t, Type::Function(_, _)),
        None => panic!("ERROR : unknown identifier {}", identifier),
    };
    if is_function {
        parse_function_call(parser, identifier)
    } else {
        // var use
        Ok(ASTNode::VarUse { name: identifier })
    }
    
}

fn parse_if(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
    let cond_expr = parse_node(parser)?;

    match cond_expr.get_type(parser) {
        Type::Bool => {},
        t => panic!("Error in type checking : {:?} type passed in if expr", t), // TODO : return a result instead
    }

    parser.eat_tok(Some(TokenDataTag::Then))?;

    let then_body = parse_node(parser)?;

    parser.eat_tok(Some(TokenDataTag::Else))?;

    let else_body = parse_node(parser)?;
    
    Ok(ASTNode::IfExpr { 
        cond_expr: Box::new(cond_expr), 
        then_body: Box::new(then_body), 
        else_body: Box::new(else_body) 
    })
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
            let s = buf.iter().collect::<String>();
            match s.as_str() {
                "_" => Pattern::Underscore,
                _ => Pattern::VarName(s),
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
        TokenData::String(s) => Pattern::String(s.iter().collect()),
        TokenData::ArrayOpen => {
            let elems = parse_list_form(parser, parse_pattern)?;

            Pattern::List(elems)
        },
        t => return Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, pattern_tok.range)),
    };

    Ok(pattern)

}

fn parse_match(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
    let matched_expr = parse_primary(parser)?;
    parser.eat_tok(Some(TokenDataTag::With))?;
    let mut patterns = Vec::new();
    while parser.current_tok().is_some() && matches!(parser.current_tok_data().unwrap(), TokenData::Pipe) {
        parser.eat_tok(Some(TokenDataTag::Pipe))?;
        let pattern = parse_pattern(parser)?;
        dbg!(&pattern);
        parser.eat_tok(Some(TokenDataTag::Arrow))?;
        // TODO : add vars from match pattern ? (will need a function that from a pattern and the type of the matched pattern will return the names and the types of the vars)
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
        TokenData::String(buf) => Ok(parse_string(buf)),
        TokenData::Identifier(buf) => parse_identifier_expr(parser, buf),
        TokenData::True => Ok(ASTNode::Boolean { b: true }),
        TokenData::False => Ok(ASTNode::Boolean { b: false }),
        TokenData::ParenOpen => parse_parenthesis(parser),
        TokenData::ArrayOpen => parse_static_list(parser),
        t => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: t }, tok.range))
    };

    return node;
}

fn parse_binary_rec(parser: &mut Parser, lhs: ASTNode, min_precedence: i32) -> Result<ASTNode, ParserErr> {
    let mut lhs = lhs;

    while parser.has_tokens_left() {
        let current_tok_data = parser.current_tok_data();
        let operator = match current_tok_data {
            Some(TokenData::Op(op)) => *op,
            Some(_) | None => break,
        };
        let first_precedence = *parser.precedences.get(&operator).unwrap();
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
            let precedence = *parser.precedences.get(new_operator).unwrap();
            if precedence <= first_precedence {
                break;
            }
            let new_precedence = if precedence > first_precedence {
                first_precedence + 1
            } else {
                first_precedence
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

pub fn parse(tokens: Vec<Token>) -> Result<ASTNode, ParserErr> {
    let mut parser = Parser { 
        tokens, 
        pos: 0,
        vars: FxHashMap::default(),
        precedences: init_precedences(),
    };
    let root_node = parse_top_level_node(&mut parser)?;
    dbg!(&root_node);
    Ok(root_node)
}


#[cfg(test)]
mod tests {
    use crate::lexer::TokenData;

    use super::*;

    #[test]
    fn parser_simple() {
        let input = vec![TokenData::Let, TokenData::Identifier(vec!['a']), TokenData::Op(Operator::Equal), TokenData::Integer(2), TokenData::EndOfExpr].into_iter().map(|t| Token::new(t, 0..0)).collect();
        let result = parse(input).unwrap();
        let expected =  ASTNode::VarDecl { name: "a".to_string(), val: Box::new(ASTNode::Integer { nb: 2 }), body: None };
        let expected_toplevel = ASTNode::TopLevel { nodes: vec![expected] };
        assert_eq!(result,  expected_toplevel);
    }
}