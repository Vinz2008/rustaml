use std::ops::Range;

use rustc_hash::FxHashMap;

use enum_tags::{Tag, TaggedEnum};

use crate::lexer::{Operator, Token, TokenData, TokenDataTag};

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub name : String,
    arg_type : Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    VarName(String), // | x pattern
    Integer(i64), // | 2
    Float(f64), // | 2.6
    Range(i64, i64, bool), // bool is for the inclusivity | 0..1
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
    Boolean {
        b : bool,
    },
    BinaryOp {
        op: Operator,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
    },
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
    Unit,
}

impl ASTNode {
    fn get_type(&self, parser: &Parser) -> Type {
        match self {
            ASTNode::Boolean { b: _ } => Type::Bool,
            ASTNode::Integer { nb: _ } => Type::Integer,
            ASTNode::Float { nb : _ } => Type::Float,
            ASTNode::BinaryOp { op, lhs: _, rhs: _ } => op.get_type(), // TODO
            ASTNode::VarDecl { name: _, val: _ } => Type::Unit, // TODO
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
    p.insert(Operator::SuperiorOrEqual, 10);
    p.insert(Operator::InferiorOrEqual, 10);
    p.insert(Operator::Plus, 20);
    p.insert(Operator::Minus, 20);
    p.insert(Operator::Mult, 30);
    p.insert(Operator::Div, 30);
    p
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    vars : FxHashMap<String, Type>, // include functions (which are just vars with function types)
    precedences : FxHashMap<Operator, i32>,
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

#[derive(Debug, Tag)]
pub enum ParserErrData {
    UnexpectedEOF,
    UnexpectedTok {
        tok : Box<TokenData>,
    },
    WrongTok {
        // TODO : put more infos ? (entire token ? range ?)
        expected_tok : Box<TokenDataTag>,
        got_tok : Box<TokenData>,
    },
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
                expected_tok: Box::new(tok_type),
                got_tok: Box::new(self.tokens[self.pos].tok_data.clone()),
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

// TODO : add type system (Hindley–Milner ?) (and move type checking to after building the AST ?)
fn parse_type_annotation(parser: &mut Parser) -> Result<Type, ParserErr> {
    parser.eat_tok(Some(TokenDataTag::Colon))?;
    let tok = parser.eat_tok(None)?;
    match &tok.tok_data {
        TokenData::Identifier(b) => {
            let type_annot = match b.iter().collect::<String>().as_str() {
                "int" => Type::Integer,
                "bool" => Type::Bool,
                "float" => Type::Float,
                _ => panic!("Unknown type"),
            };
            Ok(type_annot)
        },
        TokenData::ParenOpen => {
            parser.eat_tok(Some(TokenDataTag::ParenClose))?;
            Ok(Type::Unit)
        },
        _ => Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: Box::new(tok.tok_data) }, tok.range.clone())),
    }
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
        let mut args = Vec::new();
        while matches!(parser.current_tok_data(), Some(TokenData::Identifier(_))) {
            let arg_identifier = parser.eat_tok(Some(TokenDataTag::Identifier)).unwrap();
            let arg_name = match arg_identifier.tok_data {
                TokenData::Identifier(s) => s.iter().collect::<String>(),
                _ => unreachable!(),
            };

            // TODO : move the type annotation after the args and add function types to make it work
            let arg_type = match parser.current_tok_data() {
                Some(TokenData::Colon) => Some(parse_type_annotation(parser)?),
                Some(_) | None => None,
            };

            args.push(Arg {name: arg_name, arg_type});
        }

        //let arg_types = vec![Type::Number; args.len()];

        let arg_types = args.iter().map(|arg| arg.arg_type.clone().unwrap()).collect::<Vec<Type>>();
        parser.vars.insert(name.clone(),  Type::Function(arg_types.clone(), Box::new(Type::Unit))); // unit is a placeholder (change it to 'a in the future ?)

        for Arg { name, arg_type} in &args {
            parser.vars.insert(name.clone(), arg_type.clone().unwrap()); // TODO : remove unwrap ?
        }

        let equal_tok = parser.eat_tok(Some(TokenDataTag::Op));

        match equal_tok.map(|t| t.tok_data) {
            Ok(TokenData::Op(Operator::Equal)) => {},
            Ok(t) => panic!("expected equal in let expr, got {:?}", t),
            Err(e) => panic!("Error when expecting equal in let expr : {:?}", e),
        };

        let body = parse_node(parser)?;

        
        let body_type = body.get_type(parser);

        for Arg {name, arg_type: _} in &args {
            parser.vars.remove(name);
        }


        parser.vars.insert(name.clone(),  Type::Function(arg_types, Box::new(body_type.clone())));
        
        ASTNode::FunctionDefinition { 
            name, 
            args, 
            body: Box::new(body),
            return_type: body_type,
        }
    } else {
        let mut var_type = match parser.current_tok_data() {
            Some(TokenData::Colon) => Some(parse_type_annotation(parser)?),
            Some(_) | None => None,
        };


        let tok = parser.eat_tok(Some(TokenDataTag::Op))?;
        match &tok.tok_data {
            TokenData::Op(Operator::Equal) => {},
            _ => return Err(ParserErr::new(ParserErrData::UnexpectedTok { tok: Box::new(tok.tok_data) }, tok.range)),
        };

        let val_node = parse_node(parser)?;
        if var_type.is_none() {
            var_type = Some(val_node.get_type(parser))
        }
        parser.vars.insert(name.clone(), var_type.unwrap());

        ASTNode::VarDecl {
            name,
            val: Box::new(val_node),
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

    while parser.has_tokens_left() &&  !matches!(parser.current_tok_data(), Some(TokenData::EndOfExpr)) && !matches!(parser.current_tok_data(), Some(TokenData::Op(_))) {
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
        t => panic!("Unexpected token in pattern : {:?}", t),
    };

    Ok(pattern)

}

fn parse_match(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
    let matched_expr = parse_primary(parser)?;
    parser.eat_tok(Some(TokenDataTag::With)).unwrap();
    let mut patterns = Vec::new();
    while parser.current_tok().is_some() && matches!(parser.current_tok_data().unwrap(), TokenData::Pipe) {
        parser.eat_tok(Some(TokenDataTag::Pipe)).unwrap();
        let pattern = parse_pattern(parser)?;
        dbg!(&pattern);
        parser.eat_tok(Some(TokenDataTag::Arrow)).unwrap();
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

fn parse_primary(parser: &mut Parser) -> Result<ASTNode, ParserErr> {
    let tok = parser.eat_tok(None).unwrap();
    let node = match tok.tok_data {
        TokenData::Let => parse_let(parser),
        TokenData::If => parse_if(parser),
        TokenData::Match => parse_match(parser),
        TokenData::Integer(nb) => Ok(parse_integer(nb)),
        TokenData::Float(nb) => Ok(parse_float(nb)),
        TokenData::Identifier(buf) => parse_identifier_expr(parser, buf),
        TokenData::True => Ok(ASTNode::Boolean { b: true }),
        TokenData::False => Ok(ASTNode::Boolean { b: false }),
        TokenData::ParenOpen => parse_parenthesis(parser),
        t => panic!("Unexpected token : {:?}", t),
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
        let expected =  ASTNode::VarDecl { name: "a".to_string(), val: Box::new(ASTNode::Integer { nb: 2 }) };
        let expected_toplevel = ASTNode::TopLevel { nodes: vec![expected] };
        assert_eq!(result,  expected_toplevel);
    }
}