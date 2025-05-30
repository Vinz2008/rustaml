use std::collections::HashMap;

use enum_tags::TaggedEnum;

use crate::lexer::{Operator, Token, TokenTag};

// TODO : flatten AST nodes (https://www.cs.cornell.edu/~asampson/blog/flattening.html)
#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    TopLevel {
        nodes: Vec<ASTNode>,
    },
    FunctionDefinition {
        name : String,
        args : Vec<String>,
        body : Box<ASTNode>,
    },
    VarDecl {
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
    Number {
        nb: i64,
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

#[derive(Debug, Clone)]
enum Type {
    Number,
    Bool,
    Function(Vec<Type>, Box<Type>),
    Unit,
}

impl ASTNode {
    fn get_type(&self, parser: &Parser) -> Type {
        match self {
            ASTNode::Boolean { b: _ } => Type::Bool,
            ASTNode::Number { nb: _ } => Type::Number,
            ASTNode::BinaryOp { op: _, lhs: _, rhs: _ } => Type::Number, // TODO
            ASTNode::VarDecl { name: _, val: _ } => Type::Unit, // TODO
            ASTNode::FunctionCall { name, args: _ } => parser.vars.get(name).unwrap().clone(), // need to create a hashmap for function types, in parser context ?
            ASTNode::VarUse { name} => match parser.vars.get(name){
                Some(t) => t.clone(),
                None => panic!("expected var {}", &name)
             },
            ASTNode::IfExpr { cond_expr: _, then_body, else_body : _ } => then_body.get_type(parser), // no need for typechecking the two branches because it is done when constructing the IfExpr
            ASTNode::TopLevel { nodes: _ } => Type::Unit,
            ASTNode::FunctionDefinition { name: _, args: _, body: _ } => Type::Unit,
        }
    }
}


fn init_precedences() -> HashMap<Operator, i32> {
    let mut p = HashMap::new();
    p.insert(Operator::Plus, 20);
    p.insert(Operator::Minus, 20);
    p.insert(Operator::Mult, 30);
    p.insert(Operator::Div, 30);
    p
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    vars : HashMap<String, Type>, // include functions (which are just vars with function types)
    precedences : HashMap<Operator, i32>,
}

#[derive(Debug)]
enum ParserErr {
    UnexpectedEOF,
    WrongTok,
}

impl Parser {
    fn has_tokens_left(&self) -> bool {
        self.pos + 1 < self.tokens.len()
    }

    fn eat_tok(&mut self, token_type: Option<TokenTag>) -> Result<Token, ParserErr> {
        if self.pos >= self.tokens.len() {
            return Err(ParserErr::UnexpectedEOF);
        }
        if token_type.is_some() && self.tokens[self.pos].tag() != token_type.unwrap() {
            return Err(ParserErr::WrongTok);
        }
        let current_tok = self.tokens[self.pos].clone();
        self.pos += 1;

        Ok(current_tok)
    }

    fn current_tok(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
}

fn parse_number(nb: i64) -> ASTNode {
    ASTNode::Number { nb }
}

fn parse_let(parser: &mut Parser) -> ASTNode {
    // TODO : pass error handling (by making all parse functions return results, than handling in the main function)
    let name_tok = parser.eat_tok(Some(TokenTag::Identifier)).unwrap();
    let name = match name_tok {
        Token::Identifier(s) => s,
        _ => unreachable!(),
    }.iter().collect::<String>();
    let node = if matches!(parser.current_tok(), Some(Token::Identifier(_))) {
        // function definition
        let mut args = Vec::new();
        while matches!(parser.current_tok(), Some(Token::Identifier(_))) {
            let arg_identifier = parser.eat_tok(Some(TokenTag::Identifier)).unwrap();
            let arg = match arg_identifier {
                Token::Identifier(s) => s,
                _ => unreachable!(),
            }.iter().collect::<String>();
            args.push(arg);
        }

        let arg_types = vec![Type::Number; args.len()];


        for (arg_name, arg_type) in (&args).iter().zip(&arg_types) {
            parser.vars.insert(arg_name.clone(), arg_type.clone());
        }

        parser.eat_tok(Some(TokenTag::Equal)).unwrap();

        let body = parse_node(parser);

        
        let body_type = body.get_type(&parser);

        for arg_name in &args {
            parser.vars.remove(arg_name);
        }

        parser.vars.insert(name.clone(),  Type::Function(arg_types, Box::new(body_type)));
        
        ASTNode::FunctionDefinition { 
            name, 
            args, 
            body: Box::new(body),
        }
    } else {
        parser.eat_tok(Some(TokenTag::Equal)).unwrap();

        let val_node = parse_node(parser);
        parser.vars.insert(name.clone(), val_node.get_type(&parser));

        ASTNode::VarDecl {
            name: name,
            val: Box::new(val_node),
        }
    };

    if let Some(Token::EndOfExpr) = parser.current_tok() {
        parser.eat_tok(Some(TokenTag::EndOfExpr)).unwrap();
    }

    node
    
}

// for parsing operators https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

// TODO : parse parenthessis

fn parse_function_call(parser: &mut Parser, function_name : String) -> ASTNode {
    let mut args = Vec::new();
    while parser.has_tokens_left() && !matches!(parser.current_tok(), Some(Token::EndOfExpr)) {
        let arg = parse_node(parser);
        args.push(arg);
    }
    dbg!(&args);
    ASTNode::FunctionCall { 
        name: function_name, 
        args,
    }
}

fn parse_identifier_expr(parser: &mut Parser, identifier_buf : Vec<char>) -> ASTNode {
    let identifier = identifier_buf.iter().collect();
    let is_function = match parser.vars.get(&identifier) {
        Some(t) => matches!(t, Type::Function(_, _)),
        None => panic!("ERROR : unknown identifier {}", identifier),
    };
    if is_function {
        parse_function_call(parser, identifier)
    } else {
        // var use
        ASTNode::VarUse { name: identifier }
    }
    
}

fn parse_if(parser: &mut Parser) -> ASTNode {
    let cond_expr = parse_node(parser);

    match cond_expr.get_type(&parser) {
        Type::Bool => {},
        t => panic!("Error in type checking : {:?} type passed in if expr", t), // TODO : return a result instead
    }

    parser.eat_tok(Some(TokenTag::Then)).unwrap();

    let then_body = parse_node(parser);

    parser.eat_tok(Some(TokenTag::Else)).unwrap();

    let else_body = parse_node(parser);
    
    ASTNode::IfExpr { cond_expr: Box::new(cond_expr), then_body: Box::new(then_body), else_body: Box::new(else_body) }
}

fn parse_primary(parser: &mut Parser) -> ASTNode {
    let tok = parser.eat_tok(None).unwrap();
    let node = match tok {
        Token::Let => parse_let(parser),
        Token::If => parse_if(parser),
        Token::Number(nb) => parse_number(nb),
        Token::Identifier(buf) => parse_identifier_expr(parser, buf),
        Token::True => ASTNode::Boolean { b: true },
        Token::False => ASTNode::Boolean { b: false },
        t => panic!("Unexpected token : {:?}", t),
    };

    return node;
}

fn parse_binary_rec(parser: &mut Parser, lhs: ASTNode, min_precedence: i32) -> ASTNode {
    let mut lhs = lhs;

    while parser.has_tokens_left() {
        let current_tok = parser.current_tok();
        let operator = match current_tok {
            Some(Token::Op(op)) => op.clone(),
            Some(_) | None => break,
        };
        let first_precedence = *parser.precedences.get(&operator).unwrap();
        if first_precedence < min_precedence {
            break;
        }
        parser.eat_tok(Some(TokenTag::Op)).unwrap();
        let mut rhs = parse_primary(parser);

        while parser.has_tokens_left() {
            let current_tok = parser.current_tok();
            let new_operator =  match current_tok {
                Some(Token::Op(op)) => op,
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
            rhs = parse_binary_rec(parser, rhs, new_precedence);
        }

        lhs = ASTNode::BinaryOp {
            op: operator.clone(),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };
    }

    lhs
}

fn parse_binary(parser: &mut Parser, lhs: ASTNode) -> ASTNode {
    parse_binary_rec(parser, lhs, 0)
}

fn parse_node(parser: &mut Parser) -> ASTNode {
    let lhs = parse_primary(parser);
    let ret_expr = parse_binary(parser, lhs);
    ret_expr
}

fn parse_top_level_node(parser: &mut Parser) -> ASTNode {
    let mut nodes: Vec<ASTNode> = Vec::new();
    while parser.has_tokens_left() {
        nodes.push(parse_node(parser));
    }
    ASTNode::TopLevel { nodes }
}

pub fn parse(tokens: Vec<Token>) -> ASTNode {
    let mut parser = Parser { 
        tokens, 
        pos: 0,
        vars: HashMap::new(),
        precedences: init_precedences(),
    };
    let root_node = parse_top_level_node(&mut parser);
    dbg!(&root_node);
    root_node
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_simple() {
        let input = vec![Token::Let, Token::Identifier(vec!['a']), Token::Equal, Token::Number(2), Token::EndOfExpr];
        let result = parse(input);
        let expected =  ASTNode::VarDecl { name: "a".to_string(), val: Box::new(ASTNode::Number { nb: 2 }) };
        let expected_toplevel = ASTNode::TopLevel { nodes: vec![expected] };
        assert_eq!(result,  expected_toplevel);
    }
}