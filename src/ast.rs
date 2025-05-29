use lazy_static::lazy_static;
use std::collections::HashMap;

use enum_tags::TaggedEnum;

use crate::lexer::{Operator, Token, TokenTag};

// TODO : flatten AST nodes (https://www.cs.cornell.edu/~asampson/blog/flattening.html)
#[derive(Debug, Clone)]
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
    Number {
        nb: i64,
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

lazy_static! {
    static ref PRECEDENCES: HashMap<Operator, i32> = {
        let mut p = HashMap::new();
        p.insert(Operator::Plus, 20);
        p.insert(Operator::Minus, 20);
        p.insert(Operator::Mult, 30);
        p.insert(Operator::Div, 30);
        p
    };
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
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

        parser.eat_tok(Some(TokenTag::Equal)).unwrap();

        let body = parse_node(parser);
        
        ASTNode::FunctionDefinition { 
            name, 
            args, 
            body: Box::new(body),
        }
    } else {
        parser.eat_tok(Some(TokenTag::Equal)).unwrap();

        let val_node = parse_node(parser);

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

fn parse_function_call(parser: &mut Parser, function_name_buf : Vec<char>) -> ASTNode {
    let mut args = Vec::new();
    while parser.has_tokens_left() && !matches!(parser.current_tok(), Some(Token::EndOfExpr)) {
        let arg = parse_node(parser);
        args.push(arg);
    }
    dbg!(&args);
    ASTNode::FunctionCall { 
        name: function_name_buf.iter().collect::<String>(), 
        args,
    }
}

fn parse_primary(parser: &mut Parser) -> ASTNode {
    let tok = parser.eat_tok(None).unwrap();
    let node = match tok {
        Token::Let => parse_let(parser),
        Token::Number(nb) => parse_number(nb),
        Token::Identifier(buf) => parse_function_call(parser, buf),
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
        let first_precedence = *PRECEDENCES.get(&operator).unwrap();
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
            let precedence = *PRECEDENCES.get(new_operator).unwrap();
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
    let mut parser = Parser { tokens, pos: 0 };
    let root_node = parse_top_level_node(&mut parser);
    dbg!(&root_node);
    root_node
}
