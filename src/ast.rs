use enum_tags::TaggedEnum;

use crate::lexer::{Token, TokenTag};

// TODO : flatten AST nodes (https://www.cs.cornell.edu/~asampson/blog/flattening.html)
#[derive(Debug)]
pub enum ASTNode {
    TopLevel {
        nodes : Vec<ASTNode>,
    },
    VarDecl {
        name : String,
        val : Box<ASTNode>
    },
    Number {
        nb : i64,
    }
}

struct Parser {
    tokens : Vec<Token>,
    pos : usize
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

    // TODO : make it specify which token type to pass (have to pass a token type ? use generics ?, use https://crates.io/crates/enum-tags ?)
    fn eat_tok(&mut self, token_type : Option<TokenTag>) -> Result<Token, ParserErr> {
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
}

fn parse_number(nb : i64) -> ASTNode {
    ASTNode::Number { nb }
}

fn parse_let(parser : &mut Parser) -> ASTNode {
    // TODO : pass error handling (by making all parse functions return results, than handling in the main function) 
    let var_name_tok = parser.eat_tok(Some(TokenTag::Identifier)).unwrap();
    let var_name = match var_name_tok {
        Token::Identifier(s) => s,
        _ => unreachable!()
    };
    parser.eat_tok(Some(TokenTag::Equal)).unwrap();
    
    let val_node = parse_node(parser);
    
    ASTNode::VarDecl { 
        name: var_name.iter().collect::<String>(), 
        val: Box::new(val_node)  
    }
}


// for parsing operators https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn parse_node(parser : &mut Parser) -> ASTNode {
    let tok = match parser.eat_tok(None).unwrap() {
        Token::Let => parse_let(parser),
        Token::Number(nb) => parse_number(nb),
        t => panic!("Unexpected token : {:?}", t),
    };

    return tok;
}

fn parse_top_level_node(parser : &mut Parser) -> ASTNode {
    let mut nodes : Vec<ASTNode> = Vec::new();
    while parser.has_tokens_left() {
        nodes.push(parse_node(parser));
    } 
    ASTNode::TopLevel { nodes }
}

pub fn parse(tokens : Vec<Token>) -> ASTNode {
    let mut parser = Parser {
        tokens,
        pos : 0,
    };
    let root_node = parse_top_level_node(&mut parser);
    dbg!(&root_node);
    root_node
}