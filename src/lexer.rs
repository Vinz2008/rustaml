use core::panic;
use std::{ops::Range, vec};
use crate::ast::Type;

use enum_tags::Tag;

// TODO : add operator for floats (+., *., etc) for type inference

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Mult,
    Div,
    Equal,
    IsEqual,
    SuperiorOrEqual,
    InferiorOrEqual,
    // TODO : add Superior and Inferior
}

impl Operator {
    pub fn get_type(&self) -> Type {
        match self {
            Self::IsEqual | Self::SuperiorOrEqual | Self::InferiorOrEqual => Type::Bool,
            _ => Type::Integer,
        }
    }

    fn is_char_op(c : char) -> bool {
        match c {
            '+' | '-' | '*' | '/' | '=' | '<' | '>' => true,
            _ => false, 
        }
    }

    // TODO : pass char slice to support multi chars op
    pub fn str_to_op(s: &str) -> Operator {
        match s {
            "+" => Operator::Plus,
            "-" => Operator::Minus,
            "*" => Operator::Mult,
            "/" => Operator::Div,
            "=" => Operator::Equal,
            "==" => Operator::IsEqual,
            ">=" => Operator::SuperiorOrEqual,
            "<=" => Operator::InferiorOrEqual,
            _ => panic!("Invalid operator {}", s),
        }
    }
}

// TODO : replace all the Vec<char> with slices
#[derive(Debug, Clone, Tag, PartialEq)]
pub enum Token {
    Identifier(Vec<char>),
    Op(Operator),
    Integer(i64),
    Float(f64),
    Let,
    If,
    Then,
    Else,
    Match,
    With,
    True,
    False,
    ParenOpen,
    ParenClose,
    Colon,
    Arrow, // ->
    Pipe, // |
    EndOfExpr, // ;;
}



// TODO : rename Token to TokenData and uncomment it to add ranges to token
/* pub struct Token {
    tok : TokenData,
    range : Range<usize>,
} */

struct Lexer {
    content: Vec<char>,
    pos: usize,
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
}



fn lex_nb(lexer: &mut Lexer) -> Token {
    // TODO : floats

    fn continue_nb(c: char, is_float : &mut bool) -> bool {
        match c {
            '0'..='9' => true,
            '.' => {
                *is_float = true;
                true
            },
            _ => false,
        }
    }

    let mut is_float = false;
    let start_pos = lexer.pos - 1;

    while lexer.pos < lexer.content.len() && continue_nb(lexer.current_char().unwrap(), &mut is_float) {
        lexer.read_char();
    }

    let buf = lexer.content[start_pos..lexer.pos].to_vec();

    //dbg!(&buf);

    let str = buf.iter().collect::<String>();

    if is_float {
        let nb = str::parse::<f64>(str.as_str())
            .unwrap_or_else(|err| panic!("Failed when parsing nb {} : {}", str, err));

        //dbg!(nb);

        Token::Float(nb)
    } else {

        let nb = str::parse::<i64>(str.as_str())
            .unwrap_or_else(|err| panic!("Failed when parsing nb {} : {}", str, err));

        //dbg!(nb);

        Token::Integer(nb)
    }
}

fn lex_alphabetic(lexer: &mut Lexer) -> Token {
    fn continue_alphabetic(c: char) -> bool {
        match c {
            'a'..'z' | 'A'..'Z' | '0'..'9' | '_' => true,
            _ => false,
        }
    }

    let start_pos = lexer.pos - 1;

    while lexer.pos < lexer.content.len() && continue_alphabetic(lexer.current_char().unwrap()) {
        lexer.read_char();
    }

    let buf = lexer.content[start_pos..lexer.pos].to_vec();

    //dbg!(&buf);

    // TODO : replace the match with a global keyword hashmap access, if not there,it is identifier
    match buf.iter().collect::<String>().as_str() {
        "let" => Token::Let,
        "if" => Token::If,
        "match" => Token::Match,
        "with" => Token::With,
        "then" => Token::Then,
        "else" => Token::Else,
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Identifier(buf),
    }
}

fn lex_op(lexer: &mut Lexer) -> Token {
    fn continue_op(c: char) -> bool {
        Operator::is_char_op(c)
    }

    let start_pos = lexer.pos - 1;

    while lexer.pos < lexer.content.len() && continue_op(lexer.current_char().unwrap()) {
        lexer.read_char();
    }

    let buf = lexer.content[start_pos..lexer.pos].to_vec();

    //dbg!(&buf);

    let op_str = buf.iter().collect::<String>();

    match op_str.as_str() {
        "->" => Token::Arrow,
        _ => Token::Op(Operator::str_to_op(&op_str))
    }
}

pub fn lex(content: Vec<char>) -> Vec<Token> {
    dbg!(&content);
    let mut lexer = Lexer { content, pos: 0 };

    let mut tokens = vec![];

    while let Some(c) = lexer.read_char() {
        let tok: Option<Token> = match c {
            ' ' | '\t' | '\n' => None,
            '(' => Some(Token::ParenOpen),
            ')' => Some(Token::ParenClose),
            ':' => Some(Token::Colon),
            ';' => {
                match lexer.read_char(){
                    Some(';') => Some(Token::EndOfExpr),
                    _ => panic!("Not complete \";;\" token"),
                }
            },
            '|' => Some(Token::Pipe),
            op_char if Operator::is_char_op(op_char) => Some(lex_op(&mut lexer)),
            '0'..='9' => Some(lex_nb(&mut lexer)),
            'a'..='z' | 'A'..='Z' | '_' => Some(lex_alphabetic(&mut lexer)),
            _ => panic!("ERROR : unexpected char {}", c),
        };
        if let Some(t) = tok {
            tokens.push(t);
        }
    }

    dbg!(&tokens);

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_simple() {
        let input = "let a = 2 ;;".to_string().chars().collect();
        let result = lex(input);
        let expected = vec![Token::Let, Token::Identifier(vec!['a']), Token::Op(Operator::Equal), Token::Integer(2), Token::EndOfExpr];
        assert_eq!(result, expected);
    }
}