use core::panic;
use std::vec;

use enum_tags::Tag;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Mult,
    Div,
}

impl Operator {
    // TODO : pass char slice to support multi chars op
    pub fn char_to_op(c: char) -> Operator {
        match c {
            '+' => Operator::Plus,
            '-' => Operator::Minus,
            '*' => Operator::Mult,
            '/' => Operator::Div,
            _ => unreachable!(),
        }
    }
}

// TODO : replace all the Vec<char> with slices
#[derive(Debug, Clone, Tag)]
pub enum Token {
    Let,
    Equal,
    Identifier(Vec<char>),
    Op(Operator),
    Number(i64),
    ParenOpen,
    ParenClose,
    EndOfExpr,
}

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

    fn continue_nb(c: char) -> bool {
        match c {
            '0'..'9' => true,
            _ => false,
        }
    }

    let start_pos = lexer.pos - 1;

    while lexer.pos < lexer.content.len() && continue_nb(lexer.current_char().unwrap()) {
        lexer.read_char();
    }

    let buf = lexer.content[start_pos..lexer.pos].to_vec();

    //dbg!(&buf);

    let str = buf.iter().collect::<String>();

    let nb = str::parse::<i64>(str.as_str())
        .unwrap_or_else(|err| panic!("Failed when parsing nb {} : {}", str, err));

    //dbg!(nb);

    Token::Number(nb)
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

    match buf.iter().collect::<String>().as_str() {
        "let" => Token::Let,
        _ => Token::Identifier(buf),
    }
}

// TODO : replace String with &str ?
pub fn lex(content: Vec<char>) -> Vec<Token> {
    dbg!(&content);
    let mut lexer = Lexer { content, pos: 0 };

    let mut tokens = vec![];

    while let Some(c) = lexer.read_char() {
        let tok: Option<Token> = match c {
            ' ' | '\t' | '\n' => None,
            '=' => Some(Token::Equal),
            '(' => Some(Token::ParenOpen),
            ')' => Some(Token::ParenClose),
            ';' => {
                match lexer.read_char(){
                    Some(';') => Some(Token::EndOfExpr),
                    _ => panic!("Not complete \";;\" token"),
                }
            },
            '+' | '-' | '*' | '/' => Some(Token::Op(Operator::char_to_op(c))),
            '0'..'9' => Some(lex_nb(&mut lexer)),
            'a'..'z' | 'A'..'Z' | '_' => Some(lex_alphabetic(&mut lexer)),
            _ => panic!("ERROR : unexpected char {}", c),
        };
        if let Some(t) = tok {
            tokens.push(t);
        }
    }

    dbg!(&tokens);

    tokens
}
