use std::ops::Range;

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
        matches!(c, '+' | '-' | '*' | '/' | '=' | '<' | '>')
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
pub enum TokenData {
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
    In,
    True,
    False,
    ParenOpen,
    ParenClose,
    Colon,
    Arrow, // ->
    Pipe, // |
    EndOfExpr, // ;;
    Range, // ..
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub tok_data : TokenData,
    pub range : Range<usize>,
}

impl Token {
    pub fn new(tok_data : TokenData, range : Range<usize>) -> Token {
        Token { tok_data, range }
    }
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
    
    // not advised to use very often
    pub fn go_back_pos(&mut self, count : usize){
        self.pos -= count;
    }
}



fn lex_nb(lexer: &mut Lexer) -> Token {
    // TODO : floats

    fn continue_nb(c: char, is_float : &mut bool, is_range : &mut bool) -> bool {
        match c {
            '0'..='9' => true,
            '.' => {
                if *is_float {
                    *is_range = true;
                }
                *is_float = true;
                true
            },
            _ => false,
        }
    }

    let mut is_float = false;
    let mut is_range = false;
    let start_pos = lexer.pos - 1;

    while lexer.pos < lexer.content.len() && continue_nb(lexer.current_char().unwrap(), &mut is_float, &mut is_range) {
        if is_range {
            // if is range, go back before the range and break
            lexer.go_back_pos(1);
            // more complicated to know if float (should not assume int) because there could still be some pattern in the future like "1.0..2.0"
            is_float = lexer.content[start_pos..lexer.pos].contains(&'.');
            break;
        }
        lexer.read_char();
    }

    let buf = lexer.content[start_pos..lexer.pos].to_vec();
    let range = start_pos..lexer.pos;

    //dbg!(&buf);

    let str = buf.iter().collect::<String>();

    if is_float {
        let nb = str::parse::<f64>(str.as_str())
            .unwrap_or_else(|err| panic!("Failed when parsing nb {} : {}", str, err));

        //dbg!(nb);

        Token::new(TokenData::Float(nb), range) // TODO
    } else {

        let nb = str::parse::<i64>(str.as_str())
            .unwrap_or_else(|err| panic!("Failed when parsing nb {} : {}", str, err));

        //dbg!(nb);

        Token::new(TokenData::Integer(nb), range) // TODO
    }
}

fn lex_alphabetic(lexer: &mut Lexer) -> Token {
    fn continue_alphabetic(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
    }

    let start_pos = lexer.pos - 1;

    while lexer.pos < lexer.content.len() && continue_alphabetic(lexer.current_char().unwrap()) {
        lexer.read_char();
    }

    let buf = lexer.content[start_pos..lexer.pos].to_vec();

    //dbg!(&buf);

    let range = start_pos..lexer.pos-1; // TODO

    // TODO : replace the match with a global keyword hashmap access, if not there,it is identifier
    let tok_data = match buf.iter().collect::<String>().as_str() {
        "let" => TokenData::Let,
        "if" => TokenData::If,
        "match" => TokenData::Match,
        "with" => TokenData::With,
        "then" => TokenData::Then,
        "else" => TokenData::Else,
        "in" => TokenData::In,
        "true" => TokenData::True,
        "false" => TokenData::False,
        _ => TokenData::Identifier(buf),
    };

    Token::new(tok_data, range)
}

fn handle_comment(lexer: &mut Lexer){
    while let Some(c) = lexer.read_char() && c != '\n' {}
}

// optional because it can be a comment
fn lex_op(lexer: &mut Lexer) -> Option<Token> {
    fn continue_op(c: char) -> bool {
        Operator::is_char_op(c)
    }

    let start_pos = lexer.pos - 1;

    while lexer.pos < lexer.content.len() && continue_op(lexer.current_char().unwrap()) {
        lexer.read_char();
    }

    let buf = lexer.content[start_pos..lexer.pos].to_vec();

    //dbg!(&buf);

    let range = start_pos..lexer.pos; // TODO

    let op_str = buf.iter().collect::<String>();

    let tok_data = match op_str.as_str() {
        "->" => TokenData::Arrow,
        "//" => { 
            handle_comment(lexer);
            return None
        },
        _ => TokenData::Op(Operator::str_to_op(&op_str))
    };

    Some(Token::new(tok_data, range))


}

pub fn lex(content: Vec<char>) -> Vec<Token> {
    //dbg!(&content);
    let mut lexer = Lexer { content, pos: 0 };

    let mut tokens = vec![];



    while let Some(c) = lexer.read_char() {
        let tok: Option<Token> = match c {
            ' ' | '\t' | '\n' => None,
            '(' => Some(Token::new(TokenData::ParenOpen, lexer.pos-1..lexer.pos-1 )),
            ')' => Some(Token::new(TokenData::ParenClose, lexer.pos-1..lexer.pos-1)),
            ':' => Some(Token::new(TokenData::Colon, lexer.pos-1..lexer.pos-1)),
            '.' => {
                match lexer.read_char() {
                    Some('.') => Some(Token::new(TokenData::Range, lexer.pos-2..lexer.pos-1)),
                    _ => panic!("Not complete \"..\" token"),
                }
            },
            ';' => {
                match lexer.read_char(){
                    Some(';') => Some(Token::new(TokenData::EndOfExpr, lexer.pos-2..lexer.pos-1)),
                    _ => panic!("Not complete \";;\" token"),
                }
            },
            '|' => Some(Token::new(TokenData::Pipe, lexer.pos-1..lexer.pos-1)),
            op_char if Operator::is_char_op(op_char) => lex_op(&mut lexer),
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
        let result = lex(input).into_iter().map(|t| t.tok_data).collect::<Vec<_>>();
        let expected = vec![TokenData::Let, TokenData::Identifier(vec!['a']), TokenData::Op(Operator::Equal), TokenData::Integer(2), TokenData::EndOfExpr];
        assert_eq!(result, expected);
    }
}