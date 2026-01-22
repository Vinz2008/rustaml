use std::ops::Range;

use crate::{ast::Type, debug_println, rustaml::RustamlContext, types_debug::PrintTypedContext};

use debug_with_context::DebugWithContext;
use enum_tags::Tag;
use macro_static_str_to_char::str_to_char;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
#[debug_context(PrintTypedContext)]
#[repr(u8)] // use this to be sure that there will be no problem with indexing in the precedence map
pub(crate) enum Operator {
    Plus,
    Minus,
    Mult,
    Div,
    Rem,
    PlusFloat,
    MinusFloat,
    MultFloat,
    DivFloat,
    RemFloat,
    IsEqual,
    IsNotEqual,
    SuperiorOrEqual,
    InferiorOrEqual,
    Superior,
    Inferior,
    And, // &&
    Or, // ||
    Not, // !
    StrAppend, // ^
    ListAppend, // ::
    ListMerge, // @
}

impl Operator {
    pub(crate) const OPERATORS: [&'static str; 22] = ["+", "-", "*", "/", "%", "+.", "-.", "*.", "/.", "%.", "==", "!=", ">=", "<=", ">", "<", "&&", "||", "^", "::", "!", "@"];
    pub(crate) const OPERATORS_COUNT : usize = Operator::OPERATORS.len();
    pub(crate) fn get_type(&self) -> Type {
        match self {
            Self::Plus | Self::Minus | Self::Mult | Self::Div | Self::Rem => Type::Integer,
            Self::PlusFloat | Self::MinusFloat | Self::MultFloat | Self::DivFloat | Self::RemFloat => Type::Float,
            Self::IsEqual | Self::IsNotEqual | Self::SuperiorOrEqual | Self::InferiorOrEqual | Self::Superior | Self::Inferior | Self::Or | Self::And => Type::Bool,
            Self::StrAppend => Type::Str,
            Self::ListAppend | Self::ListMerge => Type::List(Box::new(Type::Any)),
            Self::Not => unreachable!(),
        }
    }

    fn is_char_op(c : char) -> bool {
        matches!(c, '+' | '-' | '*' | '/' | '%' | '=' | '<' | '>' | '^' | ':' | '!' | '.' | '&' | '|' | '@')
    }

    pub(crate) fn str_to_op(s: &str, range : &Range<usize>) -> Result<Operator, LexerErr> {
        let op = match s {
            "+" => Operator::Plus,
            "-" => Operator::Minus,
            "*" => Operator::Mult,
            "/" => Operator::Div,
            "%" => Operator::Rem,
            "+." => Operator::PlusFloat,
            "-." => Operator::MinusFloat,
            "*." => Operator::MultFloat,
            "/." => Operator::DivFloat,
            "%." => Operator::RemFloat,
            "==" => Operator::IsEqual,
            "!=" => Operator::IsNotEqual,
            ">=" => Operator::SuperiorOrEqual,
            "<=" => Operator::InferiorOrEqual,
            ">" => Operator::Superior,
            "<" => Operator::Inferior,
            "^" => Operator::StrAppend,
            "::" => Operator::ListAppend,
            "&&" => Operator::And,
            "||" => Operator::Or,
            "!" => Operator::Not,
            "@" => Operator::ListMerge,
            _ => return Err(LexerErr::new(LexerErrData::InvalidOp(s.to_owned()), range.clone())),
        };
        Ok(op)
    }
}

// TODO : replace all the Box<[char]> with borrowed slices
#[derive(Debug, Clone, Tag, PartialEq)]
pub(crate) enum TokenData {
    Identifier(Box<[char]>),
    String(Box<[char]>),
    Char(char),
    Op(Operator),
    Integer(i128),
    Float(f64),
    Let,
    If,
    Then,
    Else,
    Match,
    With,
    In,
    Import,
    Function,
    Extern,
    Cast,
    Type,
    True,
    False,
    Equal,
    ParenOpen,
    ParenClose,
    ArrayOpen,
    ArrayClose,
    Colon, // :
    Comma, // ,
    Dot, // .
    Apostrophe, // '
    Arrow, // ->
    Pipe, // |
    EndOfExpr, // ;;
    Range(bool), // .., bool is if inclusive
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token {
    pub(crate) tok_data : TokenData,
    pub(crate) range : Range<usize>,
}

impl Token {
    pub(crate) fn new(tok_data : TokenData, range : Range<usize>) -> Token {
        Token { tok_data, range }
    }
}


struct Lexer {
    content: Vec<char>,
    pos: usize,
}

#[derive(Debug)]
pub(crate) enum NbTypeError {
    Float,
    Integer,
}

#[derive(Debug, Tag)]
pub(crate) enum LexerErrData {
    NumberParsingFailure(Box<[char]>, NbTypeError),
    InvalidOp(String),
    UnexpectedChar(char),
    UnexpectedEOF,
    NotCompleteEndOfExpr,
}


#[derive(Debug)]
pub(crate) struct LexerErr {
    pub(crate) lexer_err_data : Box<LexerErrData>,
    pub(crate) range : Range<usize>,
}

impl LexerErr {
    fn new(lexer_err_data : LexerErrData, range: Range<usize>) -> LexerErr {
        LexerErr {
            lexer_err_data: Box::new(lexer_err_data),
            range
        }
    }
}


impl Lexer {
    fn has_char_left(&self) -> bool {
        self.pos < self.content.len()
    }

    /*pub(crate) fn peek(&self) -> Option<char> {
        if self.pos + 1 >= self.content.len() {
            return None;
        }
        Some(self.content[self.pos + 1])
    }*/

    pub(crate) fn current_char(&self) -> Option<char> {
        if self.pos >= self.content.len() {
            return None;
        }
        Some(self.content[self.pos])
    }

    pub(crate) fn read_char(&mut self) -> Option<char> {
        if !self.has_char_left() {
            return None;
        }
        let c = self.content[self.pos];
        self.pos += 1;
        Some(c)
    }
    
    // not advised to use very often
    pub(crate) fn go_back_pos(&mut self, count : usize){
        self.pos -= count;
    }
}



fn lex_nb(lexer: &mut Lexer) -> Result<Token, LexerErr> {
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

    let buf = &lexer.content[start_pos..lexer.pos];
    let range = start_pos..lexer.pos;

    //dbg!(&buf);

    let str = buf.iter().collect::<String>();

    if is_float {
        let nb = str::parse::<f64>(str.as_str());
        let nb = match nb {
            Ok(n) => n,
            Err(_) => return Err(LexerErr::new(LexerErrData::NumberParsingFailure(buf.to_vec().into_boxed_slice(), NbTypeError::Float), range)),
        };

        //dbg!(nb);

        Ok(Token::new(TokenData::Float(nb), range))
    } else {

        let nb = str::parse::<i128>(str.as_str());

        let nb = match nb {
            Ok(n) => n,
            Err(_) => return Err(LexerErr::new(LexerErrData::NumberParsingFailure(buf.to_vec().into_boxed_slice(), NbTypeError::Integer), range)),
        };

        //dbg!(nb);

        Ok(Token::new(TokenData::Integer(nb), range))
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

    let buf = &lexer.content[start_pos..lexer.pos];

    //dbg!(&buf);

    let range = start_pos..lexer.pos;

    // TODO : use the str_to_char! macro in other places
    let tok_data = match buf {
        str_to_char!("let") => TokenData::Let,
        str_to_char!("if") => TokenData::If,
        str_to_char!("match") => TokenData::Match,
        str_to_char!("with") => TokenData::With,
        str_to_char!("then") => TokenData::Then,
        str_to_char!("else") => TokenData::Else,
        str_to_char!("in") => TokenData::In,
        str_to_char!("import") => TokenData::Import,
        str_to_char!("function") => TokenData::Function,
        str_to_char!("extern") => TokenData::Extern,
        str_to_char!("cast") => TokenData::Cast,
        str_to_char!("type") => TokenData::Type,
        str_to_char!("true") => TokenData::True,
        str_to_char!("false") => TokenData::False,
        _ => TokenData::Identifier(buf.to_vec().into_boxed_slice()),
    };

    Token::new(tok_data, range)
}

fn handle_comment(lexer: &mut Lexer){
    while let Some(c) = lexer.read_char() && c != '\n' {}
}

// optional because it can be a comment
fn lex_op(lexer: &mut Lexer) -> Result<Option<Token>, LexerErr> {

    let start_pos = lexer.pos - 1;

    while lexer.pos < lexer.content.len() && Operator::is_char_op(lexer.current_char().unwrap()) {
        lexer.read_char();
    }

    let buf = &lexer.content[start_pos..lexer.pos];

    //dbg!(&buf);

    let range = start_pos..lexer.pos;

    let op_str = buf.iter().collect::<String>();

    let tok_data = match op_str.as_str() {
        "->" => TokenData::Arrow,
        "=" => TokenData::Equal,
        "//" => { 
            handle_comment(lexer);
            return Ok(None)
        },
        "|" => TokenData::Pipe,
        ":" => TokenData::Colon,
        ".." => TokenData::Range(false),
        "=.." => TokenData::Range(true),
        "." => TokenData::Dot,
        _ => TokenData::Op(Operator::str_to_op(&op_str, &range)?)
    };

    Ok(Some(Token::new(tok_data, range)))


}

fn lex_string(lexer: &mut Lexer) -> Result<Token, LexerErr> {
    let start_pos = lexer.pos-1;
    let mut buf = Vec::new();
    while let Some(c) = lexer.current_char() && c != '\"' {
        let c = lexer.read_char().unwrap();
        buf.push(c);
    }

    let range = start_pos..lexer.pos+1;
    
    match lexer.read_char() {
        Some('\"') => {},
        Some(_) => unreachable!(),
        None => return Err(LexerErr::new(LexerErrData::UnexpectedEOF, lexer.pos..lexer.pos))
    }
    Ok(Token::new(TokenData::String(buf.into_boxed_slice()), range))
}

pub(crate) fn lex(content: Vec<char>, is_debug_print : bool) -> Result<Vec<Token>, LexerErr> {
    //dbg!(&content);
    let mut lexer = Lexer { content, pos: 0 };

    let mut tokens = vec![];

    

    while let Some(c) = lexer.read_char() {
        let range = lexer.pos-1..lexer.pos;
        let tok: Option<Token> = match c {
            ' ' | '\t' | '\n' => None,
            '(' => Some(Token::new(TokenData::ParenOpen, range)),
            ')' => Some(Token::new(TokenData::ParenClose, range)),
            '[' => Some(Token::new(TokenData::ArrayOpen, range)),
            ']' => Some(Token::new(TokenData::ArrayClose, range)),
            ',' => Some(Token::new(TokenData::Comma, range)),
            ';' => {
                match lexer.read_char(){
                    Some(';') => Some(Token::new(TokenData::EndOfExpr, lexer.pos-2..lexer.pos-1)),
                    _ => return Err(LexerErr::new(LexerErrData::NotCompleteEndOfExpr, lexer.pos-1..lexer.pos-1)),
                }
            },
            '\'' => {
                let char_c = lexer.read_char();
                if let Some(c) = char_c && let Some('\'') = lexer.read_char() {
                    Some(Token::new(TokenData::Char(c), lexer.pos-3..lexer.pos-1))
                } else {
                    lexer.go_back_pos(lexer.pos-range.end);
                    Some(Token::new(TokenData::Apostrophe, range))
                }
            }
            '\"' => Some(lex_string(&mut lexer)?),
            op_char if Operator::is_char_op(op_char) => lex_op(&mut lexer)?,
            '0'..='9' => Some(lex_nb(&mut lexer)?),
            'a'..='z' | 'A'..='Z' | '_' => Some(lex_alphabetic(&mut lexer)),
            c => return Err(LexerErr::new(LexerErrData::UnexpectedChar(c), range)),
        };
        if let Some(t) = tok {
            tokens.push(t);
        }
    }

    //dbg!(&tokens);
    debug_println!(is_debug_print, "tokens = {:#?}", tokens);

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_simple() {
        let input = "let a = 2 ;;".to_string().chars().collect();
        let result = lex(input, false).unwrap().into_iter().map(|t| t.tok_data).collect::<Vec<_>>();
        let expected = vec![TokenData::Let, TokenData::Identifier(vec!['a'].into()), TokenData::Equal, TokenData::Integer(2), TokenData::EndOfExpr];
        assert_eq!(result, expected);
    }
}