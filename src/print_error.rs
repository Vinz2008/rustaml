use std::{ops::Range, path::Path, process::ExitCode, usize};
use levenshtein::levenshtein;

use crate::{ast::{ParserErr, ParserErrData}, lexer::{LexerErr, Operator, TokenData, TokenDataTag}};
use crate::lexer::LexerErrData;

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use enum_tags::TaggedEnum;

const PARSER_ERROR_OFFSET : u32 = 100;



fn print_number_parsing_failure(error_nb : u8, range : Range<usize>, filename : &str, content : &str, buf  : Vec<char>){
    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message(format!("Failure when parsing number \"{:?}\"", buf))
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn nearest_op(s : &str) -> &'static str {
    let mut min_distance = usize::MAX;
    let mut nearest = Operator::OPERATORS[0]; 
    for op in Operator::OPERATORS {
        let distance = levenshtein(s, op);
        if distance < min_distance {
            min_distance = distance;
            nearest = op;
        }
    }
    nearest
}

fn print_invalid_op_error(error_nb : u8, range : Range<usize>, filename : &str, content : &str, op : String){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (filename, range.clone()))
    .with_code(error_nb)
    .with_message(format!("Invalid operator \"{}\"", op))
    .with_label(Label::new((filename, range.clone())).with_message("This operator doesn't exist").with_color(a))
    .with_note(format!("maybe you meant {}", nearest_op(&op)))
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

pub fn print_lexer_error(lexer_error : LexerErr, filename : &Path, content : &str) -> ExitCode {
    // println!("Parsing error : {:?}", parser_error);

    let error_nb = lexer_error.lexer_err_data.tag() as u8;
    let range = lexer_error.range;
    let filename_str = filename.to_str().unwrap();

    match *lexer_error.lexer_err_data {
        LexerErrData::NumberParsingFailure(b) => print_number_parsing_failure(error_nb, range, filename_str, content, *b),
        LexerErrData::InvalidOp(s) => print_invalid_op_error(error_nb, range, filename_str, content, *s),
    }
    

    ExitCode::FAILURE
}

// TODO : refactor this code ? (make only one function ?)

fn print_unexpected_eof_error(error_nb : u32, range : Range<usize>, filename : &str, content : &str){
    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb as u32)
    .with_message("Unexpected end of file")
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn print_wrong_tok_error(error_nb : u32, range : Range<usize>, filename : &str, content : &str, expected_tok : TokenDataTag, got_tok : TokenData){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (filename, range.clone()))
    .with_code(error_nb)
    .with_message(format!("Wrong Token : expected {:?} but got {:?}", expected_tok, got_tok))
    .with_label(Label::new((filename, range.clone())).with_message("This is the wrong token").with_color(a))
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn print_unexpected_tok_error(error_nb : u32, range : Range<usize>, filename : &str, content : &str, tok : TokenData){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (filename, range.clone()))
    .with_code(error_nb)
    .with_message(format!("Unexpected Token {:?}", tok))
    .with_label(Label::new((filename, range.clone())).with_message("This is the wrong token").with_color(a))
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

pub fn print_parser_error(parser_error : ParserErr, filename : &Path, content : &str) -> ExitCode {
    
    // println!("Parsing error : {:?}", parser_error);

    let error_nb =  PARSER_ERROR_OFFSET + parser_error.parser_err_data.tag() as u32;
    let range = parser_error.range;
    let filename_str = filename.to_str().unwrap();

    match *parser_error.parser_err_data {
        ParserErrData::UnexpectedEOF => print_unexpected_eof_error(error_nb, range, filename_str, content),
        ParserErrData::WrongTok { expected_tok, got_tok } => print_wrong_tok_error(error_nb, range, filename_str, content, *expected_tok, *got_tok),
        ParserErrData::UnexpectedTok {tok } => print_unexpected_tok_error(error_nb, range, filename_str, content, *tok),
    };

    ExitCode::FAILURE
}