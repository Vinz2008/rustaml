use std::{ops::Range, path::PathBuf, process::ExitCode};

use crate::{ast::ParserErr, lexer::TokenTag};

use ariadne::{Report, ReportKind, Source};
use enum_tags::TaggedEnum;


fn print_unexpected_eof_error(parser_error : ParserErr, error_nb : u8, range : Range<usize>, filename : &str, content : &str){
    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message("Unexpected end of file")
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn print_wrong_tok_error(parser_error : ParserErr, error_nb : u8, range : Range<usize>, filename : &str, content : &str, expected_tok : TokenTag, got_tok : TokenTag){

    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message(format!("Wrong Token : expected {:?} but got {:?}", expected_tok, got_tok))
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn print_unexpected_tok_error(parser_error : ParserErr, error_nb : u8, range : Range<usize>, filename : &str, content : &str){
    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message("Unexpected end of file")
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

pub fn print_parser_error(parser_error : ParserErr, filename : &PathBuf, range : Range<usize>, content : &str) -> ExitCode {
    // println!("Parsing error : {:?}", parser_error);

    let error_nb = parser_error.tag() as u8;

    let filename_str = filename.to_str().unwrap().to_owned();

    match parser_error {
        ParserErr::UnexpectedEOF => print_unexpected_eof_error(parser_error, error_nb, range, filename_str.as_str(), content),
        ParserErr::WrongTok { expected_tok, got_tok } => print_wrong_tok_error(parser_error, error_nb, range, filename_str.as_str(), content, expected_tok, got_tok),
        ParserErr::UnexpectedTok {tok } => todo!(),
    };

    ExitCode::FAILURE
}