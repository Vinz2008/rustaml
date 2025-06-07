use std::{hint::black_box, ops::Range, path::Path, process::ExitCode};

use crate::{ast::{ParserErr, ParserErrData}, lexer::{Token, TokenData, TokenDataTag}};

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use enum_tags::TaggedEnum;


// TODO : refactor this code ? (make only one function ?)

fn print_unexpected_eof_error(error_nb : u8, range : Range<usize>, filename : &str, content : &str){
    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message("Unexpected end of file")
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn print_wrong_tok_error(error_nb : u8, range : Range<usize>, filename : &str, content : &str, expected_tok : TokenDataTag, got_tok : TokenData){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (filename, range.clone()))
    .with_code(error_nb)
    .with_message(format!("Wrong Token : expected {:?} but got {:?}", expected_tok, got_tok))
    .with_label(Label::new((filename, range.clone())).with_message("This is the wrong token").with_color(a))
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn print_unexpected_tok_error(error_nb : u8, range : Range<usize>, filename : &str, content : &str, tok : TokenData){
    black_box(tok); // TODO  : for linting purposes only, remove this after writing the proper error messages cotnaining the token
    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message("Unexpected end of file")
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

pub fn print_parser_error(parser_error : ParserErr, filename : &Path, content : &str) -> ExitCode {
    
    // println!("Parsing error : {:?}", parser_error);

    let error_nb = parser_error.parser_err_data.tag() as u8;

    let range = parser_error.range;

    let filename_str = filename.to_str().unwrap();

    match *parser_error.parser_err_data {
        ParserErrData::UnexpectedEOF => print_unexpected_eof_error(error_nb, range, filename_str, content),
        ParserErrData::WrongTok { expected_tok, got_tok } => print_wrong_tok_error(error_nb, range, filename_str, content, *expected_tok, *got_tok),
        ParserErrData::UnexpectedTok {tok } => print_unexpected_tok_error(error_nb, range, filename_str, content, *tok),
    };

    ExitCode::FAILURE
}