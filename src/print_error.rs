use std::{hint::black_box, ops::Range, path::Path, process::ExitCode};

use crate::{ast::ParserErr, lexer::TokenDataTag};

use ariadne::{Report, ReportKind, Source};
use enum_tags::TaggedEnum;


fn print_unexpected_eof_error(error_nb : u8, range : Range<usize>, filename : &str, content : &str){
    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message("Unexpected end of file")
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn print_wrong_tok_error(error_nb : u8, range : Range<usize>, filename : &str, content : &str, expected_tok : TokenDataTag, got_tok : TokenDataTag){

    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message(format!("Wrong Token : expected {:?} but got {:?}", expected_tok, got_tok))
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

fn print_unexpected_tok_error(error_nb : u8, range : Range<usize>, filename : &str, content : &str, tok : TokenDataTag){
    black_box(tok); // TODO  : for linting purposes only, remove this after writing the proper error messages cotnaining the token
    Report::build(ReportKind::Error, (filename, range))
    .with_code(error_nb)
    .with_message("Unexpected end of file")
    .finish()
    .print((filename, Source::from(content))).unwrap();
}

pub fn print_parser_error(parser_error : ParserErr, filename : &Path, range : Range<usize>, content : &str) -> ExitCode {
    // println!("Parsing error : {:?}", parser_error);

    let error_nb = parser_error.tag() as u8;

    let filename_str = filename.to_str().unwrap();

    match parser_error {
        ParserErr::UnexpectedEOF => print_unexpected_eof_error(error_nb, range, filename_str, content),
        ParserErr::WrongTok { expected_tok, got_tok } => print_wrong_tok_error(error_nb, range, filename_str, content, expected_tok, got_tok),
        ParserErr::UnexpectedTok {tok } => print_unexpected_tok_error(error_nb, range, filename_str, content, tok),
    };

    ExitCode::FAILURE
}