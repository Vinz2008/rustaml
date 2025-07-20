use std::{ops::Range, path::Path, process::ExitCode};
use levenshtein::levenshtein;

use crate::{ast::{ParserErr, ParserErrData}, lexer::{LexerErr, Operator, TokenData, TokenDataTag}};
use crate::lexer::LexerErrData;

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use enum_tags::TaggedEnum;

const PARSER_ERROR_OFFSET : u32 = 100;

struct ErrorBasicInfos<'a> {
    error_nb : u32,
    range : Range<usize>,
    filename : &'a str,
    content : &'a str
}

fn print_number_parsing_failure(error_basic_infos : ErrorBasicInfos, buf  : Vec<char>){
    Report::build(ReportKind::Error, (error_basic_infos.filename, error_basic_infos.range))
    .with_code(error_basic_infos.error_nb)
    .with_message(format!("Failure when parsing number \"{:?}\"", buf))
    .finish()
    .print((error_basic_infos.filename, Source::from(error_basic_infos.content))).unwrap();
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

fn print_invalid_op_error(error_basic_infos : ErrorBasicInfos, op : String){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (error_basic_infos.filename, error_basic_infos.range.clone()))
    .with_code(error_basic_infos.error_nb)
    .with_message(format!("Invalid operator \"{}\"", op))
    .with_label(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This operator doesn't exist").with_color(a))
    .with_note(format!("maybe you meant {}", nearest_op(&op)))
    .finish()
    .print((error_basic_infos.filename, Source::from(error_basic_infos.content))).unwrap();
}

fn print_unexpected_char_error(error_basic_infos : ErrorBasicInfos, c : char){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (error_basic_infos.filename, error_basic_infos.range.clone()))
    .with_code(error_basic_infos.error_nb)
    .with_message(format!("Unexpected char \'{}\'", c))
    .with_label(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This character shouldn't be there").with_color(a))
    .finish()
    .print((error_basic_infos.filename, Source::from(error_basic_infos.content))).unwrap();
}

pub fn print_lexer_error(lexer_error : LexerErr, filename : &Path, content : &str) -> ExitCode {
    // println!("Parsing error : {:?}", parser_error);

    let error_nb = lexer_error.lexer_err_data.tag() as u32;
    let range = lexer_error.range;
    let filename_str = filename.to_str().unwrap();

    let error_basic_infos = ErrorBasicInfos {
        error_nb,
        range,
        filename: filename_str,
        content: content
    };

    match *lexer_error.lexer_err_data {
        LexerErrData::NumberParsingFailure(b) => print_number_parsing_failure(error_basic_infos, *b),
        LexerErrData::InvalidOp(s) => print_invalid_op_error(error_basic_infos, *s),
        LexerErrData::UnexpectedEOF => print_unexpected_eof_error(error_basic_infos),
        LexerErrData::UnexpectedChar(c) => print_unexpected_char_error(error_basic_infos, c),
    }
    

    ExitCode::FAILURE
}

// TODO : refactor this code ? (make only one function ?)

fn print_unexpected_eof_error(error_basic_infos : ErrorBasicInfos){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (error_basic_infos.filename, error_basic_infos.range.clone()))
    .with_code(error_basic_infos.error_nb)
    .with_message("Unexpected end of file")
     .with_label(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("There is here a EOF that should not be here").with_color(a))
    .finish()
    .print((error_basic_infos.filename, Source::from(error_basic_infos.content))).unwrap();
}

fn print_wrong_tok_error(error_basic_infos : ErrorBasicInfos, expected_tok : TokenDataTag, got_tok : TokenData){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (error_basic_infos.filename, error_basic_infos.range.clone()))
    .with_code(error_basic_infos.error_nb)
    .with_message(format!("Wrong Token : expected {:?} but got {:?}", expected_tok, got_tok))
    .with_label(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This is the wrong token").with_color(a))
    .finish()
    .print((error_basic_infos.filename, Source::from(error_basic_infos.content))).unwrap();
}

fn print_unexpected_tok_error(error_basic_infos : ErrorBasicInfos, tok : TokenData){
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (error_basic_infos.filename, error_basic_infos.range.clone()))
    .with_code(error_basic_infos.error_nb)
    .with_message(format!("Unexpected Token {:?}", tok))
    .with_label(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This is the wrong token").with_color(a))
    .finish()
    .print((error_basic_infos.filename, Source::from(error_basic_infos.content))).unwrap();
}

fn print_type_inference_error(error_basic_infos : ErrorBasicInfos, arg_name : &str){
let mut colors = ColorGenerator::new();
    let a = colors.next();
    Report::build(ReportKind::Error, (error_basic_infos.filename, error_basic_infos.range.clone()))
    .with_code(error_basic_infos.error_nb)
    .with_message(format!("Type inference error with the argument {:?}", arg_name))
    .with_label(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This argument's type couldn't be deduced from the body of the function").with_color(a))
    .with_note("Either add a type annotation, or change the body to disambiguate")
    .finish()
    .print((error_basic_infos.filename, Source::from(error_basic_infos.content))).unwrap();
}

fn print_unknown_type_annotation(error_basic_infos : ErrorBasicInfos){
    todo!()
}

pub fn print_parser_error(parser_error : ParserErr, filename : &Path, content : &str) -> ExitCode {
    
    // println!("Parsing error : {:?}", parser_error);

    let error_nb =  PARSER_ERROR_OFFSET + parser_error.parser_err_data.tag() as u32;
    let range = parser_error.range;
    let filename_str = filename.to_str().unwrap();

    let error_basic_infos = ErrorBasicInfos {
        error_nb,
        range,
        filename: filename_str,
        content: content
    };

    match *parser_error.parser_err_data {
        ParserErrData::UnexpectedEOF => print_unexpected_eof_error(error_basic_infos),
        ParserErrData::WrongTok { expected_tok, got_tok } => print_wrong_tok_error(error_basic_infos, expected_tok, got_tok),
        ParserErrData::UnexpectedTok {tok } => print_unexpected_tok_error(error_basic_infos, tok),
        ParserErrData::TypeInferenceErr { arg_name } => print_type_inference_error(error_basic_infos, &arg_name),
        ParserErrData::UnknownTypeAnnotation {  } => print_unknown_type_annotation(error_basic_infos), // TODO
        ParserErrData::NotFunctionTypeInAnnotationLet { name } => todo!(),
    };

    ExitCode::FAILURE
}