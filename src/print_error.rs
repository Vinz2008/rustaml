use std::{ops::Range, path::Path};
use levenshtein::levenshtein;

use crate::{ast::{ParserErr, ParserErrData}, lexer::{LexerErr, Operator, TokenData, TokenDataTag}};
use crate::lexer::LexerErrData;

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use enum_tags::TaggedEnum;

const PARSER_ERROR_OFFSET : u32 = 100;

#[derive(Default, Clone)]
struct ErrorBasicInfos<'a> {
    error_nb : u32,
    range : Range<usize>,
    filename : &'a str,
    content : &'a str
}


#[derive(Default)]
struct ErrorPrint<'a> {
    error_basic_infos : ErrorBasicInfos<'a>,
    message : String,
    label : Option<Label<(&'a str, std::ops::Range<usize>)>>,
    note : Option<String>,
}

fn print_error(err : ErrorPrint){
    let mut report = Report::build(ReportKind::Error, (err.error_basic_infos.filename, err.error_basic_infos.range.clone()))
    .with_code(err.error_basic_infos.error_nb)
    .with_message(err.message);
    
    if let Some(label) = err.label {
        report = report.with_label(label);
    }

    if let Some(note) = err.note {
        report = report.with_note(note);
    }

    report.finish()
    .print((err.error_basic_infos.filename, Source::from(err.error_basic_infos.content))).unwrap();
}



fn print_number_parsing_failure(error_basic_infos : ErrorBasicInfos, buf  : Vec<char>) -> ErrorPrint{
    ErrorPrint {
        error_basic_infos,
        message: format!("Failure when parsing number \"{:?}\"", buf),

        ..Default::default()
    }
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

fn print_invalid_op_error(error_basic_infos : ErrorBasicInfos, op : String) -> ErrorPrint {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let label = Some(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This operator doesn't exist").with_color(a));

    ErrorPrint {
        error_basic_infos,
        message: format!("Invalid operator \"{}\"", op),
        label,
        note: Some(format!("maybe you meant {}", nearest_op(&op))),
    }
}

fn print_unexpected_char_error(error_basic_infos : ErrorBasicInfos, c : char) -> ErrorPrint {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let label = Some(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This character shouldn't be there").with_color(a));

    ErrorPrint {
        error_basic_infos,
        message: format!("Unexpected char \'{}\'", c),
        label,
        ..Default::default()
    }
}

pub fn print_lexer_error(lexer_error : LexerErr, filename : &Path, content : &str) {
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

    let error_print = match *lexer_error.lexer_err_data {
        LexerErrData::NumberParsingFailure(b) => print_number_parsing_failure(error_basic_infos, *b),
        LexerErrData::InvalidOp(s) => print_invalid_op_error(error_basic_infos, *s),
        LexerErrData::UnexpectedEOF => print_unexpected_eof_error(error_basic_infos),
        LexerErrData::UnexpectedChar(c) => print_unexpected_char_error(error_basic_infos, c),
    };

    print_error(error_print);
}

// TODO : refactor this code ? (make only one function ?)

fn print_unexpected_eof_error(error_basic_infos : ErrorBasicInfos) -> ErrorPrint {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let label = Some(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("There is here a EOF that should not be here").with_color(a));

    ErrorPrint {
        error_basic_infos,
        message: "Unexpected end of file".to_owned(),
        label,
        ..Default::default()
    }
}

fn print_wrong_tok_error(error_basic_infos : ErrorBasicInfos, expected_tok : TokenDataTag, got_tok : TokenData) -> ErrorPrint {
    let mut colors = ColorGenerator::new();
    let a = colors.next();

    let label = Some(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This is the wrong token").with_color(a));

    ErrorPrint {
        error_basic_infos,
        message: format!("Wrong Token : expected {:?} but got {:?}", expected_tok, got_tok),
        label,
        ..Default::default()
    }
}

fn print_unexpected_tok_error(error_basic_infos : ErrorBasicInfos, tok : TokenData) -> ErrorPrint {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let label = Some(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This is the wrong token").with_color(a));

    ErrorPrint {
        error_basic_infos,
        message: format!("Unexpected Token {:?}", tok),
        label,
        ..Default::default()
    }
}

fn print_type_inference_error<'a>(error_basic_infos : ErrorBasicInfos<'a>, arg_name : &str) -> ErrorPrint<'a> {
    let mut colors = ColorGenerator::new();
    let a = colors.next();

    let label = Some(Label::new((error_basic_infos.filename, error_basic_infos.range.clone())).with_message("This argument's type couldn't be deduced from the body of the function").with_color(a));

    ErrorPrint {
        error_basic_infos,
        message: format!("Type inference error with the argument {:?}", arg_name),
        label,
        note: Some("Either add a type annotation, or change the body to disambiguate".to_owned()),
    }
}

fn print_unknown_type_annotation(error_basic_infos : ErrorBasicInfos) -> ErrorPrint {
    todo!()
}

pub fn print_parser_error(parser_error : ParserErr, filename : &Path, content : &str) {
    
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

    let error_print = match *parser_error.parser_err_data {
        ParserErrData::UnexpectedEOF => print_unexpected_eof_error(error_basic_infos),
        ParserErrData::WrongTok { expected_tok, got_tok } => print_wrong_tok_error(error_basic_infos, expected_tok, got_tok),
        ParserErrData::UnexpectedTok {tok } => print_unexpected_tok_error(error_basic_infos, tok),
        ParserErrData::TypeInferenceErr { arg_name } => print_type_inference_error(error_basic_infos, &arg_name),
        ParserErrData::UnknownTypeAnnotation {  } => print_unknown_type_annotation(error_basic_infos), // TODO
        ParserErrData::NotFunctionTypeInAnnotationLet { name } => todo!(),
    };

    print_error(error_print);
}