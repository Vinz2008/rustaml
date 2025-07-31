use std::{ops::Range, path::Path};
use levenshtein::levenshtein;

use crate::{ast::{ParserErr, ParserErrData}, lexer::{LexerErr, Operator, TokenData, TokenDataTag}};
use crate::lexer::LexerErrData;

use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};
use enum_tags::TaggedEnum;

const PARSER_ERROR_OFFSET : u32 = 100;

#[derive(Default, Clone)]
struct ErrorBasicInfos<'a> {
    error_nb : u32,
    range : Range<usize>,
    filename : &'a str,
    content : &'a str,
    color: Color,
}


#[derive(Default)]
struct ErrorPrint<'a> {
    error_basic_infos : ErrorBasicInfos<'a>,
    message : String,
    label : Option<&'a str>,
    note : Option<String>,
}

fn print_error(err : ErrorPrint){
    let mut report = Report::build(ReportKind::Error, (err.error_basic_infos.filename, err.error_basic_infos.range.clone()))
    .with_code(err.error_basic_infos.error_nb)
    .with_message(err.message);
    
    if let Some(label) = err.label {
        report = report.with_label(Label::new((err.error_basic_infos.filename, err.error_basic_infos.range.clone())).with_message(label).with_color(err.error_basic_infos.color));
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
    ErrorPrint {
        error_basic_infos,
        message: format!("Invalid operator \"{}\"", op),
        label: Some("This operator doesn't exist"),
        note: Some(format!("maybe you meant {}", nearest_op(&op))),
    }
}

fn print_unexpected_char_error(error_basic_infos : ErrorBasicInfos, c : char) -> ErrorPrint {
    ErrorPrint {
        error_basic_infos,
        message: format!("Unexpected char \'{}\'", c),
        label: Some("This character shouldn't be there"),
        ..Default::default()
    }
}

fn print_not_complete_end_of_expr(error_basic_infos : ErrorBasicInfos) -> ErrorPrint {
    ErrorPrint { 
        error_basic_infos, 
        message: "Not complete end of expression token \";;\"".to_owned(),
        label: Some("This is where the character is missing"),
        note: Some("Just add a ';' to fix this error".to_owned()),
    }
}

pub fn print_lexer_error(lexer_error : LexerErr, filename : &Path, content : &str) {
    // println!("Parsing error : {:?}", parser_error);

    let error_nb = lexer_error.lexer_err_data.tag() as u32;
    let range = lexer_error.range;
    let filename_str = filename.to_str().unwrap();

    let mut colors = ColorGenerator::new();
    let color = colors.next();

    let error_basic_infos = ErrorBasicInfos {
        error_nb,
        range,
        filename: filename_str,
        content,
        color
    };

    let error_print = match *lexer_error.lexer_err_data {
        LexerErrData::NumberParsingFailure(b) => print_number_parsing_failure(error_basic_infos, b),
        LexerErrData::InvalidOp(s) => print_invalid_op_error(error_basic_infos, s),
        LexerErrData::UnexpectedEOF => print_unexpected_eof_error(error_basic_infos),
        LexerErrData::UnexpectedChar(c) => print_unexpected_char_error(error_basic_infos, c),
        LexerErrData::NotCompleteEndOfExpr => print_not_complete_end_of_expr(error_basic_infos),
    };

    print_error(error_print);
}

// TODO : refactor this code ? (make only one function ?)

fn print_unexpected_eof_error(error_basic_infos : ErrorBasicInfos) -> ErrorPrint {
    ErrorPrint {
        error_basic_infos,
        message: "Unexpected end of file".to_owned(),
        label: Some("There is here a EOF that should not be here"),
        ..Default::default()
    }
}

fn print_wrong_tok_error(error_basic_infos : ErrorBasicInfos, expected_tok : TokenDataTag, got_tok : TokenData) -> ErrorPrint {
    ErrorPrint {
        error_basic_infos,
        message: format!("Wrong Token : expected {:?} but got {:?}", expected_tok, got_tok),
        label: Some("This is the wrong token"),
        ..Default::default()
    }
}

fn print_unexpected_tok_error(error_basic_infos : ErrorBasicInfos, tok : TokenData) -> ErrorPrint {
    ErrorPrint {
        error_basic_infos,
        message: format!("Unexpected Token {:?}", tok),
        label: Some("This is the wrong token"),
        ..Default::default()
    }
}

fn print_type_inference_error<'a>(error_basic_infos : ErrorBasicInfos<'a>, arg_name : &str) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("Type inference error with the argument {:?}", arg_name),
        label: Some("This argument's type couldn't be deduced from the body of the function"),
        note: Some("Either add a type annotation, or change the body to disambiguate".to_owned()),
    }
}

fn print_unknown_type_annotation<'a>(error_basic_infos : ErrorBasicInfos<'a>, type_str : &str) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("Unknown type annotation : {}", type_str),
        ..Default::default()
    }
}

fn print_not_function_type_in_let<'a>(error_basic_infos : ErrorBasicInfos<'a>, name : &str) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("The type for {} is not a function type", name),
        ..Default::default()
    }
}

// TODO : add most near var (need to add another string in the err enum and find it when constructing the error)
fn print_unknown_var<'a>(error_basic_infos : ErrorBasicInfos<'a>, name: &str) -> ErrorPrint<'a> {
    ErrorPrint { 
        error_basic_infos,
        message: format!("Unknown var {}", name),
        ..Default::default()  
    }
}

pub fn print_parser_error(parser_error : ParserErr, filename : &Path, content : &str) {
    
    // println!("Parsing error : {:?}", parser_error);

    let error_nb =  PARSER_ERROR_OFFSET + parser_error.parser_err_data.tag() as u32;
    let range = parser_error.range;
    let filename_str = filename.to_str().unwrap();

    let mut colors = ColorGenerator::new();
    let color = colors.next();

    let error_basic_infos = ErrorBasicInfos {
        error_nb,
        range,
        filename: filename_str,
        content,
        color
    };

    let error_print = match *parser_error.parser_err_data {
        ParserErrData::UnexpectedEOF => print_unexpected_eof_error(error_basic_infos),
        ParserErrData::WrongTok { expected_tok, got_tok } => print_wrong_tok_error(error_basic_infos, expected_tok, got_tok),
        ParserErrData::UnexpectedTok {tok } => print_unexpected_tok_error(error_basic_infos, tok),
        ParserErrData::UnknownVar { name } => print_unknown_var(error_basic_infos, &name),
        ParserErrData::TypeInferenceErr { arg_name } => print_type_inference_error(error_basic_infos, &arg_name),
        ParserErrData::UnknownTypeAnnotation { type_str } => print_unknown_type_annotation(error_basic_infos, &type_str), // TODO
        ParserErrData::NotFunctionTypeInAnnotationLet { function_name: name } => print_not_function_type_in_let(error_basic_infos, &name),
    };

    //assert!((error_print.note.is_some() && error_print.label.is_some()) || (error_print.note.is_none() && error_print.label.is_none()));

    print_error(error_print);
}