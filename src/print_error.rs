use std::{ops::Range, path::Path};

use crate::{ast::{ParserErr, ParserErrData, Type}, check::{CheckError, CheckErrorData}, lexer::{LexerErr, NbTypeError, Operator, TokenData, TokenDataTag}, rustaml::nearest_string, types::{TypesErr, TypesErrData}};
use crate::lexer::LexerErrData;

use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};
use const_format::formatc;
use enum_tags::TaggedEnum;

const PARSER_ERROR_OFFSET : u32 = 100;
const TYPE_ERROR_OFFSET : u32 = 200;
const CHECK_ERROR_OFFSET : u32 = 300;

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

    assert!(!(err.note.is_some() && err.label.is_none()));

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



fn print_number_parsing_failure(error_basic_infos : ErrorBasicInfos, buf  : Box<[char]>, nb_type : NbTypeError) -> ErrorPrint{
    ErrorPrint {
        error_basic_infos,
        message: format!("Failure when parsing number \"{:?}\" of type {:?}", buf, nb_type),

        ..Default::default()
    }
}

fn nearest_op(s : &str) -> String {
    nearest_string(s, Operator::OPERATORS, Some(Operator::OPERATORS[0])).unwrap().to_owned()
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

pub(crate) fn print_lexer_error(lexer_error : LexerErr, filename : &Path, content : &str) {
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
        LexerErrData::NumberParsingFailure(b, nb_type) => print_number_parsing_failure(error_basic_infos, b, nb_type),
        LexerErrData::InvalidOp(s) => print_invalid_op_error(error_basic_infos, s),
        LexerErrData::UnexpectedEOF => print_unexpected_eof_error(error_basic_infos),
        LexerErrData::UnexpectedChar(c) => print_unexpected_char_error(error_basic_infos, c),
        LexerErrData::NotCompleteEndOfExpr => print_not_complete_end_of_expr(error_basic_infos),
    };

    print_error(error_print);
}

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

pub(crate) fn print_parser_error(parser_error : ParserErr, filename : &Path, content : &str) {
    
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
        ParserErrData::ImportError => return, // skip because the error has already be printen by get_ast_from_string
        ParserErrData::UnexpectedEOF => print_unexpected_eof_error(error_basic_infos),
        ParserErrData::WrongTok { expected_tok, got_tok } => print_wrong_tok_error(error_basic_infos, expected_tok, got_tok),
        ParserErrData::UnexpectedTok { tok } => print_unexpected_tok_error(error_basic_infos, tok),
        //ParserErrData::UnknownVar { name } => print_unknown_var(error_basic_infos, &name),
        //ParserErrData::TypeInferenceErr { arg_name } => print_type_inference_error(error_basic_infos, &arg_name),
        ParserErrData::UnknownTypeAnnotation { type_str } => print_unknown_type_annotation(error_basic_infos, &type_str),
        ParserErrData::NotFunctionTypeInAnnotationLet { function_name: name } => print_not_function_type_in_let(error_basic_infos, &name),
        //ParserErrData::WrongNumberOfArgs { function_name, expected_nb, got_nb } => print_wrong_number_of_args(error_basic_infos, &function_name, expected_nb, got_nb),
        //ParserErrData::WrongArgType { function_name, expected_type, got_type } => print_wrong_arg_type(error_basic_infos, &function_name, expected_type, got_type),
        //ParserErrData::MismatchedBinOpType { op, expected_type, got_type } => print_mismatched_binop_type(error_basic_infos, &op, expected_type, got_type),
    };

    //assert!((error_print.note.is_some() && error_print.label.is_some()) || (error_print.note.is_none() && error_print.label.is_none()));

    print_error(error_print);
}

fn print_function_type_expected<'a>(error_basic_infos : ErrorBasicInfos<'a>, wrong_type : Type) -> ErrorPrint<'a> {
    ErrorPrint { 
        error_basic_infos, 
        message: format!("Wrong type, expected a function type, but got {:?}", wrong_type),
        label: Some("This is where a function should be"),
        note: Some("You have probably tried to call something that is not a function".to_owned()),
    }
}

fn print_incompatible_types_error<'a>(error_basic_infos : ErrorBasicInfos<'a>, type1 : Type, type2 : Type) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("Incompatible types, {:?} and {:?} are not compatible", type1, type2),
        label: Some("This is where the types are incompatible"),
        note: Some("Check if you have ifs and else branches with incompatible types or if you are appending an element to a list of a different type".to_owned()),
    }
}

fn print_list_type_expected_error<'a>(error_basic_infos : ErrorBasicInfos<'a>, wrong_type : Type) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("Wrong type, expected a list type, but got {:?}", wrong_type),
        label: Some("This is where the type is wrong"),
        note: Some("Check if you use the :: operator or use the e :: l pattern on a value that is not a list".to_owned()),
    }
}

fn print_var_not_found_error<'a>(error_basic_infos : ErrorBasicInfos<'a>, name: &str, nearest_var_name : Option<String>) -> ErrorPrint<'a> {
    let note = nearest_var_name.map(|n| format!("Maybe you meant {}", n));
    ErrorPrint {
        error_basic_infos,
        message: format!("Variable \"{}\" not found", name),
        label: Some("This is the variable not found"),
        note,
    }
}

fn print_wrong_arg_nb<'a>(error_basic_infos : ErrorBasicInfos<'a>, function_name : &str, expected_nb : usize, got_nb : usize) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("There is a wrong argument number for the function {}, the expected number was {} but we got {}", function_name, expected_nb, got_nb),
        label: Some("Here the number of args is wrong"),
        note: Some("Check if there is function call to this function with the wrong number of arguments there".to_owned())
    }
}

fn print_wrong_arg_type<'a>(error_basic_infos : ErrorBasicInfos<'a>, function_name : &str, expected_type : Type, got_type : Type) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("There is a wrong argument type for the function {}, the expected types was {:?}, but we got {:?}", function_name, expected_type, got_type),
        label: Some("Here the type of args is wrong"),
        note: Some("Check if there is function call to this function with the wrong type of arguments there".to_owned())
    }
}

fn print_wrong_type_error<'a>(error_basic_infos : ErrorBasicInfos<'a>, expected_type : Type, got_type : Type) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("Wrong type, expected {:?} type but got {:?}", expected_type, got_type), // TODO : replace these debug prints by a real Display implementation for types
        label: Some("This is where the types are wrong"),
        note: Some("Check if a type is obviously wrong (for example a non bool value in a if or a + with a float)".to_owned()),
    }
}

pub(crate) fn print_type_error(type_error : TypesErr, filename : &Path, content : &str){

    let error_nb =  TYPE_ERROR_OFFSET + type_error.err_data.tag() as u32;
    let range = type_error.range;
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

    let error_print = match *type_error.err_data {
        TypesErrData::FunctionTypeExpected { wrong_type } => print_function_type_expected(error_basic_infos, wrong_type),
        TypesErrData::IncompatibleTypes { type1, type2 } => print_incompatible_types_error(error_basic_infos, type1, type2),
        TypesErrData::ListTypeExpected { wrong_type } => print_list_type_expected_error(error_basic_infos, wrong_type),
        TypesErrData::VarNotFound { name, nearest_var_name } => print_var_not_found_error(error_basic_infos, &name, nearest_var_name),
        TypesErrData::WrongArgNb { function_name, expected_nb, got_nb } => print_wrong_arg_nb(error_basic_infos, &function_name, expected_nb, got_nb),
        TypesErrData::WrongArgType { function_name, expected_type, got_type } => print_wrong_arg_type(error_basic_infos, &function_name, expected_type, got_type),
        TypesErrData::WrongRetType { function_name: _, expected_type: _, got_type: _ } => todo!(), // TODO (can you create this error, have not been able to do it)
        TypesErrData::WrongType { expected_type, got_type } => print_wrong_type_error(error_basic_infos, expected_type, got_type),
    };

    print_error(error_print);
}

fn print_integer_out_of_range_check_error<'a>(error_basic_infos : ErrorBasicInfos<'a>, nb : i128) -> ErrorPrint<'a> {
    ErrorPrint {
        error_basic_infos,
        message: format!("Integer literal out of range {}", nb),
        label: Some(formatc!("The literal should be in the range [{}, {}] ", CheckError::INT_LITERAL_RANGE.start, CheckError::INT_LITERAL_RANGE.end)),
        ..Default::default()
    }
}

pub(crate) fn print_check_error(check_error : CheckError, filename : &Path, content : &str){

    let error_nb =  CHECK_ERROR_OFFSET + check_error.err_data.tag() as u32;
    let range = check_error.range;
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


    let error_print = match check_error.err_data {
        CheckErrorData::IntegerOutOfRange { nb } => print_integer_out_of_range_check_error(error_basic_infos, nb),
    };

    print_error(error_print);
}