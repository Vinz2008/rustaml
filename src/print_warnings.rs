use std::{ops::Range, path::Path};

use ariadne::{Color, ColorGenerator, Label, Report, Source};
use enum_tags::{Tag, TaggedEnum};

use crate::ast::Type;

#[derive(Tag)]
pub enum WarningData {
    MatchNotExhaustive {
        matched_type : Type,
        // TODO : add some Range struct to track what is the current range and what is missing (this range can be strings or numbers or etc)
    }
}

pub struct Warning {
    warning_data : WarningData,
    range: Range<usize>,
}

impl Warning {
    pub fn new(warning_data : WarningData, range: Range<usize>) -> Warning {
        Warning {
            warning_data,
            range
        }
    }
}

#[derive(Default)]
struct WarningBasicInfos<'a> {
    warning_nb : u32,
    range: Range<usize>,
    filename : &'a str,
    content : &'a str,
    color: Color,
}

#[derive(Default)]
struct WarningPrint<'a> {
    warning_basic_infos : WarningBasicInfos<'a>,
    message : String,
    label : Option<&'a str>,
    note : Option<String>,
}

fn print_any_warning(warn : WarningPrint){
    let mut report = Report::build(ariadne::ReportKind::Warning, (warn.warning_basic_infos.filename, warn.warning_basic_infos.range.clone()))
    .with_code(warn.warning_basic_infos.warning_nb).with_message(warn.message);
    if let Some(label) = warn.label {
        report = report.with_label(Label::new((warn.warning_basic_infos.filename, warn.warning_basic_infos.range)).with_message(label).with_color(warn.warning_basic_infos.color));
    }

    if let Some(note) = warn.note {
        report = report.with_note(note);
    }
    
    report.finish().print((warn.warning_basic_infos.filename, Source::from(warn.warning_basic_infos.content))).unwrap();
}

// TODO : check for number not in i64 (except abs(INT_MIN))
// TODO : check for number not in f64

fn print_match_not_exhaustive_warning(warning_basic_infos : WarningBasicInfos, matched_type : Type) -> WarningPrint {
    WarningPrint {
        warning_basic_infos,
        message: format!("Not exhaustive match of type {}", matched_type),
        label: Some("Some cases are not handled for in the match"),
        ..Default::default()
    }
}

pub fn print_warning(warning : Warning, filename : &Path, content : &str){
    let filename_str = filename.to_str().unwrap();

    let mut colors = ColorGenerator::new(); // TODO : not generate one at each warning ?
    let color = colors.next();

    let warning_basic_infos = WarningBasicInfos {
        warning_nb: warning.warning_data.tag() as u32,
        range:  warning.range,
        filename: filename_str,
        content,
        color,
    };
    let warn = match warning.warning_data {
        WarningData::MatchNotExhaustive { matched_type } => print_match_not_exhaustive_warning(warning_basic_infos, matched_type),
    };
    print_any_warning(warn);
}