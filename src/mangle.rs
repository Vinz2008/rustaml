use crate::ast::{ExternLang, Type};

pub fn mangle_name(s : &str, function_type : &Type, lang : ExternLang) -> String {
    match lang {
        ExternLang::C => s.to_owned(),
    }
}