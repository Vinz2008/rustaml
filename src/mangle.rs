use mangler::{mangle, Symbol};

use crate::ast::{ExternLang, Type};

fn get_type_symbol(t : &Type) -> Symbol {
    let s = match t {
        Type::Integer => "long",
        Type::Float => "double",
        Type::Bool => "bool",
        Type::CType(_) => todo!(),
        _ => unreachable!(),
    }.to_owned();

    Symbol::Type(s)
}

fn mangle_cpp(s : &str, function_type : &Type) -> String {
    let (ret_type, arg_types) = match function_type {
        Type::Function(args, ret, _) => (ret.as_ref(), args.as_ref()), 
        _ => unreachable!(),
    };

    let func = Symbol::Type(s.to_owned());
    let args = arg_types.iter().map(get_type_symbol).collect::<Vec<_>>();
    let function_symbol = Symbol::Function(Box::new(func), args);
    mangle(function_symbol)
    
}

pub fn mangle_name_external(s : &str, function_type : &Type, lang : ExternLang) -> String {
    match lang {
        ExternLang::C => s.to_owned(),
        ExternLang::Cpp => mangle_cpp(s, function_type),
    }
}