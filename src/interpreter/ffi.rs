use std::os::raw::c_void;

use crate::{ast::{ExternLang, Type}, interpreter::{InterpretContext, Val}, mangle::mangle_name, rustaml::RustamlContext, string_intern::StringRef};

use debug_with_context::DebugWithContext;
use libffi::{low::CodePtr, middle::{Arg, Cif, Type as FFIType}};

#[derive(Clone)]
pub struct FFIFunc {
    code_ptr : CodePtr,
    cif : Cif,
    ret_type : Type,
}

impl DebugWithContext<RustamlContext> for FFIFunc {
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, _context: &RustamlContext) -> std::fmt::Result {
        f.debug_struct("FFIFunc").field("code_ptr", &self.code_ptr).field("cif", &self.cif).finish()
    }
}

impl PartialEq for FFIFunc {
    fn eq(&self, other: &Self) -> bool {
        self.code_ptr.as_ptr() == other.code_ptr.as_ptr() && self.cif.as_raw_ptr() == other.cif.as_raw_ptr()
    }
}

// TODO : return a result and do better error handling
pub fn get_ffi_func(context : &mut InterpretContext, name: StringRef, func_type : Type, external_lang : ExternLang) -> FFIFunc {
    let mangled_name = mangle_name(name.get_str(&context.rustaml_context.str_interner), &func_type, external_lang);


    // TODO : add support for explicit shared library name in extern function
    #[cfg(unix)]
    let lib = libloading::os::unix::Library::this();

    #[cfg(windows)]
    let lib = libloading::windows::unix::Library::this();

    let (ret_type, arg_types) = match func_type {
        Type::Function(args, ret, _) => (ret, args),
        _ => unreachable!(),
    };

    unsafe {
        let function_ptr: *const c_void = *lib.get(mangled_name.as_bytes()).unwrap();
        
        let code_ptr = CodePtr::from_ptr(function_ptr);

        let ret_type_ffi = FFIType::f64(); // TODO
        let arg_types = vec![FFIType::f64()]; // TODO

        let cif = Cif::new(arg_types, ret_type_ffi);

        FFIFunc {
            code_ptr,
            cif,
            ret_type: *ret_type,
        }

    }
}

// TODO
fn get_arg(v : &Val) -> Arg {
    match v {
        Val::Float(f) => Arg::new(f),
        _ => unreachable!(),
    }
}

pub fn call_ffi_function(context : &mut InterpretContext, ffi_func : &FFIFunc, args : &[Val]) -> Val {
    unsafe  {
        let args = args.iter().map(get_arg).collect::<Vec<_>>();
        match ffi_func.ret_type {
            Type::Float => {
                let f = ffi_func.cif.call(ffi_func.code_ptr, &args);
                Val::Float(f)
            },
            _ => unreachable!(),
        }
        
    }
}