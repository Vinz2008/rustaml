use std::{ffi::CString, os::raw::c_void, rc::Rc};

use crate::{ast::{CType, ExternLang, Type}, interpreter::{InterpretContext, Val}, mangle::mangle_name_external, rustaml::RustamlContext, string_intern::StringRef};

use debug_with_context::DebugWithContext;
use libffi::{low::CodePtr, middle::{Arg, Cif, Type as FFIType}};
use libloading::Library;
use pathbuf::pathbuf;

#[derive(Clone)]
pub struct FFIFunc {
    _lib : Rc<Library>, // never read it, just put it there to prevent drop
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

fn get_ffi_type(t : &Type) -> FFIType {
    match t {
        Type::Integer => FFIType::i64(),
        Type::Float => FFIType::f64(),
        Type::Bool => FFIType::u8(),
        Type::Function(_, _, _) | Type::Str => FFIType::pointer(),
        Type::CType(c_type) => {
            match c_type {
                CType::U8 => FFIType::u8(),
                CType::I8 => FFIType::i8(),
                CType::U16 => FFIType::u16(),
                CType::I16 => FFIType::i16(),
                CType::U32 => FFIType::u32(),
                CType::I32 => FFIType::i32(),
                CType::U64 => FFIType::u64(),
                CType::I64 => FFIType::i64(),
                CType::F32 => FFIType::f32(),
                CType::F64 => FFIType::f64(),
            }
        }
        _ => unreachable!()
    }
}

// TODO : return a result and do better error handling
pub fn get_ffi_func(context : &mut InterpretContext, name: StringRef, func_type : Type, external_lang : ExternLang, so_str : Option<StringRef>) -> FFIFunc {
    let mangled_name = mangle_name_external(name.get_str(&context.rustaml_context.str_interner), &func_type, external_lang);


    // TODO : add support for explicit shared library name in extern function
    let lib = if let Some(so_name) = so_str {

        let path = pathbuf![so_name.get_str(&context.rustaml_context.str_interner)];
        unsafe { 
            Library::new(path).unwrap()
        }
    }  else {
        #[cfg(unix)]
        let lib = libloading::os::unix::Library::this();

        #[cfg(windows)]
        let lib = libloading::windows::unix::Library::this();

        lib.into()
    };

    let (ret_type, arg_types) = match func_type {
        Type::Function(args, ret, _) => (ret, args),
        _ => unreachable!(),
    };

    unsafe {
        let function_ptr: *const c_void = *lib.get(mangled_name.as_bytes()).unwrap();
        
        let code_ptr = CodePtr::from_ptr(function_ptr);

        let ret_type_ffi = get_ffi_type(ret_type.as_ref());
        let arg_types = arg_types.iter().map(|e| get_ffi_type(e)).collect::<Vec<_>>(); // TODO

        let cif = Cif::new(arg_types, ret_type_ffi);

        FFIFunc {
            code_ptr,
            cif,
            ret_type: *ret_type,
            _lib : Rc::new(lib),
        }

    }
}

// TODO : use libffi to turn a closure for interpreting the function into a function pointer
// TODO : is it really possible ? (need to match on : arg nb, ret type, etc)
/*fn get_function_ptr(context : &mut InterpretContext, func_def : &FunctionDef) -> *const c_void {
    let closure_f = || match &func_def.body {
        FunctionBody::AST(ast) => interpret_node(context, *ast),
        FunctionBody::FFI(ffi) => todo!(),
    };

    let closure = Closure0::new(&closure_f);
    todo!()
}*/

struct FFIContext {
    u8s : Vec<u8>,
    c_strs : Vec<CString>,
    c_str_ptrs : Vec<*const i8>,
}

impl FFIContext {
    fn new(args_len : usize) -> FFIContext {
        FFIContext {
            u8s: Vec::with_capacity(args_len), // to prevent pointer invalidation, reserve the max size possible, even if bigger than needed
            c_strs: Vec::with_capacity(args_len),
            c_str_ptrs: Vec::with_capacity(args_len),
        }
    }
}

// TODO
fn get_arg(context : &mut InterpretContext, ffi_context : &mut FFIContext, v : &Val) -> Arg {
    match v {
        Val::Integer(i) => Arg::new(i),
        Val::Float(f) => Arg::new(f),
        Val::String(s) => {
            let arg_str = s.get_str(&context.rustaml_context.str_interner);
            let cstring = std::ffi::CString::new(arg_str).unwrap();
            
            // TODO : leak the CString ? add it to a list to free it manually when returning the ffi part
            ffi_context.c_strs.push(cstring);
            let cstring_ptr = ffi_context.c_strs.last().unwrap().as_ptr();
            ffi_context.c_str_ptrs.push(cstring_ptr);
            Arg::new(ffi_context.c_str_ptrs.last().unwrap())
        },
        Val::Bool(b) => {
            ffi_context.u8s.push(*b as u8);
            Arg::new(ffi_context.u8s.last().unwrap())
        }
        /*Val::Function(func_def) => {
            let arg_ptr = get_function_ptr(context, func_def);
            Arg::new(&arg_ptr)
        },*/
        _ => unreachable!(),
    }
}

pub fn call_ffi_function(context : &mut InterpretContext, ffi_func : &FFIFunc, args : &[Val]) -> Val {
    let mut ffi_context = FFIContext::new(args.len());

    unsafe  {
        let args = args.iter().map(|e| get_arg(context, &mut ffi_context, e)).collect::<Vec<_>>();
        match &ffi_func.ret_type {
            Type::Integer => {
                let i = ffi_func.cif.call(ffi_func.code_ptr, &args);
                Val::Integer(i)
            },
            Type::Float => {
                let f = ffi_func.cif.call(ffi_func.code_ptr, &args);
                Val::Float(f)
            },
            Type::Bool => {
                let b_u8 : u8 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                let b = match b_u8 {
                    0 => false,
                    1 => true,
                    _ => unreachable!(),
                };
                
                Val::Bool(b)
            }
            Type::CType(c_type) => match c_type {
                CType::U8 => {
                    let u : u8 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Integer(u as i64)
                }
                CType::I8 => {
                    let i : i8 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Integer(i as i64)
                }
                CType::U16 => {
                    let u : u16 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Integer(u as i64)
                }
                CType::I16 => {
                    let i : i16 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Integer(i as i64)
                }
                CType::U32 => {
                    let u : u32 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Integer(u as i64)
                }
                CType::I32 => {
                    let i : i32 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Integer(i as i64)
                }
                CType::U64 => {
                    let i : u64 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Integer(i.try_into().expect("Couldn't convert a c_type.u64 to a Integer (which is under the hood a i64)"))
                }
                CType::F32 => {
                    let f : f32 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Float(f as f64)
                },
                CType::F64 => {
                    let f : f64 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Float(f)
                }
                _ => todo!(),
            }
            _ => unreachable!(),
        }
        
    }
}