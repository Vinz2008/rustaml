use std::{ffi::CString, mem::{ManuallyDrop, MaybeUninit}, ops::Deref, os::raw::c_void, ptr, rc::Rc};

use crate::{ast::{CType, ExternLang, Type}, interpreter::{call_function, FunctionBody, FunctionDef, InterpretContext, Val}, mangle::mangle_name_external, rustaml::RustamlContext, string_intern::StringRef};

use debug_with_context::DebugWithContext;
use libffi::{low::CodePtr, middle::{Arg, Cif, Closure, Type as FFIType}, raw::ffi_cif};
use libloading::Library;
use pathbuf::pathbuf;

#[derive(Clone)]
pub struct FFIFunc {
    _lib : Option<Rc<Library>>, // never read it, just put it there to prevent drop, optional for functions returned from a function where we don't know the library
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
        t => panic!("t : {:?}", t),
        //_ => unreachable!()
    }
}

// TODO : return a result and do better error handling
pub fn get_ffi_func(context : &mut InterpretContext, name: StringRef, func_type : Type, external_lang : ExternLang, so_str : Option<StringRef>) -> FFIFunc {
    let mangled_name = mangle_name_external(name.get_str(&context.rustaml_context.str_interner), &func_type, external_lang);


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
        let arg_types = arg_types.iter().map(get_ffi_type).collect::<Vec<_>>();

        let cif = Cif::new(arg_types, ret_type_ffi);

        FFIFunc {
            code_ptr,
            cif,
            ret_type: *ret_type,
            _lib : Some(Rc::new(lib)),
        }

    }
}

fn get_function_closure(context : &mut InterpretContext, ffi_context : &mut FFIContext, func_def : &FunctionDef) -> Closure<'static> {
    let cif = if let Some(function_def_ast) = func_def.function_def_ast {
        let function_type = function_def_ast.get_type(&context.rustaml_context.ast_pool);
        let (args, ret) = match function_type {
            Type::Function(args, ret, is_variadic) => {
                if *is_variadic {
                    panic!("Can't pass a variadic function to a ffi function in the interpreter")
                } else {
                    (args.as_ref(), ret.as_ref())
                }
            },
            _ => unreachable!(),
        };
        let arg_types = match args {
            [Type::Unit] => vec![],
            args => args.iter().map(get_ffi_type).collect::<Vec<_>>(),
        };
        
        let ret_type = get_ffi_type(ret);

        Cif::new(arg_types, ret_type)
    } else {
        let ffi_func = match &func_def.body {
            FunctionBody::Ffi(ffi) => ffi,
            FunctionBody::Ast(_) => unreachable!(),
        };
        ffi_func.cif.clone()
    };

    
    let user_data = UserData {
        ctx: context as *mut _ as *mut c_void,
        function_def: func_def as *const FunctionDef,
        ffi_context: ffi_context as *mut FFIContext,
    };

    let user_data = Box::new(user_data);

    let user_data_ref = unsafe {
        let user_ptr = Box::into_raw(user_data);
        if ffi_context.user_data_ffi.len() == USER_DATA_MAX_NB {
            panic!("the number of ffi call data has overflowed USER_DATA_MAX_NB");
        }
        ffi_context.user_data_ffi.push(user_ptr);
        &mut *(user_ptr)
    };

    let closure = Closure::new_mut(cif, function_ptr_trampoline, user_data_ref);

    closure
}

struct UserData {
    ctx : *mut c_void,
    function_def : *const FunctionDef,
    ffi_context : *mut FFIContext,
}

unsafe fn get_val_from_arg(arg : *const c_void, arg_type : &Type) -> Val {
    unsafe {
        match arg_type {
            Type::Integer => Val::Integer(*(arg as *const i64)),
            Type::CType(c_type) => {
                match c_type {
                    CType::I32 => Val::Integer(*(arg as *const i32) as i64),
                    CType::I64 => get_val_from_arg(arg, &Type::Integer),
                    CType::F64 => get_val_from_arg(arg, &Type::Float),
                    _ => todo!()
                }
            }
            _ => todo!()
        }
    }
}

// TODO : add a catch_unwind
// like this ? : let _ = catch_unwind(AssertUnwindSafe(|| {
        // body
//    })).unwrap_or_else(|_| {
//        std::process::abort();
//    });
unsafe extern "C" fn function_ptr_trampoline(cif: &ffi_cif, result : &mut c_void, args_ptr: *const *const c_void, user_data : &mut UserData){
    unsafe {
        let user_data = &mut *(user_data as *mut UserData);
        let context = &mut *(user_data.ctx as *mut InterpretContext);
        let func_def = &*user_data.function_def;
        let ffi_context = &mut *(user_data.ffi_context);
        let arg_nb = cif.nargs;
        let mut args = Vec::with_capacity(arg_nb as usize);

        for i in 0..arg_nb {
            args.push(*args_ptr.wrapping_add(i as usize));
        }
        // TODO : work on the unwrap ?
        let arg_types = match func_def.function_def_ast.unwrap().get_type(&context.rustaml_context.ast_pool) {
            Type::Function(args, _, _) => args.as_ref(),
            _ => unreachable!(),
        };
        //dbg!(arg_types);

        let args_val = args.into_iter().zip(arg_types).map(|(i, arg_type)| get_val_from_arg(i, arg_type)).collect::<Vec<_>>();

        let res_val = call_function(context, func_def, args_val);

        match res_val {
            Val::Integer(i) => *(result as *mut _ as *mut i64) = i,
            Val::Float(f) => *(result as *mut _ as *mut f64) = f,
            Val::Bool(b) => *(result as *mut _ as *mut u8) = b as u8,
            Val::Function(f) => {
                *(result as *mut _ as *mut *const c_void) = *get_func_ptr(context, ffi_context, &f);
            },
            // TODO : add more
            _ => todo!()
        }
    }
    
}

struct FFIContext {
    // TODO, : replace these vecs with impossible to grow vecs
    u8s : FixedVec<u8>,
    c_strs : FixedVec<CString>,
    c_str_ptrs : FixedVec<*const i8>,
    // TODO : verify if they are needed
    closures : FixedVec<Closure<'static>>,
    fn_ptrs : FixedVec<*const c_void>,
    user_data_ffi : FixedVec<*mut UserData>,
}

struct FixedVec<T> {
    array : Box<[MaybeUninit<T>]>,
    len : usize,
}

impl<T> FixedVec<T> {
    fn new(capacity : usize) -> FixedVec<T>{
        assert_ne!(capacity, 0);
        let mut v = Vec::with_capacity(capacity);
        v.resize_with(capacity, || MaybeUninit::uninit());
        FixedVec { 
            array: v.into_boxed_slice(), 
            len: 0
        }
    }
    fn push(&mut self, val : T){
        if self.len == self.array.len(){
            panic!("Capacity exceeded in FixedVec");
        }
        let idx = self.len;
        self.len += 1;
        unsafe {
            self.array.get_unchecked_mut(idx).write(val);
        }
    }

    fn len(&self) -> usize {
        self.len
    }

    fn as_slice(&self) -> &[T] {
        unsafe { 
            self.array[..self.len].assume_init_ref() 
        }
    }

}

impl<T> Deref for FixedVec<T> {
    type Target = [T];
    
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

struct FixedVecIter<T> {
    array: Box<[MaybeUninit<T>]>,
    pos: usize,
    len: usize,
}

impl<T> Iterator for FixedVecIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos == self.len {
            return None;
        }
        let val = unsafe {
            self.array[self.pos].assume_init_read()
        };
        self.pos += 1;
        Some(val)
    }
}

impl<T> IntoIterator for FixedVec<T> {
    type Item = T;

    type IntoIter = FixedVecIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        let manually_drop_array = ManuallyDrop::new(self);
        FixedVecIter {
            array: unsafe  {
                ptr::read(&manually_drop_array.array)
            },
            pos: 0,
            len: manually_drop_array.len,
        }
    }
}

impl<T> Drop for FixedVecIter<T> {
    fn drop(&mut self) {
        for i in self.pos..self.len {
            unsafe {
                self.array[i].assume_init_drop();
            }
        }
    }
}

impl<T> Drop for FixedVec<T> {
    fn drop(&mut self) {
        for i in 0..self.len {
            unsafe {
                self.array[i].assume_init_drop();
            }
        }
    }
}

// TODO : what number should it be ?
const USER_DATA_MAX_NB : usize = 20;

impl FFIContext {
    fn new(args_len : usize) -> FFIContext {
        FFIContext {
            u8s: FixedVec::new(args_len), // to prevent pointer invalidation, reserve the max size possible, even if bigger than needed
            c_strs: FixedVec::new(args_len),
            c_str_ptrs: FixedVec::new(args_len),
            closures: FixedVec::new(args_len),
            fn_ptrs: FixedVec::new(args_len),
            // TODO : put this in the stack instead ? (because constant size ?)
            user_data_ffi: FixedVec::new(USER_DATA_MAX_NB), // only USER_DATA_MAX_NB user data can be used, more will do pointer invalidation, so you need to call no more than USER_DATA_MAX_NB times ffi functions during a function call (ex : calling a ffi function, which calls an arg that is a rustaml function that calls a ffi function), make this number not too big because there is an allocation of USER_DATA_MAX_NB * 24 bytes
        }
    }
}

fn prepare_func_ptr(context : &mut InterpretContext, ffi_context : &mut FFIContext, func_def : &FunctionDef){
    let closure = get_function_closure(context, ffi_context, func_def);
    ffi_context.closures.push(closure);

    let fn_arg: unsafe extern "C" fn() = *ffi_context.closures.last().unwrap().code_ptr();
    let arg_ptr = fn_arg as *const () as *const c_void;

    ffi_context.fn_ptrs.push(arg_ptr);
}

// return a ref to make it work with libffi Arg
fn get_func_ptr<'a>(context : &mut InterpretContext, ffi_context : &'a mut FFIContext, func_def : &FunctionDef) -> &'a *const c_void {
    let closure = get_function_closure(context, ffi_context, func_def);
    ffi_context.closures.push(closure);

    let fn_arg: unsafe extern "C" fn() = *ffi_context.closures.last().unwrap().code_ptr();
    let arg_ptr = fn_arg as *const () as *const c_void;

    ffi_context.fn_ptrs.push(arg_ptr);
    //ffi_context.fn_ptrs.push(arg_ptr);
    ffi_context.fn_ptrs.last().unwrap()
}

// TODO: need this ?
// return a ref to make it work with libffi Arg
/*fn get_func_ptr<'a>(ffi_context : &'a mut FFIContext) -> &'a *const c_void {
    ffi_context.fn_ptrs.last().unwrap()
}*/


fn prepare_args_data(context : &mut InterpretContext, ffi_context : &mut FFIContext, args : &[Val]){
    for v in args {
        match v {
            Val::String(s) => {
                let arg_str = s.get_str(&context.rustaml_context.str_interner);
                let cstring = std::ffi::CString::new(arg_str).unwrap();
                ffi_context.c_strs.push(cstring);
                let cstring_ptr = ffi_context.c_strs.last().unwrap().as_ptr();
                ffi_context.c_str_ptrs.push(cstring_ptr);
            }
            Val::Bool(b) => {
                ffi_context.u8s.push(*b as u8);
            }
            Val::Function(func_def) => {
                prepare_func_ptr(context, ffi_context, func_def);
            }
            Val::Integer(_) | Val::Float(_) => {} 
            Val::Char(c) => todo!(), // TODO
            _ => unreachable!()
        }
    }
}

// TODO : need to call prepare_arg_data before
fn get_ffi_args<'a>(ffi_context : &'a FFIContext, args : &'a [Val]) -> Vec<Arg<'a>> {
    let mut ffi_args= Vec::with_capacity(args.len());
    let mut string_count = 0;
    let mut func_count = 0;
    let mut bool_count = 0;
    for a in args.iter().rev() {
        let arg = match a {
            Val::Integer(i) => Arg::new(i),
            Val::Float(f) => Arg::new(f),
            Val::String(_) => {
                let str_pos = ffi_context.c_str_ptrs.len()-1-string_count;
                string_count += 1;
                Arg::new(ffi_context.c_str_ptrs.get(str_pos).unwrap())
            },
            Val::Bool(_) => {
                let bool_pos = ffi_context.u8s.len()-1-bool_count;
                bool_count += 1;
                Arg::new(ffi_context.u8s.get(bool_pos).unwrap())
            }
            Val::Function(_) => {
                let func_pos = ffi_context.fn_ptrs.len()-1-func_count;
                func_count += 1;
                Arg::new(ffi_context.fn_ptrs.get(func_pos).unwrap())
                //Arg::new(get_func_ptr(ffi_context))
            },
            _ => unreachable!(),
        };
        ffi_args.push(arg);
    }
    ffi_args.reverse(); // reverse it because it was created reversed
    ffi_args
}


/*fn get_arg<'a>(context : &mut InterpretContext, ffi_context : &'a mut FFIContext, v : &'a Val) -> Arg<'a> {
    match v {
        Val::Integer(i) => Arg::new(i),
        Val::Float(f) => Arg::new(f),
        Val::String(s) => {
            let arg_str = s.get_str(&context.rustaml_context.str_interner);
            let cstring = std::ffi::CString::new(arg_str).unwrap();
            
            ffi_context.c_strs.push(cstring);
            let cstring_ptr = ffi_context.c_strs.last().unwrap().as_ptr();
            ffi_context.c_str_ptrs.push(cstring_ptr);
            Arg::new(ffi_context.c_str_ptrs.last().unwrap())
        },
        Val::Bool(b) => {
            ffi_context.u8s.push(*b as u8);
            Arg::new(ffi_context.u8s.last().unwrap())
        }
        Val::Function(func_def) => {
            Arg::new(get_func_ptr(context, ffi_context, func_def))
        },
        _ => unreachable!(),
    }
}*/

pub fn call_ffi_function(context : &mut InterpretContext, ffi_func : &FFIFunc, args : &[Val]) -> Val {
    let mut ffi_context = FFIContext::new(args.len());

    unsafe  {
        //let args = args.iter().map(|e| get_arg(context, &ffi_context, e)).collect::<Vec<_>>();
        prepare_args_data(context, &mut ffi_context, args);
        let args = get_ffi_args(&ffi_context, args);
        let val = match &ffi_func.ret_type {
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
            Type::Function(arg_types, ret_type, _) => {
                // TODO : test this
                let func_ptr : *const c_void = ffi_func.cif.call(ffi_func.code_ptr, &args);
                let ret_type_ffi = get_ffi_type(ret_type.as_ref());
                let arg_types = arg_types.iter().map(get_ffi_type).collect::<Vec<_>>();
                Val::Function(FunctionDef::new_ffi(context, FFIFunc { 
                    _lib: None, 
                    code_ptr: CodePtr::from_ptr(func_ptr), 
                    cif: Cif::new(arg_types, ret_type_ffi), 
                    ret_type: ret_type.as_ref().to_owned() 
                }))
            },
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
                CType::I64 => {
                    let i : i64 = ffi_func.cif.call(ffi_func.code_ptr, &args);
                    Val::Integer(i)
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
            }
            _ => unreachable!(),
        };
        for user_data_ptr in ffi_context.user_data_ffi {
            let _user_data = Box::from_raw(user_data_ptr); // drop user data here
        }

        val
        
    }
}