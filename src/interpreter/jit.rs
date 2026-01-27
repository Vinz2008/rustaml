use std::{ffi::CStr, io::Write, path::Path, process::{Command, Stdio}, time::Duration};

use inkwell::{AddressSpace, OptimizationLevel, context::Context, execution_engine::{ExecutionEngine, JitFunction}, module::{Linkage, Module}, passes::PassBuilderOptions, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, types::{FunctionType, StructType}, values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, IntValue, PointerValue, StructValue, ValueKind}};
use libloading::Library;
use rustc_hash::FxHashMap;

use crate::{ast::{ASTNode, ASTRef, CType, Type}, compiler::{CompileContext, JITCpuInfos, cast::cast_val, compile_function, compiler_utils::{add_function, get_fn_type, get_type_tag}, debuginfo::DebugInfo, get_var_id, linker::STD_C_CONTENT}, interpreter::{FunctionBody, FunctionDef, InterpretContext, Val}, rustaml::RustamlContext, string_intern::StringRef, types::TypeInfos};

#[cfg(feature = "debug-llvm")]
use inkwell::support::LLVMString;

// TODO : should I migrate to ORC ? (use llvm_sys ? but very limited, or use a custom cpp bindings, but complicated)

#[derive(Default, Debug)]
struct FuncMeta {
    call_count : u64,
    total_time_ns : u64,
    current_recursion_depth : u64,
    max_recursion_depth : u64,
    cached_fun : Option<JitFunction<'static, WrapperFN>>,
}

pub(crate) struct JitContext {
    functions_meta : FxHashMap<ASTRef, FuncMeta>, // ASTRef of function
    context : &'static Context,
    type_infos : Option<TypeInfos>,
    dump_jit_ir : bool,
    dump_jit_asm : bool,
}

impl JitContext {
    pub(crate) fn new(type_infos : Option<TypeInfos>, dump_jit_ir : bool, dump_jit_asm : bool) -> JitContext {
        // box the context and leak it because only 8 bytes, and to prevent lifetime annotation (can they be optionnal) (TODO ?)
        let context = Box::new(Context::create());
        let context = Box::leak(context);
        

        JitContext { 
            functions_meta: FxHashMap::default(),
            context,
            type_infos,
            dump_jit_ir,
            dump_jit_asm,
        }
    }
}

// TODO : change the layout to minimize size ?
// TODO : is the tag really needed ?
// typedef struct {
//     // i8
//     enum {
//         INTEGER,
//         FLOAT,
//         BOOL_TYPE,
//         CHAR_TYPE,
//     }
//     uint64_t data;
// } Value;

fn get_value_struct_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context) -> StructType<'llvm_ctx> {
    let fields = &[
        llvm_context.i8_type().into(),
        llvm_context.i64_type().into(),
    ];
    llvm_context.struct_type(fields, false)
}

pub(crate) fn get_jit_entry_function_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context) -> FunctionType<'llvm_ctx> {
    let value_struct = get_value_struct_type(llvm_context);
    // only pass the pointer, do I need a size or is it statically known ?
    get_fn_type(llvm_context, value_struct.into(), &[llvm_context.ptr_type(AddressSpace::default()).into()], false)
}

fn jit_wrap_val<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, val : BasicValueEnum<'llvm_ctx>, val_type : &Type) -> StructValue<'llvm_ctx> {
    let value_struct_ty = get_value_struct_type(compile_context.context);
    let type_tag = get_type_tag(val_type);
    let i64_type = compile_context.context.i64_type();
    let val_data = match val_type {
        Type::Integer | Type::Bool | Type::Char | Type::Str => 
            cast_val(compile_context, val, val_type, &Type::CType(CType::U64)).into_int_value(),
        Type::Float => compile_context.builder.build_bit_cast(val, i64_type, "bitcast_float_to_jit_val").unwrap().into_int_value(),
        _ => todo!(), // TODO
    };
    let field_vals: &[BasicValueEnum] = &[
        compile_context.context.i8_type().const_int(type_tag as u64, false).into(),
        val_data.into(),
    ];
    let mut struct_val = value_struct_ty.get_undef();
    for (field_idx, field_val) in field_vals.iter().enumerate() {
        struct_val = compile_context.builder.build_insert_value(struct_val, *field_val, field_idx.try_into().unwrap(), "insert_return_field").unwrap().into_struct_value();
    }
    struct_val
}


fn jit_unwrap_val_data<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, val_data : IntValue<'llvm_ctx>, val_type : &Type) -> BasicValueEnum<'llvm_ctx> {
    match val_type {
        Type::Integer | Type::Bool | Type::Char => cast_val(compile_context, val_data.as_basic_value_enum(), &Type::CType(CType::U64), val_type),
        Type::Float => compile_context.builder.build_bit_cast(val_data, compile_context.context.f64_type(), "bitcast_jit_val_to_float").unwrap(),
        Type::Str => cast_val(compile_context, val_data.as_basic_value_enum(), &Type::CType(CType::U64), val_type),
        _ => panic!("{:?}", val_type) // TODO
    }
}

pub(crate) fn jit_unwrap_val_from_args<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, args_array : PointerValue<'llvm_ctx>, arg_idx: usize, arg_type : &Type) -> BasicMetadataValueEnum<'llvm_ctx> {
    let i64_type = compile_context.context.i64_type();
    let i32_type = compile_context.context.i32_type();
    let value_struct = get_value_struct_type(compile_context.context);
    let arr_indexes = &[i64_type.const_int(arg_idx as u64, false)];
    let struct_val_ptr = unsafe {
        compile_context.builder.build_in_bounds_gep(value_struct, args_array, arr_indexes, "gep_arr_unwrap_jit").unwrap()
    };
    let struct_indexes = &[
        i32_type.const_int(0, false),
        i32_type.const_int(1, false),
    ];
    let data_ptr = unsafe {
        compile_context.builder.build_in_bounds_gep(value_struct, struct_val_ptr, struct_indexes, "gep_struct_unwrap_jit").unwrap()
    };
    let load_data = compile_context.builder.build_load(i64_type, data_ptr, "jit_load_data").unwrap();
    return jit_unwrap_val_data(compile_context, load_data.into_int_value(), arg_type).into();
}

fn get_jit_fun_wrapper_name(name : &str) -> String {
    format!("{}__//__wrapper", name)
}

fn generate_jit_fun_wrapper<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, name : StringRef, jit_wrapper_name : &str, arg_types : &[Type], res_type : Type){
    let wrapper_fun_ty = get_jit_entry_function_type(compile_context.context);
    let wrapper_fun = add_function(compile_context, jit_wrapper_name, wrapper_fun_ty, Some(Linkage::External));
    let entry = compile_context.context.append_basic_block(wrapper_fun, "entry");
    compile_context.builder.position_at_end(entry);

    let args_array = wrapper_fun.get_first_param().unwrap().into_pointer_value();

    let mut args_val = Vec::with_capacity(arg_types.len());

    for (arg_idx, arg_type) in arg_types.iter().enumerate() {
        let arg_val = jit_unwrap_val_from_args(compile_context, args_array, arg_idx, arg_type);
        args_val.push(arg_val);
    }

    let real_fun = compile_context.module.get_function(name.get_str(&compile_context.rustaml_context.str_interner)).unwrap();
    let res_call = compile_context.builder.build_call(real_fun, &args_val, "call_real_jit_fun").unwrap().try_as_basic_value();
    let res_call = match res_call {
        ValueKind::Basic(b) => b,
        ValueKind::Instruction(_) => unreachable!(),
    };

    let wrapped_res = jit_wrap_val(compile_context, res_call, &res_type);
    compile_context.builder.build_return(Some(&wrapped_res)).unwrap();
}

fn get_shared_library_std<'llvm_ctx>(context : &mut InterpretContext) -> Library {
    // TODO : save the library in the JitContext
    let temp_folder = std::env::temp_dir();
    let std_so = temp_folder.join("std.so");

    if !std_so.exists(){
        let opt_level = 1;
        let mut clang_std = Command::new("clang");
        clang_std.arg("-x").arg("c").arg(format!("-O{}", opt_level)).arg("-shared");

        if opt_level != 0{
            clang_std.arg("-DNDEBUG");
        }

        // TODO : enable gc    
        //clang_std.arg("-D_GC_")
        
        clang_std.arg("-march=native");

        // TODO ?
        // clang_std.arg("-g");
        
        let mut clang_std = clang_std.arg("-").arg("-o").arg(&std_so).stdin(Stdio::piped()).spawn().expect("compiling std failed");
        clang_std.stdin.as_mut().unwrap().write_all(STD_C_CONTENT.as_bytes()).unwrap();
        clang_std.wait().unwrap();
    }

    let lib_std = unsafe { Library::new(std_so).unwrap() };

    lib_std
}

type PlaceholderFunType = unsafe extern "C" fn() -> ();

unsafe extern "C" {
    // don't care type, because we just need the ptr
    unsafe fn fprintf() -> i32;
    unsafe fn exit() -> usize;
    static mut stderr: *mut ();
}

fn register_external_functions<'llvm_ctx>(std_lib : &Library, module : &Module<'llvm_ctx>, execution_engine : &ExecutionEngine<'llvm_ctx>){
    for fun in module.get_functions(){
        let bytes = fun.get_name().to_bytes();
        match bytes {
            b"__str_append" | b"__chars" | b"__print_val" => unsafe {
                let fun_symbol = std_lib.get::<PlaceholderFunType>(bytes).unwrap();
                execution_engine.add_global_mapping(&fun, fun_symbol.try_as_raw_ptr().unwrap() as usize);
            }
            b"fprintf" => {
                execution_engine.add_global_mapping(&fun, fprintf as *const () as usize);
            }
            b"exit" => {
                execution_engine.add_global_mapping(&fun, exit as *const () as usize);
            }
            _ => {}
        }
    }
    for global in module.get_globals(){
        let bytes = global.get_name().to_bytes();
        match bytes {
            b"stderr" => {
                execution_engine.add_global_mapping(&global, &raw mut stderr as usize );
            }
            _ => {}
        }
    }
}

pub(crate) fn update_jit_heuristics_function_start_call(context : &mut InterpretContext, ast : ASTRef){
    match context.jit_context.functions_meta.get_mut(&ast){
        Some(func_meta) => {
            func_meta.call_count += 1;
            func_meta.current_recursion_depth += 1;
            if func_meta.current_recursion_depth > func_meta.max_recursion_depth {
                func_meta.max_recursion_depth = func_meta.current_recursion_depth;
            }
        },
        None => {
            let func_meta = FuncMeta{
                call_count: 1,
                current_recursion_depth: 1,
                max_recursion_depth: 1,
                ..Default::default()
            };
            context.jit_context.functions_meta.insert(ast, func_meta);
        },
    }
}

pub(crate) fn update_jit_heuristics_function_end_call(context : &mut InterpretContext, ast : ASTRef, time_taken : Duration){
    match context.jit_context.functions_meta.get_mut(&ast){
        Some(func_meta) => {
            func_meta.current_recursion_depth -= 1;
            func_meta.total_time_ns += time_taken.as_nanos() as u64;
        },
        None => unreachable!(),
    }
}

fn is_valid_jit_type(t : &Type, is_return : bool) -> bool {
    // TODO : enable this after making lists work
    /*if !is_return && matches!(t, Type::List(_)) {
        return true;
    }*/
    matches!(t, Type::Integer | Type::Float | Type::Bool | Type::Char | Type::Str)
}

pub(crate) fn should_use_jit_function(context : &InterpretContext, func_def : &FunctionDef) -> bool  {
    if context.jit_context.type_infos.is_none() {
        return false;
    }

    
    let func_type = match func_def.function_def_ast {
        Some(function_def_ast) => function_def_ast.get_type(&context.rustaml_context.ast_pool),
        None => return false,
    };
    let (args, ret) = match func_type {
        Type::Function(args, ret, _is_variadic) => (args, ret),
        _ => unreachable!(),
    };

    // for now only this type
    if args.as_ref().iter().any(|e| !is_valid_jit_type(e, false)) || !is_valid_jit_type(ret.as_ref(), true) {
        return false;
    }

    match func_def.body {
        FunctionBody::Ast(ast) => {
            if let Some(meta) = context.jit_context.functions_meta.get(&ast) {
                // meta.total_time_ns / meta.call_count > 30_000
                (meta.call_count > 10 && meta.total_time_ns > 30_000 * meta.call_count) || meta.total_time_ns > 2_000_000 || meta.max_recursion_depth >= 8
            } else {
                false
            }
            
        }
        _ => false
    }
}

// TODO : create a special struct Value to have as a special ABI to every function that would be : 
// Value f(Value* args)
// with : (similar layout as the type tag and val in ListNode in std.c)
// (add later the strings and list types)
// typedef struct {
//     enum {} tag;
//     uint64_t value;
// } Value;

// TODO : while compiling the JIT, find used functions for runtime ?
// TODO : make the layout of the same the same as the TypeTag in std.c ?

#[repr(u8)]
enum JITValueTag {
    Integer = 0,
    Float = 1,
    Bool = 2,
    Str = 4,
    List = 5,
    Char = 6,
}

#[repr(C)]
struct JITValue {
    tag : JITValueTag,
    val : u64,
}

fn create_jit_value<'llvm_ctx>(rustaml_context : &RustamlContext, val : Val) -> JITValue {
    let (tag, data) = match val {
        Val::Integer(i) => (JITValueTag::Integer, i as u64),
        Val::Float(f) => (JITValueTag::Float, f.to_bits()),
        Val::Bool(b) => (JITValueTag::Bool, b as u64),
        Val::Char(c) => (JITValueTag::Char, c as u64),
        Val::String(s) => {
            let ptr = s.get_str(&rustaml_context.str_interner).as_ptr();
            (JITValueTag::Str, ptr as u64)
        },
        Val::List(_) => todo!(),
        Val::Function(_) => todo!(), // do like ffi
        Val::Regex(_) | Val::SumType(_) | Val::Vec(_) | Val::Unit => panic!("Unsupported JIT Value {}", val.display(rustaml_context)),
    };
    JITValue { 
        tag, 
        val: data,
    }
}

type WrapperFN = unsafe extern "C" fn(*mut JITValue) -> JITValue;


pub(crate) fn call_jit_function(context : &mut InterpretContext, func_def : &FunctionDef, args_val : Vec<Val>) -> Val {
    let function_def_ast = func_def.function_def_ast.unwrap();


    // TODO : cache the lib
    let lib = get_shared_library_std(context);

    let fun = if let Some(fun_meta) = context.jit_context.functions_meta.get(&function_def_ast) && let Some(cached_fun) = fun_meta.cached_fun.clone() {  // only clone small rc
        cached_fun
    } else {
        let llvm_context = context.jit_context.context;
        let module = llvm_context.create_module("jit");
        let optimization_level = OptimizationLevel::Less; // -O1 (in some cases do O2 if very hot function ?)
        let execution_engine = module.create_jit_execution_engine(optimization_level).unwrap();

        let builder = llvm_context.create_builder();
        
        let is_optimized = false;

        // TODO : deduplicate this code with compiler ?
        // is this even needed (will not be used by JIT)
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let target_triple = TargetMachine::get_default_triple();

        let target = Target::from_triple(&target_triple).unwrap();

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                optimization_level,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        let target_data = target_machine.get_target_data();

        let type_infos = context.jit_context.type_infos.as_ref().unwrap().clone(); // TODO : prevent this clone ?

        let cpu_name = TargetMachine::get_host_cpu_name().to_str().unwrap().to_owned();
        let cpu_features = TargetMachine::get_host_cpu_features().to_str().unwrap().to_owned();

        let cpu_infos = JITCpuInfos {
            cpu_features,
            cpu_name,
        };
        
        let mut compile_context = CompileContext::new(context.rustaml_context, llvm_context, module, builder, DebugInfo { inner: None }, is_optimized, type_infos, target_data, Some(cpu_infos));

        match function_def_ast.get(&compile_context.rustaml_context.ast_pool).clone() {
            ASTNode::FunctionDefinition { name, args, body, type_annotation: _ } => {
                compile_function(&mut compile_context, function_def_ast, name, args, body)
            }
            _ => unreachable!(),
        }
        

        let wrapper_fun_name = get_jit_fun_wrapper_name(func_def.name.get_str(&compile_context.rustaml_context.str_interner));
        let function_id = get_var_id(&compile_context, function_def_ast);
        let (arg_types, res_type) = match compile_context.typeinfos.vars_env.get(&function_id).unwrap() {
            Type::Function(args, res_type, _) => (args.clone(), res_type.as_ref().clone()),
            t => panic!("BUG : the function definition has not a function type, it is {:?} instead", t), // TODO : replace this with an unreachable
        };
        generate_jit_fun_wrapper(&mut compile_context, func_def.name, &wrapper_fun_name, &arg_types, res_type);

        // TODO : just not create the main function instead ?
        
        let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
        compile_context.builder.position_at_end(last_main_bb);
        compile_context.builder.build_return(Some(&compile_context.context.i32_type().const_int(0, false))).unwrap();

        register_external_functions(&lib, &compile_context.module, &execution_engine);

        #[cfg(feature = "debug-llvm")]
        compile_context.module.verify().or_else(|e| -> Result<_, LLVMString> { 
            compile_context.module.print_to_file("jit_error.ll").unwrap();
            panic!("LLVM ERROR {}", e.to_string()) 
        }).unwrap();

        if context.jit_context.dump_jit_ir {
            compile_context.module.print_to_file("jit.ll").unwrap();
        }
        if context.jit_context.dump_jit_ir || context.jit_context.dump_jit_asm {
            let passes_str = format!("default<O{}>", optimization_level as u8);
            // run passes to dump the optimized jit IR (simulate the optimizations by the JIT, should be around the same)
            let tmp_module = compile_context.module.clone();
            tmp_module.run_passes(&passes_str, &target_machine, PassBuilderOptions::create()).unwrap();
            if context.jit_context.dump_jit_ir {
                tmp_module.print_to_file("jit-opt.ll").unwrap();
            }
            if context.jit_context.dump_jit_asm {
                target_machine.write_to_file(&tmp_module, FileType::Assembly, &Path::new("jit.s")).unwrap();
            }
        }


        let fun = unsafe { execution_engine.get_function::<WrapperFN>(&wrapper_fun_name) }.unwrap();    
        
        if let Some(fun_meta) = context.jit_context.functions_meta.get_mut(&function_def_ast){
            fun_meta.cached_fun = Some(fun.clone());
        } else {
            let fun_meta = FuncMeta {
                cached_fun: Some(fun.clone()),
                ..Default::default()
            };
            context.jit_context.functions_meta.insert(function_def_ast, fun_meta);
        }

        
        fun
    };

    let mut val_args = args_val.into_iter().map(|e| create_jit_value(context.rustaml_context, e)).collect::<Box<[JITValue]>>();
    let ret = unsafe { fun.call(val_args.as_mut_ptr()) };
    match ret.tag {
        JITValueTag::Integer => Val::Integer(ret.val as i64),
        JITValueTag::Float => Val::Float(f64::from_bits(ret.val)),
        JITValueTag::Bool => Val::Bool(ret.val != 0),
        JITValueTag::Char => Val::Char(char::from_u32(ret.val.try_into().unwrap()).unwrap()),
        JITValueTag::Str => {
            let cstr = unsafe { CStr::from_ptr(ret.val as *const i8) };
            let str = context.rustaml_context.str_interner.intern_runtime(cstr.to_str().unwrap());
            Val::String(str)
        }
        JITValueTag::List => panic!("List unsupported in return type of JIT"),
    }
}