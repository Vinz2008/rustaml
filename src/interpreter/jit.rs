use inkwell::{OptimizationLevel, builder::Builder, context::Context, execution_engine::ExecutionEngine, module::Module, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine}};
use rustc_hash::FxHashMap;

use crate::{ast::ASTRef, compiler::{CompileContext, compile_top_level_node, debuginfo::DebugInfo}, interpreter::{FunctionBody, FunctionDef, InterpretContext, Val}, rustaml::RustamlContext, types::TypeInfos};

#[cfg(feature = "debug-llvm")]
use inkwell::support::LLVMString;

pub(crate) struct JitContext {
    functions_count : FxHashMap<ASTRef, u64>, // ASTRef of function
    context : &'static Context,
    type_infos : Option<TypeInfos>,
}

impl JitContext {
    pub(crate) fn new(type_infos : Option<TypeInfos>) -> JitContext {
        // box the context and leak it because only 8 bytes, and to prevent lifetime annotation (can they be optionnal) (TODO ?)
        let context = Box::new(Context::create());
        let context = Box::leak(context);
        

        JitContext { 
            functions_count: FxHashMap::default(),
            context,
            type_infos,
        }
    }
}

pub(crate) fn update_jit_heuristics_function_call(context : &mut InterpretContext, ast : ASTRef){
    match context.jit_context.functions_count.get_mut(&ast){
        Some(r) => *r += 1,
        None  => {
            context.jit_context.functions_count.insert(ast, 0);
        },
    }
}

pub(crate) fn should_use_jit_function(context : &InterpretContext, func_def : &FunctionDef) -> bool {
    // TODO : better heuristics
    if context.jit_context.type_infos.is_none() {
        return false;
    }
    match func_def.body {
        FunctionBody::Ast(ast) => {
            if let Some(count) = context.jit_context.functions_count.get(&ast) {
                *count > 10
            } else {
                false
            }
            
        }
        _ => false
    }
}

type OpaqueJITFn = unsafe extern "C" fn(*mut std::ffi::c_void) -> *mut std::ffi::c_void;

type TestLen = unsafe extern "C" fn(i64) -> i64;


pub(crate) fn call_jit_function(context : &mut InterpretContext, func_def : &FunctionDef, args_val : Vec<Val>) -> Val {
    let llvm_context = context.jit_context.context;
    let module = llvm_context.create_module("jit");
    let optimization_level = OptimizationLevel::None;
    let execution_engine = module.create_jit_execution_engine(optimization_level).unwrap();

    let function_def_ast = func_def.function_def_ast.unwrap();

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
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let target_data = target_machine.get_target_data();

    let type_infos = context.jit_context.type_infos.as_ref().unwrap().clone(); // TODO : prevent this clone ?

    let mut compile_context = CompileContext::new(context.rustaml_context, llvm_context, module, builder, DebugInfo { inner: None }, is_optimized, type_infos, target_data, true);
    
    compile_top_level_node(&mut compile_context, function_def_ast);

    // TODO : just not create the main function instead ?
    
    let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
    compile_context.builder.position_at_end(last_main_bb);
    compile_context.builder.build_return(Some(&compile_context.context.i32_type().const_int(0, false))).unwrap();

    #[cfg(feature = "debug-llvm")]
    compile_context.module.verify().or_else(|e| -> Result<_, LLVMString> { 
        compile_context.module.print_to_file("jit_error.ll").unwrap();
        panic!("LLVM ERROR {}", e.to_string()) 
    }).unwrap();

    // TODO : add this with a flag (ex : --dump-jit)
    compile_context.module.print_to_file("jit.ll").unwrap();

    let function_name = func_def.name.get_str(&compile_context.rustaml_context.str_interner);

    dbg!(function_name);

    // TODO : make this work with any function
    let fun = dbg!(unsafe { execution_engine.get_function::<TestLen>(function_name) }).unwrap();    
    let arg = match args_val[0] {
        Val::Integer(i) => i,
        _ => unreachable!(),
    };
    let ret = unsafe { fun.call(arg) };
    return Val::Integer(ret);
    //let ret = unsafe { context.jit_context.execution_engine.get_function::<OpaqueJITFn>(function_name).ok() }.unwrap();
    
    // use libffi ?

    todo!()
}