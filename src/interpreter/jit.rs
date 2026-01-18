use inkwell::{OptimizationLevel, builder::Builder, context::Context, module::Module};
use rustc_hash::FxHashMap;

use crate::{ast::ASTRef, interpreter::{FunctionBody, FunctionDef, InterpretContext, Val}};

pub(crate) struct JitContext {
    functions_count : FxHashMap<ASTRef, u64>, // ASTRef of function
    // TODO : replace these with compileContext
    context : &'static Context,
    module: Module<'static>,
    builder: Builder<'static>,
}

impl JitContext {
    pub(crate) fn new() -> JitContext {
        // box the context and leak it because only 8 bytes, and to prevent lifetime annotation (can they be optionnal) (TODO ?)
        let context = Box::new(Context::create());
        let context = Box::leak(context);
        let module = context.create_module("jit");
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        JitContext { 
            functions_count: FxHashMap::default(),
            context,
            module,
            builder: context.create_builder(),
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
    match func_def.body {
        FunctionBody::Ast(ast) => {
            if let Some(count) = context.jit_context.functions_count.get(&ast) {
                *count > 100
            } else {
                false
            }
            
        }
        _ => false
    }
}

pub(crate) fn call_jit_function(context : &InterpretContext, func_def : &FunctionDef) -> Val {

    todo!()
}