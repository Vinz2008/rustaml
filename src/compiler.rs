
use std::process::ExitCode;
use crate::{ast::ASTRef, rustaml::RustamlContext};
use inkwell::{builder::Builder, context::Context, module::Module, passes::PassBuilderOptions, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple}, OptimizationLevel};

struct CompileContext<'context, 'llvm_ctx> {
    pub rustaml_context : &'context RustamlContext,
    context : &'llvm_ctx Context,
    module : Module<'llvm_ctx>,
    builder :Builder<'llvm_ctx>
}



fn run_passes_on(module: &Module, optimization_level : OptimizationLevel, target_triple : TargetTriple) {
    Target::initialize_all(&InitializationConfig::default());
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


    // TODO : this probably runs these in any case, ignoring optimization level
    // either :
    // - remove optimizations levels, because optimizations can be very important for basic usage, ex : tail call elimination
    // - add ifs for passes or use default<Onb> (see run_passes doc)

    let passes: &[&str] = &[
        // inkwell default
        "instcombine",
        "reassociate",
        "gvn",
        "simplifycfg",
        // "basic-aa",
        "mem2reg",

        // TODO : is it necessary ?
        "loop-vectorize", 
        "slp-vectorizer",
        
        // added by me, very important (?)
        "tailcallelim",
        // add inlining ?
    ];

    
    module
        .run_passes(passes.join(",").as_str(), &target_machine, PassBuilderOptions::create())
        .unwrap();
}

fn compile_node(compile_context: &mut CompileContext, ast_node : ASTRef) {
    todo!()
}

pub fn compile(ast : ASTRef, rustaml_context: &RustamlContext) -> ExitCode{
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");

    let mut compile_context = CompileContext {
        rustaml_context,
        context : &context,
        module,
        builder
    };

    compile_node(&mut compile_context, ast);


    
    let target_triple = TargetMachine::get_default_triple();
    run_passes_on(&compile_context.module, OptimizationLevel::Default, target_triple);

    ExitCode::SUCCESS
}