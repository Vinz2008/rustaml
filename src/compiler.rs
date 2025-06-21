
use std::{hash::{Hash, Hasher}, path::Path, process::{Command, ExitCode}, time::{SystemTime, UNIX_EPOCH}};
use crate::{ast::{ASTNode, ASTRef, Type}, debug::DebugWrapContext, lexer::Operator, rustaml::RustamlContext, string_intern::StringRef};
use inkwell::{builder::Builder, context::Context, module::Module, passes::PassBuilderOptions, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple}, types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType}, values::{AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue}, AddressSpace, OptimizationLevel};
use pathbuf::pathbuf;
use rustc_hash::{FxHashMap, FxHasher};

struct CompileContext<'context, 'refs, 'llvm_ctx> {
    pub rustaml_context : &'context RustamlContext,
    context : &'llvm_ctx Context,
    module : &'refs Module<'llvm_ctx>,
    builder : &'refs Builder<'llvm_ctx>,
    functions : FxHashMap<StringRef, FunctionValue<'llvm_ctx>>,
    main_function : FunctionValue<'llvm_ctx>,
    var_types : FxHashMap<StringRef, Type>
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


fn get_llvm_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context, rustaml_type : &Type) -> AnyTypeEnum<'llvm_ctx> {
    match rustaml_type {
        Type::Integer => llvm_context.i64_type().into(),
        Type::Bool => llvm_context.i8_type().into(),
        Type::Float => llvm_context.f64_type().into(),
        Type::Function(args, ret) => {
            let ret_llvm = get_llvm_type(llvm_context, &ret);
            // TODO for expect : create a function that would be get_basic_metatadata_type which will transform the function pointers into pointers ?
            let param_types = args.iter().map(|t| get_llvm_type(llvm_context, t).try_into().expect("arg is not a basic metadata type")).collect::<Vec<BasicMetadataTypeEnum>>();
            get_fn_type(llvm_context, ret_llvm, &param_types, false).into()
        },
        Type::List(t) => {
            todo!()
        },

        Type::Str => llvm_context.ptr_type(AddressSpace::default()).into(),
        Type::Unit => llvm_context.void_type().into(),
        Type::Any => panic!("Encountered any when compiling"),

    }
}

fn get_fn_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context, llvm_type : AnyTypeEnum<'llvm_ctx>,  param_types: &[BasicMetadataTypeEnum<'llvm_ctx>], is_var_args: bool) -> FunctionType<'llvm_ctx> {
    match llvm_type {
        AnyTypeEnum::ArrayType(a) => a.fn_type(param_types, is_var_args),
        AnyTypeEnum::FloatType(f) => f.fn_type(param_types, is_var_args),
        AnyTypeEnum::FunctionType(_) => llvm_context.ptr_type(AddressSpace::default()).fn_type(param_types, is_var_args), // consider function pointers just as pointers because llvm doesn't authorize function types as returns (is technically implementation defined, but most of the times it is just a pointer)
        AnyTypeEnum::IntType(i) => i.fn_type(param_types, is_var_args),
        AnyTypeEnum::PointerType(p) => p.fn_type(param_types, is_var_args),
        AnyTypeEnum::VectorType(_) => unreachable!(),
        AnyTypeEnum::ScalableVectorType(_) => unreachable!(), // TODO ?
        AnyTypeEnum::StructType(_) => unreachable!(), // TODO ?
        AnyTypeEnum::VoidType(v) => v.fn_type(param_types, is_var_args),
    }
}

fn get_current_function<'llvm_ctx>(builder: &Builder<'llvm_ctx>) -> FunctionValue<'llvm_ctx> {
    builder.get_insert_block().unwrap().get_parent().unwrap()
}

fn create_entry_block_alloca<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : &str, alloca_type : AnyTypeEnum<'llvm_ctx>) -> PointerValue<'llvm_ctx> 
{
    let builder = compile_context.context.create_builder();
    let entry = get_current_function(&compile_context.builder).get_first_basic_block().unwrap();
    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry),
    }

    dbg!(alloca_type);
    builder.build_alloca(TryInto::<BasicTypeEnum>::try_into(alloca_type).unwrap(), name).unwrap()
}

fn compile_var_decl<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, val : ASTRef, body : Option<ASTRef>) -> AnyValueEnum<'llvm_ctx> {
    let var_type = compile_context.var_types.get(&name).unwrap();
    let var_alloca = create_entry_block_alloca(compile_context, name.get_str(&compile_context.rustaml_context.str_interner), get_llvm_type(compile_context.context, var_type));
    //todo!()

    let val = compile_expr(compile_context, val);

    compile_context.builder.build_store(var_alloca, TryInto::<BasicValueEnum>::try_into(val).unwrap()).unwrap();

    match body {
        Some(b) => compile_expr(compile_context, b),
        None => val,
    }
}

fn compile_if<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, cond_expr : ASTRef, then_body : ASTRef, else_body : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    let this_function = get_current_function(compile_context.builder);
    let then_bb= compile_context.context.append_basic_block(this_function, "if");
    let else_bb = compile_context.context.append_basic_block(this_function, "else");
    let after_bb = compile_context.context.append_basic_block(this_function, "afterif");

    
    // TODO : create br

    let bool_val = compile_expr(compile_context, cond_expr);

    compile_context.builder.build_conditional_branch(TryInto::<IntValue>::try_into(bool_val).unwrap(), then_bb, else_bb).unwrap();

    compile_context.builder.position_at_end(then_bb);

    let if_val = compile_expr(compile_context, then_body);

    compile_context.builder.position_at_end(else_bb);

    let else_val = compile_expr(compile_context, else_body);

    compile_context.builder.position_at_end(after_bb);

    compile_context.builder.build_phi(TryInto::<BasicTypeEnum>::try_into(if_val.get_type()).unwrap(), "if_phi").unwrap().into()
}

fn compile_function_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, args: &[ASTRef]) -> AnyValueEnum<'llvm_ctx>{
    todo!()
}

fn compile_binop<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs : ASTRef, rhs : ASTRef)-> AnyValueEnum<'llvm_ctx> {
    todo!()
}

fn compile_expr<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, ast_node : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    match ast_node.get(&compile_context.rustaml_context.ast_pool){
        ASTNode::Integer { nb } =>  compile_context.context.i64_type().const_int(*nb as u64, false).into(), // TODO : sign extend or not ?
        ASTNode::Float { nb } => compile_context.context.f64_type().const_float(*nb).into(),
        ASTNode::Boolean { b } => compile_context.context.i8_type().const_int(*b as u64, false).into(),
        ASTNode::VarDecl { name, val, body } => compile_var_decl(compile_context, *name, *val, *body),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => compile_if(compile_context, *cond_expr, *then_body, *else_body),
        ASTNode::FunctionCall { name, args } => compile_function_call(compile_context, *name, &args),
        ASTNode::BinaryOp { op, lhs, rhs } => compile_binop(compile_context, *op, *lhs, *rhs),
        t => panic!("unknown AST : {:?}", DebugWrapContext::new(t, compile_context.rustaml_context)), 
        //_ => todo!()
    }
}

// TODO : test to replace AnyValueEnum with &dyn AnyValue ?
fn compile_top_level_node(compile_context: &mut CompileContext, ast_node : ASTRef) {
    match ast_node.get(&compile_context.rustaml_context.ast_pool) {
        ASTNode::FunctionDefinition { name, args, body, return_type } => {
            let return_type_llvm = get_llvm_type(compile_context.context, return_type);
            let param_types = args.iter().map(|a| get_llvm_type(compile_context.context, &a.arg_type).try_into().unwrap()).collect::<Vec<_>>();
            let function_type = get_fn_type(compile_context.context, return_type_llvm, &param_types, false);
            let function = compile_context.module.add_function(name.get_str(&compile_context.rustaml_context.str_interner), function_type, Some(inkwell::module::Linkage::Internal));
            let entry = compile_context.context.append_basic_block(function, "entry");
            compile_context.builder.position_at_end(entry);
            let ret = compile_expr(compile_context, *body);

            let return_val: Option<&dyn BasicValue<'_>> = match return_type {
                Type::Unit => None,
                _ => Some(&TryInto::<BasicValueEnum>::try_into(ret).unwrap()),
            };

            compile_context.builder.build_return(return_val).unwrap();
        },

        ASTNode::VarDecl { name, val, body } => {

            let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
            compile_context.builder.position_at_end(last_main_bb);

            compile_var_decl(compile_context, *name, *val, *body);
        }
        t => panic!("top level node = {:?}", DebugWrapContext::new(t, compile_context.rustaml_context)),
        // _ => unreachable!()
    }
}


fn get_main_function<'llvm_ctx, 'refs>(llvm_context : &'llvm_ctx Context, module : &'refs Module<'llvm_ctx>) -> FunctionValue<'llvm_ctx> {
    let param_types = &[BasicMetadataTypeEnum::IntType(llvm_context.i32_type()), BasicMetadataTypeEnum::PointerType(llvm_context.ptr_type(AddressSpace::default()))];
    let main = module.add_function("main", llvm_context.i32_type().fn_type(param_types, false), Some(inkwell::module::Linkage::External));
    llvm_context.append_basic_block(main, "entry");
    main
}


fn link_exe(filename_out : &Path, bitcode_file : &Path){
    // use cc ?
    // TODO : use lld (https://github.com/mun-lang/lld-rs) for linking instead ?
    // TODO
    //Command::new("clang").arg("-o").arg(filename_out).arg(bitcode_file).spawn().expect("linker failed");
}

pub fn compile(ast : ASTRef, var_types : FxHashMap<StringRef, Type>, rustaml_context: &RustamlContext, filename : &Path, filename_out : &Path, should_keep_temp : bool) -> ExitCode{
    let context = Context::create();
    let builder = context.create_builder();

    let filename_str = filename.as_os_str().to_str().expect("not UTF-8 filename");
    let module = context.create_module(filename_str);

    let main_function = get_main_function(&context, &module);

    let mut compile_context = CompileContext {
        rustaml_context,
        context : &context,
        module: &module,
        builder: &builder,
        functions: FxHashMap::default(),
        main_function,
        var_types
    };

    let top_level_nodes = match ast.get(&rustaml_context.ast_pool) {
        ASTNode::TopLevel { nodes } => nodes,
        _ => unreachable!(),
    };

    for n in top_level_nodes {
        compile_top_level_node(&mut compile_context, *n);
    }

    let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
    compile_context.builder.position_at_end(last_main_bb);
    compile_context.builder.build_return(Some(&compile_context.context.i32_type().const_int(0, false))).unwrap();


    
    let target_triple = TargetMachine::get_default_triple();
    // TODO : readd this for optimizations
    //run_passes_on(&compile_context.module, OptimizationLevel::Default, target_triple);


    let temp_path = if should_keep_temp {
        Path::new(".").to_owned() 
    } else { 
        std::env::temp_dir()
    };


    let filename_without_ext = filename.file_stem().unwrap().to_str().expect("not UTF-8 filename").to_owned();

    
    let filename_with_hash = if should_keep_temp {
        filename_without_ext
    } else {
    
        let mut hasher = FxHasher::default();

        let start = SystemTime::now();
        let since_the_epoch = start
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards");

        since_the_epoch.as_millis().hash(&mut hasher);

        let hash = hasher.finish().to_string();
        
        format!("{}-{}", &filename_without_ext, &hash)
    };

    
    let temp_path_bitcode = pathbuf![&temp_path, &format!("{}.bc", &filename_with_hash)];
    let temp_path_ir = pathbuf![&temp_path, &format!("{}.ll", &filename_with_hash)];

    
    if should_keep_temp {
        compile_context.module.print_to_file(&temp_path_ir).expect("Couldn't write llvm ir file");
    }
    
    compile_context.module.write_bitcode_to_path(&temp_path_bitcode);


    

    link_exe(filename_out,  &temp_path_bitcode);

    ExitCode::SUCCESS
}