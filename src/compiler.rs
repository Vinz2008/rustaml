use std::{hash::{Hash, Hasher}, io::Write, path::Path, process::{Command, ExitCode, Stdio}, time::{SystemTime, UNIX_EPOCH}};
use crate::{ast::{ASTNode, ASTRef, Pattern, Type}, debug::DebugWrapContext, lexer::Operator, rustaml::RustamlContext, string_intern::StringRef};
use inkwell::{attributes::Attribute, basic_block::BasicBlock, builder::Builder, context::Context, module::{Linkage, Module}, passes::PassBuilderOptions, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine}, types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, StructType}, values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue}, AddressSpace, Either, FloatPredicate, IntPredicate, OptimizationLevel};
use pathbuf::pathbuf;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

struct CompileContext<'context, 'refs, 'llvm_ctx> {
    pub rustaml_context : &'context RustamlContext,
    context : &'llvm_ctx Context,
    module : &'refs Module<'llvm_ctx>,
    builder : &'refs Builder<'llvm_ctx>,
    functions : FxHashMap<StringRef, FunctionValue<'llvm_ctx>>,
    main_function : FunctionValue<'llvm_ctx>,
    var_types : FxHashMap<StringRef, Type>,
    var_vals : FxHashMap<StringRef, PointerValue<'llvm_ctx>>,
    external_functions_declared : FxHashSet<&'static str>,
    internal_functions : Vec<BuiltinFunction<'llvm_ctx>>,
}

#[derive(Clone)]
struct BuiltinFunction<'llvm_ctx> {
    name : &'static str,
    args : Vec<BasicMetadataTypeEnum<'llvm_ctx>>,
    ret : AnyTypeEnum<'llvm_ctx>,
}


// TODO : replace these strings with an enum ?
fn get_internal_functions<'llvm_ctx>(llvm_context : &'llvm_ctx Context) -> Vec<BuiltinFunction<'llvm_ctx>>{
    let ptr_type = llvm_context.ptr_type(AddressSpace::default()).into();
    vec![
        BuiltinFunction {
            name: "__str_cmp",
            args: vec![ptr_type, ptr_type],
            ret: llvm_context.i8_type().into()
        },
        BuiltinFunction {
            name: "__str_append",
            args: vec![ptr_type, ptr_type],
            ret: llvm_context.ptr_type(AddressSpace::default()).into(),
        },
        BuiltinFunction {
            name: "__list_node_append",
            args: vec![ptr_type, llvm_context.i8_type().into(), llvm_context.i64_type().into()],
            ret: llvm_context.ptr_type(AddressSpace::default()).into(),
        }
    ]
}

impl<'context, 'refs, 'llvm_ctx> CompileContext<'context, 'refs, 'llvm_ctx> {
    fn get_internal_function(&mut self, name : &'static str) -> FunctionValue<'llvm_ctx> {
        if self.external_functions_declared.contains(name){
            self.module.get_function(name).unwrap()
        } else {
            // use find instead of a hashmap because the number of internal functions is low
            let builtin_function = self.internal_functions.iter().find(|f| f.name == name).unwrap();
            let function_type = get_fn_type(self.context, builtin_function.ret, &builtin_function.args, false);
            let function_decl = self.module.add_function(name, function_type, Some(Linkage::External));
            self.external_functions_declared.insert(name);
            function_decl
        }
    }
}

// TODO : add a print function that returns unit for the compiler


fn get_type_tag(t : &Type) -> u8 {
    match t {
        Type::Integer => 0,
        Type::Float => 1,
        Type::Bool => 2,
        Type::Function(_, _) => 3,
        Type::Str => 4,
        Type::List(_) => 5,
        Type::Any | Type::Unit => panic!("no type tag for this type {:?} !!", t),
    }
}

fn get_type_tag_val<'llvm_ctx>(llvm_context : &'llvm_ctx Context, t : &Type) -> IntValue<'llvm_ctx> {
    llvm_context.i8_type().const_int(get_type_tag(t) as u64, false)
}

fn get_list_type<'llvm_ctx>(llvm_context: &'llvm_ctx Context) -> StructType<'llvm_ctx>{
    // put it in an Optional in the context

    let field_types: &[BasicTypeEnum] = &[llvm_context.i8_type().into(), llvm_context.i64_type().into(), llvm_context.ptr_type(AddressSpace::default()).into()];
    llvm_context.struct_type(field_types, false)
}


// TODO : make strings a pointer to a struct with a string and a len
fn get_llvm_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context, rustaml_type : &Type) -> AnyTypeEnum<'llvm_ctx> {
    match rustaml_type {
        Type::Integer => llvm_context.i64_type().into(),
        Type::Bool => llvm_context.i8_type().into(),
        Type::Float => llvm_context.f64_type().into(),
        Type::Function(args, ret) => {
            let ret_llvm = get_llvm_type(llvm_context, ret);
            // TODO for expect : create a function that would be get_basic_metatadata_type which will transform the function pointers into pointers ?
            let param_types = args.iter().map(|t| get_llvm_type(llvm_context, t).try_into().expect("arg is not a basic metadata type")).collect::<Vec<BasicMetadataTypeEnum>>();
            get_fn_type(llvm_context, ret_llvm, &param_types, false).into()
        },

        // TODO : layout of list
        // struct ListNode {
        //      uint8_t type_tag;
        //      void* val; // can be also a i64 or f64 depending on type_tag
        //      struct ListNode* next; // if empty null 
        // }
        Type::List(_t) => llvm_context.ptr_type(AddressSpace::default()).into(), // TODO ?
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
        AnyTypeEnum::StructType(_) => unreachable!(),
        AnyTypeEnum::VoidType(v) => v.fn_type(param_types, is_var_args),
    }
}

fn get_current_function<'llvm_ctx>(builder: &Builder<'llvm_ctx>) -> FunctionValue<'llvm_ctx> {
    builder.get_insert_block().unwrap().get_parent().unwrap()
}

fn create_entry_block_alloca<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : &str, alloca_type : AnyTypeEnum<'llvm_ctx>) -> PointerValue<'llvm_ctx> 
{
    let builder = compile_context.context.create_builder();
    let entry = get_current_function(compile_context.builder).get_first_basic_block().unwrap();
    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry),
    }

    dbg!(alloca_type);
    builder.build_alloca(TryInto::<BasicTypeEnum>::try_into(alloca_type).unwrap(), name).unwrap()
}

fn create_var<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, val : AnyValueEnum<'llvm_ctx>, alloca_type : AnyTypeEnum<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    let var_alloca = create_entry_block_alloca(compile_context, name.get_str(&compile_context.rustaml_context.str_interner), alloca_type);
    compile_context.builder.build_store(var_alloca, TryInto::<BasicValueEnum>::try_into(val).unwrap()).unwrap();
    compile_context.var_vals.insert(name, var_alloca);
    var_alloca
}

fn runtime_error<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, message : &str){
    // TODO : only if in a hashset of already added symbols (is it needed ?)
    let ptr_type = compile_context.context.ptr_type(AddressSpace::default());
    
    let fprintf_param_types: Vec<BasicMetadataTypeEnum> = vec![ptr_type.into(), ptr_type.into()];
    let fprintf_type = get_fn_type(compile_context.context, compile_context.context.i32_type().into(), &fprintf_param_types, true);
    let fprintf_fun = compile_context.module.add_function("fprintf", fprintf_type, Some(inkwell::module::Linkage::External));



    let exit_param_types: Vec<BasicMetadataTypeEnum> = vec![compile_context.context.i32_type().into()];
    let exit_type = get_fn_type(compile_context.context, compile_context.context.void_type().into(), &exit_param_types, false);
    let exit_fun = compile_context.module.add_function("exit", exit_type, Some(inkwell::module::Linkage::External));
    let noreturn_id = Attribute::get_named_enum_kind_id("noreturn");
    let noreturn_attr = compile_context.context.create_enum_attribute(noreturn_id, 0);
    exit_fun.add_attribute(inkwell::attributes::AttributeLoc::Function, noreturn_attr);

    let stderr_global = compile_context.module.add_global(ptr_type, None, "stderr");
    stderr_global.set_linkage(inkwell::module::Linkage::External);
    let message_str  = compile_context.builder.build_global_string_ptr(&format!("{}\n", message), "error_message").unwrap();
    let stderr_load = compile_context.builder.build_load(ptr_type, stderr_global.as_pointer_value(), "stderr_load").unwrap();
    let fprintf_args: Vec<BasicMetadataValueEnum> = vec![stderr_load.into(), message_str.as_pointer_value().into()];
    compile_context.builder.build_call(fprintf_fun, &fprintf_args, "error_fprintf").unwrap();

    let exit_args = vec![compile_context.context.i32_type().const_int(1, false).into()];
    compile_context.builder.build_call(exit_fun, &exit_args, "error_exit").unwrap();
    compile_context.builder.build_unreachable().unwrap();
}

fn compile_var_decl<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, val : ASTRef, body : Option<ASTRef>, is_global : bool) -> AnyValueEnum<'llvm_ctx> {
    dbg!(DebugWrapContext::new(&compile_context.var_types, compile_context.rustaml_context));
    dbg!(name.get_str(&compile_context.rustaml_context.str_interner));
    let var_type = compile_context.var_types.get(&name).unwrap_or_else(|| panic!("No type found for var {}", name.get_str(&compile_context.rustaml_context.str_interner)));
    let alloca_type = get_llvm_type(compile_context.context, var_type);
    
    // TODO : if is global and the val is const, just generate a global var
    let val = compile_expr(compile_context, val);
    
    create_var(compile_context, name, val, alloca_type);

    
    let ret = match body {
        Some(b) => compile_expr(compile_context, b),
        None => val,
    };

    if !is_global {
        compile_context.var_vals.remove(&name);
    }

    ret
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
    compile_context.builder.build_unconditional_branch(after_bb).unwrap();

    compile_context.builder.position_at_end(else_bb);

    let else_val = compile_expr(compile_context, else_body);
    compile_context.builder.build_unconditional_branch(after_bb).unwrap();

    compile_context.builder.position_at_end(after_bb);

    let phi_node = compile_context.builder.build_phi(TryInto::<BasicTypeEnum>::try_into(if_val.get_type()).unwrap(), "if_phi").unwrap();
    let if_val_basic = TryInto::<BasicValueEnum>::try_into(if_val).unwrap();
    let else_val_basic = TryInto::<BasicValueEnum>::try_into(else_val).unwrap();
    phi_node.add_incoming(vec![(&if_val_basic as _, then_bb), (&else_val_basic as _, else_bb)].as_slice());
    phi_node.as_any_value_enum()
}

fn compile_function_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, args: &[ASTRef]) -> AnyValueEnum<'llvm_ctx>{
    let args_vals = args.iter().map(|a| compile_expr(compile_context, *a).try_into().unwrap()).collect::<Vec<BasicMetadataValueEnum>>();
    let ret = compile_context.builder.build_call(*compile_context.functions.get(&name).unwrap(), args_vals.as_slice(), name.get_str(&compile_context.rustaml_context.str_interner)).unwrap().try_as_basic_value();
    match ret {
        Either::Left(l) => l,
        Either::Right(_) => compile_context.context.i64_type().const_int(0, false).into(), // void, dummy value
    }.into()
}

fn compile_binop_nb<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs_val : AnyValueEnum<'llvm_ctx>, rhs_val : AnyValueEnum<'llvm_ctx>, name : &str) -> AnyValueEnum<'llvm_ctx>{
    
    match (lhs_val, rhs_val){
        (AnyValueEnum::IntValue(i),  AnyValueEnum::IntValue(i2)) => {
            match op {
                Operator::Plus => compile_context.builder.build_int_add(i, i2, name).unwrap(),
                Operator::Minus => compile_context.builder.build_int_sub(i, i2, name).unwrap(),
                // TODO : add check for overflow like in rust
                Operator::Mult => compile_context.builder.build_int_mul(i, i2, name).unwrap(),
                Operator::Div => compile_context.builder.build_int_signed_div(i, i2, name).unwrap(),
                _ => unreachable!(),
            }.into()
        }
        (AnyValueEnum::FloatValue(f),  AnyValueEnum::FloatValue(f2)) => {
            match op {
                Operator::Plus => compile_context.builder.build_float_add(f, f2, name).unwrap(),
                Operator::Minus => compile_context.builder.build_float_sub(f, f2, name).unwrap(),
                Operator::Mult => compile_context.builder.build_float_mul(f, f2, name).unwrap(),
                Operator::Div => compile_context.builder.build_float_div(f, f2, name).unwrap(),
                _ => unreachable!(),
            }.into()
        },
        _ => panic!("Invalid type for nb op {:?}", op),
    }
    
}

// TODO : replace most of AnyValueEnum with BasicValueEnum ?
fn compile_binop_bool<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs_val : AnyValueEnum<'llvm_ctx>, rhs_val : AnyValueEnum<'llvm_ctx>, operand_type : Type, name : &str) -> IntValue<'llvm_ctx>{

    match (lhs_val, rhs_val){
        (AnyValueEnum::IntValue(i),  AnyValueEnum::IntValue(i2)) => {
            let predicate = match op {
                Operator::IsEqual => IntPredicate::EQ,
                Operator::IsNotEqual => IntPredicate::NE,
                Operator::Inferior => IntPredicate::SLT,
                Operator::Superior => IntPredicate::SGT,
                Operator::InferiorOrEqual => IntPredicate::SLE,
                Operator::SuperiorOrEqual => IntPredicate::SGE,
                _ => unreachable!(),
            };
            compile_context.builder.build_int_compare(predicate, i, i2, name).unwrap()
        },
        (AnyValueEnum::FloatValue(f),  AnyValueEnum::FloatValue(f2)) => {
            let predicate = match op {
                Operator::Equal => FloatPredicate::OEQ,
                Operator::IsNotEqual => FloatPredicate::ONE,
                Operator::Inferior => FloatPredicate::OLT,
                Operator::Superior => FloatPredicate::OGT,
                Operator::InferiorOrEqual => FloatPredicate::OLE,
                Operator::SuperiorOrEqual => FloatPredicate::OGE,
                _ => unreachable!(),
            };
            compile_context.builder.build_float_compare(predicate, f, f2, name).unwrap()
        },
        // TODO : add comparison of list and strings (need to add a lhs_type arg to match it here to differentiate pointers to lists and pointers to strings)
        (AnyValueEnum::PointerValue(p),  AnyValueEnum::PointerValue(p2)) => {
            let args = vec![p.into(), p2.into()];
            let cmp_call = match operand_type {
                Type::List(_) => {
                    compile_context.builder.build_call(compile_context.get_internal_function("__list_cmp"), &args, name)
                },
                Type::Str => {
                    compile_context.builder.build_call(compile_context.get_internal_function("__str_cmp"), &args, name)
                },
                _ => unreachable!(),
            };

            cmp_call.unwrap().as_any_value_enum().into_int_value()
        },
        _ => panic!("Invalid type for bool op {:?}", op),
    }
}

fn compile_binop_str<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs_val : AnyValueEnum<'llvm_ctx>, rhs_val : AnyValueEnum<'llvm_ctx>, name : &str) -> AnyValueEnum<'llvm_ctx>{
    match op {
        Operator::StrAppend => {
            let str_append = compile_context.get_internal_function("__str_append");
            let args = vec![lhs_val.try_into().unwrap(), rhs_val.try_into().unwrap()];
            compile_context.builder.build_call(str_append, &args, name).unwrap().as_any_value_enum()
        },
        _ => unreachable!()
    }
}

fn compile_binop_list<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs_val : AnyValueEnum<'llvm_ctx>, rhs_val : AnyValueEnum<'llvm_ctx>, elem_type : &Type) -> AnyValueEnum<'llvm_ctx>{
    

    // TODO : check that lhs_val is a val of the same tag as the rhs_val elements and that rhs_val is a list
    match op {
        Operator::ListAppend => {
            // TODO : to make this work, need to have better type inference for case like i :: l to not stop when finding that l is a List::Any, but also add the info that l is appended an int, so l is a List(Int)
            dbg!(lhs_val, rhs_val);
            dbg!(elem_type);
            let type_tag_val = get_type_tag_val(compile_context.context, elem_type);
            create_list_append_call(compile_context, rhs_val.into_pointer_value(), type_tag_val, lhs_val).into()
        },
        _ => unreachable!(),
    }
}

fn compile_binop<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs : ASTRef, rhs : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    let name = format!("{:?}", op).to_lowercase();
    let lhs_val = compile_expr(compile_context, lhs);
    let rhs_val = compile_expr(compile_context, rhs);
    match op.get_type() {
        Type::Integer => compile_binop_nb(compile_context, op, lhs_val, rhs_val, &name),
        Type::Bool => compile_binop_bool(compile_context, op, lhs_val, rhs_val, lhs.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types), &name).into(),
        Type::Str => compile_binop_str(compile_context, op, lhs_val, rhs_val, &name),
        // here do not trust the -e (it is Type::Any), use get_type on the head
        Type::List(_e) => compile_binop_list(compile_context, op, lhs_val, rhs_val, /*e.as_ref()*/ &lhs.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types)),
        _ => unreachable!(),
    }
}

fn compile_var_use<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef) -> AnyValueEnum<'llvm_ctx> {
    
    for (v, v_t) in &compile_context.var_types {
        println!("{} = {:?}", v.get_str(&compile_context.rustaml_context.str_interner), v_t);
    }

    let var_type = compile_context.var_types.get(&name).unwrap_or_else(|| panic!("Unknown variable {:?}", name.get_str(&compile_context.rustaml_context.str_interner)));
    let load_type = get_llvm_type(compile_context.context, var_type);
    let load_basic_type = TryInto::<BasicTypeEnum>::try_into(load_type).unwrap();
    let ptr = compile_context.var_vals.get(&name).unwrap();
    compile_context.builder.build_load(load_basic_type, *ptr, name.get_str(&compile_context.rustaml_context.str_interner)).unwrap().into()
}

fn create_list_append_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : PointerValue<'llvm_ctx>, type_tag_val : IntValue<'llvm_ctx>, val : AnyValueEnum<'llvm_ctx> ) -> PointerValue<'llvm_ctx> {
    let function = compile_context.get_internal_function("__list_node_append");
    dbg!(function);
    let args = &[list.into(), type_tag_val.into(), val.try_into().unwrap()];
    compile_context.builder.build_call(function, args, "list_append").unwrap().as_any_value_enum().into_pointer_value()
}

fn compile_static_list<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : &[ASTRef]) -> AnyValueEnum<'llvm_ctx> {
    let mut current_node = compile_context.context.ptr_type(AddressSpace::default()).const_null();
    
    // TODO : optimize this by keeping the last node and just appending to it to not have to go through the list each time by doing append ?

    let list_element_type = match list.first() {
        Some(f) => f.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types),
        None => Type::Integer// empty list, so type will not be used, dummy time
    };

    /*for e in list {
        println!("e = {:?}", DebugWrapContext::new(e, compile_context.rustaml_context));
    }


    dbg!(&list_element_type);*/

    let type_tag_val = get_type_tag_val(compile_context.context, &list_element_type);

    for e in list {
        let val = compile_expr(compile_context, *e);
        let node_val = create_list_append_call(compile_context, current_node, type_tag_val, val);

        current_node = node_val;
    }

    current_node.into()
}


fn load_type_tag<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : PointerValue<'llvm_ctx>) -> IntValue<'llvm_ctx> {
    let list_type = get_list_type(compile_context.context);
    let gep_ptr = compile_context.builder.build_struct_gep(list_type, list, 0, "load_type_tag_gep").unwrap();
    compile_context.builder.build_load(compile_context.context.i8_type(), gep_ptr, "load_tag_gep").unwrap().into_int_value()
}

fn load_list_val<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, elem_type : &Type, list : PointerValue<'llvm_ctx>) -> BasicValueEnum<'llvm_ctx> {
    let list_type = get_list_type(compile_context.context);
    let gep_ptr = compile_context.builder.build_struct_gep(list_type, list, 1, "load_list_val_gep").unwrap();
    let elem_type_llvm = get_llvm_type(compile_context.context, elem_type);
    compile_context.builder.build_load( TryInto::<BasicTypeEnum>::try_into(elem_type_llvm).unwrap(), gep_ptr, "load_val_gep").unwrap()
}

fn load_list_tail<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : PointerValue<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    let list_type = get_list_type(compile_context.context);
    let gep_ptr = compile_context.builder.build_struct_gep(list_type, list, 2, "load_list_tail_gep").unwrap();
    compile_context.builder.build_load(compile_context.context.ptr_type(AddressSpace::default()), gep_ptr, "load_tail_gep").unwrap().into_pointer_value()
}

fn compile_pattern_match_bool_val<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : &Pattern, matched_val : AnyValueEnum<'llvm_ctx>) -> IntValue<'llvm_ctx>{
    match pattern {
        Pattern::Integer(i) => compile_context.builder.build_int_compare(inkwell::IntPredicate::EQ, matched_val.try_into().unwrap(), compile_context.context.i64_type().const_int(*i as u64, false), "match_int_cmp").unwrap(),
        Pattern::Range(lower, upper, inclusivity) => {
            let (lower_predicate, upper_predicate) = if *inclusivity {
                (IntPredicate::SLE, IntPredicate::SGE)
            }  else {
                (IntPredicate::SLT, IntPredicate::SGT)
            };
            // TODO : replace with compile function like below ?
            let lower_cmp = compile_context.builder.build_int_compare(lower_predicate, matched_val.try_into().unwrap(), compile_context.context.i64_type().const_int(*lower as u64, false), "match_int_cmp_range_lower").unwrap();
            let upper_cmp = compile_context.builder.build_int_compare(upper_predicate, matched_val.try_into().unwrap(), compile_context.context.i64_type().const_int(*upper as u64, false), "match_int_cmp_range_upper").unwrap();
            
            // TODO : instead of creating a and, hotplug this with multiple branches ? (return a vec with the branches that need to be made ?)
            let combined_bool_val = compile_context.builder.build_and(lower_cmp, upper_cmp, "match_range_and").unwrap();
            combined_bool_val
        }
        Pattern::VarName(_) => compile_context.context.bool_type().const_int(true as u64, false),
        Pattern::List(pattern_list) => {
            
            // empty list (just for now before the todo is done ?)
            if pattern_list.is_empty(){
                return compile_context.builder.build_int_compare(IntPredicate::EQ, matched_val.into_pointer_value(), compile_context.context.ptr_type(AddressSpace::default()).const_null(), "match_list_empty").unwrap();
            }
            
            
            // TODO
            // generate all the code, create a loop instead ? (no need for this function instead ?)
            // create an impl of into for Pattern to AnyValue ?
            todo!()
            /*for p in pattern_list {
                let val_list = 
                let pattern_bool = compile_pattern_match_bool_val(compile_context, p, );
            }*/
        },
        // the type should be checked before (TODO ?)
        Pattern::ListDestructure(_, _) => compile_context.builder.build_int_compare(IntPredicate::NE, matched_val.into_pointer_value(), compile_context.context.ptr_type(AddressSpace::default()).const_null(), "cmp_destructure_empty").unwrap(),
        // TODO
        p => panic!("unknown pattern {:?}", DebugWrapContext::new(p, compile_context.rustaml_context)),
        //_ => unreachable!()
    }
}


// init vars, etc
fn compile_pattern_match_prologue<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : &Pattern, matched_val : AnyValueEnum<'llvm_ctx>, matched_val_type : &Type){
    match pattern {
        Pattern::VarName(n) => {
            //let matched_val_type = matched_val.get_type();
            let matched_val_type_llvm = get_llvm_type(compile_context.context, matched_val_type);
            create_var(compile_context, *n, matched_val, matched_val_type_llvm);
            compile_context.var_types.insert(*n, matched_val_type.clone());
        }
        Pattern::ListDestructure(head, tail) => {
            let element_type = match matched_val_type {
                Type::List(e) => e.as_ref(),
                _ => unreachable!(),
            };
            
            // TODO : add this before (during AST -> so need to have a list of stacks instead of a hashmap ? or a hashmap of vecs ?)
            // TODO : would need a stack to use the old_val (use a HashMap of Vec is another solution to use during compilation, so there's more work because need to find the type of match multiple types during compilation, but could have just good caching instead ?)
            let _old_val = compile_context.var_types.insert(*head, element_type.clone());

            let element_type_llvm = get_llvm_type(compile_context.context, element_type);
            let matched_val_list = matched_val.into_pointer_value();
            let head_val = load_list_val(compile_context, element_type, matched_val_list);
            create_var(compile_context, *head, head_val.into(), element_type_llvm);
            let tail_val = load_list_tail(compile_context, matched_val_list);
            compile_pattern_match_prologue(compile_context, tail.as_ref(), tail_val.into(), matched_val_type);

            /*if let Some(old_v) = old_val {
                // ...
            }*/
        }
       _ => {}
    }
}

fn compile_pattern_match_epilogue<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : &Pattern){
    match pattern {
        Pattern::VarName(name) => {
            compile_context.var_types.remove(name);
        }
        Pattern::ListDestructure(head, _tail) => {
            compile_context.var_types.remove(head);
        }
        _ => {}
    }
}

fn compile_pattern_match<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : &Pattern, matched_val : AnyValueEnum<'llvm_ctx>, bb: BasicBlock<'llvm_ctx>, else_bb : BasicBlock<'llvm_ctx>){
    let bool_val = compile_pattern_match_bool_val(compile_context, pattern, matched_val);

    compile_context.builder.build_conditional_branch(bool_val, bb, else_bb).unwrap();
}

fn compile_match<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, match_node : ASTRef, matched_expr : ASTRef, patterns : &[(Pattern, ASTRef)]) -> AnyValueEnum<'llvm_ctx> {
    
    let matched_val = compile_expr(compile_context, matched_expr);
    let matched_val_type = matched_expr.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types);
    let function = get_current_function(compile_context.builder);


    // TODO : instead of calling get_type here, get all the types and store them in the nodes during parsing ?
    let match_type = match_node.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types);
    let match_type_llvm: AnyTypeEnum = get_llvm_type(compile_context.context, &match_type);

    let mut match_bbs = Vec::new();
    for _ in patterns {
        let match_bb = compile_context.context.append_basic_block(function, "match_case");
        let match_else_bb = compile_context.context.append_basic_block(function, "match_else");
        match_bbs.push((match_bb, match_else_bb));
    }

    let after_match = compile_context.context.append_basic_block(function, "after_match");

    let mut pattern_vals = Vec::new();

    for (pattern, pattern_bbs) in patterns.iter().zip(&match_bbs) {
        let (pattern, pattern_body) = pattern;
        let (pattern_bb, pattern_else_bb) = pattern_bbs;
        compile_pattern_match(compile_context, pattern, matched_val, *pattern_bb, *pattern_else_bb);
        compile_context.builder.position_at_end(*pattern_bb);
        compile_pattern_match_prologue(compile_context, pattern, matched_val, &matched_val_type);
        let pattern_body_val = compile_expr(compile_context, *pattern_body);
        compile_pattern_match_epilogue(compile_context, pattern);
        compile_context.builder.build_unconditional_branch(after_match).unwrap();
        pattern_vals.push(pattern_body_val);
        compile_context.builder.position_at_end(*pattern_else_bb);
    }

    // TODO : exit with error if no case was matched
    // TODO : add line number ? 
    runtime_error(compile_context, "no match branch was found");

    compile_context.builder.position_at_end(after_match);
    let phi_node = compile_context.builder.build_phi(TryInto::<BasicTypeEnum>::try_into(match_type_llvm).unwrap(), "match_phi").unwrap();
    let mut incoming_phi = Vec::new();
    for (val, (bb, _else_bb)) in pattern_vals.iter().zip(&match_bbs) {
        let basic_val = TryInto::<BasicValueEnum>::try_into(*val).unwrap();
        incoming_phi.push((basic_val, *bb));
    }

    let incoming_phi = incoming_phi.iter().map(|(val, bb)| (val as &dyn BasicValue, *bb)).collect::<Vec<_>>();

    phi_node.add_incoming(&incoming_phi);
    
    phi_node.as_any_value_enum()
}

fn compile_str<'llvm_ctx>(builder: &Builder<'llvm_ctx>, str : &str) -> PointerValue<'llvm_ctx> {
    builder.build_global_string_ptr(str, "str").unwrap().as_pointer_value()
}

fn compile_expr<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, ast_node : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    match ast_node.get(&compile_context.rustaml_context.ast_pool){
        ASTNode::Integer { nb } =>  compile_context.context.i64_type().const_int(*nb as u64, false).into(), // TODO : sign extend or not ?
        ASTNode::Float { nb } => compile_context.context.f64_type().const_float(*nb).into(),
        ASTNode::Boolean { b } => compile_context.context.i8_type().const_int(*b as u64, false).into(),
        ASTNode::String { str } => compile_str(compile_context.builder, str.get_str(&compile_context.rustaml_context.str_interner)).into(),
        ASTNode::VarDecl { name, val, body } => compile_var_decl(compile_context, *name, *val, *body, false),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => compile_if(compile_context, *cond_expr, *then_body, *else_body),
        ASTNode::FunctionCall { name, args } => compile_function_call(compile_context, *name, args),
        ASTNode::BinaryOp { op, lhs, rhs } => compile_binop(compile_context, *op, *lhs, *rhs),
        ASTNode::VarUse { name } => compile_var_use(compile_context, *name),
        ASTNode::List { list } => compile_static_list(compile_context, list),
        ASTNode::MatchExpr { matched_expr, patterns } => compile_match(compile_context, ast_node, *matched_expr, patterns),
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
            compile_context.functions.insert(*name, function);
            
            let entry = compile_context.context.append_basic_block(function, "entry");
            compile_context.builder.position_at_end(entry);

            for (arg, arg_val) in args.iter().zip(function.get_param_iter()) {
                let arg_type = get_llvm_type(compile_context.context, &arg.arg_type);
                compile_context.var_types.insert(arg.name, arg.arg_type.clone());
                let arg_alloca = create_entry_block_alloca(compile_context, arg.name.get_str(&compile_context.rustaml_context.str_interner), arg_type);
                compile_context.var_vals.insert(arg.name, arg_alloca);
                compile_context.builder.build_store(arg_alloca, arg_val).unwrap();
            }

            let ret = compile_expr(compile_context, *body);

            dbg!(ret);

            let return_val: Option<&dyn BasicValue<'_>> = match return_type {
                Type::Unit => None,
                _ => Some(&TryInto::<BasicValueEnum>::try_into(ret).unwrap()),
            };

            compile_context.builder.build_return(return_val).unwrap();

            for arg in args {
                compile_context.var_vals.remove(&arg.name);
                compile_context.var_types.remove(&arg.name);
            }

            // TODO : add function.verify(true) and return an error if it doesn't work 
        },

        ASTNode::VarDecl { name, val, body } => {

            let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
            compile_context.builder.position_at_end(last_main_bb);

            compile_var_decl(compile_context, *name, *val, *body, true);
        }
        t => panic!("top level node = {:?}", DebugWrapContext::new(t, compile_context.rustaml_context)),
        // _ => unreachable!()
    }
}


fn get_main_function<'llvm_ctx>(llvm_context : &'llvm_ctx Context, module : &Module<'llvm_ctx>) -> FunctionValue<'llvm_ctx> {
    let param_types = &[BasicMetadataTypeEnum::IntType(llvm_context.i32_type()), BasicMetadataTypeEnum::PointerType(llvm_context.ptr_type(AddressSpace::default()))];
    let main = module.add_function("main", llvm_context.i32_type().fn_type(param_types, false), Some(inkwell::module::Linkage::External));
    llvm_context.append_basic_block(main, "entry");
    main
}


fn run_passes_on(module: &Module, target_machine : &TargetMachine) {
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
        .run_passes(passes.join(",").as_str(), target_machine, PassBuilderOptions::create())
        .unwrap();
}

// TODO : instead install file in filesystem ?
const STD_C_CONTENT: &str = include_str!("../std.c");

fn link_exe(filename_out : &Path, bitcode_file : &Path){
    // use cc ?
    // TODO : use lld (https://github.com/mun-lang/lld-rs) for linking instead ?
    // TODO : use libclang ? (clang-rs ? https://github.com/llvm/llvm-project/blob/main/clang/tools/driver/cc1_main.cpp#L85 ?)

    let out_std_path = pathbuf![&std::env::temp_dir(), "std.bc"];
    let out_std_path_str = out_std_path.as_os_str();

    let mut clang_std = Command::new("clang").arg("-x").arg("c").arg("-emit-llvm").arg("-O3").arg("-c").arg("-").arg("-o").arg(out_std_path_str).stdin(Stdio::piped()).spawn().expect("compiling std failed");
    clang_std.stdin.as_mut().unwrap().write_all(STD_C_CONTENT.as_bytes()).unwrap();
    clang_std.wait().unwrap();
    if !Command::new("clang").arg("-o").arg(filename_out).arg(out_std_path_str).arg(bitcode_file).spawn().expect("linker failed").wait().unwrap().success() {
        return;
    }
    std::fs::remove_file(&out_std_path).expect("Couldn't delete std bitcode file");
}

pub fn compile(ast : ASTRef, var_types : FxHashMap<StringRef, Type>, rustaml_context: &RustamlContext, filename : &Path, filename_out : &Path, should_keep_temp : bool) -> ExitCode{
    let context = Context::create();
    let builder = context.create_builder();

    let filename_str = filename.as_os_str().to_str().expect("not UTF-8 filename");
    let module = context.create_module(filename_str);

    Target::initialize_all(&InitializationConfig::default());
    let target_triple = TargetMachine::get_default_triple();

    let target = Target::from_triple(&target_triple).unwrap();

    let optimization_level = OptimizationLevel::Default;

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

    let data_layout = target_machine.get_target_data().get_data_layout();

    module.set_triple(&target_triple);
    module.set_data_layout(&data_layout);

    let main_function = get_main_function(&context, &module);

    let internal_functions = get_internal_functions(&context);

    let mut compile_context = CompileContext {
        rustaml_context,
        context : &context,
        module: &module,
        builder: &builder,
        functions: FxHashMap::default(),
        main_function,
        var_types,
        var_vals: FxHashMap::default(),
        internal_functions,
        external_functions_declared: FxHashSet::default()
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
    

    
    if should_keep_temp {
        let temp_path_ir = pathbuf![&temp_path, &format!("{}.ll", &filename_with_hash)];
        compile_context.module.print_to_file(&temp_path_ir).expect("Couldn't write llvm ir file");
    }


    // TODO : readd this for optimizations
    run_passes_on(compile_context.module, &target_machine);
    
    compile_context.module.write_bitcode_to_path(&temp_path_bitcode);


    

    link_exe(filename_out,  &temp_path_bitcode);

    if !should_keep_temp {
        std::fs::remove_file(temp_path_bitcode).expect("Couldn't delete bitcode file");
    }

    ExitCode::SUCCESS
}