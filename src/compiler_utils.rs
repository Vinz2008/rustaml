use inkwell::{attributes::Attribute, builder::Builder, context::Context, types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, StructType}, values::{AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue}, AddressSpace};

use crate::{ast::Type, compiler::CompileContext, string_intern::StringRef};

pub fn get_type_tag(t : &Type) -> u8 {
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

pub fn get_type_tag_val<'llvm_ctx>(llvm_context : &'llvm_ctx Context, t : &Type) -> IntValue<'llvm_ctx> {
    llvm_context.i8_type().const_int(get_type_tag(t) as u64, false)
}

fn get_list_type<'llvm_ctx>(llvm_context: &'llvm_ctx Context) -> StructType<'llvm_ctx>{
    // put it in an Optional in the context

    let field_types: &[BasicTypeEnum] = &[llvm_context.i8_type().into(), llvm_context.i64_type().into(), llvm_context.ptr_type(AddressSpace::default()).into()];
    llvm_context.struct_type(field_types, false)
}


// TODO : make strings a pointer to a struct with a string and a len
pub fn get_llvm_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context, rustaml_type : &Type) -> AnyTypeEnum<'llvm_ctx> {
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

pub fn get_fn_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context, llvm_type : AnyTypeEnum<'llvm_ctx>,  param_types: &[BasicMetadataTypeEnum<'llvm_ctx>], is_var_args: bool) -> FunctionType<'llvm_ctx> {
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

pub fn get_current_function<'llvm_ctx>(builder: &Builder<'llvm_ctx>) -> FunctionValue<'llvm_ctx> {
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

    //dbg!(alloca_type);
    builder.build_alloca(TryInto::<BasicTypeEnum>::try_into(alloca_type).unwrap(), name).unwrap()
}

pub fn create_var<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, val : AnyValueEnum<'llvm_ctx>, alloca_type : AnyTypeEnum<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    let var_alloca = create_entry_block_alloca(compile_context, name.get_str(&compile_context.rustaml_context.str_interner), alloca_type);
    compile_context.builder.build_store(var_alloca, TryInto::<BasicValueEnum>::try_into(val).unwrap()).unwrap();
    compile_context.var_vals.insert(name, var_alloca);
    var_alloca
}

pub fn codegen_runtime_error<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, message : &str){
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

fn load_type_tag<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : PointerValue<'llvm_ctx>) -> IntValue<'llvm_ctx> {
    let list_type = get_list_type(compile_context.context);
    let gep_ptr = compile_context.builder.build_struct_gep(list_type, list, 0, "load_type_tag_gep").unwrap();
    compile_context.builder.build_load(compile_context.context.i8_type(), gep_ptr, "load_tag_gep").unwrap().into_int_value()
}

pub fn load_list_val<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, elem_type : &Type, list : PointerValue<'llvm_ctx>) -> BasicValueEnum<'llvm_ctx> {
    let list_type = get_list_type(compile_context.context);
    let gep_ptr = compile_context.builder.build_struct_gep(list_type, list, 1, "load_list_val_gep").unwrap();
    let elem_type_llvm = get_llvm_type(compile_context.context, elem_type);
    compile_context.builder.build_load( TryInto::<BasicTypeEnum>::try_into(elem_type_llvm).unwrap(), gep_ptr, "load_val_gep").unwrap()
}

pub fn load_list_tail<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : PointerValue<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    let list_type = get_list_type(compile_context.context);
    let gep_ptr = compile_context.builder.build_struct_gep(list_type, list, 2, "load_list_tail_gep").unwrap();
    compile_context.builder.build_load(compile_context.context.ptr_type(AddressSpace::default()), gep_ptr, "load_tail_gep").unwrap().into_pointer_value()
}