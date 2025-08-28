use inkwell::{basic_block::BasicBlock, builder::Builder, context::Context, types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType}, values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue}, AddressSpace};

use crate::{ast::Type, compiler::CompileContext, string_intern::StringRef};

pub fn encountered_any_type() -> ! {
    panic!("Encountered Any when compiling")
}

pub fn get_type_tag(t : &Type) -> u8 {
    match t {
        Type::Integer => 0,
        Type::Float => 1,
        Type::Bool => 2,
        Type::Function(_, _, _) => 3,
        Type::Str => 4,
        Type::List(_) => 5,
        // TODO : add a type tag for Unit ?
        Type::Any | Type::Unit | Type::Never => panic!("no type tag for this type {:?} !!", t),
    }
}

pub fn get_type_tag_val<'llvm_ctx>(llvm_context : &'llvm_ctx Context, t : &Type) -> IntValue<'llvm_ctx> {
    llvm_context.i8_type().const_int(get_type_tag(t) as u64, false)
}

pub fn get_list_type<'llvm_ctx>(llvm_context: &'llvm_ctx Context) -> StructType<'llvm_ctx>{
    // put it in an Optional in the context

    let field_types: &[BasicTypeEnum] = &[llvm_context.i8_type().into(), llvm_context.i64_type().into(), llvm_context.ptr_type(AddressSpace::default()).into()];
    llvm_context.struct_type(field_types, false)
}


// TODO : make strings a pointer to a struct with a string and a len
pub fn get_llvm_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context, rustaml_type : &Type) -> AnyTypeEnum<'llvm_ctx> {
    match rustaml_type {
        Type::Integer => llvm_context.i64_type().into(),
        Type::Bool => llvm_context.bool_type().into(),
        Type::Float => llvm_context.f64_type().into(),
        Type::Function(args, ret, is_variadic) => {
            let ret_llvm = get_llvm_type(llvm_context, ret);
            // TODO for expect : create a function that would be get_basic_metatadata_type which will transform the function pointers into pointers ?
            let param_types = args.iter().map(|t| any_type_to_metadata(llvm_context, get_llvm_type(llvm_context, t)) ).collect::<Vec<_>>();
            get_fn_type(llvm_context, ret_llvm, &param_types, *is_variadic).into()
        },

        // layout of list
        // struct ListNode {
        //      uint8_t type_tag;
        //      void* val; // can be also a i64 or f64 depending on type_tag
        //      struct ListNode* next; // if empty null 
        // }
        Type::List(_t) => llvm_context.ptr_type(AddressSpace::default()).into(), // TODO ?
        Type::Str => llvm_context.ptr_type(AddressSpace::default()).into(),
        Type::Unit | Type::Never => llvm_context.void_type().into(),
        Type::Any => encountered_any_type(),

    }
}

pub fn create_int<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, nb : i64) -> IntValue<'llvm_ctx> {
    compile_context.context.i64_type().const_int(nb as u64, true)
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

pub fn move_bb_after_current<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, bb : BasicBlock<'llvm_ctx>){
    let current_bb = compile_context.builder.get_insert_block().unwrap();
    bb.move_after(current_bb).unwrap();
}

pub fn append_bb_just_after<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, function : FunctionValue<'llvm_ctx>, name : &str) -> BasicBlock<'llvm_ctx> {
    
    let bb = compile_context.context.append_basic_block(function, name);
    move_bb_after_current(compile_context, bb);
    bb
}

pub fn any_type_to_metadata<'llvm_ctx>(context : &'llvm_ctx Context, t : AnyTypeEnum<'llvm_ctx>) -> BasicMetadataTypeEnum<'llvm_ctx> {
    match t {
        AnyTypeEnum::FunctionType(_) => BasicMetadataTypeEnum::PointerType(context.ptr_type(AddressSpace::default())),
        t => t.try_into().unwrap(),
    }
}

pub fn any_type_to_basic<'llvm_ctx>(context : &'llvm_ctx Context, t : AnyTypeEnum<'llvm_ctx>) -> BasicTypeEnum<'llvm_ctx> {
    match t {
        AnyTypeEnum::FunctionType(_) => BasicTypeEnum::PointerType(context.ptr_type(AddressSpace::default())),
        t => t.try_into().unwrap(),
    }
}

pub fn any_val_to_metadata<'llvm_ctx>(v : AnyValueEnum<'llvm_ctx>) -> BasicMetadataValueEnum<'llvm_ctx> {
    match v {
        AnyValueEnum::FunctionValue(f) => BasicMetadataValueEnum::PointerValue(f.as_global_value().as_pointer_value()),
        v => v.try_into().unwrap(),
    }
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
    builder.build_alloca(any_type_to_basic(compile_context.context, alloca_type), name).unwrap()
}

pub fn create_var<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, val : AnyValueEnum<'llvm_ctx>, alloca_type : AnyTypeEnum<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    if alloca_type.is_void_type(){
        return compile_context.context.ptr_type(AddressSpace::default()).const_null(); // to represent a var containing a void, if it is written to, it is a bug
    }
    let var_alloca = create_entry_block_alloca(compile_context, name.get_str(&compile_context.rustaml_context.str_interner), alloca_type);
    compile_context.builder.build_store(var_alloca, TryInto::<BasicValueEnum>::try_into(val).unwrap()).unwrap();
    compile_context.var_vals.insert(name, var_alloca);
    var_alloca
}

pub fn create_string<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, str : &str) -> PointerValue<'llvm_ctx> {
    match compile_context.global_strs.get(str){
        Some(s) => *s,
        None => {
            let str_ptr = compile_context.builder.build_global_string_ptr(str, "str").unwrap().as_pointer_value();
            compile_context.global_strs.insert(str.to_owned(), str_ptr);
            str_ptr
        },
    }
    
}

pub fn codegen_runtime_error<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, message : &str){
    let message_str = create_string(compile_context, message);
    _codegen_runtime_error(compile_context, message_str)
}

pub fn _codegen_runtime_error<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, message_str : PointerValue<'llvm_ctx>){
    // TODO : only if in a hashset of already added symbols (is it needed ?)
    let ptr_type = compile_context.context.ptr_type(AddressSpace::default());
    
    let fprintf_fun = compile_context.get_internal_function("fprintf");

    let exit_fun = compile_context.get_internal_function("exit");

    let stderr_global = compile_context.get_internal_global_var("stderr", ptr_type.as_basic_type_enum());
    
    let stderr_load = compile_context.builder.build_load(ptr_type, stderr_global.as_pointer_value(), "stderr_load").unwrap();
    let fprintf_args: Vec<BasicMetadataValueEnum> = vec![stderr_load.into(), message_str.into()];
    compile_context.builder.build_call(fprintf_fun, &fprintf_args, "error_fprintf").unwrap();

    let exit_args = vec![compile_context.context.i32_type().const_int(1, false).into()];
    compile_context.builder.build_call(exit_fun, &exit_args, "error_exit").unwrap();
    compile_context.builder.build_unreachable().unwrap();
}

fn is_last_instruction_terminator<'llvm_ctx>(compile_context: &CompileContext<'_, '_, 'llvm_ctx>) -> bool {
    let current_bb = compile_context.builder.get_insert_block().unwrap();
    match current_bb.get_last_instruction() {
        Some(instr) => instr.is_terminator(),
        None => false,
    }
}

pub fn create_br_conditional<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, comparison: IntValue<'llvm_ctx>, then_block: BasicBlock<'llvm_ctx>, else_block: BasicBlock<'llvm_ctx>) -> bool {
    if !is_last_instruction_terminator(compile_context) {
        compile_context.builder.build_conditional_branch(comparison, then_block, else_block).unwrap();
        true
    } else {
        false
    }
}

pub fn create_br_unconditional<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, dest_bb: BasicBlock<'llvm_ctx>) -> bool {
    if !is_last_instruction_terminator(compile_context) {
        compile_context.builder.build_unconditional_branch(dest_bb).unwrap();
        true
    } else {
        false
    }
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

pub fn promote_val_var_arg<'llvm_ctx>(compile_context: &CompileContext<'_, '_, 'llvm_ctx>, val_type : Type, val : AnyValueEnum<'llvm_ctx>) -> AnyValueEnum<'llvm_ctx>{
    match val_type {
        Type::Bool => compile_context.builder.build_int_z_extend(val.into_int_value(), compile_context.context.i32_type(), "zext_va_arg").unwrap().as_any_value_enum(),
        _ => val,
    }
}

// dummy val for void, if it is used as a real value, it is a bug
pub fn get_void_val<'llvm_ctx>(llvm_context : &'llvm_ctx Context) -> AnyValueEnum<'llvm_ctx> {
    llvm_context.i64_type().get_undef().into()
}