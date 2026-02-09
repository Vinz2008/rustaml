use inkwell::{types::BasicTypeEnum, values::BasicValueEnum};

use crate::{ast::{CType, Type}, compiler::{CompileContext, compiler_utils::{get_llvm_type, get_void_val}}};

pub(crate) fn cast_val<'llvm_ctx>(compile_context : &CompileContext<'_, 'llvm_ctx>, val : BasicValueEnum<'llvm_ctx>, from_ty : &Type, to_ty : &Type) -> BasicValueEnum<'llvm_ctx> {
    let to_ty_llvm = get_llvm_type(compile_context, to_ty);
    let to_ty_llvm_basic : BasicTypeEnum = to_ty_llvm.try_into().unwrap();
    match (from_ty, to_ty){
        (t1, t2) if t1 == t2 => val,
        // compatible layout types
        (Type::Integer, Type::CType(CType::I64 | CType::U64)) | (Type::CType(CType::I64 | CType::U64), Type::Integer,) => val,
        (Type::Char, Type::CType(CType::U32 | CType::I32)) | (Type::CType(CType::U32 | CType::I32), Type::Char)  => val,
        (Type::Float, Type::CType(CType::F64)) | (Type::CType(CType::F64), Type::Float) => val,
        
        // in all cases, create a void
        (Type::Unit | Type::Never, _) => {
            let void_val = get_void_val(compile_context.context);
            compile_context.builder.build_bit_cast(void_val, to_ty_llvm_basic, "bitcast_void").unwrap()
        }
        (Type::Integer, Type::Float) => compile_context.builder.build_signed_int_to_float(val.into_int_value(), to_ty_llvm_basic.into_float_type(), "int_to_float").unwrap().into(),

        (Type::CType(CType::I32), Type::Integer) => compile_context.builder.build_int_s_extend(val.into_int_value(), to_ty_llvm_basic.into_int_type(), "c_i32_to_int").unwrap().into(),
        (Type::Integer, Type::CType(CType::I32)) => compile_context.builder.build_int_truncate(val.into_int_value(), to_ty_llvm_basic.into_int_type(), "int_to_c_i32").unwrap().into(),

        (Type::Bool, Type::CType(CType::U8 | CType::I8 | CType::U16 | CType::I16 | CType::U32 | CType::I32 | CType::U64 | CType::I64)) => compile_context.builder.build_int_z_extend(val.into_int_value(), to_ty_llvm_basic.into_int_type(), "zextend_bool").unwrap().into(),
        (Type::Char, Type::CType(CType::U64 | CType::I64)) => compile_context.builder.build_int_z_extend(val.into_int_value(), to_ty_llvm_basic.into_int_type(), "zextend_char").unwrap().into(),

        (Type::CType(CType::U8 | CType::I8 | CType::U16 | CType::I16 | CType::U32 | CType::I32 | CType::U64 | CType::I64), Type::Bool) => compile_context.builder.build_int_truncate(val.into_int_value(), to_ty_llvm_basic.into_int_type(), "truncate_to_bool").unwrap().into(),
        (Type::CType(CType::U64 | CType::I64), Type::Char) => compile_context.builder.build_int_truncate(val.into_int_value(), to_ty_llvm_basic.into_int_type(), "truncate_to_char").unwrap().into(),

        // those u64 implementation are used for as_val_in_list (prevent those in normal lang casts, TODO)
        (Type::Str | Type::List(_) | Type::Function(_, _, _), Type::CType(CType::U64)) => compile_context.builder.build_ptr_to_int(val.into_pointer_value(), to_ty_llvm_basic.into_int_type(), "ptrtoint_to_u64").unwrap().into(),
        (Type::CType(CType::U64), Type::Str | Type::List(_) | Type::Function(_, _, _)) => compile_context.builder.build_int_to_ptr(val.into_int_value(), to_ty_llvm_basic.into_pointer_type(), "inttoptr_from_u64").unwrap().into(),
        _ => panic!("Wrong cast"),
    }
}