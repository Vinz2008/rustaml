use inkwell::{context::Context, debug_info::{AsDIScope, DICompileUnit, DILocation, DISubprogram, DIType, DebugInfoBuilder, LLVMDWARFTypeEncoding}, llvm_sys::debuginfo::{LLVMDIFlagPrivate, LLVMDIFlagPublic}, AddressSpace};
use rustc_hash::FxHashMap;

use crate::ast::Type;

pub struct DebugInfosInner<'llvm_ctx> {
    debug_builder : DebugInfoBuilder<'llvm_ctx>,
    debug_compile_unit : DICompileUnit<'llvm_ctx>,
    ptr_size_in_bit : u32,
    ptr_alignement_in_bit : u32,
    list_size_in_bit : u64,
    list_alignement_in_bit : u32,
    types : FxHashMap<Type, DIType<'llvm_ctx>>,
    type_data : FxHashMap<Type, TypeData>,
    last_func_scope : Option<DISubprogram<'llvm_ctx>>,
}

// TODO : add a way to transform a range to a line number and column

const DW_ATE_ADDRESS: LLVMDWARFTypeEncoding = 0x01;
const DW_ATE_BOOLEAN: LLVMDWARFTypeEncoding = 0x02;
const DW_ATE_FLOAT: LLVMDWARFTypeEncoding = 0x04;
const DW_ATE_SIGNED: LLVMDWARFTypeEncoding = 0x05;
const DW_ATE_NUMERIC_STRING: LLVMDWARFTypeEncoding = 0x0b;

fn init_type_data(ptr_size_in_bit : u32) -> FxHashMap<Type, TypeData> {
    FxHashMap::from_iter([
        (Type::Unit, TypeData { name: "unit".to_owned(), size_in_bits: 0, encoding: 0x00 }),
        (Type::Integer, TypeData { name: "int".to_owned(), size_in_bits: 64, encoding: DW_ATE_SIGNED }),
        (Type::Float, TypeData { name: "float".to_owned(), size_in_bits: 64, encoding: DW_ATE_FLOAT }),
        // TODO : check size in bits of bool ?
        (Type::Bool, TypeData { name: "bool".to_owned(), size_in_bits: 8, encoding: DW_ATE_BOOLEAN }),
        (Type::Str, TypeData { name: "str".to_owned(), size_in_bits: ptr_size_in_bit as u64, encoding: DW_ATE_NUMERIC_STRING }),
    ])
}

impl<'llvm_ctx> DebugInfosInner<'llvm_ctx>{
    pub fn new(ptr_size_in_bit : u32, ptr_alignement_in_bit : u32, list_size_in_bit : u64, list_alignement_in_bit : u32, debug_builder : DebugInfoBuilder<'llvm_ctx>, debug_compile_unit : DICompileUnit<'llvm_ctx>) -> DebugInfosInner<'llvm_ctx> {
        DebugInfosInner { 
            debug_builder, 
            debug_compile_unit,
            types: FxHashMap::default(),
            ptr_size_in_bit,
            ptr_alignement_in_bit,
            list_size_in_bit,
            list_alignement_in_bit,
            type_data: init_type_data(ptr_size_in_bit),
            last_func_scope: None,
        }
    }
}


struct TypeData {
    name : String,
    size_in_bits : u64,
    encoding : LLVMDWARFTypeEncoding,
}

fn get_list_type<'llvm_ctx>(inner : &mut DebugInfosInner<'llvm_ctx>) -> DIType<'llvm_ctx> {
    // TODO : cache this
    let scope = inner.debug_compile_unit.as_debug_info_scope();
    let file = inner.debug_compile_unit.get_file();

    // TODO
    //let forward_list_struct_ty = inner.debug_builder.create_placeholder_derived_type(inner.debug_builder);
    let i8_ty = inner.debug_builder.create_basic_type("i8_list_tag", 8, DW_ATE_SIGNED, LLVMDIFlagPublic).unwrap().as_type();
    let val_ty = get_debug_info_type(inner, &Type::Integer); // TODO : use an union instead
    let unit_ty = get_debug_info_type(inner, &Type::Unit);
    let ptr_ty = inner.debug_builder.create_pointer_type("list_pointer", unit_ty, inner.ptr_size_in_bit as u64, inner.ptr_alignement_in_bit, AddressSpace::default()).as_type();
    let elements = &[i8_ty, val_ty, ptr_ty]; // TODO
    let unique_id = "list_struct"; // TODO ?
    let runtime_language = 0; // TODO
    let line_nb = 0;
    let list_struct_ty = inner.debug_builder.create_struct_type(scope, "list_struct", file, line_nb, inner.list_size_in_bit as u64, inner.list_alignement_in_bit, 0x0, None, elements, runtime_language, None, unique_id);
    inner.debug_builder.create_pointer_type("list_pointer", list_struct_ty.as_type(), inner.ptr_size_in_bit as u64, inner.ptr_alignement_in_bit, AddressSpace::default()).as_type()
}

fn get_debug_info_type<'llvm_ctx>(inner : &mut DebugInfosInner<'llvm_ctx>, t : &Type) -> DIType<'llvm_ctx> {
    
    if inner.types.contains_key(t) {
        return *inner.types.get(t).unwrap();
    }

    match t {
        Type::List(e) => {
            let line_nb = 0;
            let pointer_to_struct_ty = get_list_type(inner);
            let scope = inner.debug_compile_unit.as_debug_info_scope();
            let align_in_bits = inner.ptr_alignement_in_bit;
            inner.debug_builder.create_typedef(pointer_to_struct_ty, &format!("list[{}]", e.as_ref()), inner.debug_compile_unit.get_file(), line_nb, scope, align_in_bits).as_type()
        }
        _ => {
            let type_data = inner.type_data.get(t).unwrap();
            let di_type = inner.debug_builder.create_basic_type(&type_data.name, type_data.size_in_bits, type_data.encoding, LLVMDIFlagPublic).unwrap().as_type();
        
            inner.types.insert(t.clone(), di_type);
            di_type
        }
    }    
}

pub struct DebugInfo<'llvm_ctx> {
    pub(crate) inner : Option<DebugInfosInner<'llvm_ctx>>
}

impl<'llvm_ctx> DebugInfo<'llvm_ctx> {

    pub fn add_function(&mut self, function_name : &str, param_types : &[Type], ret_type : &Type, is_optimized : bool) -> Option<DISubprogram<'llvm_ctx>> {
        if let Some(i) = &mut self.inner {
            
            let ditype = get_debug_info_type(i, ret_type);

            let flags = if function_name == "main" {
                LLVMDIFlagPublic
            } else {
                LLVMDIFlagPrivate
            };
            let subroutine_type = i.debug_builder.create_subroutine_type(
                i.debug_compile_unit.get_file(),
                Some(ditype),
                &[],
                flags,
            );
            let line_nb = 0; // TODO
            let scope_line = 0; // TODO ?
            let func_scope: DISubprogram<'_> = i.debug_builder.create_function(i.debug_compile_unit.as_debug_info_scope(), function_name, None, i.debug_compile_unit.get_file(), line_nb, subroutine_type, true, true, scope_line, LLVMDIFlagPublic, is_optimized);
            i.last_func_scope = Some(func_scope);
            Some(func_scope) // must use set_subprogram on FunctionValue on this value
        } else {
            None
        }
        
    }

    pub fn get_function_call_dbg(&self, context : &'llvm_ctx Context, line_nb : u32, column : u32) -> Option<DILocation<'llvm_ctx>>{
        // add real lexical blocks instead of creating one for each function call (put them in self)
        
        // TODO : move to a set_function_call_dbg
        if let Some(i) = &self.inner {
            let func_scope = i.last_func_scope.unwrap().as_debug_info_scope();
            let lexical_block = i.debug_builder.create_lexical_block(func_scope, i.debug_compile_unit.get_file(), 0, 0);
            Some(i.debug_builder.create_debug_location(context, line_nb, column, lexical_block.as_debug_info_scope(), None))
        } else {
            None
        }
    }

    

    pub fn finalize(&self){
        if let Some(i) = &self.inner {
            i.debug_builder.finalize();
        }
    }
}