use std::{fmt, ops::Range};

use inkwell::{basic_block::BasicBlock, context::Context, debug_info::{AsDIScope, DICompileUnit, DILexicalBlock, DILocation, DISubprogram, DIType, DebugInfoBuilder, LLVMDWARFTypeEncoding}, llvm_sys::debuginfo::{LLVMDIFlagPrivate, LLVMDIFlagPublic}, values::{FunctionValue, PointerValue}, AddressSpace};
use rustc_hash::FxHashMap;

use crate::ast::Type;

pub struct TargetInfos {
    ptr_size : u32,
    ptr_alignement : u32,
    list_size : u64,
    list_alignement : u32,
}

impl TargetInfos {
    pub fn new(ptr_size : u32, ptr_alignement : u32, list_size : u64, list_alignement : u32) -> TargetInfos {
        TargetInfos {
            ptr_size,
            ptr_alignement,
            list_size,
            list_alignement,
        }
    }

    fn get_ptr_size_in_bits(&self) -> u32 {
        return self.ptr_size * 8;
    }

    fn get_ptr_alignement_in_bits(&self) -> u32 {
        return self.ptr_alignement * 8;
    }

    fn get_list_size_in_bits(&self) -> u64 {
        return self.list_size * 8;
    }

    fn get_list_alignement_in_bits(&self) -> u32 {
        return self.list_alignement * 8;
    }
}

pub struct DebugInfosInner<'llvm_ctx> {
    debug_builder : DebugInfoBuilder<'llvm_ctx>,
    debug_compile_unit : DICompileUnit<'llvm_ctx>,
    target_infos : TargetInfos,

    types : FxHashMap<Type, DIType<'llvm_ctx>>,
    type_data : FxHashMap<Type, TypeData>,
    last_func_scope : Option<DISubprogram<'llvm_ctx>>,
    main_func_scope : DISubprogram<'llvm_ctx>, // will always be some, but need it to call get_debug_info_type which needs to have constructed the DebugInfosInner
    current_lexical_block : Option<DILexicalBlock<'llvm_ctx>>,
    current_debug_loc : Option<DILocation<'llvm_ctx>>,
    main_lexical_block : DILexicalBlock<'llvm_ctx>,
}

// TODO : add a way to transform a range to a line number and column

const DW_ATE_ADDRESS: LLVMDWARFTypeEncoding = 0x01;
const DW_ATE_BOOLEAN: LLVMDWARFTypeEncoding = 0x02;
const DW_ATE_FLOAT: LLVMDWARFTypeEncoding = 0x04;
const DW_ATE_SIGNED: LLVMDWARFTypeEncoding = 0x05;
const DW_ATE_NUMERIC_STRING: LLVMDWARFTypeEncoding = 0x0b;

struct TypeData {
    name : String,
    size_in_bits : u64,
    encoding : LLVMDWARFTypeEncoding,
}

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

fn create_main_function<'llvm_ctx>(target_infos : &TargetInfos, debug_builder: &DebugInfoBuilder<'llvm_ctx>, debug_compile_unit: &DICompileUnit<'llvm_ctx>, is_optimized : bool) -> DISubprogram<'llvm_ctx> {

    let i32_ty = debug_builder.create_basic_type("i32_main_ret", 32, DW_ATE_SIGNED, LLVMDIFlagPublic).unwrap().as_type();
    let i8_ty = debug_builder.create_basic_type("i8_main_arg", 8, DW_ATE_SIGNED, LLVMDIFlagPublic).unwrap().as_type();
    let i8_ptr_ty = debug_builder.create_pointer_type("i8_ptr_main_arg", i8_ty, target_infos.get_ptr_size_in_bits().try_into().unwrap(), target_infos.get_ptr_alignement_in_bits(), AddressSpace::default()).as_type();
    let flags = LLVMDIFlagPrivate;
    let subroutine_type = debug_builder.create_subroutine_type(
        debug_compile_unit.get_file(),
        Some(i32_ty),
        &[i32_ty, i8_ptr_ty],
        flags,
    );
    let line_nb = 0;
    let scope_line = 0;
    debug_builder.create_function(debug_compile_unit.as_debug_info_scope(), "main", None, debug_compile_unit.get_file(), line_nb, subroutine_type, true, true, scope_line, LLVMDIFlagPublic, is_optimized)
}

fn create_main_func_lexical_block<'llvm_ctx>(debug_builder: &DebugInfoBuilder<'llvm_ctx>, debug_compile_unit: &DICompileUnit<'llvm_ctx>, main_func : DISubprogram<'llvm_ctx>) -> DILexicalBlock<'llvm_ctx> {
    debug_builder.create_lexical_block(main_func.as_debug_info_scope(), debug_compile_unit.get_file(), 0, 0)
}

impl<'llvm_ctx> DebugInfosInner<'llvm_ctx>{
    pub fn new(target_infos : TargetInfos, is_optimized : bool, debug_builder : DebugInfoBuilder<'llvm_ctx>, debug_compile_unit : DICompileUnit<'llvm_ctx>) -> DebugInfosInner<'llvm_ctx> {
        let type_data = init_type_data(target_infos.get_ptr_size_in_bits());
        let main_func = create_main_function(&target_infos, &debug_builder, &debug_compile_unit, is_optimized);
        let main_lexical_block = create_main_func_lexical_block(&debug_builder, &debug_compile_unit, main_func);
        DebugInfosInner { 
            debug_builder, 
            debug_compile_unit,
            types: FxHashMap::default(),
            target_infos,
            type_data,
            last_func_scope: None,
            main_func_scope: main_func,
            current_debug_loc: None,
            current_lexical_block: None,
            main_lexical_block, 
        }
    }
}

struct DebugLoc {
    line_nb : u32,
    column : u32,
}

#[derive(Clone)]
pub struct ContentLoc {
    newlines_idx : Vec<usize>,
}

impl ContentLoc {
    pub fn new(content_chars : &[char]) -> ContentLoc {
        let newlines_idx = content_chars.iter().enumerate().filter(|(_, e)| **e == '\n').map(|(idx, _)| idx).collect();

        ContentLoc { 
            //content_chars, 
            newlines_idx 
        }
    }
}

// TODO : add better way (for example when generating ranges, when lexing, add it in a hashmap ?)
fn get_debug_loc(content_loc : &ContentLoc, range : Range<usize>) -> DebugLoc {
    let newline_idx = content_loc.newlines_idx.partition_point(|e| *e <= range.start);

    let new_line_before_line_offset = if let Some(i) = newline_idx.checked_sub(1) {
        content_loc.newlines_idx[i]
    } else {
        0
    };

    let column_nb =  range.start - new_line_before_line_offset;
    
    let line_nb = newline_idx + 1;


    DebugLoc { 
        line_nb: line_nb.try_into().unwrap(),
        column: column_nb.try_into().unwrap(),
    }

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
    let ptr_ty = inner.debug_builder.create_pointer_type("list_pointer", unit_ty, inner.target_infos.get_ptr_size_in_bits() as u64, inner.target_infos.get_ptr_alignement_in_bits(), AddressSpace::default()).as_type();
    let elements = &[i8_ty, val_ty, ptr_ty]; // TODO
    let unique_id = "list_struct"; // TODO ?
    let runtime_language = 0; // TODO
    let line_nb = 0;
    let list_struct_ty = inner.debug_builder.create_struct_type(scope, "list_struct", file, line_nb, inner.target_infos.get_list_size_in_bits() as u64, inner.target_infos.get_list_alignement_in_bits(), 0x0, None, elements, runtime_language, None, unique_id);
    inner.debug_builder.create_pointer_type("list_pointer", list_struct_ty.as_type(), inner.target_infos.get_ptr_size_in_bits() as u64, inner.target_infos.get_ptr_alignement_in_bits(), AddressSpace::default()).as_type()
}


struct DisplayVecArgs<'a, T>(&'a [T]);

impl<'a, T: fmt::Display> fmt::Display for DisplayVecArgs<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?; // separator
            }
            write!(f, "{}", item)?;
        }
        Ok(())
    }
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
            let align_in_bits = inner.target_infos.get_ptr_alignement_in_bits();
            inner.debug_builder.create_typedef(pointer_to_struct_ty, &format!("list[{}]", e.as_ref()), inner.debug_compile_unit.get_file(), line_nb, scope, align_in_bits).as_type()
        },
        Type::Function(args, ret, is_variadic) => {
            /*let flags = 0;
            let pointee = inner.debug_builder.create_subroutine_type(
                inner.debug_compile_unit.get_file(), 
                Some(get_debug_info_type(inner, ret.as_ref())), 
                &args.iter().map(|e| get_debug_info_type(inner, e)).collect::<Vec<_>>(), 
                flags);*/ // TODO
            // TODO

            let pointee = get_debug_info_type(inner, &Type::Unit);

            let name = &format!("function({}{}) -> {}", DisplayVecArgs(&args), (if *is_variadic { ", ..." } else { "" }), ret.as_ref());
            inner.debug_builder.create_pointer_type(name, pointee, inner.target_infos.get_ptr_size_in_bits() as u64, inner.target_infos.get_ptr_alignement_in_bits(), AddressSpace::default()).as_type()
        }
        _ => {
            let type_data = inner.type_data.get(t).unwrap_or_else(|| panic!("type inner data not found : {:?}", t));
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

    pub fn add_function(&mut self, function_name : &str, param_types : &[Type], ret_type : &Type, content_loc : &ContentLoc, range : Range<usize>, is_optimized : bool) -> Option<DISubprogram<'llvm_ctx>> {
        if let Some(i) = &mut self.inner {
            
            let ditype = get_debug_info_type(i, ret_type);

            let flags = LLVMDIFlagPrivate;

            let args_debug_types = param_types.iter().map(|t| get_debug_info_type(i, t)).collect::<Vec<_>>();
            let subroutine_type = i.debug_builder.create_subroutine_type(
                i.debug_compile_unit.get_file(),
                Some(ditype),
                &args_debug_types,
                flags,
            );
            let line_nb = get_debug_loc(content_loc, range).line_nb; // TODO (does it has a use ?)
            let scope_line = line_nb; // TODO ? change this ?
            let func_scope: DISubprogram<'_> = i.debug_builder.create_function(i.debug_compile_unit.as_debug_info_scope(), function_name, None, i.debug_compile_unit.get_file(), line_nb, subroutine_type, true, true, scope_line, LLVMDIFlagPublic, is_optimized);
            i.last_func_scope = Some(func_scope);
            Some(func_scope) // must use set_subprogram on FunctionValue on this value
        } else {
            None
        }
        
    }

    pub fn create_lexical_block(&mut self) -> Option<DILexicalBlock<'llvm_ctx>> {
        if let Some(i) = &mut self.inner {
            let func_scope = i.last_func_scope.unwrap().as_debug_info_scope();
            i.current_lexical_block = Some(i.debug_builder.create_lexical_block(func_scope, i.debug_compile_unit.get_file(), 0, 0));
            i.current_lexical_block
        } else {
            None
        }
    }

    pub fn end_lexical_block(&mut self){
        if let Some(i) = &mut self.inner {
            i.current_lexical_block.take();
        }
    }

    pub fn enter_top_level(&mut self){
        if let Some(i) = &mut self.inner {
            i.last_func_scope = Some(i.main_func_scope);
            i.current_lexical_block = Some(i.main_lexical_block);
        }
    }

    pub fn create_debug_location(&mut self, context : &'llvm_ctx Context, content : &ContentLoc, range : Range<usize>) -> Option<DILocation<'llvm_ctx>>{
        // add real lexical blocks instead of creating one for each function call (put them in self)
        
        // TODO : move to a set_function_call_dbg
        if let Some(i) = &mut self.inner {
            let debug_loc = get_debug_loc(content, range);
            let lexical_block = i.current_lexical_block.unwrap();
            i.current_debug_loc = Some(i.debug_builder.create_debug_location(context, debug_loc.line_nb, debug_loc.column, lexical_block.as_debug_info_scope(), None));
            i.current_debug_loc
        } else {
            None
        }
    }

    pub fn end_function(&mut self){
        if let Some(i) = &mut self.inner {
            i.last_func_scope.take();
        }
    }

    pub fn declare_var(&mut self, name : &str, var_type : &Type, storage : PointerValue<'llvm_ctx>, current_bb : BasicBlock<'llvm_ctx>, content : &ContentLoc, range : Range<usize>){
        if let Some(i) = &mut self.inner {
            let scope = i.last_func_scope.unwrap().as_debug_info_scope();
            let file = i.debug_compile_unit.get_file();
            let debug_location_var = get_debug_loc(content, range);
            let var_ty_debug = get_debug_info_type(i, var_type);
            let always_preserve = true; // TODO : will it affect optimizations
            let flags = 0; // TODO
            let align_in_bits = 0; // TODO
            let var_info = Some(i.debug_builder.create_auto_variable(scope, name, file, debug_location_var.line_nb, var_ty_debug, always_preserve, flags, align_in_bits));
            let expr = None; // TODO

            let debug_loc = i.current_debug_loc.unwrap();

            // TODO : at some point (when the inkwell release supports llvm 20), switch to debug record instead of this instrisic (because the intrisic is being dropped in llvm 20)
            i.debug_builder.insert_declare_at_end(storage, var_info, expr, debug_loc, current_bb);
        }
    }

    

    pub fn finalize(&self, function_value : &mut FunctionValue<'llvm_ctx>){
        if let Some(i) = &self.inner {
            function_value.set_subprogram(i.main_func_scope);
            i.debug_builder.finalize();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debuginfo_get_debug_loc() {
        let content = ContentLoc::new("aa\nbbb\ncd".chars().collect::<Vec<_>>());
        let range = 7..9;
        let debug_loc = get_debug_loc(&content, range);
        assert_eq!(debug_loc.line_nb, 3);
    }
}