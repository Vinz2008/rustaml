use core::panic;
use std::{cell::Cell, fs, hash::{Hash, Hasher}, io::Write, ops::Range, path::{MAIN_SEPARATOR, Path, PathBuf}, process::{Command, Stdio}, time::{SystemTime, UNIX_EPOCH}};
use debug_with_context::DebugWrapContext;
use crate::{ast::{ASTNode, ASTRef, CType, Type}, compiler::{compile_match::compile_match, compiler_utils::{_codegen_runtime_error, add_function, any_type_to_basic, any_type_to_metadata, as_val_in_list, codegen_lang_runtime_error, create_br_conditional, create_br_unconditional, create_entry_block_alloca, create_entry_block_array_alloca, create_int, create_string, create_var, encountered_any_type, get_current_function, get_fn_type, get_list_type, get_llvm_type, get_main_function, get_type_tag_val, get_variant_tag, get_void_val, move_bb_after_current, promote_val_var_arg, vec_to_c_struct_ptr}, debuginfo::{DebugInfo, DebugInfosInner, TargetInfos, get_debug_loc}, internal_monomorphized::{compile_monomorphized_filter, compile_monomorphized_map, init_monomorphized_internal_fun}}, debug_println, lexer::Operator, mangle::mangle_name_external, rustaml::{FrontendOutput, RustamlContext}, string_intern::StringRef, types::{TypeInfos, VarId}};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel, attributes::{Attribute, AttributeLoc}, basic_block::BasicBlock, builder::Builder, context::Context, debug_info::{DWARFEmissionKind, DWARFSourceLanguage}, intrinsics::Intrinsic, llvm_sys::{core::LLVMPrintValueToString, prelude::LLVMValueRef}, module::{FlagBehavior, Linkage, Module}, passes::PassBuilderOptions, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine}, types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum}, values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, GlobalValue, IntValue, PointerValue, ValueKind}};
use pathbuf::pathbuf;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use cfg_if::cfg_if;

// TODO : add generic enums to have results for error handling

// TODO : add a flag which would do the same as -march=native

#[cfg(feature = "debug-llvm")]
use inkwell::support::LLVMString;


// TODO : only declare these structs when cache feature is enabled (so the we don't generate a useless implementation of Seralize and Deserialize)
#[derive(serde::Serialize, serde::Deserialize)]
pub(crate) struct CachedCompMeta {
    pub(crate) shared_libs : Vec<String>,
}

pub(crate) struct CachedCompilation {
    pub(crate) bitcode_path : PathBuf,
    pub(crate) metadata : CachedCompMeta,
}

#[derive(Clone, Copy)]
pub(crate) enum CompilerValue<'llvm_ctx> {
    BasicValue(BasicValueEnum<'llvm_ctx>),
    FunctionValue(FunctionValue<'llvm_ctx>),
}

impl<'llvm_ctx> CompilerValue<'llvm_ctx> {
    pub(crate) fn unwrap_basic(self) -> BasicValueEnum<'llvm_ctx> {
        match self {
            CompilerValue::BasicValue(b) => b,
            CompilerValue::FunctionValue(_) => panic!("Panic in unwrap_basic of CompilerValue")
        }
    }

    pub(crate) fn into_basic(self) -> BasicValueEnum<'llvm_ctx> {
        match self {
            CompilerValue::BasicValue(b) => b,
            CompilerValue::FunctionValue(f) => f.as_global_value().as_pointer_value().as_basic_value_enum(),
        }
    }

    fn get_basic_type(&self, llvm_context : &'llvm_ctx Context) -> BasicTypeEnum<'llvm_ctx> {
        match self {
            CompilerValue::BasicValue(b) => b.get_type(),
            CompilerValue::FunctionValue(_) => llvm_context.ptr_type(AddressSpace::default()).as_basic_type_enum(),
        }
    }

    fn as_any_value_enum(&self) -> AnyValueEnum<'llvm_ctx> {
        match self {
            CompilerValue::BasicValue(b) => b.as_any_value_enum(),
            CompilerValue::FunctionValue(f) => f.as_any_value_enum(),
        }
    }
}

impl<'llvm_ctx> From<BasicValueEnum<'llvm_ctx>> for CompilerValue<'llvm_ctx> {
    fn from(val: BasicValueEnum<'llvm_ctx>) -> CompilerValue<'llvm_ctx> {
        CompilerValue::BasicValue(val)
    }
}

impl<'llvm_ctx> From<FunctionValue<'llvm_ctx>> for CompilerValue<'llvm_ctx> {
    fn from(val: FunctionValue<'llvm_ctx>) -> CompilerValue<'llvm_ctx> {
        CompilerValue::FunctionValue(val)
    }
}



cfg_if! {
    if #[cfg(feature = "cache")]{
        use crate::cache::{write_cached_llvm_ir, get_cached_llvm_ir};
    } else {
        fn write_cached_llvm_ir(_bitcode_path : &Path, _opt_level : OptimizationLevel, _content : &str, _shared_libs : &[String]){}
        fn get_cached_llvm_ir(_content : &str, _opt_level : OptimizationLevel) -> Option<CachedCompilation>{
            None
        }
    }
}


// what is used a specific monomorphised for a generic function (used to verify if already generated)
#[derive(Eq, Hash, PartialEq)]
struct GenericFunIdentifier {
    name : StringRef,
    arg_types : Vec<Type>,
    ret_type : Type,
}

#[allow(unused)]
pub(crate) struct JITCpuInfos {
    pub(crate) cpu_features : String,
    pub(crate) cpu_name : String,
}

pub(crate) struct CompileContext<'context, 'llvm_ctx> {
    pub(crate) rustaml_context : &'context mut RustamlContext,
    pub(crate) context : &'llvm_ctx Context,
    pub(crate) module : Module<'llvm_ctx>,
    pub(crate) builder : Builder<'llvm_ctx>,
    pub(crate) debug_info : DebugInfo<'llvm_ctx>,
    pub(crate) typeinfos : TypeInfos,
    functions : FxHashMap<StringRef, FunctionValue<'llvm_ctx>>,
    pub(crate) main_function : FunctionValue<'llvm_ctx>,
    pub(crate) var_vals : FxHashMap<StringRef, PointerValue<'llvm_ctx>>,
    pub(crate) external_symbols_declared : FxHashSet<&'static str>,
    internal_functions : Vec<BuiltinFunction<'llvm_ctx>>, // TODO : replace this with a hashmap ?
    pub(crate) global_strs : FxHashMap<String, PointerValue<'llvm_ctx>>,
    pub(crate) is_optimized : bool,
    shared_libs : Vec<String>,

    closure_idx : Cell<u32>,

    pub(crate) target_data : TargetData,

    generic_functions : FxHashMap<GenericFunIdentifier, FunctionValue<'llvm_ctx>>,
    pub(crate) generic_map : FxHashMap<u32, Type>,
    generic_func_def_ast_node : FxHashMap<StringRef, ASTRef>,

    pub(crate) monomorphized_internal_fun : FxHashMap<&'static str, FxHashMap<(Type, Type), FunctionValue<'llvm_ctx>>>, // (Type A, Type B) = function List A -> List B

    #[cfg(feature = "jit")]
    pub(crate) jit_cpu_infos : Option<JITCpuInfos>, // needed for JIT to annotate functions with the target features
}


#[derive(Clone, Default)]
struct BuiltinFunction<'llvm_ctx> {
    name : &'static str,
    args : Box<[BasicMetadataTypeEnum<'llvm_ctx>]>,
    ret : Option<AnyTypeEnum<'llvm_ctx>>, // will always be Some, just for the default implementation
    attributes : Vec<(AttributeLoc, Attribute)>,
    is_variadic: bool,
}


fn new_attribute(llvm_context : &Context, name : &'static str) -> Attribute {
    let attribute_id = Attribute::get_named_enum_kind_id(name);
    llvm_context.create_enum_attribute(attribute_id, 0)
}

// TODO : replace these strings with an enum ?
fn get_internal_functions<'llvm_ctx>(llvm_context : &'llvm_ctx Context) -> Vec<BuiltinFunction<'llvm_ctx>>{
    // TODO : make these 2 only one
    let ptr_type = llvm_context.ptr_type(AddressSpace::default()).into();
    let ptr_type_ret = llvm_context.ptr_type(AddressSpace::default()).into();

    let attr = |n| (AttributeLoc::Function, new_attribute(llvm_context, n));
    let attr_return = |n| (AttributeLoc::Return, new_attribute(llvm_context, n));
    let attr_args = |n, idx| (AttributeLoc::Param(idx), new_attribute(llvm_context, n));
    
    vec![
        BuiltinFunction {
            name: "__init",
            args: Box::new([]),
            ret: Some(llvm_context.void_type().into()),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__str_cmp",
            args: Box::new([ptr_type, ptr_type]),
            ret: Some(llvm_context.bool_type().into()),
            attributes: vec![attr_args("noundef", 0), attr_args("noundef", 1)],
            ..Default::default()
        },
        BuiltinFunction {
            name: "__str_append",
            args: Box::new([ptr_type, ptr_type]),
            ret: Some(ptr_type_ret),
            attributes: vec![attr_return("noalias")],
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_node_init_static",
            args: Box::new([llvm_context.i8_type().into(), ptr_type, llvm_context.i64_type().into()]),
            ret: Some(ptr_type_ret),
            attributes: vec![attr_return("noalias")],
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_node_append",
            args: Box::new([ptr_type, llvm_context.i8_type().into(), llvm_context.i64_type().into()]),
            ret: Some(ptr_type_ret),
            attributes: vec![attr_return("noalias")],
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_node_append_back",
            args: Box::new([ptr_type, llvm_context.i8_type().into(), llvm_context.i64_type().into()]),
            ret: Some(ptr_type_ret),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_node_merge",
            args: Box::new([ptr_type, ptr_type]),
            ret: Some(ptr_type_ret),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_len",
            args: Box::new([ptr_type]),
            ret: Some(llvm_context.i64_type().into()),
            attributes: vec![attr_args("noundef", 0)],
            ..Default::default()
        },
        BuiltinFunction {
            name: "__bool_to_str",
            args: Box::new([llvm_context.bool_type().into()]),
            ret: Some(ptr_type_ret),
            attributes: vec![attr_return("noundef"), attr_return("nonnull")],
            ..Default::default()
        },
        BuiltinFunction {
            name: "__char_to_str",
            args: Box::new([llvm_context.i32_type().into()]),
            ret: Some(ptr_type_ret),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__rand",
            args: Box::new([]),
            ret: Some(llvm_context.i64_type().into()),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__format_string",
            is_variadic: true,
            args: Box::new([ptr_type]),
            ret: Some(ptr_type_ret),
            attributes: vec![attr_return("noalias"), attr_args("noundef", 0)],
        },
        BuiltinFunction {
            name: "__chars",
            args: Box::new([ptr_type]),
            ret: Some(ptr_type_ret),
            attributes: vec![attr_return("noalias"), attr_args("nonnull", 0)],
            ..Default::default()
        },
        BuiltinFunction {
            name: "__regex_create",
            args: Box::new([ptr_type]),
            ret: Some(ptr_type_ret),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__regex_has_match",
            args: Box::new([ptr_type, ptr_type]),
            ret: Some(llvm_context.bool_type().into()),
            ..Default::default()
        },
        // TODO : remove this ?
        BuiltinFunction {
            name: "fprintf",
            is_variadic: true,
            args: Box::new([ptr_type, ptr_type]),
            ret: Some(llvm_context.i32_type().into()),
            attributes: vec![attr_args("noundef", 0), attr_args("noundef", 1)],
        },
        BuiltinFunction {
            name: "__print_val",
            is_variadic: true,
            args: Box::new([ptr_type]),
            ret: Some(llvm_context.void_type().into()),
            attributes: vec![attr_args("noundef", 0), attr_args("readonly", 0)],
        },
        BuiltinFunction {
            name: "exit",
            args: Box::new([llvm_context.i32_type().into()]),
            ret: Some(llvm_context.void_type().into()),
            attributes: vec![attr("noreturn"), attr_args("noundef", 0)],
            ..Default::default()
        },
    ]
}

impl<'context, 'llvm_ctx> CompileContext<'context, 'llvm_ctx> {
    pub(crate) fn new(
            rustaml_context: &'context mut RustamlContext, 
            context : &'llvm_ctx Context, 
            module : Module<'llvm_ctx>, 
            builder : Builder<'llvm_ctx>, 
            debug_info: DebugInfo<'llvm_ctx>,
            is_optimized : bool,
            type_infos : TypeInfos,
            target_data : TargetData,
            jit_cpu_infos : Option<JITCpuInfos>,
        ) -> CompileContext<'context, 'llvm_ctx> {
        let main_function = get_main_function(context, &module);
        let internal_functions = get_internal_functions(context);

        #[cfg(not(feature = "jit"))]
        let _ = jit_cpu_infos;

        CompileContext {
            rustaml_context,
            context,
            module,
            builder,
            debug_info,
            is_optimized,
            typeinfos: type_infos,
            main_function,
            
            internal_functions,
            target_data,
            
            monomorphized_internal_fun: init_monomorphized_internal_fun(),
            closure_idx: Cell::new(0),
            var_vals: FxHashMap::default(),
            external_symbols_declared: FxHashSet::default(),
            global_strs: FxHashMap::default(),
            shared_libs: Vec::new(),
            generic_map: FxHashMap::default(),
            generic_functions: FxHashMap::default(),
            generic_func_def_ast_node: FxHashMap::default(),
            functions: FxHashMap::default(),

            #[cfg(feature = "jit")]
            jit_cpu_infos,
        }
    }

    pub(crate) fn get_internal_function(&mut self, name : &'static str) -> FunctionValue<'llvm_ctx> {
        if self.external_symbols_declared.contains(name){
            self.module.get_function(name).unwrap()
        } else {
            // use find instead of a hashmap because the number of internal functions is low
            let builtin_function = self.internal_functions.iter().find(|f| f.name == name).unwrap();
            let function_type = get_fn_type(self.context, builtin_function.ret.unwrap(), &builtin_function.args, builtin_function.is_variadic);
            let function_decl = add_function(self, name, function_type, Some(Linkage::External));
            for &(attr_loc, attr) in &builtin_function.attributes {
                function_decl.add_attribute(attr_loc, attr);
            }
            self.external_symbols_declared.insert(name);
            function_decl
        }
    }


    // TODO : if this function become more used, make a list like the internal function for builtin_global_vars types
    // or use an enum for name because it is static
    pub(crate) fn get_internal_global_var(&mut self, name : &'static str, type_var : BasicTypeEnum<'llvm_ctx>) -> GlobalValue<'llvm_ctx> {
        if self.external_symbols_declared.contains(name){
            self.module.get_global(name).unwrap()
        } else {
            let global = self.module.add_global(type_var, None, "stderr");
            global.set_linkage(inkwell::module::Linkage::External);
            self.external_symbols_declared.insert(name);
            global
        }
    }
}

fn compile_var_decl<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, ast_node : ASTRef, name : StringRef, val : ASTRef, body : Option<ASTRef>, is_global : bool) -> CompilerValue<'llvm_ctx> {
    let is_underscore = name.get_str(&compile_context.rustaml_context.str_interner) == "_";
    
    //println!("test vars types : {:#?}", DebugWrapContext::new(&compile_context.var_types, compile_context.rustaml_context));

    let val = compile_expr(compile_context, val).into_basic();

    if !is_underscore {
        let var_id = get_var_id(compile_context, ast_node);
        debug_println!(compile_context.rustaml_context.is_debug_print, "var_id  : {:?}", DebugWrapContext::new(&var_id, compile_context.rustaml_context));
        let var_type = get_var_type(compile_context, var_id, name).clone();
        debug_println!(compile_context.rustaml_context.is_debug_print, "var_type decl {:?} : {:?}", name.get_str(&compile_context.rustaml_context.str_interner), var_type);
        let alloca_type = get_llvm_type(compile_context, &var_type);
        let var_ptr = create_var(compile_context, name, val, alloca_type);
        //dbg!(compile_context.builder.get_insert_block().unwrap());
        
        if compile_context.debug_info.inner.is_some(){
            // TODO : why it fixes the debug value declaring (URGENT !! PROBLEM WITH DBG_DECLARE)
            unsafe { LLVMPrintValueToString(compile_context.builder.get_insert_block().unwrap().as_mut_ptr() as LLVMValueRef); }
        }
        //dbg!(name.get_str(&compile_context.rustaml_context.str_interner));
        compile_context.debug_info.declare_var(name.get_str(&compile_context.rustaml_context.str_interner), &var_type, var_ptr, compile_context.builder.get_insert_block().unwrap(), compile_context.rustaml_context.content.as_ref().unwrap(), ast_node.get_range(&compile_context.rustaml_context.ast_pool));
    }
        
    let ret = match body {
        Some(b) => compile_expr(compile_context, b),
        None => val.into(),
    };

    if !is_global && !is_underscore {
        compile_context.var_vals.remove(&name);
    }


    ret
}

fn compile_if<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, cond_expr : ASTRef, then_body : ASTRef, else_body : ASTRef) -> BasicValueEnum<'llvm_ctx> {
    let this_function = get_current_function(&compile_context.builder);
    let then_bb= compile_context.context.append_basic_block(this_function, "if");
    let else_bb = compile_context.context.append_basic_block(this_function, "else");
    let after_bb = compile_context.context.append_basic_block(this_function, "afterif");

    let bool_val = compile_expr(compile_context, cond_expr).unwrap_basic();

    create_br_conditional(compile_context, bool_val.into_int_value(), then_bb, else_bb);

    compile_context.builder.position_at_end(then_bb);


    let if_val = compile_expr(compile_context, then_body);
    let has_br_then = create_br_unconditional(compile_context, after_bb);

    let then_bb_last = compile_context.builder.get_insert_block().unwrap();

    move_bb_after_current(compile_context, else_bb);


    compile_context.builder.position_at_end(else_bb);

    let else_val = compile_expr(compile_context, else_body);
    let has_br_else = create_br_unconditional(compile_context, after_bb);

    let else_bb_last = compile_context.builder.get_insert_block().unwrap();


    move_bb_after_current(compile_context, after_bb);
    compile_context.builder.position_at_end(after_bb);

    let phi_node = compile_context.builder.build_phi(if_val.get_basic_type(compile_context.context), "if_phi").unwrap();
    let if_val_basic = if_val.into_basic();
    let else_val_basic = else_val.into_basic();
    
    let mut incoming = Vec::new();

    if has_br_then {
        incoming.push((&if_val_basic as _, then_bb_last));
    }

    if has_br_else {
        incoming.push((&else_val_basic as _, else_bb_last));
    }

    //phi_node.add_incoming(&[(&if_val_basic as _, then_bb_last), (&else_val_basic as _, else_bb_last)]);
    phi_node.add_incoming(&incoming);
    phi_node.as_basic_value()
}

fn get_format_ctype(c_type : &CType) -> &'static str {
    match c_type {
        CType::I32 => todo!(),
        CType::U64 => "%Cu64",
        _ => panic!("Can't print ctypes {:?}", c_type)  // TODO
    }
    
}

fn get_format_string(print_type : &Type) -> &'static str {
    match print_type {
        Type::Integer => "%d",
        Type::Float => "%f",
        Type::Str => "%s",
        Type::Bool => "%b", 
        Type::Char => "%c",
        Type::List(_) => "%l",
        Type::Function(_, _, _) => panic!("Can't print functions"),
        Type::Unit => "%u",
        Type::Never => "%n", // can't print it, normally if the function is really a never type, it should be never return, so the print should never be called 
        Type::CType(c_type) => get_format_ctype(c_type),
        Type::Any => encountered_any_type(),
        Type::Regex => panic!("Can't print regex"), // TODO ?
        Type::Vec(_, _) => "%v",
        Type::SumType(_) => unreachable!(), // TODO ?
        Type::Generic(_) => unreachable!(),
    }
}

fn compile_print<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, print_val : BasicValueEnum<'llvm_ctx>, print_val_type : &Type) -> BasicValueEnum<'llvm_ctx> {

    let printf_fun = compile_context.get_internal_function("__print_val");
    let format_str = get_format_string(print_val_type);
    let format_str = create_string(compile_context, format_str);
    let print_val = promote_val_var_arg(compile_context, print_val_type, print_val);
    let print_val = match (print_val, print_val_type) {
        (print_val, Type::Vec(e, size)) => {
            vec_to_c_struct_ptr(compile_context, print_val.into_vector_value(), e.as_ref()).into()
        }
        _ => print_val,
    };
    let printf_args = [format_str.into(), print_val.into()];
    compile_context.builder.build_call(printf_fun, &printf_args, "print_internal_call").unwrap();
    get_void_val(compile_context.context)
}

fn compile_rand<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>) -> BasicValueEnum<'llvm_ctx>{
    let rand_fun = compile_context.get_internal_function("__rand");
    compile_context.builder.build_call(rand_fun, &[], "rand_internal_call").unwrap().try_as_basic_value().unwrap_basic()
}

fn get_format_string_format(format_str : &str, arg_types : &[Type]) -> String {
    let mut format_string_ret = "".to_string();
    format_string_ret.reserve(format_str.len());
    let format_str_chars = format_str.chars().collect::<Vec<_>>();
    let mut arg_idx = 0;
    let mut i = 0;
    while i < format_str.len() {
        let c = format_str_chars[i];
        match c {
            '{' => {
                i += 1;
                let c = format_str_chars[i];
                match c {
                    '}' => {
                        let arg_type = arg_types.get(arg_idx);
                        let arg_type = match arg_type {
                            Some(a) => a,
                            None => panic!("ERROR: missing {{}} for the arg number {}", arg_idx),
                        };
                        let arg_format_str = get_format_string(arg_type);
                        format_string_ret.push_str(arg_format_str);

                        arg_idx += 1;
                    },
                    '{' => format_string_ret.push_str("{{"),
                    _ => panic!("ERROR : wrong char in format string : {}", c),
                }
            },
            _ => format_string_ret.push(c),
        }
        i += 1;
    }

    if arg_idx != arg_types.len() {
        panic!("mismatched number of {{}} and args in format ({{}} : {}, args : {})", i, arg_types.len());
    }

    //println!("REAL FORMAT STRING : {}", &format_string_ret);
    format_string_ret
}

fn compile_format<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, format_str : StringRef, args_val : Vec<BasicValueEnum<'llvm_ctx>>, arg_types : Vec<Type>) -> BasicValueEnum<'llvm_ctx>{
    let format_fun = compile_context.get_internal_function("__format_string");
    let format_str = format_str.get_str(&compile_context.rustaml_context.str_interner);
    let format_str = get_format_string_format(format_str, arg_types.as_slice());
    let mut args= vec![create_string(compile_context, &format_str).into()];
    let mut args_val = args_val.into_iter().zip(arg_types).map(|(e, t)| promote_val_var_arg(compile_context, &t, e)).collect::<Vec<_>>();
    args.append(&mut args_val);
    let args = args.into_iter().map(|e| e.into()).collect::<Vec<_>>();
    compile_context.builder.build_call(format_fun, &args, "format_string_internal_call").unwrap().try_as_basic_value().unwrap_basic()
}

fn compile_panic<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, message_str : PointerValue<'llvm_ctx>) -> BasicValueEnum<'llvm_ctx>{
    _codegen_runtime_error(compile_context, message_str);
    get_void_val(compile_context.context)

}

fn compile_map<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, list_ast : ASTRef, fun_ast : ASTRef) -> BasicValueEnum<'llvm_ctx> {
    let ret_elem_type = match fun_ast.get_type(&compile_context.rustaml_context.ast_pool) {
        Type::Function(_, ret, _) => ret.as_ref().clone(),
        _ => unreachable!(),
    };
    
    let elem_type = match list_ast.get_type(&compile_context.rustaml_context.ast_pool) {
        Type::List(e) => e.as_ref().clone(),
        _ => unreachable!()
    };

    
    let fun_val = compile_expr(compile_context, fun_ast).into_basic();

    let list_val = compile_expr(compile_context, list_ast).into_basic();


    let args= vec![list_val.into(), fun_val.into()];
    
    let map_fun = compile_monomorphized_map(compile_context, &elem_type, &ret_elem_type);

    compile_context.builder.build_call(map_fun, &args, "map_call").unwrap().try_as_basic_value().unwrap_basic()
}

fn compile_filter<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, list_ast : ASTRef, fun_ast : ASTRef) -> BasicValueEnum<'llvm_ctx> {
    let arg_type = match fun_ast.get_type(&compile_context.rustaml_context.ast_pool) {
        Type::Function(args, _, _) => args.iter().next().unwrap().clone(),
        _ => unreachable!(),
    };
    
    let elem_type = match list_ast.get_type(&compile_context.rustaml_context.ast_pool) {
        Type::List(e) => e.as_ref().clone(),
        _ => unreachable!()
    };

    assert_eq!(arg_type, elem_type);

    
    let fun_val = compile_expr(compile_context, fun_ast).into_basic();

    let list_val = compile_expr(compile_context, list_ast).into_basic();


    let args= vec![list_val.into(), fun_val.into()];
    
    let filter_fun = compile_monomorphized_filter(compile_context, &elem_type);

    compile_context.builder.build_call(filter_fun, &args, "filter_call").unwrap().try_as_basic_value().unwrap_basic()
}

fn compile_chars<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, s : PointerValue<'llvm_ctx>) -> BasicValueEnum<'llvm_ctx> {
    let chars_fun = compile_context.get_internal_function("__chars");
    let args = vec![s.into()];
    compile_context.builder.build_call(chars_fun, &args, "chars_call").unwrap().try_as_basic_value().unwrap_basic()
}

fn compile_regex_create<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, re_str : PointerValue<'llvm_ctx>) -> BasicValueEnum<'llvm_ctx> {
    let create_fun = compile_context.get_internal_function("__regex_create");
    let args = vec![re_str.into()];
    compile_context.builder.build_call(create_fun, &args, "regex_create_call").unwrap().try_as_basic_value().unwrap_basic()
}

fn compile_regex_has_match<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, re : PointerValue<'llvm_ctx>, str : PointerValue<'llvm_ctx>) -> BasicValueEnum<'llvm_ctx> {
    let has_match_fun = compile_context.get_internal_function("__regex_has_match");
    let args = vec![re.into(), str.into()];
    compile_context.builder.build_call(has_match_fun, &args, "regex_has_match_call").unwrap().try_as_basic_value().unwrap_basic()
}

// TODO : generate a monomorphized black_box function to not have a lot of junk stack allocations because of it in the function ?
fn compile_black_box<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, arg : BasicValueEnum<'llvm_ctx>) -> BasicValueEnum<'llvm_ctx> {
    let arg_llvm_type = arg.get_type();
    let buf_ptr = create_entry_block_alloca(compile_context, "buf_blackbox", arg_llvm_type.as_any_type_enum());
    
    compile_context.builder.build_store(buf_ptr, arg).unwrap();

    let inline_asm_ty = get_fn_type(compile_context.context, compile_context.context.void_type().into(), &[compile_context.context.ptr_type(AddressSpace::default()).into()], false);
    let constraints = "r,~{memory}".to_owned();
    let inline_asm = 
        compile_context.context.create_inline_asm(inline_asm_ty, "".to_owned(), constraints, true, false, None, false);
    
    compile_context.builder.build_indirect_call(inline_asm_ty, inline_asm, &[buf_ptr.into()], "black_box").unwrap();
    compile_context.builder.build_load(arg_llvm_type, buf_ptr, "load_black_box").unwrap();
    return get_void_val(compile_context.context); // TODO : return what is loaded instead, but would need work to make return generics work (ex : need that every generic have each an unique index, so would need to keep the number of generics in the ast somewhere because of type generics annotation)
}

fn should_monomorphize_function(arg_types : &[Type], ret_type : &Type) -> bool {
    matches!(ret_type, Type::Generic(_)) || arg_types.iter().any(|e| matches!(e, Type::Generic(_)))
}

fn mangle_name<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, name : StringRef, arg_types : &[Type], ret_type : &Type) -> StringRef {
    let mut name_str = name.get_str(&compile_context.rustaml_context.str_interner).to_owned();
    name_str.push(' ');
    // TODO : only have args that are generics in the mangling ?
    for (idx, arg_type) in arg_types.iter().enumerate() {
        if idx != 0 {
            name_str.push_str(", ");
        }
        name_str.push_str(&format!("{}", arg_type));
    }
    name_str.push_str(&format!(" -> {}", ret_type));
    compile_context.rustaml_context.str_interner.intern_compiler(&name_str)
}

// TODO : add support for return type generic

fn monomophize_function<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, name : StringRef, args_call_types : Vec<Type>, args_def_types : Box<[Type]>, ret_type : &Type) -> FunctionValue<'llvm_ctx> {
    let ast_node = *compile_context.generic_func_def_ast_node.get(&name).unwrap();
    let (args, body) = match ast_node.get(&compile_context.rustaml_context.ast_pool){
        ASTNode::FunctionDefinition { name: _, args, body, type_annotation: _ } => {
            (args.clone(), *body)
        }
        _ => unreachable!(),
    };

    let mut arg_types_without_generics = Vec::new();

    for (arg_call, args_def_type) in args_call_types.into_iter().zip(args_def_types) {
        match args_def_type {
            Type::Generic(gen_idx) => {
                compile_context.generic_map.insert(gen_idx, arg_call.clone()); // TODO : handle if the gen_idx was already set to something ? (is it even possible ?)
                arg_types_without_generics.push(arg_call);
            }
            a => arg_types_without_generics.push(a),
        }
    }
    
    let current_bb = compile_context.builder.get_insert_block().unwrap();
    let mangled_name = mangle_name(compile_context, name, &arg_types_without_generics, ret_type);
    let def = compile_function_def(compile_context, mangled_name, &args, body, ast_node, &arg_types_without_generics, ret_type);
    compile_context.builder.position_at_end(current_bb);
    def
}

fn compile_function_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, callee : ASTRef, args: &[ASTRef], range : Range<usize>) -> BasicValueEnum<'llvm_ctx>{
    let (name_str, name) = if let ASTNode::VarUse { name } = callee.get(&compile_context.rustaml_context.ast_pool) {
        match name.get_str(&compile_context.rustaml_context.str_interner) {
            "print" => {
                //let print_val_type = args[0].get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types).unwrap();
                let print_val_type = args[0].get_type(&compile_context.rustaml_context.ast_pool).clone();
                let print_val = compile_expr(compile_context, args[0]).into_basic();
                return compile_print(compile_context, print_val, &print_val_type);
            }
            "rand" => {
                return compile_rand(compile_context);
            },
            "format" => {
                let (format_ast, args_ast) = args.split_first().unwrap();
                let format_str = match format_ast.get(&compile_context.rustaml_context.ast_pool) {
                    ASTNode::String { str } => *str,
                    _ => unreachable!(),
                };
                //let args_types = args_ast.iter().map(|&a| a.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types).unwrap()).collect::<Vec<_>>();
                let args_types = args_ast.iter().map(|&a| a.get_type(&compile_context.rustaml_context.ast_pool).clone()).collect();
                let args_val = args_ast.iter().map(|&a| compile_expr(compile_context, a).into_basic()).collect::<Vec<_>>();
                
                return compile_format(compile_context, format_str, args_val, args_types);
            }
            "panic" => {
                let message_val = compile_expr(compile_context, args[0]).unwrap_basic().into_pointer_value();
                return compile_panic(compile_context, message_val);
            }
            "map" => {
                let list = args[0];
                let func = args[1];
                return compile_map(compile_context, list, func);
            }
            "filter" => {
                let list = args[0];
                let func = args[1];
                return compile_filter(compile_context, list, func);
            }
            "chars" => {
                let s = compile_expr(compile_context, args[0]).unwrap_basic().into_pointer_value();
                return compile_chars(compile_context, s);
            }
            "regex_create" => {
                let re_str = compile_expr(compile_context, args[0]).unwrap_basic().into_pointer_value();
                return compile_regex_create(compile_context, re_str);
            }
            "regex_has_match" => {
                let re = compile_expr(compile_context, args[0]).unwrap_basic().into_pointer_value();
                let str = compile_expr(compile_context, args[1]).unwrap_basic().into_pointer_value();
                return compile_regex_has_match(compile_context, re, str);
            }
            "black_box" => {
                let arg = compile_expr(compile_context, args[0]).unwrap_basic();
                return compile_black_box(compile_context, arg);
            }
            n => (Some(n.to_owned()), Some(*name)),
        }
    } else {
        (None, None)
    };
    
    
    
    //let fun = *compile_context.functions.get(&name).unwrap();

    let callee_type = callee.get_type(&compile_context.rustaml_context.ast_pool).clone();

    let (arg_types, ret_type) = match &callee_type {
        Type::Function(args, ret, _) => (args.clone(), ret.as_ref().clone()),
        _ => unreachable!(),
    };
    

    //let name_str = name.get_str(&compile_context.rustaml_context.str_interner);
    let arg_call_types = args.iter().map(|&a| a.get_type(&compile_context.rustaml_context.ast_pool).clone()).collect::<Vec<_>>();
    let args_vals = args.iter().map(|&a| compile_expr(compile_context, a).into_basic().into()).collect::<Vec<BasicMetadataValueEnum>>();
    
    let should_monomophize = should_monomorphize_function(&arg_types, &ret_type);

    let callee_val = if should_monomophize {
        let name = name.unwrap(); // TODO : make this support anonymous functions ? (hash function value instead of name ?)
        let ret_call_type = Type::Any; // TODO
        let function_identifier = GenericFunIdentifier {
            name,
            arg_types: arg_call_types.clone(),
            ret_type: ret_call_type,
        };
        if let Some(func) = compile_context.generic_functions.get(&function_identifier) {
            *func
        } else {
            monomophize_function(compile_context, name, arg_call_types, arg_types, &ret_type)
        }.as_any_value_enum()
    } else {
        compile_expr(compile_context, callee).as_any_value_enum()
    };

    let callee_type_llvm = get_llvm_type(compile_context, &callee_type).into_function_type();
    
    

    let function_call_dbg = compile_context.debug_info.create_debug_location(compile_context.context, compile_context.rustaml_context.content.as_ref().unwrap(), range);
    if let Some(function_call_dbg) = function_call_dbg {
        compile_context.builder.set_current_debug_location(function_call_dbg);
    }

    let name_anon_func_call = "anon_func_call";

    let ret = match callee_val {
        AnyValueEnum::FunctionValue(fun) => compile_context.builder.build_call(fun, args_vals.as_slice(), name_str.as_deref().unwrap_or(name_anon_func_call)),
        _ => compile_context.builder.build_indirect_call(callee_type_llvm, callee_val.into_pointer_value(), args_vals.as_slice(), name_anon_func_call)
    }.unwrap().try_as_basic_value();

    match ret {
        ValueKind::Basic(basic) => basic,
        ValueKind::Instruction(i) => panic!("error, returning {:?}", i), // TODO ?
    }

    /*match ret {
        Either::Left(l) => l.into(),
        Either::Right(_) => get_void_val(compile_context.context), // void, dummy value
    }*/
}

fn compile_check_overflow<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, intrisic_name : &'static str, message : &'static str, i : IntValue<'llvm_ctx>, i2 : IntValue<'llvm_ctx>, name : &str, range : Range<usize>) -> IntValue<'llvm_ctx> {
    // TODO : do the instrisic find only one time ?
    let llvm_sadd_intrisic = Intrinsic::find(intrisic_name).unwrap();
    let llvm_sadd_decl = llvm_sadd_intrisic.get_declaration(&compile_context.module, &[compile_context.context.i64_type().into()]).unwrap();

    let res_struct_val = compile_context.builder.build_call(llvm_sadd_decl, &[i.into(), i2.into()], name).unwrap().as_any_value_enum().into_struct_value();
    let res_val = compile_context.builder.build_extract_value(res_struct_val, 0, &format!("extract_val_{}", name)).unwrap().into_int_value();
    let is_overflow = compile_context.builder.build_extract_value(res_struct_val, 1, &format!("extract_overflow_{}", name)).unwrap().into_int_value();
                    
    let this_function = get_current_function(&compile_context.builder);
    let overflow_bb = compile_context.context.append_basic_block(this_function, &format!("overflow_{}", name));
    let after_overflow_check_bb = compile_context.context.append_basic_block(this_function, &format!("after_overflow_check_{}", name));

    compile_context.builder.build_conditional_branch(is_overflow, overflow_bb, after_overflow_check_bb).unwrap();

    compile_context.builder.position_at_end(overflow_bb);
    codegen_lang_runtime_error(compile_context, message, get_debug_loc(compile_context.rustaml_context.content.as_ref().unwrap(), range));

    compile_context.builder.position_at_end(after_overflow_check_bb);

    res_val
}

fn compile_div_or_rem_checked<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, i : IntValue<'llvm_ctx>, i2 : IntValue<'llvm_ctx>, is_div : bool, name : &str, range : Range<usize>) -> IntValue<'llvm_ctx> {
    let line_col = get_debug_loc(compile_context.rustaml_context.content.as_ref().unwrap(), range);
    
    let const_zero = compile_context.context.i64_type().const_zero();
    let is_zero = compile_context.builder.build_int_compare(IntPredicate::EQ, i2, const_zero, "cmp_div_zero").unwrap();

    let this_function = get_current_function(&compile_context.builder);
    let is_zero_bb = compile_context.context.append_basic_block(this_function, &format!("{}_zero", name));

    let check_overflow_bb = compile_context.context.append_basic_block(this_function, &format!("check_overflow_{}", name));
    
    
    compile_context.builder.build_conditional_branch(is_zero, is_zero_bb, check_overflow_bb).unwrap();


    compile_context.builder.position_at_end(is_zero_bb);
    let zero_error = if is_div {
        "Division by zero"
    } else {
        "Calculating remainder with zero"
    };
    codegen_lang_runtime_error(compile_context, zero_error, line_col.clone());

    let is_overflow_bb = compile_context.context.append_basic_block(this_function, &format!("overflow_{}", name));

    let after_checks = compile_context.context.append_basic_block(this_function, &format!("after_checks_{}", name));

    compile_context.builder.position_at_end(check_overflow_bb);
    let const_minus_one = compile_context.context.i64_type().const_int((-1i64) as u64, true);
    let is_rhs_minus_one = compile_context.builder.build_int_compare(IntPredicate::EQ, i2, const_minus_one, "cmp_lhs_-1").unwrap();
    
    let const_i64_min = compile_context.context.i64_type().const_int(i64::MIN as u64, true);
    let is_lhs_i64_min = compile_context.builder.build_int_compare(IntPredicate::EQ, i, const_i64_min, "cmp_rhs_i64_min").unwrap();
    
    let is_overflow = compile_context.builder.build_and(is_rhs_minus_one, is_lhs_i64_min, &format!("and_overflow_check_{}", name)).unwrap();

    compile_context.builder.position_at_end(check_overflow_bb);
    compile_context.builder.build_conditional_branch(is_overflow, is_overflow_bb, after_checks).unwrap();

    compile_context.builder.position_at_end(is_overflow_bb);
    let overflow_error = if is_div {
        "Overflow when dividing"
    } else {
        "Overflow when calculating remainder"
    };

    codegen_lang_runtime_error(compile_context, overflow_error, line_col);


    compile_context.builder.position_at_end(after_checks);
    
    if is_div {
        compile_context.builder.build_int_signed_div(i, i2, "div").unwrap()
    } else {
        compile_context.builder.build_int_signed_rem(i, i2, "rem").unwrap()
    }
    
}

fn compile_binop_int<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, lhs_val : BasicValueEnum<'llvm_ctx>, rhs_val : BasicValueEnum<'llvm_ctx>, name : &str, range : Range<usize>) -> IntValue<'llvm_ctx>{
    
    match (lhs_val, rhs_val){
        (BasicValueEnum::IntValue(i),  BasicValueEnum::IntValue(i2)) => {
            match op {
                // TODO : add a flag to disable the overflow checks
                Operator::Plus => compile_check_overflow(compile_context, "llvm.sadd.with.overflow", "Overflow when adding", i, i2, name, range),
                Operator::Minus => compile_check_overflow(compile_context, "llvm.ssub.with.overflow", "Overflow when substracting", i, i2, name, range),
                Operator::Mult => compile_check_overflow(compile_context, "llvm.smul.with.overflow", "Overflow when multiplying", i, i2, name, range),
                Operator::Div => compile_div_or_rem_checked(compile_context, i, i2, true, name, range),
                Operator::Rem => compile_div_or_rem_checked(compile_context, i, i2, false, name, range),
                _ => unreachable!(),
            }
        }
        _ => panic!("Invalid type for integer op {:?} (lhs : {:?}, rhs : {:?})", op, lhs_val, rhs_val),
    }
    
}

fn compile_binop_float<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, lhs_val : BasicValueEnum<'llvm_ctx>, rhs_val : BasicValueEnum<'llvm_ctx>, name : &str) -> FloatValue<'llvm_ctx>{
    match (lhs_val, rhs_val){
        (BasicValueEnum::FloatValue(f),  BasicValueEnum::FloatValue(f2)) => {
            match op {
                Operator::PlusFloat => compile_context.builder.build_float_add(f, f2, name).unwrap(),
                Operator::MinusFloat => compile_context.builder.build_float_sub(f, f2, name).unwrap(),
                Operator::MultFloat => compile_context.builder.build_float_mul(f, f2, name).unwrap(),
                Operator::DivFloat => compile_context.builder.build_float_div(f, f2, name).unwrap(),
                Operator::RemFloat => compile_context.builder.build_float_rem(f, f2, name).unwrap(),
                _ => unreachable!(),
            }
        },

        _ => panic!("Invalid type for float op {:?} ({:?}, {:?})", op, lhs_val, rhs_val),
    }
}

fn compile_binop_bool<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, lhs_val : BasicValueEnum<'llvm_ctx>, rhs_val : BasicValueEnum<'llvm_ctx>, operand_type : &Type, name : &str) -> IntValue<'llvm_ctx>{

    if let Type::Unit = operand_type {
        // both types should be unit, so return true
        return compile_context.context.bool_type().const_int(true as u64, false);
    }

    match (lhs_val, rhs_val){
        (BasicValueEnum::IntValue(i),  BasicValueEnum::IntValue(i2)) => {
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
        (BasicValueEnum::FloatValue(f),  BasicValueEnum::FloatValue(f2)) => {
            let predicate = match op {
                Operator::IsEqual => FloatPredicate::OEQ,
                Operator::IsNotEqual => FloatPredicate::ONE,
                Operator::Inferior => FloatPredicate::OLT,
                Operator::Superior => FloatPredicate::OGT,
                Operator::InferiorOrEqual => FloatPredicate::OLE,
                Operator::SuperiorOrEqual => FloatPredicate::OGE,
                _ => unreachable!(),
            };
            compile_context.builder.build_float_compare(predicate, f, f2, name).unwrap()
        },
        (BasicValueEnum::PointerValue(p),  BasicValueEnum::PointerValue(p2)) => {
            let args = vec![p.into(), p2.into()];
            let cmp_call = match operand_type {
                Type::List(_) => {
                    let list_cmp_fun = compile_context.get_internal_function("__list_cmp");
                    compile_context.builder.build_call(list_cmp_fun, &args, name)
                },
                Type::Str => {
                    let str_cmp_fun = compile_context.get_internal_function("__str_cmp");
                    compile_context.builder.build_call(str_cmp_fun, &args, name)
                },
                _ => unreachable!(),
            };

            cmp_call.unwrap().as_any_value_enum().into_int_value()
        },
        _ => panic!("Invalid type for bool op {:?}, {:?}", op, (lhs_val, rhs_val)),
    }
}

fn compile_binop_str<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, lhs_val : BasicValueEnum<'llvm_ctx>, rhs_val : BasicValueEnum<'llvm_ctx>, name : &str) -> BasicValueEnum<'llvm_ctx>{
    match op {
        Operator::StrAppend => {
            let str_append = compile_context.get_internal_function("__str_append");
            let args = vec![lhs_val.into(), rhs_val.into()];
            compile_context.builder.build_call(str_append, &args, name).unwrap().try_as_basic_value().unwrap_basic()
        },
        _ => unreachable!()
    }
}

fn compile_binop_list<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, lhs_val : BasicValueEnum<'llvm_ctx>, rhs_val : BasicValueEnum<'llvm_ctx>, elem_type : &Type) -> BasicValueEnum<'llvm_ctx>{

    // TODO : check that lhs_val is a val of the same tag as the rhs_val elements and that rhs_val is a list
    match op {
        Operator::ListAppend => {
            // TODO : to make this work, need to have better type inference for case like i :: l to not stop when finding that l is a List::Any, but also add the info that l is appended an int, so l is a List(Int)
            //dbg!(lhs_val, rhs_val);
           // dbg!(elem_type);
            let type_tag_val = get_type_tag_val(compile_context.context, elem_type);
            let std_lhs_val = as_val_in_list(compile_context, lhs_val, elem_type);
            create_list_append_call(compile_context, rhs_val.into_pointer_value(), type_tag_val, std_lhs_val).into()
        },
        Operator::ListMerge => {
            create_list_merge(compile_context, lhs_val.into_pointer_value(), rhs_val.into_pointer_value()).into()
        }
        _ => unreachable!(),
    }
}

fn compile_binop_vec<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, lhs_val : BasicValueEnum<'llvm_ctx>, rhs_val : BasicValueEnum<'llvm_ctx>) -> BasicValueEnum<'llvm_ctx> {
    let lhs_vec = lhs_val.into_vector_value();
    let rhs_vec = rhs_val.into_vector_value();
    let vec_element_type = lhs_vec.get_type().get_element_type();
    match op {
        Operator::PlusVec => {
            if vec_element_type.is_float_type(){
                compile_context.builder.build_float_add(lhs_vec, rhs_vec, "add_vec").unwrap()
            } else {
                compile_context.builder.build_int_add(lhs_vec, rhs_vec, "add_vec").unwrap()
            }
            
        }
        Operator::MinusVec => {
            if vec_element_type.is_float_type(){
                compile_context.builder.build_float_sub(lhs_vec, rhs_vec, "minus_vec").unwrap()
            } else {
                compile_context.builder.build_int_sub(lhs_vec, rhs_vec, "minus_vec").unwrap()
            }
        }
        Operator::MultVec => {
            if vec_element_type.is_float_type(){
                compile_context.builder.build_float_mul(lhs_vec, rhs_vec, "mult_vec").unwrap()
            } else {
                compile_context.builder.build_int_mul(lhs_vec, rhs_vec, "mult_vec").unwrap()
            }
        }
        Operator::DivVec => {
            if vec_element_type.is_float_type(){
                compile_context.builder.build_float_div(lhs_vec, rhs_vec, "div_vec").unwrap()
            } else {
                compile_context.builder.build_int_signed_div(lhs_vec, rhs_vec, "div_vec").unwrap()
            }
        }
        _ => unreachable!(),
    }.as_basic_value_enum()
}

// TODO : make also these work with vals (for example with an enum ShortCircuitingArg that can be an ASTRef or a LLVM value)
fn compile_short_circuiting_and<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, lhs : ASTRef, rhs : ASTRef) -> IntValue<'llvm_ctx> {
    let this_function = get_current_function(&compile_context.builder);
    let b1_true_bb = compile_context.context.append_basic_block(this_function, "and_b1_true");
    let b2_true_bb = compile_context.context.append_basic_block(this_function, "and_b2_true");
    let after_bb = compile_context.context.append_basic_block(this_function, "and_after");
    
    let lhs_val = compile_expr(compile_context, lhs).unwrap_basic();
    let b1 = match lhs_val {
        BasicValueEnum::IntValue(i) => i,
        _ => unreachable!(),
    };

    let start_bb = compile_context.builder.get_insert_block().unwrap();

    let has_start_br = create_br_conditional(compile_context, b1, b1_true_bb, after_bb);

    move_bb_after_current(compile_context, b1_true_bb);
    compile_context.builder.position_at_end(b1_true_bb);

    let rhs_val = compile_expr(compile_context, rhs).unwrap_basic();
    let b2 = match rhs_val {
        BasicValueEnum::IntValue(i) => i,
        _ => unreachable!(),
    };

    let has_b1_true_br = create_br_conditional(compile_context, b2, b2_true_bb, after_bb);

    let b1_true_bb_last = compile_context.builder.get_insert_block().unwrap();

    move_bb_after_current(compile_context, b2_true_bb);
    compile_context.builder.position_at_end(b2_true_bb);
    let has_b2_true_br = create_br_unconditional(compile_context, after_bb);

    let b2_true_bb_last = compile_context.builder.get_insert_block().unwrap();

    move_bb_after_current(compile_context, after_bb);

    compile_context.builder.position_at_end(after_bb);
    let const_false = compile_context.context.bool_type().const_int(false as u64, false);
    let const_true = compile_context.context.bool_type().const_int(true as u64, false);

    let phi_and = compile_context.builder.build_phi(compile_context.context.bool_type(), "and_phi").unwrap();
    let mut incoming: Vec<(&dyn BasicValue<'_>, BasicBlock<'_>)> = Vec::new();

    if has_start_br {
        incoming.push((&const_false, start_bb));
    }

    if has_b1_true_br {
        incoming.push((&const_false, b1_true_bb_last));
    }

    if has_b2_true_br {
        incoming.push((&const_true, b2_true_bb_last));
    }

    phi_and.add_incoming(&incoming);
    //phi_and.add_incoming(&[(&const_false, start_bb), (&const_false, b1_true_bb_last), (&const_true, b2_true_bb_last)]);
    phi_and.as_basic_value().into_int_value()
}

fn compile_short_circuiting_or<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, lhs : ASTRef, rhs : ASTRef) -> IntValue<'llvm_ctx> {
    let this_function = get_current_function(&compile_context.builder);
    let b1_false_bb = compile_context.context.append_basic_block(this_function, "or_b1_true");
    let b2_false_bb = compile_context.context.append_basic_block(this_function, "or_b2_true");
    let after_bb = compile_context.context.append_basic_block(this_function, "or_after");
    
    let lhs_val = compile_expr(compile_context, lhs).unwrap_basic();
    let b1 = match lhs_val {
        BasicValueEnum::IntValue(i) => i,
        _ => unreachable!(),
    };

    let start_bb = compile_context.builder.get_insert_block().unwrap();

    let has_start_br = create_br_conditional(compile_context, b1, after_bb, b1_false_bb);

    move_bb_after_current(compile_context, b1_false_bb);
    compile_context.builder.position_at_end(b1_false_bb);

    let rhs_val = compile_expr(compile_context, rhs).unwrap_basic();
    let b2 = match rhs_val {
        BasicValueEnum::IntValue(i) => i,
        _ => unreachable!(),
    };

    let has_b1_false_br = create_br_conditional(compile_context, b2, after_bb, b2_false_bb);

    let b1_false_bb_last = compile_context.builder.get_insert_block().unwrap();

    move_bb_after_current(compile_context, b2_false_bb);
    compile_context.builder.position_at_end(b2_false_bb);
    
    let has_b2_false_br = create_br_unconditional(compile_context, after_bb);

    let b2_false_bb_last = compile_context.builder.get_insert_block().unwrap();

    move_bb_after_current(compile_context, after_bb);

    compile_context.builder.position_at_end(after_bb);
    let const_false = compile_context.context.bool_type().const_int(false as u64, false);
    let const_true = compile_context.context.bool_type().const_int(true as u64, false);

    let phi_and = compile_context.builder.build_phi(compile_context.context.bool_type(), "or_phi").unwrap();
    
    let mut incoming: Vec<(&dyn BasicValue<'_>, BasicBlock<'_>)> = Vec::new();

    if has_start_br {
        incoming.push((&const_true, start_bb));
    }

    if has_b1_false_br {
        incoming.push((&const_true, b1_false_bb_last));
    }

    if has_b2_false_br {
        incoming.push((&const_false, b2_false_bb_last));
    }

    phi_and.add_incoming(&incoming);

    phi_and.as_basic_value().into_int_value()
}

fn compile_binop_bool_logical<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, lhs : ASTRef, rhs : ASTRef) -> IntValue<'llvm_ctx> {
    match op {
        Operator::And => compile_short_circuiting_and(compile_context, lhs, rhs),
        Operator::Or => compile_short_circuiting_or(compile_context, lhs, rhs),
        _ => unreachable!(),
    }
}

fn compile_binop<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, lhs : ASTRef, rhs : ASTRef, range : Range<usize>) -> BasicValueEnum<'llvm_ctx> {
    let name = format!("{:?}", op).to_lowercase();
    if matches!(op, Operator::And | Operator::Or){
        return compile_binop_bool_logical(compile_context, op, lhs, rhs).as_basic_value_enum();
    }

    let lhs_val = compile_expr(compile_context, lhs).unwrap_basic();
    let rhs_val = compile_expr(compile_context, rhs).unwrap_basic();

    let lhs_type = lhs.get_type(&compile_context.rustaml_context.ast_pool).clone();

    match op.get_res_type() {
        Type::Integer => compile_binop_int(compile_context, op, lhs_val, rhs_val, &name, range).into(),
        Type::Float => compile_binop_float(compile_context, op, lhs_val, rhs_val, &name).into(),
        Type::Bool => compile_binop_bool(compile_context, op, lhs_val, rhs_val, &lhs_type, &name).into(),
        Type::Str => compile_binop_str(compile_context, op, lhs_val, rhs_val, &name),
        Type::List(_) => compile_binop_list(compile_context, op, lhs_val, rhs_val, &lhs_type),
        Type::Vec(_, _) => compile_binop_vec(compile_context, op, lhs_val, rhs_val),
        _ => unreachable!(),
    }
}

fn compile_unop<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, op : Operator, expr : ASTRef) -> BasicValueEnum<'llvm_ctx>{
    match (op, expr.get(&compile_context.rustaml_context.ast_pool)){
        (Operator::Minus, ASTNode::Integer { nb }) => return create_int(compile_context, -nb).as_basic_value_enum(),
        _ => {}
    }
    let expr_val = compile_expr(compile_context, expr).unwrap_basic();
    
    match op {
        // TODO : just use a neg instruction instead of a 0-val ?
        // TODO : add support for floating point (and vec ?) minus unary (also change in types.rs : either just have less informations in with the unary minus, or add a constrait that says that a typevar can be a type A or a type B)
        Operator::Minus => {
            match expr_val {
                BasicValueEnum::IntValue(i) => {
                    let const_zero = compile_context.context.i64_type().const_zero();
                    compile_context.builder.build_int_sub(const_zero, i, "unary_minus").unwrap().as_basic_value_enum()
                },
                _ => unreachable!(),
            }
        },
        Operator::Not => {
            match expr_val {
                BasicValueEnum::IntValue(i) => {
                    let const_true = compile_context.context.bool_type().const_int(true as u64, false);
                    compile_context.builder.build_xor(i, const_true, "unary_not").unwrap().as_basic_value_enum()
                }
                _ => unreachable!(),
            }
        },
        _ => unreachable!()
    }
}

fn compile_var_use<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, ast_node : ASTRef, name : StringRef) -> CompilerValue<'llvm_ctx> {

    let var_id = get_var_id(compile_context, ast_node);
    
    //let var_type = compile_context.var_types.get(&name).unwrap_or_else(|| panic!("Unknown variable {:?}", name.get_str(&compile_context.rustaml_context.str_interner)));
    let var_type = get_var_type(compile_context, var_id, name);
    debug_println!(compile_context.rustaml_context.is_debug_print, "var_type use {:?} : {:?}", name.get_str(&compile_context.rustaml_context.str_interner), var_type);
    
    if let Some(ptr) = compile_context.var_vals.get(&name) {
    
        //let var_type = compile_context.typeinfos.vars_ast.get(&name).unwrap().get_type(&compile_context.rustaml_context.ast_pool);
        let load_type = get_llvm_type(compile_context, var_type);
        //let load_basic_type = TryInto::<BasicTypeEnum>::try_into(load_type).unwrap();
        let load_basic_type = any_type_to_basic(compile_context.context, load_type);
        
        compile_context.builder.build_load(load_basic_type, *ptr, name.get_str(&compile_context.rustaml_context.str_interner)).unwrap().into()
    } else if let Some(f) = compile_context.functions.get(&name) {
        (*f).into()
    } else {
        panic!("Compiler: Unknown var {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context));
    }
}

// TODO : move these in an instrisics.rs

fn create_list_init_static_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, type_tag_val : IntValue<'llvm_ctx>, vals : PointerValue<'llvm_ctx>, len_val : IntValue<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    let function = compile_context.get_internal_function("__list_node_init_static");
    //dbg!(function);
    let args = &[type_tag_val.into(), vals.into(), len_val.into()];
    compile_context.builder.build_call(function, args, "list_init_static").unwrap().as_any_value_enum().into_pointer_value()
}

fn create_list_append_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, list : PointerValue<'llvm_ctx>, type_tag_val : IntValue<'llvm_ctx>, val : IntValue<'llvm_ctx> ) -> PointerValue<'llvm_ctx> {
    let function = compile_context.get_internal_function("__list_node_append");
    //dbg!(function);
    let args = &[list.into(), type_tag_val.into(), val.into()];
    compile_context.builder.build_call(function, args, "list_append").unwrap().as_any_value_enum().into_pointer_value()
}

fn create_list_merge<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, list1 : PointerValue<'llvm_ctx>, list2 : PointerValue<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    let function = compile_context.get_internal_function("__list_node_merge");
    //dbg!(function);
    let args = &[list1.into(), list2.into()];
    compile_context.builder.build_call(function, args, "list_append").unwrap().as_any_value_enum().into_pointer_value()
}

fn compile_static_list<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, list : &[ASTRef], list_type : &Type) -> BasicValueEnum<'llvm_ctx> {

    if list.is_empty(){
        return compile_context.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum();
    }

    let list_element_type = match list_type {
        Type::List(e) => e.as_ref(),
        _ => unreachable!(),
    };

    let type_tag_val = get_type_tag_val(compile_context.context, list_element_type);
    // TODO : optimize these iterations ?
    let vals = list.iter().map(|e| compile_expr(compile_context, *e)).collect::<Vec<_>>();
    let std_vals = vals.iter().map(|e| as_val_in_list(compile_context, e.into_basic(), list_element_type)).collect::<Vec<_>>();

    let llvm_element_type = get_llvm_type(compile_context, list_element_type);
    let size = create_int(compile_context, list.len() as i128);
    let static_array = create_entry_block_array_alloca(compile_context, "temp_static_list", compile_context.context.i64_type().into(), size); // use a i64 type to prevent type aliasing UB
    

    if std_vals.iter().all(|e| e.is_const()){
        // optimization if all vals are constants
        let array_type = compile_context.context.i64_type().array_type(list.len().try_into().unwrap());
        let const_array = compile_context.context.i64_type().const_array(&std_vals);
        let global_const_array = compile_context.module.add_global(array_type.as_basic_type_enum(), None, "const_array");
        global_const_array.set_initializer(&const_array);
        global_const_array.set_constant(true);
        global_const_array.set_linkage(Linkage::Internal);
        let size_t_type = compile_context.context.ptr_sized_int_type(&compile_context.target_data, None);
        let val_size = 8; // a val is 8 bytes
        let size_size_t = size_t_type.const_int((list.len() * val_size) as u64, false);
        let unknown_align = 1; // unknown alignement so put 1
        compile_context.builder.build_memcpy(static_array, unknown_align, global_const_array.as_pointer_value(), unknown_align, size_size_t).unwrap();
    } else {
        let basic_element_type = any_type_to_basic(compile_context.context, llvm_element_type);
        for (idx, v) in std_vals.iter().enumerate() {
            let ordered_indexes = &[create_int(compile_context, idx as i128)];
            // very likely to sefgfaults, that's why it is unsafe
            let gep_ptr = unsafe {
                compile_context.builder.build_in_bounds_gep(basic_element_type, static_array, ordered_indexes, "gep_build_static_array").unwrap()
            };
            let basic_val = v.as_basic_value_enum();
            compile_context.builder.build_store(gep_ptr, basic_val).unwrap();
        }
    }

    create_list_init_static_call(compile_context, type_tag_val, static_array, size).as_basic_value_enum()

}

fn compile_static_vec<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, vec : &[ASTRef], vec_type : &Type) -> BasicValueEnum<'llvm_ctx> {

    let vals = vec.iter().map(|e| compile_expr(compile_context, *e)).collect::<Vec<_>>();

    // TODO : constant vec (don't really need it because the insert_element pattern with undef is already optimized by LLVM even with -O0, but it could help for the JIT to have LLVM do less work with instruction)

    let vec_type = get_llvm_type(compile_context, vec_type).into_vector_type();

    let mut vec = vec_type.get_undef();
    for (val_idx, val) in vals.iter().enumerate() {
        vec = compile_context.builder.build_insert_element(vec, val.unwrap_basic(), compile_context.context.i64_type().const_int(val_idx as u64, false), "insert_static_vec").unwrap();
    }

    vec.as_basic_value_enum()
}



fn compile_str<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, str : StringRef) -> PointerValue<'llvm_ctx> {
    let str = str.get_str(&compile_context.rustaml_context.str_interner).to_owned();
    create_string(compile_context, &str)
}

fn compile_anon_func<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, ast_node : ASTRef, args : &[StringRef], body : ASTRef) -> FunctionValue<'llvm_ctx> {
    
    let current_loc = compile_context.debug_info.get_current_debug_location(&compile_context.builder);

    let current_bb = compile_context.builder.get_insert_block().unwrap();

    let anon_func_name = format!("anon_func{}", compile_context.closure_idx.get());
    compile_context.closure_idx.update(|x| x+1);

    let (arg_types, ret_type, variadic)= match ast_node.get_type(&compile_context.rustaml_context.ast_pool) {
        Type::Function(args, ret, variadic) => (args.clone(), ret.as_ref().clone(), variadic),
        _ => unreachable!(),
    };

    let ret_type_llvm = get_llvm_type(compile_context, &ret_type);

    let arg_types_llvm = 
        arg_types.iter()
        .map(|e| get_llvm_type(compile_context, e))
        .collect::<Vec<_>>();

    
    let arg_types_metadata = arg_types_llvm.iter().map(|a| any_type_to_metadata(compile_context.context, *a)).collect::<Vec<_>>();

    let function_type = get_fn_type(compile_context.context, ret_type_llvm, &arg_types_metadata, *variadic);

    let function = add_function(compile_context, &anon_func_name, function_type, Some(inkwell::module::Linkage::Internal));

    let function_range = ast_node.get_range(&compile_context.rustaml_context.ast_pool);
    let di_subprogram = compile_context.debug_info.add_function(&anon_func_name, &arg_types, &ret_type, compile_context.rustaml_context.content.as_ref().unwrap(), function_range.clone(), compile_context.is_optimized);

    if let Some(di_subprogram) = di_subprogram {
        function.set_subprogram(di_subprogram);
        // TODO : set debuginfo location on builder
    }

    compile_context.debug_info.create_lexical_block();

    let entry = compile_context.context.append_basic_block(function, "entry");
    compile_context.builder.position_at_end(entry);

    if let Some(loc) = compile_context.debug_info.create_debug_location(compile_context.context, compile_context.rustaml_context.content.as_ref().unwrap(), function_range.clone()) {
        compile_context.builder.set_current_debug_location(loc);
    }

    let range = ast_node.get_range(&compile_context.rustaml_context.ast_pool);

    // TODO : need a way to reset vars (for example swap the current vars with an empty one, then put it back)
    for ((((arg_idx, arg_name), arg_val), arg_type_llvm), arg_type) in args.iter().enumerate().zip(function.get_param_iter()).zip(&arg_types_llvm).zip(arg_types) {
        // TODO : default parameters like in function def
        create_var(compile_context, *arg_name, arg_val, *arg_type_llvm);
        compile_context.debug_info.declare_parameter(arg_name.get_str(&compile_context.rustaml_context.str_interner), arg_idx.try_into().unwrap(), &arg_type, compile_context.rustaml_context.content.as_ref().unwrap(), range.clone());
        //compile_context.debug_info.declare_var(arg_name.get_str(&compile_context.rustaml_context.str_interner), &arg_type, var_ptr, compile_context.builder.get_insert_block().unwrap(), compile_context.rustaml_context.content.as_ref().unwrap(), range.clone());
    }
    let ret = compile_expr(compile_context, body).into_basic();

    let return_val: Option<&dyn BasicValue<'_>> = match ret_type {
        Type::Unit => None,
        _ => Some(&ret),
    };

    compile_context.builder.build_return(return_val).unwrap(); 

    compile_context.debug_info.end_lexical_block();
    compile_context.debug_info.end_function();

    compile_context.debug_info.enter_top_level();

    compile_context.builder.position_at_end(current_bb);

    if let Some(loc) = current_loc {
        compile_context.debug_info.set_debug_location(loc);
        compile_context.builder.set_current_debug_location(loc);
    }

    function
}

fn compile_cast<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, to_type : &Type, expr : ASTRef) -> BasicValueEnum<'llvm_ctx> {
    let start_type = expr.get_type(&compile_context.rustaml_context.ast_pool).clone();
    let start_val = compile_expr(compile_context, expr).unwrap_basic();

    let i32_type = compile_context.context.i32_type();
    let i64_type = compile_context.context.i64_type();
    match (&start_type, to_type){
        (t1, t2) if t1 == t2 => start_val, // TODO : add a warning in this case
        // TODO
        (Type::CType(CType::I32), Type::Integer) => compile_context.builder.build_int_s_extend(start_val.into_int_value(), i64_type, "c_i32_to_int").unwrap().into(),
        (Type::Integer, Type::CType(CType::I32)) => compile_context.builder.build_int_truncate(start_val.into_int_value(), i32_type, "int_to_c_i32").unwrap().into(),
        _ => panic!("Wrong cast"),
    }
}

fn compile_variant<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, name : StringRef, _arg : Option<ASTRef>) -> BasicValueEnum<'llvm_ctx> {
    let variant_nb = get_variant_tag(compile_context.rustaml_context, name);
    create_int(compile_context, variant_nb as i128).into()
}

pub(crate) fn compile_expr<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, ast_node : ASTRef) -> CompilerValue<'llvm_ctx> {
    let range = ast_node.get_range(&compile_context.rustaml_context.ast_pool);
    match ast_node.get(&compile_context.rustaml_context.ast_pool).clone(){
        ASTNode::Integer { nb } => create_int(compile_context, nb).as_basic_value_enum().into(), // TODO : sign extend or not ?
        ASTNode::Float { nb } => compile_context.context.f64_type().const_float(nb).as_basic_value_enum().into(),
        ASTNode::Boolean { b } => compile_context.context.bool_type().const_int(b as u64, false).as_basic_value_enum().into(),
        ASTNode::Char { c } => compile_context.context.i32_type().const_int(c as u64, false).as_basic_value_enum().into(),
        ASTNode::String { str } => compile_str(compile_context, str).as_basic_value_enum().into(),
        ASTNode::VarDecl { name, val, body, var_type: _ } => compile_var_decl(compile_context, ast_node, name, val, body, false),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => compile_if(compile_context, cond_expr, then_body, else_body).into(),
        ASTNode::FunctionCall { callee, args } => compile_function_call(compile_context, callee, &args, range).into(),
        ASTNode::BinaryOp { op, lhs, rhs } => compile_binop(compile_context, op, lhs, rhs, range).into(),
        ASTNode::UnaryOp { op, expr } => compile_unop(compile_context, op, expr).into(),
        ASTNode::VarUse { name } => compile_var_use(compile_context, ast_node, name),
        ASTNode::List { list } => { 
            let t = ast_node.get_type(&compile_context.rustaml_context.ast_pool).clone();
            compile_static_list(compile_context, &list, &t).into()
        },
        ASTNode::Vec { vec } => {
            let t = ast_node.get_type(&compile_context.rustaml_context.ast_pool).clone();
            compile_static_vec(compile_context, &vec, &t).into()
        }
        ASTNode::MatchExpr { matched_expr, patterns } => compile_match(compile_context, ast_node, matched_expr, &patterns).into(),
        ASTNode::AnonFunc { args, body, type_annotation: _ } => compile_anon_func(compile_context, ast_node, &args, body).into(),
        ASTNode::Cast { to_type, expr } => compile_cast(compile_context, &to_type, expr).into(),
        ASTNode::Variant { name, arg } => compile_variant(compile_context, name, arg).into(),
        ASTNode::Unit => get_void_val(compile_context.context).into(),
        t => panic!("unknown AST : {:?}", DebugWrapContext::new(&t, compile_context.rustaml_context)), 
    }
}

pub(crate) fn get_var_id(compile_context: &'_ CompileContext<'_, '_>, ast_node : ASTRef) -> VarId {
    *compile_context.typeinfos.ast_var_ids.get(&ast_node).unwrap()
}

// TODO : is the second part needed ?
// replace panics with unreachables and remove the name arg
fn get_var_type<'context>(compile_context: &'context CompileContext<'context, '_>, var_id : VarId, name : StringRef) -> &'context Type {
    match compile_context.typeinfos.vars_env.get(&var_id) {
        Some(t) => match t {
            Type::Any => panic!("Compiler: var {:?} is any", DebugWrapContext::new(&name, compile_context.rustaml_context)),
            t => t,
        },
        
        None => {
            panic!("Compiler: unknown var : {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context));
        }
    }
}

fn default_attributes_type<'llvm_ctx>(llvm_context : &'llvm_ctx Context, t : &Type, attribute_loc : AttributeLoc, function : FunctionValue<'llvm_ctx>){
    match t {
        Type::Integer | Type::Bool | Type::Float => function.add_attribute(attribute_loc, new_attribute(llvm_context, "noundef")), // make ints, bools, noundef to help optimizations
        Type::Str => {
            function.add_attribute(attribute_loc, new_attribute(llvm_context, "noundef"));
            function.add_attribute(attribute_loc, new_attribute(llvm_context, "nonnull"));
        }
        Type::List(_) => {
            function.add_attribute(attribute_loc, new_attribute(llvm_context, "noundef"));
        }
        _ => {}
    }
}

/*    #[cfg(feature = "jit")]
    if is_jit_entrypoint {
        let arg_array = function.get_first_param().unwrap().into_pointer_value();
        for (((arg_idx, arg_name), arg_type_llvm), arg_type) in args.iter().enumerate().zip(param_types_llvm).zip(param_types) {

            let arg_unwraped_val = jit_unwrap_val(compile_context, arg_array, arg_idx, arg_type);
            create_var(compile_context, *arg_name, arg_unwraped_val, arg_type_llvm);
            compile_context.debug_info.declare_parameter(arg_name.get_str(&compile_context.rustaml_context.str_interner), arg_idx.try_into().unwrap(), arg_type, compile_context.rustaml_context.content.as_ref().unwrap(), function_range.clone());
            //compile_context.debug_info.declare_var(arg_name.get_str(&compile_context.rustaml_context.str_interner), arg_type, var_ptr, compile_context.builder.get_insert_block().unwrap(), compile_context.rustaml_context.content.as_ref().unwrap(), function_range.clone());
        }
        return;
    } */

fn declare_function_args<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, function : FunctionValue<'llvm_ctx>, args : &[StringRef], param_types_llvm : Vec<AnyTypeEnum<'llvm_ctx>>, param_types : &[Type], function_range : Range<usize>){
    
    
    for ((((arg_idx, arg_name), arg_val), arg_type_llvm), arg_type) in args.iter().enumerate().zip(function.get_param_iter()).zip(param_types_llvm).zip(param_types) {
        default_attributes_type(compile_context.context, arg_type, AttributeLoc::Param(arg_idx.try_into().unwrap()), function);
        create_var(compile_context, *arg_name, arg_val, arg_type_llvm);
        compile_context.debug_info.declare_parameter(arg_name.get_str(&compile_context.rustaml_context.str_interner), arg_idx.try_into().unwrap(), arg_type, compile_context.rustaml_context.content.as_ref().unwrap(), function_range.clone());
        //compile_context.debug_info.declare_var(arg_name.get_str(&compile_context.rustaml_context.str_interner), arg_type, var_ptr, compile_context.builder.get_insert_block().unwrap(), compile_context.rustaml_context.content.as_ref().unwrap(), function_range.clone());
    }
}

/*#[cfg(feature = "jit")]
    if is_jit_entrypoint {
        function_type = get_jit_entry_function_type(compile_context.context);
    } */

fn compile_function_def<'llvm_ctx>(compile_context: &mut CompileContext<'_, 'llvm_ctx>, name : StringRef, args : &[StringRef], body : ASTRef, ast_node : ASTRef, arg_types : &[Type], return_type : &Type) -> FunctionValue<'llvm_ctx> {
    //println!("typeinfos function_env : {:?}", DebugWrapContext::new(&compile_context.typeinfos.functions_env, compile_context.rustaml_context));
    let previous_loc = compile_context.debug_info.get_current_debug_location(&compile_context.builder);

    let return_type_llvm = get_llvm_type(compile_context, return_type);
    let param_types = arg_types;
    debug_println!(compile_context.rustaml_context.is_debug_print, "function {:?} param types : {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context), param_types);
    let param_types_llvm = param_types.iter().map(|t| get_llvm_type(compile_context, t)).collect::<Vec<_>>();
    let param_types_metadata = param_types_llvm.iter().map(|t| any_type_to_metadata(compile_context.context, *t)).collect::<Vec<_>>();
    let function_type = get_fn_type(compile_context.context, return_type_llvm, &param_types_metadata, false);
    
    let function = add_function(compile_context, name.get_str(&compile_context.rustaml_context.str_interner), function_type, Some(Linkage::Internal));

    let function_range = ast_node.get_range(&compile_context.rustaml_context.ast_pool);
    let di_subprogram = compile_context.debug_info.add_function(name.get_str(&compile_context.rustaml_context.str_interner), param_types, return_type, compile_context.rustaml_context.content.as_ref().unwrap(), function_range.clone(), compile_context.is_optimized);
    if let Some(di_subprogram) = di_subprogram {
        function.set_subprogram(di_subprogram);
        // TODO : set debuginfo location on builder
    }

    compile_context.debug_info.create_lexical_block();
            
    compile_context.functions.insert(name, function);
            
    let entry = compile_context.context.append_basic_block(function, "entry");
    compile_context.builder.position_at_end(entry);

    if let Some(loc) = compile_context.debug_info.create_debug_location(compile_context.context, compile_context.rustaml_context.content.as_ref().unwrap(), function_range.clone()) {
        compile_context.builder.set_current_debug_location(loc);
    }

    debug_println!(compile_context.rustaml_context.is_debug_print,"function {:?} param types llvm : {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context), param_types_llvm);

            //let mut old_arg_name_type = Vec::new(); // to save the types that have the same of the args in the global vars 

    declare_function_args(compile_context, function, args, param_types_llvm, param_types, function_range);

    default_attributes_type(compile_context.context, return_type, AttributeLoc::Return, function);

    let ret = compile_expr(compile_context, body).into_basic();

    //dbg!(ret);

    let return_val: Option<&dyn BasicValue<'_>> = match return_type {
        Type::Unit => None,
        _ => Some(&ret),
    };

    compile_context.builder.build_return(return_val).unwrap();

    for arg in args {
        compile_context.var_vals.remove(arg);
    }

    compile_context.debug_info.end_lexical_block();
    compile_context.debug_info.end_function();

            // TODO : is it really needed
    compile_context.debug_info.enter_top_level();
    if let Some(loc) = previous_loc {
        compile_context.debug_info.set_debug_location(loc);
        compile_context.builder.set_current_debug_location(loc);
    }

    function
}

pub(crate) fn compile_function(compile_context: &mut CompileContext, ast_node : ASTRef, name : StringRef, args: Box<[StringRef]>, body: ASTRef){
    // TODO : replace this with accessing the type of the ASTNode ?
    let function_id = get_var_id(compile_context, ast_node);

    let (return_type, arg_types) = match compile_context.typeinfos.vars_env.get(&function_id).unwrap() {
        Type::Function(args, ret, _) => (ret.as_ref().clone(), args.clone()),
        t => panic!("BUG : the function definition has not a function type, it is {:?} instead", t), // TODO : replace this with an unreachable
    };
    if !should_monomorphize_function(&arg_types, &return_type){
        compile_function_def(compile_context, name, &args, body, ast_node, &arg_types, &return_type);
    } else {
        compile_context.generic_func_def_ast_node.insert(name, ast_node);
    }
}

fn compile_top_level_node(compile_context: &mut CompileContext, ast_node : ASTRef) {
    compile_context.debug_info.enter_top_level();
    // TODO : add this in enter_top_level
    /*if let Some(loc) = compile_context.debug_info.create_debug_location(compile_context.context, compile_context.rustaml_context.content.as_ref().unwrap(),0..0){
        compile_context.builder.set_current_debug_location(loc);
    }*/

    let ast_range = ast_node.get_range(&compile_context.rustaml_context.ast_pool);

    match ast_node.get(&compile_context.rustaml_context.ast_pool).clone() {
        ASTNode::FunctionDefinition { name, args, body, type_annotation: _ } => {
            compile_function(compile_context, ast_node, name, args, body);
        },

        ASTNode::VarDecl { name, val, body, var_type: _ } => {

            let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
            compile_context.builder.position_at_end(last_main_bb);
            if let Some(loc) = compile_context.debug_info.create_debug_location(compile_context.context, compile_context.rustaml_context.content.as_ref().unwrap(), ast_range) {
                compile_context.builder.set_current_debug_location(loc);
            }

            compile_var_decl(compile_context, ast_node, name, val, body, true);
        },
        ASTNode::ExternFunc { name, type_annotation, lang, so_str } => {
            let function_ty = get_llvm_type(compile_context, &type_annotation).into_function_type();
            let name_mangled = mangle_name_external(name.get_str(&compile_context.rustaml_context.str_interner), &type_annotation, lang);
            
            let function = add_function(compile_context, name_mangled.as_ref(), function_ty, Some(Linkage::External)); // TODO : what linkage ?
            compile_context.functions.insert(name, function);
            if let Some(so_str) = so_str {
                compile_context.shared_libs.push(so_str.get_str(&compile_context.rustaml_context.str_interner).to_owned());
            }
        }
        ASTNode::TypeAlias { name: _, type_alias: _ } => {}
        ASTNode::TopLevel { nodes } => {
            // placeholder for imports (TODO ?)
            for n in nodes {
                compile_top_level_node(compile_context, n);
            }
        }
        t => panic!("top level node = {:?}", DebugWrapContext::new(&t, compile_context.rustaml_context)),
        // _ => unreachable!()
    }
}

fn run_passes_on(module: &Module, target_machine : &TargetMachine, opt_level : OptimizationLevel, sanitizer : bool) {
    // TODO : test with "function(mem2reg)," at the start (like https://github.com/inko-lang/inko/blob/main/compiler/src/llvm/passes.rs#L553)
    // to remove allocas even with no optimizations enabled ?
    let passes_str = format!("default<O{}>", opt_level as u8);
    
    module.run_passes(&passes_str, target_machine, PassBuilderOptions::create()).unwrap();
}

// TODO : instead install file in filesystem ?
const STD_C_CONTENT: &str = include_str!("../../std.c");

// TODO : add a way to select the path of bdwgc for building it
// TODO : add static musl (for completely static linking)
// TODO : add a way to select the path of musl for building it

fn link_exe(rustaml_context: &mut RustamlContext, filename_out : &Path, bitcode_file : &Path, shared_libs : &[String], opt_level : OptimizationLevel, optional_args : &OptionalArgs){
    // use cc ?
    // TODO : use lld (https://github.com/mun-lang/lld-rs) for linking instead ?
    // TODO : use libclang ? (clang-rs ? https://github.com/llvm/llvm-project/blob/main/clang/tools/driver/cc1_main.cpp#L85 ?)

    #[cfg(not(feature = "build-bdwgc"))]
    if optional_args.build_bdwgc {
        panic!("Can't link a custom built bdwgc without enabling the build-bdwgc feature");
    }

    // TODO : only make the musl work with linux
    #[cfg(not(feature = "musl"))]
    if optional_args.musl {
        panic!("Can't link a custom built static musl without enabling the musl feature");
    }


    let temp_dir = std::env::temp_dir();
    let current_exe_path = std::env::current_exe().unwrap();
    let current_exe_folder = current_exe_path.parent().unwrap().to_path_buf();


    #[cfg(feature = "musl")]
    let sysroot_musl = if optional_args.musl {
        rustaml_context.start_section("build-musl");
        // TODO : put this in another function
        let musl_path = current_exe_folder.join("musl");

        let sysroot_musl = musl_path.join("musl_sysroot");
        if !sysroot_musl.exists(){
            std::fs::create_dir(&sysroot_musl).unwrap();
        }

        let mut configure_cmd = Command::new("./configure");
        configure_cmd.arg("--disable-shared").arg(&format!("--prefix={}", sysroot_musl.to_str().unwrap()));
        // TODO : make the CFLAGS work in one function/place (to deduplicate code with the cflags handling after that)
        let mut cflags = format!("-emit-llvm -O{} -fno-stack-protector", opt_level as u32);
        
       
        if !matches!(opt_level, OptimizationLevel::None){
            cflags += " -DNDEBUG";  // TODO : should I do this ?
            cflags += " -flto";
        }

        if optional_args.enable_debuginfos {
            cflags += " -g";
        } 

        if optional_args.march_native {
            cflags += " -march=native";
        }

        if optional_args.freestanding {
            panic!("freestanding not supported with musl"); // TODO ?
        }

        configure_cmd.envs([
            ("CC", "clang"),
            ("CFLAGS", &cflags),
            ("AR", "llvm-ar"),
            ("RANLIB", "llvm-ranlib"), 
        ]);
        configure_cmd.current_dir(&musl_path);

        // TODO : disable stdio by default
        configure_cmd.spawn().unwrap().wait().unwrap();     

        let available_threads = std::thread::available_parallelism().unwrap().get();
        let threads_flag = format!("-j{}", available_threads);

        let mut make_cmd = Command::new("make");

        make_cmd.arg(&threads_flag);
        make_cmd.current_dir(&musl_path);

        make_cmd.spawn().unwrap().wait().unwrap();   

        let mut make_install_cmd = Command::new("make");
        make_install_cmd.arg("install").arg(&threads_flag);
        make_install_cmd.current_dir(&musl_path);
        make_install_cmd.spawn().unwrap().wait().unwrap();

        rustaml_context.end_section("build-musl");
        Some(sysroot_musl)
    } else {
        None
    };

    #[cfg(feature = "build-bdwgc")]
    let (out_bdwgc_path, bdwgc_src_path) = if optional_args.build_bdwgc {
        rustaml_context.start_section("build-bdwgc");
        
        let bdwgc_path = current_exe_folder.join("bdwgc");
        let bdwgc_src_path = pathbuf![&bdwgc_path, "extra", "gc.c"];

        let out_bdwgc_path = pathbuf![&temp_dir, "bdwgc.bc"];
        let mut bdwgc_link_cmd = Command::new("clang");
        bdwgc_link_cmd.args(["-emit-llvm", "-c"]);
        let include_flag = format!("-I{}", bdwgc_path.join("include").to_str().unwrap());
        bdwgc_link_cmd.arg(include_flag);
        bdwgc_link_cmd.arg("-o").arg(out_bdwgc_path.as_os_str()).arg(bdwgc_src_path.as_os_str());
        bdwgc_link_cmd.arg(format!("-O{}", opt_level as u32));
        bdwgc_link_cmd.arg("-DNO_GETCONTEXT");

        // TODO : should I do this ?
        if !matches!(opt_level, OptimizationLevel::None){
            bdwgc_link_cmd.arg("-DNDEBUG");
        }

        if optional_args.enable_debuginfos {
            bdwgc_link_cmd.arg("-g");
        }

        if optional_args.march_native {
            bdwgc_link_cmd.arg("-march=native");
        }

        if optional_args.freestanding {
            panic!("freestanding not supported with build bdwgc"); // TODO ?
        }

        #[cfg(feature = "musl")]
        if optional_args.musl {
            bdwgc_link_cmd.arg("-fno-stack-protector").arg(&format!("--sysroot={}", sysroot_musl.as_ref().unwrap().to_str().unwrap()));
        }        
        
        bdwgc_link_cmd.spawn().unwrap().wait().unwrap();
        rustaml_context.end_section("build-bdwgc");
        (Some(out_bdwgc_path), Some(bdwgc_path))
    } else {
        (None, None)
    };
    
    
    let out_std_path = pathbuf![&temp_dir, "std.bc"];
    let out_std_path_str = out_std_path.as_os_str();

    rustaml_context.start_section("std");

    let mut clang_std = Command::new("clang");
    clang_std.arg("-x").arg("c").arg("-emit-llvm").arg(format!("-O{}", opt_level as u32)).arg("-c");

    if !matches!(opt_level, OptimizationLevel::None){
        clang_std.arg("-DNDEBUG");
    }

    if !optional_args.disable_gc {
        clang_std.arg("-D_GC_");
    }

    if optional_args.enable_debuginfos {
        clang_std.arg("-g");
    }

    if optional_args.march_native {
        clang_std.arg("-march=native");
    }


    // TODO : work on freestanding (add set target to make the os unknown in target triplet, pass linker script or just pass linker arg ?)
    if optional_args.freestanding {
        clang_std.arg("-ffreestanding").arg("-nostdlib");
    }

    #[cfg(feature = "musl")]
    if optional_args.musl {
        clang_std.arg("-fno-stack-protector");
        clang_std.arg(&format!("--sysroot={}", sysroot_musl.as_ref().unwrap().to_str().unwrap()));
        clang_std.arg("-v");
    }

    #[cfg(feature = "build-bdwgc")]
    if optional_args.build_bdwgc {
        let include_arg = bdwgc_src_path.unwrap().join("include");
        clang_std.arg(&format!("-I{}", include_arg.to_str().unwrap()));
    }
    
    let mut clang_std = clang_std.arg("-").arg("-o").arg(out_std_path_str).stdin(Stdio::piped()).spawn().expect("compiling std failed");
    clang_std.stdin.as_mut().unwrap().write_all(STD_C_CONTENT.as_bytes()).unwrap();
    clang_std.wait().unwrap();

    rustaml_context.end_section("std");

    rustaml_context.start_section("linker");

    let mut link_cmd = Command::new("clang");

    if !matches!(opt_level, OptimizationLevel::None) {
        link_cmd.arg("-flto");
    }

    if optional_args.march_native {
        link_cmd.arg("-march=native");
    }

    if !optional_args.disable_gc && !optional_args.build_bdwgc {
        link_cmd.arg("-lgc");
    }

    if optional_args.freestanding {
        link_cmd.arg("-ffreestanding").arg("-nostdlib");
    }

    #[cfg(feature = "musl")]
    if optional_args.musl {
        link_cmd.arg(&format!("--sysroot={}", sysroot_musl.unwrap().to_str().unwrap()));
        link_cmd.arg("-static");
    }

    for search_path in &optional_args.lib_search_paths {
        link_cmd.arg("-L".to_owned() + search_path);
    }

    for lib in shared_libs {
        if lib.starts_with("..") || lib.starts_with(MAIN_SEPARATOR){
            // full path
            link_cmd.arg(lib);
        } else {
            let lib = if lib.starts_with("./") || lib.starts_with(".\\"){
                lib[2..].to_owned()
            } else if lib.starts_with("lib") && lib.ends_with(".so"){
                let suffix_stripped = lib.strip_suffix(".so").unwrap();
                let lib = "-l".to_owned() + &suffix_stripped[3..];
                lib
            } else {
                lib.to_owned()
            };
            // local or global
            link_cmd.arg(lib);
        }
    }

    link_cmd.arg("-lm").arg("-o").arg(filename_out).arg(out_std_path_str).arg(bitcode_file);
    if optional_args.enable_sanitizer {
        // TODO : need undefined sanitizer ? or just reimplement the checks (some are already implemented like overflow, but need a flag to deactivate them, and need one to replace the errors with a trap instruction)
        link_cmd.arg("-fsanitize=address");
        //link_cmd.arg("-Wl,--no-gc-sections");
    }

    #[cfg(feature = "build-bdwgc")]
    if optional_args.build_bdwgc {
        link_cmd.arg(&out_bdwgc_path.as_ref().unwrap());
    }

    if !link_cmd.spawn().expect("linker failed").wait().unwrap().success() {
        panic!("linker failed");
    }

    rustaml_context.end_section("linker");

    std::fs::remove_file(&out_std_path).expect("Couldn't delete std bitcode file");

    #[cfg(feature = "build-bdwgc")]
    if optional_args.build_bdwgc {
        std::fs::remove_file(&out_bdwgc_path.unwrap()).expect("Couldn't delete bdwgc bitcode file");
    }
}

const DEBUGINFO_VERSION: u64 = 3;

pub(crate) struct OptionalArgs {
    optimization_level : u8,
    keep_temp : bool,
    disable_gc : bool, 
    enable_sanitizer : bool, 
    enable_debuginfos : bool, 
    freestanding : bool,
    march_native : bool,
    build_bdwgc : bool,
    musl : bool,
    lib_search_paths : Vec<String>, // TODO : use PathBufs instead ?
}

impl OptionalArgs {
    // TODO : make this a builder pattern ?
    #[allow(unused)]
    pub(crate) fn new(optimization_level : Option<u8>, keep_temp : bool, disable_gc : bool, enable_sanitizer : bool, enable_debuginfos : bool, freestanding : bool, march_native : bool, build_bdwgc : bool, musl : bool, lib_search_paths : Vec<String>) -> OptionalArgs {
        OptionalArgs { 
            optimization_level: optimization_level.unwrap_or(0), 
            keep_temp, 
            disable_gc, 
            enable_sanitizer, 
            enable_debuginfos,
            freestanding,
            march_native,
            build_bdwgc,
            musl,
            lib_search_paths,
        }
    }
}

// TODO : add also a std.rml for std functions that are written in rustaml which will also be included in the executable 
#[allow(unused)]
pub(crate) fn compile(frontend_output : FrontendOutput, rustaml_context: &mut RustamlContext, filename : &Path, filename_out : Option<&Path>, optional_args : OptionalArgs) {
    let optimization_level = match optional_args.optimization_level {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        3 => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::Default,
    };
    
    let temp_path = if optional_args.keep_temp {
        Path::new(".").to_owned() 
    } else { 
        std::env::temp_dir()
    };

    let filename_without_ext = filename.file_stem().unwrap().to_str().expect("not UTF-8 filename").to_owned();

    let filename_with_hash = if optional_args.keep_temp {
        filename_without_ext.clone()
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
    
    let shared_libs = if !optional_args.keep_temp && let Some(bitcode_cached_path) = get_cached_llvm_ir(&frontend_output.content, optimization_level) {
        // TODO : do ln instead of copying then suppressing it ?
        fs::copy(bitcode_cached_path.bitcode_path, &temp_path_bitcode).unwrap();
        bitcode_cached_path.metadata.shared_libs
    } else {
        rustaml_context.start_section("llvm-codegen");
        let context = Context::create();
        let builder = context.create_builder();
        let filename_path = filename.parent().map(|e| e.as_os_str().to_str().unwrap()).unwrap();
        let filename_end = filename.file_name().unwrap().to_str().unwrap();

        let filename_str = filename.as_os_str().to_str().expect("not UTF-8 filename");
        let module = context.create_module(filename_str);

        Target::initialize_all(&InitializationConfig::default());

        let target_triple = TargetMachine::get_default_triple();

        let target = Target::from_triple(&target_triple).unwrap();

        let (cpu, features) = if optional_args.march_native {
            let cpu_name = TargetMachine::get_host_cpu_name().to_str().unwrap().to_owned();
            let cpu_features = TargetMachine::get_host_cpu_features().to_str().unwrap().to_owned();
            (cpu_name, cpu_features)
        } else {
            ("generic".to_owned(), "".to_owned())
        };

        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                optimization_level,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();
        let target_data = target_machine.get_target_data();
        
        let data_layout = target_data.get_data_layout();

        module.set_triple(&target_triple);
        module.set_data_layout(&data_layout);

        let is_optimized = optional_args.optimization_level > 0;
        
        let debug_info = DebugInfo { 
            inner: if optional_args.enable_debuginfos {
                let ptr_size = target_data.get_pointer_byte_size(None);
                let ptr_alignement = target_data.get_abi_alignment(&context.ptr_type(AddressSpace::default()));
                let debug_info_version = context.i32_type().const_int(DEBUGINFO_VERSION, false);
                let list_type = get_list_type(&context);
                let list_size = target_data.get_bit_size(&list_type)/8;
                let list_alignement = target_data.get_abi_alignment(&list_type);

                let target_infos = TargetInfos::new(ptr_size, ptr_alignement, list_size, list_alignement);
                
                module.add_basic_value_flag("Debug Info Version", FlagBehavior::Warning, debug_info_version);
                
                let (debug_builder, debug_compile_unit) = module.create_debug_info_builder(true, DWARFSourceLanguage::C, filename_end, filename_path, "rustaml compiler", is_optimized, "", 0, "", DWARFEmissionKind::Full, 0, false, false, "", "");
                Some(DebugInfosInner::new(target_infos, is_optimized, debug_builder, debug_compile_unit))
            } else {
                None
            }
        };
        

        let mut compile_context = CompileContext::new(rustaml_context, &context, module, builder, debug_info, is_optimized, frontend_output.type_infos, target_data, None);

        let entry_main_bb = compile_context.main_function.get_first_basic_block().unwrap();
        compile_context.builder.position_at_end(entry_main_bb);
        let init_func = compile_context.get_internal_function("__init");
        compile_context.builder.build_call(init_func, &[], "init_call").unwrap();
        
        let top_level_nodes = match frontend_output.ast.get(&compile_context.rustaml_context.ast_pool) {
            ASTNode::TopLevel { nodes } => nodes.clone(),
            _ => unreachable!(),
        };

        for n in top_level_nodes {
            compile_top_level_node(&mut compile_context, n);
        }

        let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
        compile_context.builder.position_at_end(last_main_bb);

        let should_return = match last_main_bb.get_last_instruction(){
            Some(instr) => {
                !instr.is_terminator()
            }
            None => true,
        };

        if should_return {
            compile_context.builder.build_return(Some(&compile_context.context.i32_type().const_int(0, false))).unwrap();
        }

        compile_context.debug_info.finalize(&mut compile_context.main_function);
        


        #[cfg(feature = "debug-llvm")]
        compile_context.module.verify().or_else(|e| -> Result<_, LLVMString> { 
            compile_context.module.print_to_file(filename_without_ext + "_error.ll").unwrap();
            panic!("LLVM ERROR {}", e.to_string()) 
        }).unwrap();


        
        compile_context.rustaml_context.end_section("llvm-codegen");

        compile_context.rustaml_context.start_section("llvm-opt");
        run_passes_on(&compile_context.module, &target_machine, optimization_level, optional_args.enable_sanitizer);
        compile_context.rustaml_context.end_section("llvm-opt");

        if optional_args.keep_temp {
            let temp_path_ir = pathbuf![&temp_path, &format!("{}.ll", &filename_with_hash)];
            compile_context.module.print_to_file(&temp_path_ir).expect("Couldn't write llvm ir file");
        }
        
        compile_context.module.write_bitcode_to_path(&temp_path_bitcode);

        compile_context.rustaml_context.start_section("write-cached-bitcode");
        write_cached_llvm_ir(&temp_path_bitcode, optimization_level, &frontend_output.content, &compile_context.shared_libs);
        compile_context.rustaml_context.end_section("write-cached-bitcode");
        compile_context.shared_libs
    };

    if let Some(f_out) = filename_out {
        link_exe(rustaml_context, f_out,  &temp_path_bitcode, &shared_libs, optimization_level, &optional_args);
        if !optional_args.keep_temp {
            std::fs::remove_file(temp_path_bitcode).expect("Couldn't delete bitcode file");
        }
    }

}