use core::panic;
use std::{fs, hash::{Hash, Hasher}, io::Write, ops::Range, path::{MAIN_SEPARATOR, Path, PathBuf}, process::{Command, Stdio}, time::{SystemTime, UNIX_EPOCH}};
use debug_with_context::DebugWrapContext;
use crate::{ast::{ASTNode, ASTRef, CType, Type}, compiler::{compile_match::compile_match, compiler_utils::{_codegen_runtime_error, any_type_to_basic, any_type_to_metadata, any_val_to_basic, any_val_to_metadata, codegen_lang_runtime_error, create_br_conditional, create_br_unconditional, create_entry_block_array_alloca, create_int, create_string, create_var, encountered_any_type, get_current_function, get_fn_type, get_list_type, get_llvm_type, get_type_tag_val, get_void_val, move_bb_after_current, promote_val_var_arg}, debuginfo::{DebugInfo, DebugInfosInner, TargetInfos, get_debug_loc}, internal_monomorphized::{compile_monomorphized_filter, compile_monomorphized_map, init_monomorphized_internal_fun}}, debug_println, lexer::Operator, mangle::mangle_name_external, rustaml::{FrontendOutput, RustamlContext}, string_intern::StringRef, types::{TypeInfos, VarId}};
use inkwell::{AddressSpace, Either, FloatPredicate, IntPredicate, OptimizationLevel, attributes::{Attribute, AttributeLoc}, basic_block::BasicBlock, builder::Builder, context::Context, debug_info::{DWARFEmissionKind, DWARFSourceLanguage}, intrinsics::Intrinsic, module::{FlagBehavior, Linkage, Module}, passes::PassBuilderOptions, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine}, types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum}, values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, GlobalValue, IntValue, PointerValue}};
use pathbuf::pathbuf;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use cfg_if::cfg_if;

// TODO : do only one alloc when init a static list then put all the nodes in it (faster + better cache locality)
// TODO : add generic enums to have results for error handling

#[cfg(feature = "debug-llvm")]
use inkwell::support::LLVMString;


#[derive(serde::Serialize, serde::Deserialize)]
pub struct CachedCompMeta {
    pub shared_libs : Vec<String>,
}

pub struct CachedCompilation {
    pub bitcode_path : PathBuf,
    pub metadata : CachedCompMeta,
}

cfg_if! {
    if #[cfg(feature = "cache")]{
        use crate::cache::{write_cached_llvm_ir, get_cached_llvm_ir};
    } else {
        fn write_cached_llvm_ir(_bitcode_path : &Path, _opt_level : OptimizationLevel, _content : &str, _shared_libs : &[String]){}
        fn get_cached_llvm_ir(content : &str, opt_level : OptimizationLevel) -> Option<CachedCompilation>{
            None
        }
    }
}

pub struct CompileContext<'context, 'refs, 'llvm_ctx> {
    pub rustaml_context : &'context mut RustamlContext,
    pub context : &'llvm_ctx Context,
    pub module : &'refs Module<'llvm_ctx>,
    pub builder : &'refs Builder<'llvm_ctx>,
    pub debug_info : DebugInfo<'llvm_ctx>,
    pub typeinfos : TypeInfos,
    functions : FxHashMap<StringRef, FunctionValue<'llvm_ctx>>,
    main_function : FunctionValue<'llvm_ctx>,
    pub var_vals : FxHashMap<StringRef, PointerValue<'llvm_ctx>>,
    pub external_symbols_declared : FxHashSet<&'static str>,
    internal_functions : Vec<BuiltinFunction<'llvm_ctx>>, // TODO : replace this with a hashmap ?
    pub global_strs : FxHashMap<String, PointerValue<'llvm_ctx>>,
    pub is_optimized : bool,
    shared_libs : Vec<String>,

    pub target_data : TargetData,

    // TODO : put these in a separate struct
    generic_functions : FxHashMap<(StringRef, Vec<Type>, Type), FunctionValue<'llvm_ctx>>,
    pub generic_map : FxHashMap<u32, Type>,
    generic_func_def_ast_node : FxHashMap<StringRef, ASTRef>,

    pub monomorphized_internal_fun : FxHashMap<&'static str, FxHashMap<(Type, Type), FunctionValue<'llvm_ctx>>>, // (Type A, Type B) = function List A -> List B
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
            attributes: vec![attr_args("noundef", 1)],
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_print",
            args: Box::new([ptr_type]),
            ret: Some(llvm_context.void_type().into()),
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
            name: "fprintf",
            is_variadic: true,
            args: Box::new([ptr_type, ptr_type]),
            ret: Some(llvm_context.i32_type().into()),
            attributes: vec![attr_args("noundef", 0), attr_args("noundef", 1)],
        },
        BuiltinFunction {
            name: "printf",
            is_variadic: true,
            args: Box::new([ptr_type]),
            ret: Some(llvm_context.i32_type().into()),
            attributes: vec![attr_args("noundef", 0)],
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

impl<'context, 'refs, 'llvm_ctx> CompileContext<'context, 'refs, 'llvm_ctx> {
    pub fn get_internal_function(&mut self, name : &'static str) -> FunctionValue<'llvm_ctx> {
        if self.external_symbols_declared.contains(name){
            self.module.get_function(name).unwrap()
        } else {
            // use find instead of a hashmap because the number of internal functions is low
            let builtin_function = self.internal_functions.iter().find(|f| f.name == name).unwrap();
            let function_type = get_fn_type(self.context, builtin_function.ret.unwrap(), &builtin_function.args, builtin_function.is_variadic);
            let function_decl = self.module.add_function(name, function_type, Some(Linkage::External));
            for &(attr_loc, attr) in &builtin_function.attributes {
                function_decl.add_attribute(attr_loc, attr);
            }
            self.external_symbols_declared.insert(name);
            function_decl
        }
    }


    // TODO : if this function become more used, make a list like the internal function for builtin_global_vars types
    // or use an enum for name because it is static
    pub fn get_internal_global_var(&mut self, name : &'static str, type_var : BasicTypeEnum<'llvm_ctx>) -> GlobalValue<'llvm_ctx> {
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

// TODO : add a print function that returns unit for the compiler
// the var_type should be resolved at this point : TODO
fn compile_var_decl<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, ast_node : ASTRef, name : StringRef, val : ASTRef, body : Option<ASTRef>, is_global : bool) -> AnyValueEnum<'llvm_ctx> {
    let is_underscore = name.get_str(&compile_context.rustaml_context.str_interner) == "_";
    
    //println!("test vars types : {:#?}", DebugWrapContext::new(&compile_context.var_types, compile_context.rustaml_context));
    
    
    // TODO : if is global and the val is const, just generate a global var
    let val = compile_expr(compile_context, val);

    if !is_underscore {
        //let var_type = compile_context.var_types.get(&name).unwrap_or_else(|| panic!("No type found for var {}", name.get_str(&compile_context.rustaml_context.str_interner)));
        //let var_type = compile_context.typeinfos.vars_ast.get(&name).unwrap().get_type(&compile_context.rustaml_context.ast_pool);
        let var_id = get_var_id(compile_context, ast_node);
        debug_println!(compile_context.rustaml_context.is_debug_print, "var_id  : {:?}", DebugWrapContext::new(&var_id, compile_context.rustaml_context));
        let var_type = get_var_type(compile_context, var_id, name).clone();
        debug_println!(compile_context.rustaml_context.is_debug_print, "var_type decl {:?} : {:?}", name.get_str(&compile_context.rustaml_context.str_interner), var_type);
        let alloca_type = get_llvm_type(compile_context, &var_type);
        let var_ptr = create_var(compile_context, name, val, alloca_type);
        compile_context.debug_info.declare_var(name.get_str(&compile_context.rustaml_context.str_interner), &var_type, var_ptr, compile_context.builder.get_insert_block().unwrap(), compile_context.rustaml_context.content.as_ref().unwrap(), ast_node.get_range(&compile_context.rustaml_context.ast_pool));
    }
        
    let ret = match body {
        Some(b) => compile_expr(compile_context, b),
        None => val,
    };

    if !is_global && !is_underscore {
        compile_context.var_vals.remove(&name);
    }


    ret
}

fn compile_if<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, cond_expr : ASTRef, then_body : ASTRef, else_body : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    let this_function = get_current_function(compile_context.builder);
    let then_bb= compile_context.context.append_basic_block(this_function, "if");
    let else_bb = compile_context.context.append_basic_block(this_function, "else");
    let after_bb = compile_context.context.append_basic_block(this_function, "afterif");

    let bool_val = compile_expr(compile_context, cond_expr);

    create_br_conditional(compile_context, TryInto::<IntValue>::try_into(bool_val).unwrap(), then_bb, else_bb);

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

    let phi_node = compile_context.builder.build_phi(TryInto::<BasicTypeEnum>::try_into(if_val.get_type()).unwrap(), "if_phi").unwrap();
    let if_val_basic = TryInto::<BasicValueEnum>::try_into(if_val).unwrap();
    let else_val_basic = TryInto::<BasicValueEnum>::try_into(else_val).unwrap();
    
    let mut incoming = Vec::new();

    if has_br_then {
        incoming.push((&if_val_basic as _, then_bb_last));
    }

    if has_br_else {
        incoming.push((&else_val_basic as _, else_bb_last));
    }

    //phi_node.add_incoming(&[(&if_val_basic as _, then_bb_last), (&else_val_basic as _, else_bb_last)]);
    phi_node.add_incoming(&incoming);
    phi_node.as_any_value_enum()
}

fn get_format_ctype(c_type : &CType) -> &'static str {
    match c_type {
        CType::I32 => "%d\n",
        CType::U64 => "%ld\n",
        _ => panic!("Can't print ctypes {:?}", c_type)  // TODO
    }
    
}

fn get_format_string(print_type : &Type) -> &'static str {
    match print_type {
        Type::Integer => "%ld\n", // TODO : verify it is good
        Type::Float => "%f\n",
        Type::Str | Type::Bool => "%s\n", // TODO : add a better printing solution
        Type::List(_) => unreachable!(), // the format will not be used
        Type::Function(_, _, _) => panic!("Can't print functions"),
        Type::Unit => "%s\n",
        Type::Never => "", // can't print it, normally if the function is really a never type, it should be never return, so the print should never be called 
        Type::CType(c_type) => get_format_ctype(c_type),
        Type::Any => encountered_any_type(),
        Type::SumType(_) => unreachable!(), // TODO ?
        Type::Generic(_) => unreachable!(),
    }
}

// TODO : call a c function that will call printf ? (then write the formatting part myself ? under a feature flag ?)
fn compile_print<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, print_val : AnyValueEnum<'llvm_ctx>, print_val_type : &Type) -> AnyValueEnum<'llvm_ctx> {
    let mut print_val = TryInto::<BasicMetadataValueEnum>::try_into(print_val).unwrap();

    if let Type::List(_) = print_val_type {
        let print_list_fun = compile_context.get_internal_function("__list_print");
        let print_list_args = vec![print_val];
        compile_context.builder.build_call(print_list_fun, &print_list_args, "print_list_internal").unwrap();
        return get_void_val(compile_context.context);
    }

    let printf_fun = compile_context.get_internal_function("printf");
    // TODO : change this
    let format_str = get_format_string(print_val_type);
    let format_str = create_string(compile_context, format_str);
    match print_val_type {
        Type::Bool => {
            let bool_to_str_fun = compile_context.get_internal_function("__bool_to_str");
            let bool_to_str_args = vec![print_val];
            print_val = compile_context.builder.build_call(bool_to_str_fun, &bool_to_str_args, "bool_to_str_internal").unwrap().try_as_basic_value().unwrap_left().into();
        }
        Type::Unit => {
            print_val = create_string(compile_context, "()").as_basic_value_enum().into();
        }
        _ => {}
    }
    let printf_args = vec![format_str.into(), print_val];
    compile_context.builder.build_call(printf_fun, &printf_args, "print_internal_call").unwrap();
    get_void_val(compile_context.context)
}

fn compile_rand<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>) -> AnyValueEnum<'llvm_ctx>{
    let rand_fun = compile_context.get_internal_function("__rand");
    compile_context.builder.build_call(rand_fun, &[], "rand_internal_call").unwrap().as_any_value_enum()
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
                        let arg_format_str = match arg_type {
                            Type::Integer => "%d",
                            Type::Float => "%f",
                            Type::Bool => "%b",
                            Type::Str => "%s",
                            Type::List(_) => "%l",
                            _ => panic!("Can't format type {:?}", arg_type),
                        };
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

fn compile_format<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, format_str : StringRef, args_val : Vec<AnyValueEnum<'llvm_ctx>>, arg_types : Vec<Type>) -> AnyValueEnum<'llvm_ctx>{
    let format_fun = compile_context.get_internal_function("__format_string");
    let format_str = format_str.get_str(&compile_context.rustaml_context.str_interner);
    let format_str = get_format_string_format(format_str, arg_types.as_slice());
    let mut args= vec![create_string(compile_context, &format_str).into()];
    let mut args_val = args_val.into_iter().zip(arg_types).map(|(e, t)| promote_val_var_arg(compile_context, t, e)).collect::<Vec<_>>();
    args.append(&mut args_val);
    let args = args.into_iter().map(|e| e.try_into().unwrap()).collect::<Vec<_>>();
    compile_context.builder.build_call(format_fun, &args, "format_string_internal_call").unwrap().as_any_value_enum()
}

fn compile_panic<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, message_str : PointerValue<'llvm_ctx>) -> AnyValueEnum<'llvm_ctx>{
    _codegen_runtime_error(compile_context, message_str);
    get_void_val(compile_context.context)

}

fn compile_map<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list_ast : ASTRef, fun_ast : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    let fun_type = fun_ast.get_type(&compile_context.rustaml_context.ast_pool).clone();
    let ret_elem_type = match fun_type {
        Type::Function(_, ret, _) => *ret,
        _ => unreachable!(),
    };
    
    let list_type = list_ast.get_type(&compile_context.rustaml_context.ast_pool).clone();
    let elem_type = match list_type {
        Type::List(e) => *e,
        _ => unreachable!()
    };

    
    let fun_val = compile_expr(compile_context, fun_ast).into_function_value();

    let list_val = compile_expr(compile_context, list_ast);


    let args= vec![any_val_to_metadata(list_val), any_val_to_metadata(fun_val.as_any_value_enum())];
    
    let map_fun = compile_monomorphized_map(compile_context, &elem_type, &ret_elem_type);

    compile_context.builder.build_call(map_fun, &args, "map_call").unwrap().as_any_value_enum()
}

fn compile_filter<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list_ast : ASTRef, fun_ast : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    let fun_type = fun_ast.get_type(&compile_context.rustaml_context.ast_pool).clone();
    let arg_type = match fun_type {
        Type::Function(args, _, _) => args.into_iter().next().unwrap(),
        _ => unreachable!(),
    };
    
    let list_type = list_ast.get_type(&compile_context.rustaml_context.ast_pool).clone();
    let elem_type = match list_type {
        Type::List(e) => *e,
        _ => unreachable!()
    };

    assert_eq!(arg_type, elem_type);

    
    let fun_val = compile_expr(compile_context, fun_ast).into_function_value();

    let list_val = compile_expr(compile_context, list_ast);


    let args= vec![any_val_to_metadata(list_val), any_val_to_metadata(fun_val.as_any_value_enum())];
    
    let filter_fun = compile_monomorphized_filter(compile_context, &elem_type);

    compile_context.builder.build_call(filter_fun, &args, "filter_call").unwrap().as_any_value_enum()
}

fn should_monomorphize_function(arg_types : &[Type], ret_type : &Type) -> bool {
    matches!(ret_type, Type::Generic(_)) || arg_types.iter().any(|e| matches!(e, Type::Generic(_)))
}

fn mangle_name<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, arg_types : &[Type], ret_type : &Type) -> StringRef {
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

fn monomophize_function<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, args_call_types : Vec<Type>, args_def_types : Box<[Type]>, ret_type : &Type) -> FunctionValue<'llvm_ctx> {
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
            a => arg_types_without_generics.push(a.clone()),
        }
    }
    
    let current_bb = compile_context.builder.get_insert_block().unwrap();
    let mangled_name = mangle_name(compile_context, name, &arg_types_without_generics, ret_type);
    let def = compile_function_def(compile_context, mangled_name, &args, body, ast_node, &arg_types_without_generics, ret_type);
    compile_context.builder.position_at_end(current_bb);
    def
}

fn compile_function_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, callee : ASTRef, args: &[ASTRef], range : Range<usize>) -> AnyValueEnum<'llvm_ctx>{
    let (name_str, name) = if let ASTNode::VarUse { name } = callee.get(&compile_context.rustaml_context.ast_pool) {
        match name.get_str(&compile_context.rustaml_context.str_interner) {
            "print" => {
                //let print_val_type = args[0].get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types).unwrap();
                let print_val_type = args[0].get_type(&compile_context.rustaml_context.ast_pool).clone();
                let print_val = compile_expr(compile_context, args[0]);
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
                let args_val = args_ast.iter().map(|&a| compile_expr(compile_context, a)).collect::<Vec<_>>();
                
                return compile_format(compile_context, format_str, args_val, args_types);
            }
            "panic" => {
                let message_val = compile_expr(compile_context, args[0]).into_pointer_value();
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
            n => (Some(n.to_owned()), Some(*name)),
        }
    } else {
        (None, None)
    };
    
    
    
    //let fun = *compile_context.functions.get(&name).unwrap();

    let callee_type = callee.get_type(&compile_context.rustaml_context.ast_pool).clone();

    let (arg_types, ret_type) = match callee_type.clone() {
        Type::Function(args, ret, _) => (args, *ret),
        _ => unreachable!(),
    };
    

    //let name_str = name.get_str(&compile_context.rustaml_context.str_interner);
    let arg_call_types = args.iter().map(|&a| a.get_type(&compile_context.rustaml_context.ast_pool).clone()).collect::<Vec<_>>();
    let args_vals = args.iter().map(|&a| any_val_to_metadata(compile_expr(compile_context, a))).collect::<Vec<BasicMetadataValueEnum>>();
    
    let should_monomophize = should_monomorphize_function(&arg_types, &ret_type);

    let callee_val = if should_monomophize {
        let name = name.unwrap(); // TODO : make this support anonymous functions ? (hash function value instead of name ?)
        let ret_call_type = Type::Any; // TODO
        if let Some(func) = compile_context.generic_functions.get(&(name, arg_call_types.clone(), ret_call_type)) {
            *func
        } else {
            monomophize_function(compile_context, name, arg_call_types, arg_types, &ret_type)
        }.as_any_value_enum()
    } else {
        compile_expr(compile_context, callee)
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
        Either::Left(l) => l.into(),
        Either::Right(_) => get_void_val(compile_context.context), // void, dummy value
    }
}

fn compile_check_overflow<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, intrisic_name : &'static str, message : &'static str, i : IntValue<'llvm_ctx>, i2 : IntValue<'llvm_ctx>, name : &str, range : Range<usize>) -> IntValue<'llvm_ctx> {
    // TODO : do the instrisic find only one time ?
    let llvm_sadd_intrisic = Intrinsic::find(intrisic_name).unwrap();
    let llvm_sadd_decl = llvm_sadd_intrisic.get_declaration(compile_context.module, &[compile_context.context.i64_type().into()]).unwrap();

    let res_struct_val = compile_context.builder.build_call(llvm_sadd_decl, &[i.into(), i2.into()], name).unwrap().as_any_value_enum().into_struct_value();
    let res_val = compile_context.builder.build_extract_value(res_struct_val, 0, &format!("extract_val_{}", name)).unwrap().into_int_value();
    let is_overflow = compile_context.builder.build_extract_value(res_struct_val, 1, &format!("extract_overflow_{}", name)).unwrap().into_int_value();
                    
    let this_function = get_current_function(compile_context.builder);
    let overflow_bb = compile_context.context.append_basic_block(this_function, &format!("overflow_{}", name));
    let after_overflow_check_bb = compile_context.context.append_basic_block(this_function, &format!("after_overflow_check_{}", name));

    compile_context.builder.build_conditional_branch(is_overflow, overflow_bb, after_overflow_check_bb).unwrap();

    compile_context.builder.position_at_end(overflow_bb);
    codegen_lang_runtime_error(compile_context, message, get_debug_loc(compile_context.rustaml_context.content.as_ref().unwrap(), range));

    compile_context.builder.position_at_end(after_overflow_check_bb);

    res_val
}

fn compile_div_or_rem_checked<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, i : IntValue<'llvm_ctx>, i2 : IntValue<'llvm_ctx>, is_div : bool, name : &str, range : Range<usize>) -> IntValue<'llvm_ctx> {
    let line_col = get_debug_loc(compile_context.rustaml_context.content.as_ref().unwrap(), range);
    
    let const_zero = compile_context.context.i64_type().const_zero();
    let is_zero = compile_context.builder.build_int_compare(IntPredicate::EQ, i2, const_zero, "cmp_div_zero").unwrap();

    let this_function = get_current_function(compile_context.builder);
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

fn compile_binop_int<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs_val : AnyValueEnum<'llvm_ctx>, rhs_val : AnyValueEnum<'llvm_ctx>, name : &str, range : Range<usize>) -> IntValue<'llvm_ctx>{
    
    match (lhs_val, rhs_val){
        (AnyValueEnum::IntValue(i),  AnyValueEnum::IntValue(i2)) => {
            match op {
                // TODO : add check for overflow like in rust (with a flag to activate it)
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

fn compile_binop_float<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs_val : AnyValueEnum<'llvm_ctx>, rhs_val : AnyValueEnum<'llvm_ctx>, name : &str) -> FloatValue<'llvm_ctx>{
    match (lhs_val, rhs_val){
        (AnyValueEnum::FloatValue(f),  AnyValueEnum::FloatValue(f2)) => {
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

// TODO : replace most of AnyValueEnum with BasicValueEnum ?
fn compile_binop_bool<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs_val : AnyValueEnum<'llvm_ctx>, rhs_val : AnyValueEnum<'llvm_ctx>, operand_type : Type, name : &str) -> IntValue<'llvm_ctx>{

    if let Type::Unit = operand_type {
        // both types should be unit, so return true
        return compile_context.context.bool_type().const_int(true as u64, false);
    }

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
        _ => panic!("Invalid type for bool op {:?}, {:?}", op, (lhs_val, rhs_val)),
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
            //dbg!(lhs_val, rhs_val);
           // dbg!(elem_type);
            let type_tag_val = get_type_tag_val(compile_context.context, elem_type);
            let std_lhs_val = to_std_c_val(compile_context, lhs_val, elem_type);
            create_list_append_call(compile_context, rhs_val.into_pointer_value(), type_tag_val, std_lhs_val).into()
        },
        Operator::ListMerge => {
            create_list_merge(compile_context, lhs_val.into_pointer_value(), rhs_val.into_pointer_value()).into()
        }
        _ => unreachable!(),
    }
}

// TODO : make also these work with vals (for example with an enum ShortCircuitingArg that can be an ASTRef or an AnyValue)
fn compile_short_circuiting_and<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, lhs : ASTRef, rhs : ASTRef) -> IntValue<'llvm_ctx> {
    let this_function = get_current_function(compile_context.builder);
    let b1_true_bb = compile_context.context.append_basic_block(this_function, "and_b1_true");
    let b2_true_bb = compile_context.context.append_basic_block(this_function, "and_b2_true");
    let after_bb = compile_context.context.append_basic_block(this_function, "and_after");
    
    let lhs_val = compile_expr(compile_context, lhs);
    let b1 = match lhs_val {
        AnyValueEnum::IntValue(i) => i,
        _ => unreachable!(),
    };

    let start_bb = compile_context.builder.get_insert_block().unwrap();

    let has_start_br = create_br_conditional(compile_context, b1, b1_true_bb, after_bb);

    move_bb_after_current(compile_context, b1_true_bb);
    compile_context.builder.position_at_end(b1_true_bb);

    let rhs_val = compile_expr(compile_context, rhs);
    let b2 = match rhs_val {
        AnyValueEnum::IntValue(i) => i,
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

fn compile_short_circuiting_or<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, lhs : ASTRef, rhs : ASTRef) -> IntValue<'llvm_ctx> {
    let this_function = get_current_function(compile_context.builder);
    let b1_false_bb = compile_context.context.append_basic_block(this_function, "or_b1_true");
    let b2_false_bb = compile_context.context.append_basic_block(this_function, "or_b2_true");
    let after_bb = compile_context.context.append_basic_block(this_function, "or_after");
    
    let lhs_val = compile_expr(compile_context, lhs);
    let b1 = match lhs_val {
        AnyValueEnum::IntValue(i) => i,
        _ => unreachable!(),
    };

    let start_bb = compile_context.builder.get_insert_block().unwrap();

    let has_start_br = create_br_conditional(compile_context, b1, after_bb, b1_false_bb);

    move_bb_after_current(compile_context, b1_false_bb);
    compile_context.builder.position_at_end(b1_false_bb);

    let rhs_val = compile_expr(compile_context, rhs);
    let b2 = match rhs_val {
        AnyValueEnum::IntValue(i) => i,
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

fn compile_binop_bool_logical<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs : ASTRef, rhs : ASTRef, name : String) -> IntValue<'llvm_ctx> {
    match op {
        Operator::And => compile_short_circuiting_and(compile_context, lhs, rhs),
        Operator::Or => compile_short_circuiting_or(compile_context, lhs, rhs),
        _ => unreachable!(),
    }
}

fn compile_binop<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs : ASTRef, rhs : ASTRef, range : Range<usize>) -> AnyValueEnum<'llvm_ctx> {
    let name = format!("{:?}", op).to_lowercase();
    if matches!(op, Operator::And | Operator::Or){
        return compile_binop_bool_logical(compile_context, op, lhs, rhs, name).as_any_value_enum();
    }

    let lhs_val = compile_expr(compile_context, lhs);
    let rhs_val = compile_expr(compile_context, rhs);

    let lhs_type = lhs.get_type(&compile_context.rustaml_context.ast_pool).clone();

    match op.get_type() {
        Type::Integer => compile_binop_int(compile_context, op, lhs_val, rhs_val, &name, range).into(),
        Type::Float => compile_binop_float(compile_context, op, lhs_val, rhs_val, &name).into(),
        Type::Bool => compile_binop_bool(compile_context, op, lhs_val, rhs_val, lhs_type, &name).into(),
        Type::Str => compile_binop_str(compile_context, op, lhs_val, rhs_val, &name),
        Type::List(_) => compile_binop_list(compile_context, op, lhs_val, rhs_val, &lhs_type),
        _ => unreachable!(),
    }
}

fn compile_unop<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, expr : ASTRef) -> AnyValueEnum<'llvm_ctx>{
    match (op, expr.get(&compile_context.rustaml_context.ast_pool)){
        (Operator::Minus, ASTNode::Integer { nb }) => return create_int(compile_context, -nb).as_any_value_enum(),
        _ => {}
    }
    let expr_val = compile_expr(compile_context, expr);
    
    match op {
        Operator::Minus => {
            match expr_val {
                AnyValueEnum::IntValue(i) => {
                    let const_zero = compile_context.context.i64_type().const_zero();
                    compile_context.builder.build_int_sub(const_zero, i, "unary_minus").unwrap().as_any_value_enum()
                },
                _ => unreachable!(),
            }
        },
        Operator::Not => {
            match expr_val {
                AnyValueEnum::IntValue(i) => {
                    let const_true = compile_context.context.bool_type().const_int(true as u64, false);
                    compile_context.builder.build_xor(i, const_true, "unary_not").unwrap().as_any_value_enum()
                }
                _ => unreachable!(),
            }
        },
        _ => unreachable!()
    }
}

fn compile_var_use<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, ast_node : ASTRef, name : StringRef) -> AnyValueEnum<'llvm_ctx> {

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
        f.as_any_value_enum()
    } else {
        panic!("Compiler: Unknown var {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context));
    }
}

// TODO : move these in an instrisics.rs

fn create_list_init_static_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, type_tag_val : IntValue<'llvm_ctx>, vals : PointerValue<'llvm_ctx>, len_val : IntValue<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    let function = compile_context.get_internal_function("__list_node_init_static");
    //dbg!(function);
    let args = &[type_tag_val.into(), vals.into(), len_val.into()];
    compile_context.builder.build_call(function, args, "list_init_static").unwrap().as_any_value_enum().into_pointer_value()
}

fn create_list_append_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : PointerValue<'llvm_ctx>, type_tag_val : IntValue<'llvm_ctx>, val : IntValue<'llvm_ctx> ) -> PointerValue<'llvm_ctx> {
    let function = compile_context.get_internal_function("__list_node_append");
    //dbg!(function);
    let args = &[list.into(), type_tag_val.into(), val.into()];
    compile_context.builder.build_call(function, args, "list_append").unwrap().as_any_value_enum().into_pointer_value()
}

fn create_list_merge<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list1 : PointerValue<'llvm_ctx>, list2 : PointerValue<'llvm_ctx>) -> PointerValue<'llvm_ctx> {
    let function = compile_context.get_internal_function("__list_node_merge");
    //dbg!(function);
    let args = &[list1.into(), list2.into()];
    compile_context.builder.build_call(function, args, "list_append").unwrap().as_any_value_enum().into_pointer_value()
}

fn to_std_c_val<'llvm_ctx>(compile_context: &CompileContext<'_, '_, 'llvm_ctx>, val: AnyValueEnum<'llvm_ctx>, val_type : &Type) -> IntValue<'llvm_ctx> {
    match val_type {
        Type::Float | Type::Bool | Type::List(_) | Type::Str | Type::Never => compile_context.builder.build_bit_cast(TryInto::<BasicValueEnum>::try_into(val).unwrap(), compile_context.context.i64_type(), "bitcast_to_uint64_t").unwrap().into_int_value(),
        Type::Integer => val.into_int_value(),
        Type::Unit => {
            let void_val = get_void_val(compile_context.context);
            compile_context.builder.build_bit_cast(TryInto::<BasicValueEnum>::try_into(void_val).unwrap(), compile_context.context.i64_type(), "bitcast_to_uint64_t").unwrap().into_int_value()
        },
        Type::Any => encountered_any_type(),
        Type::CType(_) => todo!(),
        Type::SumType(_) => todo!(),
        Type::Function(_, _, _) | Type::Generic(_) => unreachable!(),
    }
}

fn compile_static_list<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : &[ASTRef], list_type : &Type) -> AnyValueEnum<'llvm_ctx> {

    if list.is_empty(){
        return compile_context.context.ptr_type(AddressSpace::default()).const_null().as_any_value_enum();
    }

    let list_element_type = match list_type {
        Type::List(e) => e.as_ref(),
        _ => unreachable!(),
    };

    let type_tag_val = get_type_tag_val(compile_context.context, list_element_type);
    // TODO : optimize these iterations ?
    let vals = list.iter().map(|e| compile_expr(compile_context, *e)).collect::<Vec<_>>();
    let std_vals = vals.iter().map(|e| to_std_c_val(compile_context, *e, list_element_type)).collect::<Vec<_>>();

    let llvm_element_type = get_llvm_type(compile_context, list_element_type);
    let size = create_int(compile_context, list.len() as i128);
    let static_array = create_entry_block_array_alloca(compile_context, "temp_static_list", llvm_element_type, size);
    

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

    create_list_init_static_call(compile_context, type_tag_val, static_array, size).as_any_value_enum()

}



fn compile_str<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, str : StringRef) -> PointerValue<'llvm_ctx> {
    let str = str.get_str(&compile_context.rustaml_context.str_interner).to_owned();
    create_string(compile_context, &str)
}

fn compile_anon_func<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, ast_node : ASTRef, args : &[StringRef], body : ASTRef) -> FunctionValue<'llvm_ctx> {
    
    let current_bb = compile_context.builder.get_insert_block().unwrap();

    let anon_func_name = "anon_func"; // TODO : add an index ? a hash ? (index : no linking two rustaml obj files, but is it a problem ?)

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

    let function = compile_context.module.add_function(anon_func_name, function_type, Some(inkwell::module::Linkage::Internal));

    // TODO : add debuginfos

    let entry = compile_context.context.append_basic_block(function, "entry");
    compile_context.builder.position_at_end(entry);

    let range = ast_node.get_range(&compile_context.rustaml_context.ast_pool);

    // TODO : need a way to reset vars (for example swap the current vars with an empty one, then put it back)
    for (((arg_name, arg_val), arg_type_llvm), arg_type) in args.iter().zip(function.get_param_iter()).zip(&arg_types_llvm).zip(arg_types) {
        let var_ptr = create_var(compile_context, *arg_name, arg_val.as_any_value_enum(), *arg_type_llvm);
        compile_context.debug_info.declare_var(arg_name.get_str(&compile_context.rustaml_context.str_interner), &arg_type, var_ptr, compile_context.builder.get_insert_block().unwrap(), compile_context.rustaml_context.content.as_ref().unwrap(), range.clone());
    }
    let ret = compile_expr(compile_context, body);

    let return_val: Option<&dyn BasicValue<'_>> = match ret_type {
        Type::Unit => None,
        _ => Some(&TryInto::<BasicValueEnum>::try_into(ret).unwrap()),
    };

    compile_context.builder.build_return(return_val).unwrap(); 

    compile_context.builder.position_at_end(current_bb);

    function
}

fn compile_cast<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, to_type : &Type, expr : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    let start_type = expr.get_type(&compile_context.rustaml_context.ast_pool).clone();
    let start_val = compile_expr(compile_context, expr);

    let i32_type = compile_context.context.i32_type();
    let i64_type = compile_context.context.i64_type();
    match (&start_type, to_type){
        (t1, t2) if t1 == t2 => start_val, // TODO : add a warning in this case
        // TODO
        (Type::CType(CType::I32), Type::Integer) => compile_context.builder.build_int_s_extend(start_val.into_int_value(), i64_type, "c_i32_to_int").unwrap().as_any_value_enum(),
        (Type::Integer, Type::CType(CType::I32)) => compile_context.builder.build_int_truncate(start_val.into_int_value(), i32_type, "int_to_c_i32").unwrap().as_any_value_enum(),
        _ => panic!("Wrong cast"),
    }
}

fn compile_variant<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, _arg : Option<ASTRef>) -> AnyValueEnum<'llvm_ctx> {
    let mut variant_nb = None;
    for (_k, t) in &compile_context.rustaml_context.type_aliases {
        match t {
            Type::SumType(sum_type) => {
                if let Some(pos) = sum_type.variants.iter().position(|v| v.name.as_ref() == name.get_str(&compile_context.rustaml_context.str_interner)) {
                    variant_nb = Some(pos);
                    break;
                }
            }
            _ => {}
        }
    }
    let variant_nb = variant_nb.unwrap();
    create_int(compile_context, variant_nb as i128).into()
}

// TODO : replace AnyValueEnum with BasicMetadataValueEnum in compile_expr and other functions ?
pub fn compile_expr<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, ast_node : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    let range = ast_node.get_range(&compile_context.rustaml_context.ast_pool);
    match ast_node.get(&compile_context.rustaml_context.ast_pool).clone(){
        ASTNode::Integer { nb } => create_int(compile_context, nb).into(), // TODO : sign extend or not ?
        ASTNode::Float { nb } => compile_context.context.f64_type().const_float(nb).into(),
        ASTNode::Boolean { b } => compile_context.context.bool_type().const_int(b as u64, false).into(),
        ASTNode::String { str } => compile_str(compile_context, str).into(),
        ASTNode::VarDecl { name, val, body, var_type: _ } => compile_var_decl(compile_context, ast_node, name, val, body, false),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => compile_if(compile_context, cond_expr, then_body, else_body),
        ASTNode::FunctionCall { callee, args } => compile_function_call(compile_context, callee, &args, range),
        ASTNode::BinaryOp { op, lhs, rhs } => compile_binop(compile_context, op, lhs, rhs, range),
        ASTNode::UnaryOp { op, expr } => compile_unop(compile_context, op, expr),
        ASTNode::VarUse { name } => compile_var_use(compile_context, ast_node, name),
        ASTNode::List { list } => { 
            let t = ast_node.get_type(&compile_context.rustaml_context.ast_pool).clone();
            compile_static_list(compile_context, &list, &t)
        },
        ASTNode::MatchExpr { matched_expr, patterns } => compile_match(compile_context, ast_node, matched_expr, &patterns),
        ASTNode::AnonFunc { args, body, type_annotation: _ } => compile_anon_func(compile_context, ast_node, &args, body).as_any_value_enum(),
        ASTNode::Cast { to_type, expr } => compile_cast(compile_context, &to_type, expr),
        ASTNode::Variant { name, arg } => compile_variant(compile_context, name, arg),
        ASTNode::Unit => get_void_val(compile_context.context),
        t => panic!("unknown AST : {:?}", DebugWrapContext::new(&t, compile_context.rustaml_context)), 
    }
}

fn get_var_id(compile_context: &'_ CompileContext<'_, '_, '_>, ast_node : ASTRef) -> VarId {
    *compile_context.typeinfos.ast_var_ids.get(&ast_node).unwrap()
}

// TODO : is the second part needed ?
// replace panics with unreachables and remove the name arg
fn get_var_type<'context>(compile_context: &'context CompileContext<'context, '_, '_>, var_id : VarId, name : StringRef) -> &'context Type {
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

fn compile_function_def<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, args : &[StringRef], body : ASTRef, ast_node : ASTRef, arg_types : &[Type], return_type : &Type) -> FunctionValue<'llvm_ctx> {
    //println!("typeinfos function_env : {:?}", DebugWrapContext::new(&compile_context.typeinfos.functions_env, compile_context.rustaml_context));
            
    let return_type_llvm = get_llvm_type(compile_context, return_type);
    let param_types = arg_types;
    debug_println!(compile_context.rustaml_context.is_debug_print, "function {:?} param types : {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context), param_types);
    let param_types_llvm = param_types.iter().map(|t| get_llvm_type(compile_context, t)).collect::<Vec<_>>();
    let param_types_metadata = param_types_llvm.iter().map(|t| any_type_to_metadata(compile_context.context, *t)).collect::<Vec<_>>();
    let function_type = get_fn_type(compile_context.context, return_type_llvm, &param_types_metadata, false);
    let function = compile_context.module.add_function(name.get_str(&compile_context.rustaml_context.str_interner), function_type, Some(inkwell::module::Linkage::Internal));

    let function_range = ast_node.get_range(&compile_context.rustaml_context.ast_pool);
    let di_subprogram = compile_context.debug_info.add_function(name.get_str(&compile_context.rustaml_context.str_interner), param_types, return_type, compile_context.rustaml_context.content.as_ref().unwrap(), function_range, compile_context.is_optimized);
    if let Some(di_subprogram) = di_subprogram {
        function.set_subprogram(di_subprogram);
        // TODO : set debuginfo location on builder
    }

    compile_context.debug_info.create_lexical_block();
            
    compile_context.functions.insert(name, function);
            
    let entry = compile_context.context.append_basic_block(function, "entry");
    compile_context.builder.position_at_end(entry);

    let range = ast_node.get_range(&compile_context.rustaml_context.ast_pool); // TODO
    if let Some(loc) = compile_context.debug_info.create_debug_location(compile_context.context, compile_context.rustaml_context.content.as_ref().unwrap(), range.clone()) {
        compile_context.builder.set_current_debug_location(loc);
    }

    debug_println!(compile_context.rustaml_context.is_debug_print,"function {:?} param types llvm : {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context), param_types_llvm);

            //let mut old_arg_name_type = Vec::new(); // to save the types that have the same of the args in the global vars 

    for ((((arg_idx, arg_name), arg_val), arg_type_llvm), arg_type) in args.iter().enumerate().zip(function.get_param_iter()).zip(param_types_llvm).zip(param_types) {
        default_attributes_type(compile_context.context, arg_type, AttributeLoc::Param(arg_idx.try_into().unwrap()), function);
        let var_ptr = create_var(compile_context, *arg_name, arg_val.as_any_value_enum(), arg_type_llvm);
        compile_context.debug_info.declare_var(arg_name.get_str(&compile_context.rustaml_context.str_interner), arg_type, var_ptr, compile_context.builder.get_insert_block().unwrap(), compile_context.rustaml_context.content.as_ref().unwrap(), range.clone());
    }

    default_attributes_type(compile_context.context, return_type, AttributeLoc::Return, function);

    let ret = compile_expr(compile_context, body);

    //dbg!(ret);

    let return_val: Option<&dyn BasicValue<'_>> = match return_type {
        Type::Unit => None,
        _ => Some(&TryInto::<BasicValueEnum>::try_into(ret).unwrap()),
    };

    compile_context.builder.build_return(return_val).unwrap();

    for arg in args {
        compile_context.var_vals.remove(arg);
    }

    compile_context.debug_info.end_lexical_block();
    compile_context.debug_info.end_function();

            // TODO : is it really needed
    compile_context.debug_info.enter_top_level();
    if let Some(loc) = compile_context.debug_info.create_debug_location(compile_context.context, compile_context.rustaml_context.content.as_ref().unwrap(), 0..0) {
        compile_context.builder.set_current_debug_location(loc);
    }

    function
}

// TODO : test to replace AnyValueEnum with &dyn AnyValue ?
fn compile_top_level_node(compile_context: &mut CompileContext, ast_node : ASTRef) {
    compile_context.debug_info.enter_top_level();
    // TODO : add this in enter_top_level
    if let Some(loc) = compile_context.debug_info.create_debug_location(compile_context.context, compile_context.rustaml_context.content.as_ref().unwrap(),0..0){
        compile_context.builder.set_current_debug_location(loc);
    }

    match ast_node.get(&compile_context.rustaml_context.ast_pool).clone() {
        ASTNode::FunctionDefinition { name, args, body, type_annotation: _ } => {
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
        },

        ASTNode::VarDecl { name, val, body, var_type: _ } => {

            let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
            compile_context.builder.position_at_end(last_main_bb);

            compile_var_decl(compile_context, ast_node, name, val, body, true);
        },
        ASTNode::ExternFunc { name, type_annotation, lang, so_str } => {
            let function_ty = get_llvm_type(compile_context, &type_annotation).into_function_type();
            let name_mangled = mangle_name_external(name.get_str(&compile_context.rustaml_context.str_interner), &type_annotation, lang);
            
            let function = compile_context.module.add_function(name_mangled.as_ref(), function_ty, Some(Linkage::External)); // TODO : what linkage ?
            compile_context.functions.insert(name, function);
            if let Some(so_str) = so_str {
                compile_context.shared_libs.push(so_str.get_str(&compile_context.rustaml_context.str_interner).to_owned());
            }
        }
        ASTNode::TypeAlias { name, type_alias } => {}
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


fn get_main_function<'llvm_ctx>(llvm_context : &'llvm_ctx Context, module : &Module<'llvm_ctx>) -> FunctionValue<'llvm_ctx> {
    let param_types = &[BasicMetadataTypeEnum::IntType(llvm_context.i32_type()), BasicMetadataTypeEnum::PointerType(llvm_context.ptr_type(AddressSpace::default()))];
    let main = module.add_function("main", llvm_context.i32_type().fn_type(param_types, false), Some(inkwell::module::Linkage::External));
    llvm_context.append_basic_block(main, "entry");
    main
}

fn run_passes_on(module: &Module, target_machine : &TargetMachine, opt_level : OptimizationLevel, sanitizer : bool) {
    // TODO : test with "function(mem2reg)," at the start (like https://github.com/inko-lang/inko/blob/main/compiler/src/llvm/passes.rs#L553)
    // to remove allocas even with no optimizations enabled ?
    let passes_str = format!("default<O{}>", opt_level as u8);
    
    module.run_passes(&passes_str, target_machine, PassBuilderOptions::create()).unwrap();
}

// TODO : instead install file in filesystem ?
const STD_C_CONTENT: &str = include_str!("../../std.c");

fn link_exe(rustaml_context: &mut RustamlContext, filename_out : &Path, bitcode_file : &Path, shared_libs : &[String], opt_level : OptimizationLevel, optional_args : &OptionalArgs){
    // use cc ?
    // TODO : use lld (https://github.com/mun-lang/lld-rs) for linking instead ?
    // TODO : use libclang ? (clang-rs ? https://github.com/llvm/llvm-project/blob/main/clang/tools/driver/cc1_main.cpp#L85 ?)

    
    let out_std_path = pathbuf![&std::env::temp_dir(), "std.bc"];
    let out_std_path_str = out_std_path.as_os_str();

    rustaml_context.start_section("std");

    // TODO : pass optimization level to this function

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


    // TODO : work on freestanding (add set target to make the os unknown in target triplet, pass linker script or just pass linker arg ?)
    if optional_args.freestanding {
        clang_std.arg("-ffreestanding").arg("-nostdlib");
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

    if !optional_args.disable_gc {
        link_cmd.arg("-lgc");
    }

    if optional_args.freestanding {
        link_cmd.arg("-ffreestanding").arg("-nostdlib");
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

    if !link_cmd.spawn().expect("linker failed").wait().unwrap().success() {
        return;
    }

    rustaml_context.end_section("linker");

    std::fs::remove_file(&out_std_path).expect("Couldn't delete std bitcode file");
}

const DEBUGINFO_VERSION: u64 = 3;

pub struct OptionalArgs {
    optimization_level : u8,
    keep_temp : bool,
    disable_gc : bool, 
    enable_sanitizer : bool, 
    enable_debuginfos : bool, 
    freestanding : bool,
    lib_search_paths : Vec<String>
}

impl OptionalArgs {
    // TODO : make this a builder pattern ?
    pub fn new(optimization_level : Option<u8>, keep_temp : bool, disable_gc : bool, enable_sanitizer : bool, enable_debuginfos : bool, freestanding : bool, lib_search_paths : Vec<String>) -> OptionalArgs {
        OptionalArgs { 
            optimization_level: optimization_level.unwrap_or(0), 
            keep_temp, 
            disable_gc, 
            enable_sanitizer, 
            enable_debuginfos,
            freestanding,
            lib_search_paths 
        }
    }
}

// TODO : add self profiling (dump in file the time of lexer, ast, compilation, compiling std, linking, etc)

// TODO : pass all the args after optimization level as a struct named OptionalArgs
pub fn compile(frontend_output : FrontendOutput, rustaml_context: &mut RustamlContext, filename : &Path, filename_out : Option<&Path>, optional_args : OptionalArgs) {
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

        let main_function = get_main_function(&context, &module);

        let internal_functions = get_internal_functions(&context);

        let mut compile_context = CompileContext {
            rustaml_context,
            context : &context,
            module: &module,
            builder: &builder,
            debug_info,
            is_optimized,
            typeinfos: frontend_output.type_infos,
            functions: FxHashMap::default(),
            main_function,
            var_vals: FxHashMap::default(),
            internal_functions,
            external_symbols_declared: FxHashSet::default(),
            global_strs: FxHashMap::default(),
            shared_libs: Vec::new(),
            target_data,
            generic_map: FxHashMap::default(),
            generic_functions: FxHashMap::default(),
            generic_func_def_ast_node: FxHashMap::default(),
            monomorphized_internal_fun: init_monomorphized_internal_fun(),
        };

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
            compile_context.module.print_to_file(filename_without_ext.clone() + "_error.ll").unwrap();
            panic!("LLVM ERROR {}", e.to_string()) 
        }).unwrap();


        
        compile_context.rustaml_context.end_section("llvm-codegen");

        compile_context.rustaml_context.start_section("llvm-opt");
        run_passes_on(compile_context.module, &target_machine, optimization_level, optional_args.enable_sanitizer);
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