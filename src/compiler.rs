use core::panic;
use std::{hash::{Hash, Hasher}, io::Write, path::Path, process::{Command, Stdio}, time::{SystemTime, UNIX_EPOCH}};
use debug_with_context::DebugWrapContext;
use crate::{ast::{ASTNode, ASTRef, Pattern, PatternRef, Type}, compiler_utils::{codegen_runtime_error, create_int, create_string, create_var, encountered_any_type, get_current_function, get_fn_type, get_llvm_type, get_type_tag_val, load_list_tail, load_list_val, move_bb_after_current, promote_val_var_arg}, debug_println, lexer::Operator, rustaml::{FrontendOutput, RustamlContext}, string_intern::StringRef, types::{TypeInfos, VarId}};
use inkwell::{attributes::{Attribute, AttributeLoc}, basic_block::BasicBlock, builder::Builder, context::Context, module::{Linkage, Module}, passes::PassBuilderOptions, support::LLVMString, targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine}, types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum}, values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, GlobalValue, IntValue, PointerValue}, AddressSpace, Either, FloatPredicate, IntPredicate, OptimizationLevel};
use pathbuf::pathbuf;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

// TODO : add generic enums to have results for error handling
// TODO : add a C FFI

pub struct CompileContext<'context, 'refs, 'llvm_ctx> {
    pub rustaml_context : &'context RustamlContext,
    pub context : &'llvm_ctx Context,
    pub module : &'refs Module<'llvm_ctx>,
    pub builder : &'refs Builder<'llvm_ctx>,
    pub typeinfos : TypeInfos,
    functions : FxHashMap<StringRef, FunctionValue<'llvm_ctx>>,
    main_function : FunctionValue<'llvm_ctx>,
    pub var_vals : FxHashMap<StringRef, PointerValue<'llvm_ctx>>,
    pub external_symbols_declared : FxHashSet<&'static str>,
    internal_functions : Vec<BuiltinFunction<'llvm_ctx>>, // TODO : replace this with a hashmap ?
    pub global_strs : FxHashMap<String, PointerValue<'llvm_ctx>>,
}

#[derive(Clone, Default)]
struct BuiltinFunction<'llvm_ctx> {
    name : &'static str,
    args : Vec<BasicMetadataTypeEnum<'llvm_ctx>>,
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
    vec![
        BuiltinFunction {
            name: "__str_cmp",
            args: vec![ptr_type, ptr_type],
            ret: Some(llvm_context.bool_type().into()),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__str_append",
            args: vec![ptr_type, ptr_type],
            ret: Some(ptr_type_ret),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_node_append",
            args: vec![ptr_type, llvm_context.i8_type().into(), llvm_context.i64_type().into()],
            ret: Some(ptr_type_ret),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_len",
            args: vec![ptr_type],
            ret: Some(llvm_context.i64_type().into()),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__list_print",
            args: vec![ptr_type],
            ret: Some(llvm_context.void_type().into()),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__bool_to_str",
            args: vec![llvm_context.bool_type().into()],
            ret: Some(ptr_type_ret),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__rand",
            args: vec![],
            ret: Some(llvm_context.i64_type().into()),
            ..Default::default()
        },
        BuiltinFunction {
            name: "__format_string",
            is_variadic: true,
            args: vec![ptr_type],
            ret: Some(ptr_type_ret),
            ..Default::default()
        },
        BuiltinFunction {
            name: "fprintf",
            is_variadic: true,
            args: vec![ptr_type, ptr_type],
            ret: Some(llvm_context.i32_type().into()),
            ..Default::default()
        },
        BuiltinFunction {
            name: "printf",
            is_variadic: true,
            args: vec![ptr_type],
            ret: Some(llvm_context.i32_type().into()),
            ..Default::default()
        },
        BuiltinFunction {
            name: "exit",
            args: vec![llvm_context.i32_type().into()],
            ret: Some(llvm_context.void_type().into()),
            attributes: vec![attr("noreturn")],
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
        let var_type = get_var_type(compile_context, var_id, name);
        debug_println!(compile_context.rustaml_context.is_debug_print, "var_type decl {:?} : {:?}", name.get_str(&compile_context.rustaml_context.str_interner), var_type);
        let alloca_type = get_llvm_type(compile_context.context, var_type);
        create_var(compile_context, name, val, alloca_type);
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

    compile_context.builder.build_conditional_branch(TryInto::<IntValue>::try_into(bool_val).unwrap(), then_bb, else_bb).unwrap();

    compile_context.builder.position_at_end(then_bb);


    let if_val = compile_expr(compile_context, then_body);
    compile_context.builder.build_unconditional_branch(after_bb).unwrap();

    let then_bb_last = compile_context.builder.get_insert_block().unwrap();

    move_bb_after_current(compile_context, else_bb);


    compile_context.builder.position_at_end(else_bb);

    let else_val = compile_expr(compile_context, else_body);
    compile_context.builder.build_unconditional_branch(after_bb).unwrap();

    let else_bb_last = compile_context.builder.get_insert_block().unwrap();


    move_bb_after_current(compile_context, after_bb);
    compile_context.builder.position_at_end(after_bb);

    let phi_node = compile_context.builder.build_phi(TryInto::<BasicTypeEnum>::try_into(if_val.get_type()).unwrap(), "if_phi").unwrap();
    let if_val_basic = TryInto::<BasicValueEnum>::try_into(if_val).unwrap();
    let else_val_basic = TryInto::<BasicValueEnum>::try_into(else_val).unwrap();
    phi_node.add_incoming(vec![(&if_val_basic as _, then_bb_last), (&else_val_basic as _, else_bb_last)].as_slice());
    phi_node.as_any_value_enum()
}

// dummy val for void, if it is used as a real value, it is a bug
fn get_void_val<'llvm_ctx>(llvm_context : &'llvm_ctx Context) -> AnyValueEnum<'llvm_ctx> {
    llvm_context.i64_type().get_undef().into()
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
        Type::Any => encountered_any_type(),
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

fn compile_function_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, name : StringRef, args: &[ASTRef]) -> AnyValueEnum<'llvm_ctx>{
    match name.get_str(&compile_context.rustaml_context.str_interner) {
        "print" => {
            //let print_val_type = args[0].get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types).unwrap();
            let print_val_type = args[0].get_type(&compile_context.rustaml_context.ast_pool);
            let print_val = compile_expr(compile_context, args[0]);
            return compile_print(compile_context, print_val, print_val_type);
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
        _ => {}
    }
    
    
    let fun = *compile_context.functions.get(&name).unwrap();
    let name_str = name.get_str(&compile_context.rustaml_context.str_interner);
    let args_vals = args.iter().map(|&a| compile_expr(compile_context, a).try_into().unwrap()).collect::<Vec<BasicMetadataValueEnum>>();
    let ret = compile_context.builder.build_call(fun, args_vals.as_slice(), name_str).unwrap().try_as_basic_value();
    match ret {
        Either::Left(l) => l.into(),
        Either::Right(_) => get_void_val(compile_context.context), // void, dummy value
    }
}

fn compile_binop_int<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs_val : AnyValueEnum<'llvm_ctx>, rhs_val : AnyValueEnum<'llvm_ctx>, name : &str) -> IntValue<'llvm_ctx>{
    
    match (lhs_val, rhs_val){
        (AnyValueEnum::IntValue(i),  AnyValueEnum::IntValue(i2)) => {
            match op {
                Operator::Plus => compile_context.builder.build_int_add(i, i2, name).unwrap(),
                Operator::Minus => compile_context.builder.build_int_sub(i, i2, name).unwrap(),
                // TODO : add check for overflow like in rust
                Operator::Mult => compile_context.builder.build_int_mul(i, i2, name).unwrap(),
                Operator::Div => compile_context.builder.build_int_signed_div(i, i2, name).unwrap(),
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
            create_list_append_call(compile_context, rhs_val.into_pointer_value(), type_tag_val, lhs_val).into()
        },
        _ => unreachable!(),
    }
}

fn compile_binop<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, op : Operator, lhs : ASTRef, rhs : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    let name = format!("{:?}", op).to_lowercase();
    let lhs_val = compile_expr(compile_context, lhs);
    let rhs_val = compile_expr(compile_context, rhs);

    let lhs_type = lhs.get_type(&compile_context.rustaml_context.ast_pool);

    match op.get_type() {
        Type::Integer => compile_binop_int(compile_context, op, lhs_val, rhs_val, &name).into(),
        Type::Float => compile_binop_float(compile_context, op, lhs_val, rhs_val, &name).into(),
        Type::Bool => compile_binop_bool(compile_context, op, lhs_val, rhs_val, lhs_type.clone(), &name).into(),
        Type::Str => compile_binop_str(compile_context, op, lhs_val, rhs_val, &name),
        Type::List(_) => compile_binop_list(compile_context, op, lhs_val, rhs_val, lhs_type),
        _ => unreachable!(),
    }
}

fn compile_var_use<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, ast_node : ASTRef, name : StringRef) -> AnyValueEnum<'llvm_ctx> {
    
    /*for (v, v_t) in &compile_context.var_types {
        println!("{} = {:?}", v.get_str(&compile_context.rustaml_context.str_interner), v_t);
    }*/

    let var_id = get_var_id(compile_context, ast_node);
    
    //let var_type = compile_context.var_types.get(&name).unwrap_or_else(|| panic!("Unknown variable {:?}", name.get_str(&compile_context.rustaml_context.str_interner)));
    let var_type = get_var_type(compile_context, var_id, name);
    debug_println!(compile_context.rustaml_context.is_debug_print, "var_type use {:?} : {:?}", name.get_str(&compile_context.rustaml_context.str_interner), var_type);
    //let var_type = compile_context.typeinfos.vars_ast.get(&name).unwrap().get_type(&compile_context.rustaml_context.ast_pool);
    let load_type = get_llvm_type(compile_context.context, var_type);
    let load_basic_type = TryInto::<BasicTypeEnum>::try_into(load_type).unwrap();
    let ptr = *compile_context.var_vals.get(&name).unwrap_or_else(|| panic!("Compiler: Unknown var {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context)));
    compile_context.builder.build_load(load_basic_type, ptr, name.get_str(&compile_context.rustaml_context.str_interner)).unwrap().into()
}

fn create_list_append_call<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : PointerValue<'llvm_ctx>, type_tag_val : IntValue<'llvm_ctx>, val : AnyValueEnum<'llvm_ctx> ) -> PointerValue<'llvm_ctx> {
    let function = compile_context.get_internal_function("__list_node_append");
    //dbg!(function);
    let args = &[list.into(), type_tag_val.into(), val.try_into().unwrap()];
    compile_context.builder.build_call(function, args, "list_append").unwrap().as_any_value_enum().into_pointer_value()
}

fn to_std_c_val<'llvm_ctx>(compile_context: &CompileContext<'_, '_, 'llvm_ctx>, val: AnyValueEnum<'llvm_ctx>, val_type : &Type) -> AnyValueEnum<'llvm_ctx> {
    match val_type {
        Type::Float | Type::Bool | Type::List(_) | Type::Str | Type::Never => compile_context.builder.build_bit_cast(TryInto::<BasicValueEnum>::try_into(val).unwrap(), compile_context.context.i64_type(), "bitcast_to_uint64_t").unwrap().as_any_value_enum(),
        _ => val,  
    }
}

fn compile_static_list<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list : &[ASTRef], list_type : &Type) -> AnyValueEnum<'llvm_ctx> {
    let mut current_node = compile_context.context.ptr_type(AddressSpace::default()).const_null();
    
    // TODO : optimize this by keeping the last node and just appending to it to not have to go through the list each time by doing append ?
    // with a function like __append_with_tail that would return a struct with the head and the tail ?

    let list_element_type = match list_type {
        Type::List(e) => e.as_ref(),
        _ => unreachable!(),
    };

    /*for e in list {
        println!("e = {:?}", DebugWrapContext::new(e, compile_context.rustaml_context));
    }


    dbg!(&list_element_type);*/

    let type_tag_val = get_type_tag_val(compile_context.context, list_element_type);


    for &e in list {
        let val = compile_expr(compile_context, e);
        let val = to_std_c_val(compile_context, val, list_element_type);
        let node_val = create_list_append_call(compile_context, current_node, type_tag_val, val);

        current_node = node_val;
    }

    current_node.into()
}


fn compile_pattern_match_bool_val<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : PatternRef, matched_val : AnyValueEnum<'llvm_ctx>, matched_expr_type : &Type) -> IntValue<'llvm_ctx>{
    match pattern.get(&compile_context.rustaml_context.pattern_pool) {
        Pattern::Integer(i) => compile_context.builder.build_int_compare(inkwell::IntPredicate::EQ, matched_val.try_into().unwrap_or_else(|_| panic!("not an int value : {:?}", matched_val)), compile_context.context.i64_type().const_int(*i as u64, false), "match_int_cmp").unwrap(),
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
            
            // empty list (just for now before the todo is done ? (TODO ?))
            if pattern_list.is_empty(){
                return compile_context.builder.build_is_null(matched_val.into_pointer_value(), "match_list_empty").unwrap();
            } else {

                let elem_type = match matched_expr_type {
                    Type::List(e) => *e.to_owned(),
                    _ => unreachable!(),   
                };

                let this_function = get_current_function(compile_context.builder);
                // TODO : position these bbs ?
                let list_len_cmp = compile_context.context.append_basic_block(this_function, "match_list_len_cmp");
                let patterns_cmp = compile_context.context.append_basic_block(this_function, "match_list_element_cmp");
                let after_bb = compile_context.context.append_basic_block(this_function, "math_after_list_cmp");


                compile_context.builder.build_unconditional_branch(list_len_cmp).unwrap();
                compile_context.builder.position_at_end(list_len_cmp);
                
                let const_list_pattern_len = compile_context.context.i64_type().const_int(pattern_list.len() as u64, false);
                
                let list_len_fun = compile_context.get_internal_function("__list_len");
                let list_len = compile_context.builder.build_call(list_len_fun, &[matched_val.try_into().unwrap()], "list_len_internal").unwrap().as_any_value_enum().into_int_value();

                let is_same_len = compile_context.builder.build_int_compare(IntPredicate::EQ, list_len, const_list_pattern_len, "match_list_len_cmp").unwrap();
                compile_context.builder.build_conditional_branch(is_same_len, patterns_cmp, after_bb).unwrap();
                
                
                compile_context.builder.position_at_end(patterns_cmp);
                // TODO : add real short circuiting (check each of the element and if one is not the same, make it false)
                // either:
                // - create the list in a static list form, then loop through it
                // - unwrap the loop (so no branching, but bigger code size)
                // add both with a flag, then test them and remove one ?
                let mut bools_patterns = Vec::new();
                let mut current_node = matched_val.into_pointer_value();

                let mut is_first = true;
                
                for p in pattern_list.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        current_node = load_list_tail(compile_context, current_node);
                    }
                    
                    let current_node_val = load_list_val(compile_context, &elem_type, current_node).as_any_value_enum();

                    let b = compile_pattern_match_bool_val(compile_context, *p, current_node_val, &elem_type);
                    bools_patterns.push(b);
                }

                let true_val = compile_context.context.bool_type().const_int(true as u64, false);
                let pattern_cmp_bool  = bools_patterns.iter().fold(true_val, |acc, e| compile_context.builder.build_and(acc, *e, "match_pattern_and_list").unwrap());
                
                compile_context.builder.build_unconditional_branch(after_bb).unwrap();

                compile_context.builder.position_at_end(after_bb);

                let false_val = compile_context.context.bool_type().const_int(false as u64, false);
                let bool_phi = compile_context.builder.build_phi(compile_context.context.bool_type(), "match_list_cmp_phi").unwrap();
                let incoming_branches = &[(&false_val as _, list_len_cmp), (&pattern_cmp_bool as _, patterns_cmp)];
                bool_phi.add_incoming(incoming_branches);
                
                bool_phi.as_basic_value().into_int_value()
            }
        },
        Pattern::String(s) => {
            let str_cmp_fun = compile_context.get_internal_function("__str_cmp");
            // TODO : verify it these strings are deduplicated
            let pattern_str_val = create_string(compile_context, s.get_str(&compile_context.rustaml_context.str_interner));

            let args = vec![pattern_str_val.into(), matched_val.try_into().unwrap()];
            compile_context.builder.build_call(str_cmp_fun, &args, "pattern_match_str_cmp").unwrap().as_any_value_enum().into_int_value()
        },
        // the type should be checked before (TODO ?)
        // TODO : need to recursively make compares if there is more than one destructuring
        Pattern::ListDestructure(_, _) => compile_context.builder.build_int_compare(IntPredicate::NE, matched_val.into_pointer_value(), compile_context.context.ptr_type(AddressSpace::default()).const_null(), "cmp_destructure_not_empty").unwrap(),
        // TODO
        p => panic!("unknown pattern {:?}", DebugWrapContext::new(p, compile_context.rustaml_context)),
        //_ => unreachable!()
    }
}


// init vars, etc
fn compile_pattern_match_prologue<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : PatternRef, matched_val : AnyValueEnum<'llvm_ctx>, matched_val_type : &Type){
    match pattern.get(&compile_context.rustaml_context.pattern_pool) {
        Pattern::VarName(n) => {
            //let matched_val_type = matched_val.get_type();
            let matched_val_type_llvm = get_llvm_type(compile_context.context, matched_val_type);
            create_var(compile_context, *n, matched_val, matched_val_type_llvm);
            //compile_context.var_types.insert(*n, matched_val_type.clone());
        }
        Pattern::ListDestructure(head, tail) => {
            let element_type = match matched_val_type {
                Type::List(e) => e.as_ref(),
                _ => unreachable!(),
            };
            
            // TODO : add this before (during AST -> so need to have a list of stacks instead of a hashmap ? or a hashmap of vecs ?)
            // TODO : would need a stack to use the old_val (use a HashMap of Vec is another solution to use during compilation, so there's more work because need to find the type of match multiple types during compilation, but could have just good caching instead ?)
            //let _old_val = compile_context.var_types.insert(*head, element_type.clone());

            let element_type_llvm = get_llvm_type(compile_context.context, element_type);
            let matched_val_list = matched_val.into_pointer_value();
            let head_val = load_list_val(compile_context, element_type, matched_val_list);
            create_var(compile_context, *head, head_val.into(), element_type_llvm);
            let tail_val = load_list_tail(compile_context, matched_val_list);
            compile_pattern_match_prologue(compile_context, *tail, tail_val.into(), matched_val_type);

            /*if let Some(old_v) = old_val {
                // ...
            }*/
        }
       _ => {}
    }
}

/*fn compile_pattern_match_epilogue<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : &Pattern){
    match pattern {
        Pattern::VarName(name) => {
            compile_context.var_types.remove(name);
        }
        Pattern::ListDestructure(head, _tail) => {
            compile_context.var_types.remove(head);
        }
        _ => {}
    }
}*/

fn compile_pattern_match<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : PatternRef, matched_val : AnyValueEnum<'llvm_ctx>, matched_val_type : &Type, bb: BasicBlock<'llvm_ctx>, else_bb : BasicBlock<'llvm_ctx>){
    let bool_val = compile_pattern_match_bool_val(compile_context, pattern, matched_val, matched_val_type);

    compile_context.builder.build_conditional_branch(bool_val, bb, else_bb).unwrap();
}


// TODO : test nested matchs for problem with bb placement (use move_bb_after_current ?)
fn compile_match<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, match_node : ASTRef, matched_expr : ASTRef, patterns : &[(PatternRef, ASTRef)]) -> AnyValueEnum<'llvm_ctx> {
    
    let matched_val = compile_expr(compile_context, matched_expr);
    //let matched_val_type = matched_expr.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types).unwrap();
    let matched_val_type = matched_expr.get_type(&compile_context.rustaml_context.ast_pool);
    let function = get_current_function(compile_context.builder);


    // TODO : instead of calling get_type here, get all the types and store them in the nodes during parsing ?
    //let match_type = match_node.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types).unwrap();
    let match_type = match_node.get_type(&compile_context.rustaml_context.ast_pool);
    let match_type_llvm: AnyTypeEnum = get_llvm_type(compile_context.context, match_type);

    let mut match_bbs = Vec::new();
    for _ in patterns {
        let match_bb = compile_context.context.append_basic_block(function, "match_case");
        let match_else_bb = compile_context.context.append_basic_block(function, "match_else");
        match_bbs.push((match_bb, match_else_bb));
    }

    let after_match = compile_context.context.append_basic_block(function, "after_match");

    let mut pattern_vals = Vec::new();

    let mut match_phi_bbs = Vec::new();

    for (pattern, pattern_bbs) in patterns.iter().zip(&match_bbs) {
        let (pattern, pattern_body) = pattern;
        let (pattern_bb, pattern_else_bb) = pattern_bbs;
        compile_pattern_match(compile_context, *pattern, matched_val, matched_val_type, *pattern_bb, *pattern_else_bb);
        move_bb_after_current(compile_context, *pattern_bb);
        compile_context.builder.position_at_end(*pattern_bb);
        compile_pattern_match_prologue(compile_context, *pattern, matched_val, matched_val_type);
        let pattern_body_val = compile_expr(compile_context, *pattern_body);
        //compile_pattern_match_epilogue(compile_context, pattern);
        match_phi_bbs.push(compile_context.builder.get_insert_block().unwrap());
        compile_context.builder.build_unconditional_branch(after_match).unwrap();
        pattern_vals.push(pattern_body_val);
        move_bb_after_current(compile_context, *pattern_else_bb);
        compile_context.builder.position_at_end(*pattern_else_bb);
    }

    // TODO : exit with error if no case was matched
    // TODO : add line number ? 
    codegen_runtime_error(compile_context, "no match branch was found");

    move_bb_after_current(compile_context, after_match);
    compile_context.builder.position_at_end(after_match);
    let phi_node = compile_context.builder.build_phi(TryInto::<BasicTypeEnum>::try_into(match_type_llvm).unwrap(), "match_phi").unwrap();
    let mut incoming_phi = Vec::new();
    for (val, bb) in pattern_vals.iter().zip(&match_phi_bbs) {
        let basic_val = TryInto::<BasicValueEnum>::try_into(*val).unwrap();
        incoming_phi.push((basic_val, *bb));
    }

    let incoming_phi = incoming_phi.iter().map(|(val, bb)| (val as &dyn BasicValue, *bb)).collect::<Vec<_>>();

    phi_node.add_incoming(&incoming_phi);
    
    phi_node.as_any_value_enum()
}

fn compile_str<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, str : StringRef) -> PointerValue<'llvm_ctx> {
    create_string(compile_context, str.get_str(&compile_context.rustaml_context.str_interner))
}

// TODO : replace AnyValueEnum with BasicMetadataValueEnum in compile_expr and other functions ?
fn compile_expr<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, ast_node : ASTRef) -> AnyValueEnum<'llvm_ctx> {
    match ast_node.get(&compile_context.rustaml_context.ast_pool){
        ASTNode::Integer { nb } => create_int(compile_context, *nb).into(), // TODO : sign extend or not ?
        ASTNode::Float { nb } => compile_context.context.f64_type().const_float(*nb).into(),
        ASTNode::Boolean { b } => compile_context.context.bool_type().const_int(*b as u64, false).into(),
        ASTNode::String { str } => compile_str(compile_context, *str).into(),
        ASTNode::VarDecl { name, val, body, var_type: _ } => compile_var_decl(compile_context, ast_node, *name, *val, *body, false),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => compile_if(compile_context, *cond_expr, *then_body, *else_body),
        ASTNode::FunctionCall { name, args } => compile_function_call(compile_context, *name, args),
        ASTNode::BinaryOp { op, lhs, rhs } => compile_binop(compile_context, *op, *lhs, *rhs),
        ASTNode::VarUse { name } => compile_var_use(compile_context, ast_node, *name),
        ASTNode::List { list } => { 
            compile_static_list(compile_context, list, ast_node.get_type(&compile_context.rustaml_context.ast_pool))
        },
        ASTNode::MatchExpr { matched_expr, patterns } => compile_match(compile_context, ast_node, *matched_expr, patterns),
        ASTNode::Unit => get_void_val(compile_context.context),
        t => panic!("unknown AST : {:?}", DebugWrapContext::new(t, compile_context.rustaml_context)), 
        //_ => todo!()
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
            //let var_val_ast = *compile_context.typeinfos.vars_ast.get(&name).unwrap_or_else(|| panic!("Unknown var {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context))); // TODO : add better handling instead of unwrap (the var not found should be here ?)
            //var_val_ast.get_type(&compile_context.rustaml_context.ast_pool)
            panic!("Compiler: unknown var : {:?}", DebugWrapContext::new(&name, compile_context.rustaml_context));
            //unreachable!()
        }
    }
}

// TODO : test to replace AnyValueEnum with &dyn AnyValue ?
fn compile_top_level_node(compile_context: &mut CompileContext, ast_node : ASTRef) {
    match ast_node.get(&compile_context.rustaml_context.ast_pool) {
        ASTNode::FunctionDefinition { name, args, body, return_type: _ } => {
            //println!("typeinfos function_env : {:?}", DebugWrapContext::new(&compile_context.typeinfos.functions_env, compile_context.rustaml_context));
            let (return_type, arg_types) = match compile_context.typeinfos.functions_env.get(name).unwrap(){
                Type::Function(args, ret, _) => (ret.as_ref().clone(), args),
                t => panic!("BUG : the function definition has not a function type, it is {:?} instead", t), // TODO : replace this with an unreachable
            };

            let return_type_llvm = get_llvm_type(compile_context.context, &return_type);
            //let param_types = args.iter().map(|a| get_llvm_type(compile_context.context, &a.arg_type).try_into().unwrap()).collect::<Vec<_>>();
            //let param_types = args.iter().map(|a| get_var_type(compile_context, a.name)).collect::<Vec<_>>();
            let param_types = arg_types;
            debug_println!(compile_context.rustaml_context.is_debug_print, "function {:?} param types : {:?}", DebugWrapContext::new(name, compile_context.rustaml_context), param_types);
            let param_types = param_types.iter().map(|t| get_llvm_type(compile_context.context, t)).collect::<Vec<_>>();
            let param_types_metadata = param_types.iter().map(|t| (*t).try_into().unwrap()).collect::<Vec<_>>();
            let function_type = get_fn_type(compile_context.context, return_type_llvm, &param_types_metadata, false);
            let function = compile_context.module.add_function(name.get_str(&compile_context.rustaml_context.str_interner), function_type, Some(inkwell::module::Linkage::Internal));
            compile_context.functions.insert(*name, function);
            
            let entry = compile_context.context.append_basic_block(function, "entry");
            compile_context.builder.position_at_end(entry);

            debug_println!(compile_context.rustaml_context.is_debug_print,"function {:?} param types llvm : {:?}", DebugWrapContext::new(name, compile_context.rustaml_context), param_types);

            //let mut old_arg_name_type = Vec::new(); // to save the types that have the same of the args in the global vars 

            for ((arg, arg_val), arg_type) in args.iter().zip(function.get_param_iter()).zip(param_types) {
                /*match compile_context.var_types.insert(arg.name, arg.arg_type.clone()) {
                    Some(old_type) => old_arg_name_type.push((arg.name, old_type)),
                    None => {}
                }*/
                create_var(compile_context, arg.name, arg_val.as_any_value_enum(), arg_type);
            }

            let ret = compile_expr(compile_context, *body);

            //dbg!(ret);

            let return_val: Option<&dyn BasicValue<'_>> = match return_type {
                Type::Unit => None,
                _ => Some(&TryInto::<BasicValueEnum>::try_into(ret).unwrap()),
            };

            compile_context.builder.build_return(return_val).unwrap();

            for arg in args {
                compile_context.var_vals.remove(&arg.name);
                //compile_context.var_types.remove(&arg.name);
            }

            /*for (n, t) in old_arg_name_type {
                compile_context.var_types.insert(n, t);
            }*/
        },

        ASTNode::VarDecl { name, val, body, var_type: _ } => {

            let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
            compile_context.builder.position_at_end(last_main_bb);

            compile_var_decl(compile_context, ast_node, *name, *val, *body, true);
        },
        ASTNode::TopLevel { nodes } => {
            // placeholder for imports (TODO ?)
            for &n in nodes {
                compile_top_level_node(compile_context, n);
            }
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

fn run_passes_on(module: &Module, target_machine : &TargetMachine, opt_level : OptimizationLevel) {
    let passes_str = format!("default<O{}>", opt_level as u8);
    module.run_passes(&passes_str, target_machine, PassBuilderOptions::create()).unwrap();
}

// TODO : instead install file in filesystem ?
const STD_C_CONTENT: &str = include_str!("../std.c");

fn link_exe(filename_out : &Path, bitcode_file : &Path, opt_level : OptimizationLevel, disable_gc : bool, enable_sanitizer : bool){
    // use cc ?
    // TODO : use lld (https://github.com/mun-lang/lld-rs) for linking instead ?
    // TODO : use libclang ? (clang-rs ? https://github.com/llvm/llvm-project/blob/main/clang/tools/driver/cc1_main.cpp#L85 ?)

    let out_std_path = pathbuf![&std::env::temp_dir(), "std.bc"];
    let out_std_path_str = out_std_path.as_os_str();

    // TODO : pass optimization level to this function

    let mut clang_std = Command::new("clang");
    clang_std.arg("-x").arg("c").arg("-emit-llvm").arg("-O3").arg("-c");

    if !disable_gc {
        clang_std.arg("-D_GC_");
    }
    
    let mut clang_std = clang_std.arg("-").arg("-o").arg(out_std_path_str).stdin(Stdio::piped()).spawn().expect("compiling std failed");
    clang_std.stdin.as_mut().unwrap().write_all(STD_C_CONTENT.as_bytes()).unwrap();
    clang_std.wait().unwrap();

    

    let mut link_cmd = Command::new("clang");

    if !matches!(opt_level, OptimizationLevel::None) {
        link_cmd.arg("-flto");
    }

    if !disable_gc {
        link_cmd.arg("-lgc");
    }

    if enable_sanitizer {
        link_cmd.arg("-fsanitize=address");
    }

    if !link_cmd.arg("-lm").arg("-o").arg(filename_out).arg(out_std_path_str).arg(bitcode_file).spawn().expect("linker failed").wait().unwrap().success() {
        return;
    }
    std::fs::remove_file(&out_std_path).expect("Couldn't delete std bitcode file");
}

// TODO : pass all the args after optimization level as a struct named OptionalArgs
pub fn compile(frontend_output : FrontendOutput, rustaml_context: &mut RustamlContext, filename : &Path, filename_out : Option<&Path>, optimization_level : u8, keep_temp : bool, disable_gc : bool, enable_sanitizer : bool) {
    let context = Context::create();
    let builder = context.create_builder();

    let filename_str = filename.as_os_str().to_str().expect("not UTF-8 filename");
    let module = context.create_module(filename_str);

    Target::initialize_all(&InitializationConfig::default());
    let target_triple = TargetMachine::get_default_triple();

    let target = Target::from_triple(&target_triple).unwrap();

    let optimization_level = match optimization_level {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        3 => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::Default,
    };

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
        typeinfos: frontend_output.type_infos,
        functions: FxHashMap::default(),
        main_function,
        var_vals: FxHashMap::default(),
        internal_functions,
        external_symbols_declared: FxHashSet::default(),
        global_strs: FxHashMap::default(),
    };

    let top_level_nodes = match frontend_output.ast.get(&rustaml_context.ast_pool) {
        ASTNode::TopLevel { nodes } => nodes,
        _ => unreachable!(),
    };

    for n in top_level_nodes {
        compile_top_level_node(&mut compile_context, *n);
    }

    let last_main_bb = compile_context.main_function.get_last_basic_block().unwrap();
    compile_context.builder.position_at_end(last_main_bb);
    compile_context.builder.build_return(Some(&compile_context.context.i32_type().const_int(0, false))).unwrap();

    let filename_without_ext = filename.file_stem().unwrap().to_str().expect("not UTF-8 filename").to_owned();

    #[cfg(feature = "debug-llvm")]{}
    compile_context.module.verify().or_else(|e| -> Result<_, LLVMString> { 
        compile_context.module.print_to_file(filename_without_ext.clone() + "_error.ll").unwrap();
        panic!("LLVM ERROR {}", e.to_string()) 
    }).unwrap();

    let temp_path = if keep_temp {
        Path::new(".").to_owned() 
    } else { 
        std::env::temp_dir()
    };

    let filename_with_hash = if keep_temp {
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
    
    // TODO : readd this for optimizations
    run_passes_on(compile_context.module, &target_machine, optimization_level);
    
    if keep_temp {
        let temp_path_ir = pathbuf![&temp_path, &format!("{}.ll", &filename_with_hash)];
        compile_context.module.print_to_file(&temp_path_ir).expect("Couldn't write llvm ir file");
    }
    
    compile_context.module.write_bitcode_to_path(&temp_path_bitcode);


    if let Some(f_out) = filename_out {
        link_exe(f_out,  &temp_path_bitcode, optimization_level, disable_gc, enable_sanitizer);
        if !keep_temp {
            std::fs::remove_file(temp_path_bitcode).expect("Couldn't delete bitcode file");
        }
    }

}