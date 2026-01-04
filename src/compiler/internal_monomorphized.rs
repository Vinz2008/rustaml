use inkwell::{types::{AnyType, BasicType}, values::{AnyValue, BasicValueEnum, FunctionValue}, AddressSpace, IntPredicate};
use rustc_hash::FxHashMap;

use crate::{ast::Type, compiler::{compiler_utils::{any_val_to_metadata, as_val_in_list, create_entry_block_alloca, get_llvm_type, get_type_tag_val, load_list_tail, load_list_val, move_bb_after_current}, CompileContext}};

pub fn init_monomorphized_internal_fun<'llvm_ctx>() -> FxHashMap<&'static str, FxHashMap<(Type, Type), FunctionValue<'llvm_ctx>>> {
    let mut ret = FxHashMap::default();
    ret.insert("map", FxHashMap::default());
    ret.insert("filter", FxHashMap::default());
    ret
}

// this is the code that is tried to be recreated with this function

// struct ListNode {
//     struct ListNode* next;
//     Val val;
//     uint8_t tag;
// };

// // l is of type T1
// struct ListNode* map(struct ListNode* l, T2 (*f)(T1)){
//     struct ListNode* ret = NULL;
//     struct ListNode* current = l;
//     while (current != NULL){
//         ret = __list_node_append_back(ret, T2_tag, f((int64_t)current->val));
//         current = current->next;
//     }

//     return ret;
// }


// TODO : use a list builder with a preallocated buffer to improve cache locality

pub fn compile_monomorphized_map<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, elem_type : &Type, ret_elem_type : &Type) -> FunctionValue<'llvm_ctx> {
    if let Some(f) = compile_context.monomorphized_internal_fun.get("map").unwrap().get(&(elem_type.clone(), ret_elem_type.clone())){
        return *f;
    }

    let current_bb = compile_context.builder.get_insert_block().unwrap();

    let function_passed_type = Type::Function(Box::new([elem_type.clone()]), Box::new(ret_elem_type.clone()), false);
    let function_passed_type_llvm = get_llvm_type(compile_context, &function_passed_type).into_function_type();
    
    // TODO : if those clone are a perf problem, replace all these with Any if possible
    let map_type = Type::Function(Box::new([Type::List(Box::new(elem_type.clone())), function_passed_type]), Box::new(Type::List(Box::new(ret_elem_type.clone()))), false);
    
    let map_type_llvm = get_llvm_type(compile_context, &map_type).as_any_type_enum().into_function_type();
    let map_func_name = &format!("map {}->{}", elem_type, ret_elem_type);
    let function = compile_context.module.add_function(map_func_name, map_type_llvm, Some(inkwell::module::Linkage::Internal));
    
    let entry = compile_context.context.append_basic_block(function, "entry");
    compile_context.builder.position_at_end(entry);

    let list_arg = function.get_first_param().unwrap().into_pointer_value();
    let fun_arg = function.get_last_param().unwrap().into_pointer_value(); 

    let ptr_type = compile_context.context.ptr_type(AddressSpace::default());
    let ret_alloca = create_entry_block_alloca(compile_context, "ret", ptr_type.into());
    compile_context.builder.build_store(ret_alloca, ptr_type.const_null()).unwrap();


    let current_alloca = create_entry_block_alloca(compile_context, "current", ptr_type.into()); 
    compile_context.builder.build_store(current_alloca, list_arg).unwrap();

    let cond_bb = compile_context.context.append_basic_block(function, "cond");

    compile_context.builder.build_unconditional_branch(cond_bb).unwrap();


    let body_bb = compile_context.context.append_basic_block(function, "body");
    let after_bb = compile_context.context.append_basic_block(function, "after");

    compile_context.builder.position_at_end(cond_bb);

    let load_current = compile_context.builder.build_load(ptr_type.as_basic_type_enum(), current_alloca, "load_current").unwrap().into_pointer_value();

    let is_not_null =compile_context.builder.build_int_compare(IntPredicate::NE, load_current, ptr_type.const_null(), "cmp_null").unwrap();

    compile_context.builder.build_conditional_branch(is_not_null, body_bb, after_bb).unwrap();

    // while loop body starts
    compile_context.builder.position_at_end(body_bb);
    
    let list_node_append_fun = compile_context.get_internal_function("__list_node_append_back");

    let load_current = compile_context.builder.build_load(ptr_type.as_basic_type_enum(), current_alloca, "load_current").unwrap().into_pointer_value();
    let load_current_node_val = load_list_val(compile_context, elem_type, load_current).as_any_value_enum();

    let fun_args = vec![any_val_to_metadata(load_current_node_val)];

    let fun_arg_call = compile_context.builder.build_indirect_call(function_passed_type_llvm, fun_arg, &fun_args, "call_map_fun").unwrap().as_any_value_enum();
    let fun_arg_call = as_val_in_list(compile_context, fun_arg_call, ret_elem_type).as_any_value_enum();


    let load_ret = compile_context.builder.build_load(ptr_type.as_basic_type_enum(), ret_alloca, "load_ret").unwrap().as_any_value_enum();
    let ret_type_tag = get_type_tag_val(compile_context.context, ret_elem_type).as_any_value_enum();

    let list_append_args = vec![load_ret, ret_type_tag, fun_arg_call].into_iter().map(any_val_to_metadata).collect::<Vec<_>>();
    let list_appended = compile_context.builder.build_call(list_node_append_fun, &list_append_args, "call_list_append").unwrap().as_any_value_enum();

    compile_context.builder.build_store::<BasicValueEnum>(ret_alloca, list_appended.try_into().unwrap()).unwrap();

    
    let next_current= load_list_tail(compile_context, load_current);
    compile_context.builder.build_store(current_alloca, next_current).unwrap();


    compile_context.builder.build_unconditional_branch(cond_bb).unwrap(); // TODO : add !llvm.loop to br ? (like clang ?)

    // while loop body ends

    compile_context.builder.position_at_end(after_bb);


    let load_ret = compile_context.builder.build_load(ptr_type.as_basic_type_enum(), ret_alloca, "load_ret").unwrap();

    compile_context.builder.build_return(Some(&load_ret)).unwrap();

    compile_context.builder.position_at_end(current_bb);

    compile_context.monomorphized_internal_fun.get_mut("map").unwrap().insert((elem_type.clone(), ret_elem_type.clone()), function);

    function
}

// this is the code that is tried to be recreated with this function

// struct ListNode {
//     Val val;
//     struct ListNode* next;
// };

// // l is of type T1
// struct ListNode* map(struct ListNode* l, bool (*f)(T1)){
//     struct ListNode* ret = NULL;
//     struct ListNode* current = l;
//     while (current != NULL){
//         if (f(current->val)){
//             ret = __list_node_append_back(ret, T1_tag, current->val);
//         }
//         current = current->next;
//     }

//     return ret;
// }
pub fn compile_monomorphized_filter<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, elem_type : &Type) -> FunctionValue<'llvm_ctx> {
    if let Some(f) = compile_context.monomorphized_internal_fun.get("filter").unwrap().get(&(elem_type.clone(), Type::Bool)){
        return *f;
    }

    let current_bb = compile_context.builder.get_insert_block().unwrap();

    let function_passed_type = Type::Function(Box::new([elem_type.clone()]), Box::new(Type::Bool), false);
    let function_passed_type_llvm = get_llvm_type(compile_context, &function_passed_type).into_function_type();
    
    // TODO : if those clone are a perf problem, replace all these box with Any if possible
    let map_type = Type::Function(Box::new([Type::List(Box::new(elem_type.clone())), function_passed_type]), Box::new(Type::List(Box::new(Type::Bool))), false);
    
    let map_type_llvm = get_llvm_type(compile_context, &map_type).as_any_type_enum().into_function_type();
    let map_func_name = &format!("filter {}", elem_type);
    let function = compile_context.module.add_function(map_func_name, map_type_llvm, Some(inkwell::module::Linkage::Internal));
    
    let entry = compile_context.context.append_basic_block(function, "entry");
    compile_context.builder.position_at_end(entry);

    let list_arg = function.get_first_param().unwrap().into_pointer_value();
    let fun_arg = function.get_last_param().unwrap().into_pointer_value(); 

    let ptr_type = compile_context.context.ptr_type(AddressSpace::default());
    let ret_alloca = create_entry_block_alloca(compile_context, "ret", ptr_type.into());
    compile_context.builder.build_store(ret_alloca, ptr_type.const_null()).unwrap();


    let current_alloca = create_entry_block_alloca(compile_context, "current", ptr_type.into()); 
    compile_context.builder.build_store(current_alloca, list_arg).unwrap();

    let cond_bb = compile_context.context.append_basic_block(function, "cond");

    compile_context.builder.build_unconditional_branch(cond_bb).unwrap();


    let body_bb = compile_context.context.append_basic_block(function, "body");
    let after_bb = compile_context.context.append_basic_block(function, "after");

    compile_context.builder.position_at_end(cond_bb);

    let load_current = compile_context.builder.build_load(ptr_type.as_basic_type_enum(), current_alloca, "load_current").unwrap().into_pointer_value();

    let is_not_null =compile_context.builder.build_int_compare(IntPredicate::NE, load_current, ptr_type.const_null(), "cmp_null").unwrap();

    compile_context.builder.build_conditional_branch(is_not_null, body_bb, after_bb).unwrap();

    // while loop body starts 
    compile_context.builder.position_at_end(body_bb);
    
    let list_node_append_fun = compile_context.get_internal_function("__list_node_append_back");

    let load_current = compile_context.builder.build_load(ptr_type.as_basic_type_enum(), current_alloca, "load_current").unwrap().into_pointer_value();
    let load_current_node_val = load_list_val(compile_context, elem_type, load_current).as_any_value_enum();

    let fun_args = vec![any_val_to_metadata(load_current_node_val)];

    let filter_arg_call = compile_context.builder.build_indirect_call(function_passed_type_llvm, fun_arg, &fun_args, "call_map_fun").unwrap().as_any_value_enum().into_int_value();

    let if_bb = compile_context.context.append_basic_block(function, "if");
    let after_if_bb = compile_context.context.append_basic_block(function, "after_if");

    compile_context.builder.build_conditional_branch(filter_arg_call, if_bb, after_if_bb).unwrap();

    // start if
    compile_context.builder.position_at_end(if_bb);


    let load_ret = compile_context.builder.build_load(ptr_type.as_basic_type_enum(), ret_alloca, "load_ret").unwrap().as_any_value_enum();

    let elem_tag = get_type_tag_val(compile_context.context, elem_type).as_any_value_enum();
    let list_append_args = vec![load_ret, elem_tag, load_current_node_val].into_iter().map(any_val_to_metadata).collect::<Vec<_>>();
    let list_appended = compile_context.builder.build_call(list_node_append_fun, &list_append_args, "call_list_append").unwrap().as_any_value_enum();

    compile_context.builder.build_store::<BasicValueEnum>(ret_alloca, list_appended.try_into().unwrap()).unwrap();
    compile_context.builder.build_unconditional_branch(after_if_bb).unwrap();
    
    compile_context.builder.position_at_end(after_if_bb);
    let next_current= load_list_tail(compile_context, load_current);
    compile_context.builder.build_store(current_alloca, next_current).unwrap();


    compile_context.builder.build_unconditional_branch(cond_bb).unwrap(); // TODO : add !llvm.loop to br ? (like clang ?)

    // while loop body ends

    move_bb_after_current(compile_context, after_bb);
    compile_context.builder.position_at_end(after_bb);


    let load_ret = compile_context.builder.build_load(ptr_type.as_basic_type_enum(), ret_alloca, "load_ret").unwrap();

    compile_context.builder.build_return(Some(&load_ret)).unwrap();

    compile_context.builder.position_at_end(current_bb);

    compile_context.monomorphized_internal_fun.get_mut("filter").unwrap().insert((elem_type.clone(), Type::Bool), function);

    function
}


// TODO : add more functions