use std::{cmp::max, ops::RangeInclusive};

use inkwell::{basic_block::BasicBlock, types::{AnyTypeEnum, BasicTypeEnum}, values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, IntValue, PointerValue}, AddressSpace, FloatPredicate, IntPredicate};

use crate::{ast::{ASTRef, Pattern, PatternRef, Type}, compiler::{compile_expr, compiler_utils::{codegen_lang_runtime_error, create_br_conditional, create_br_unconditional, create_string, create_var, get_current_function, get_llvm_type, get_void_val, load_list_tail, load_list_val, move_bb_after_current}, debuginfo::get_debug_loc, CompileContext}};

// TODO : when will be added or patterns (TODO) (for ex : match a with | [1, 2, 3] | [2, 3, 4]) I can create a more optimized version when matching multiple lists by creating a decision tree in the compiled program
fn compile_short_circuiting_match_static_list<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, list_val : PointerValue<'llvm_ctx>, pattern_list : &[PatternRef], elem_type : &Type) -> IntValue<'llvm_ctx>{
    if pattern_list.is_empty(){
        return compile_context.builder.build_is_null(list_val, "match_list_empty").unwrap();
    } else {
        let this_function = get_current_function(compile_context.builder);
        let bb_list = (0..pattern_list.len()).map(|_|{
            (compile_context.context.append_basic_block(this_function, "is_null") ,compile_context.context.append_basic_block(this_function, "has_matched_element"))
        }).collect::<Vec<_>>();

        let is_null_last_bb = compile_context.context.append_basic_block(this_function, "is_null_last");
        let has_matched_everything_bb = compile_context.context.append_basic_block(this_function, "has_matched_everything");

        let after_bb = compile_context.context.append_basic_block(this_function, "after_bb");

        let mut current_node = list_val;

        let mut is_first = true;

        for (bb_idx, p) in pattern_list.iter().enumerate() {
            let (bb_is_null, bb_has_matched, ) = bb_list[bb_idx];

            if is_first {
                create_br_unconditional(compile_context, bb_is_null);
            }

            compile_context.builder.position_at_end(bb_is_null);

            if is_first {
                is_first = false;
            } else {
                current_node = load_list_tail(compile_context, current_node);
            }

            
            let is_null = compile_context.builder.build_is_null(current_node, "match_list_empty").unwrap();
            create_br_conditional(compile_context, is_null, after_bb, bb_has_matched);

            compile_context.builder.position_at_end(bb_has_matched);
            let current_node_val = load_list_val(compile_context, elem_type, current_node).as_any_value_enum();
            let has_e_matched = compile_pattern_match_bool_val(compile_context, *p, current_node_val, elem_type);
            
            let next_bb = if let Some(bb) = bb_list.get(bb_idx+1) {
                bb.0
            } else {
                is_null_last_bb
            };

            create_br_conditional(compile_context, has_e_matched, next_bb, after_bb);
        }

        compile_context.builder.position_at_end(is_null_last_bb);
        let next_that_should_be_null = load_list_tail(compile_context, current_node);
        let is_null = compile_context.builder.build_is_null(next_that_should_be_null, "match_list_empty").unwrap();
        create_br_conditional(compile_context, is_null, has_matched_everything_bb, after_bb);

        compile_context.builder.position_at_end(has_matched_everything_bb);
        create_br_unconditional(compile_context, after_bb);

        compile_context.builder.position_at_end(after_bb);
        let phi_node = compile_context.builder.build_phi(compile_context.context.bool_type(), "phi_match_static_list").unwrap();
        let mut incoming_phi : Vec<(&dyn BasicValue<'_>, inkwell::basic_block::BasicBlock<'_>)> = Vec::new();
        let const_false = compile_context.context.bool_type().const_int(false as u64, false);
        for (bb_is_null, bb_has_matched) in bb_list.iter() {
            incoming_phi.push((&const_false, *bb_is_null));
            incoming_phi.push((&const_false, *bb_has_matched));
        }
        incoming_phi.push((&const_false, is_null_last_bb));
        let const_true = compile_context.context.bool_type().const_int(true as u64, false);
        incoming_phi.push((&const_true, has_matched_everything_bb));

        phi_node.add_incoming(&incoming_phi);

        phi_node.as_basic_value().into_int_value()
    }
}

fn compile_pattern_match_bool_val<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : PatternRef, matched_val : AnyValueEnum<'llvm_ctx>, matched_expr_type : &Type) -> IntValue<'llvm_ctx>{
    match pattern.get(&compile_context.rustaml_context.pattern_pool).clone() {
        Pattern::Integer(i) => compile_context.builder.build_int_compare(inkwell::IntPredicate::EQ, matched_val.try_into().unwrap_or_else(|_| panic!("not an int value : {:?}", matched_val)), compile_context.context.i64_type().const_int(i as u64, false), "match_int_cmp").unwrap(),
        Pattern::Float(f) => compile_context.builder.build_float_compare(FloatPredicate::OEQ, matched_val.into_float_value(), compile_context.context.f64_type().const_float(f), "match_float_cmp").unwrap(),
        Pattern::Bool(b) => compile_context.builder.build_int_compare(inkwell::IntPredicate::EQ, matched_val.into_int_value(), compile_context.context.bool_type().const_int(b as u64, false), "match_bool_cmp").unwrap(),
        Pattern::Range(lower, upper, inclusivity) => {
            let (lower_predicate, upper_predicate) = if inclusivity {
                (IntPredicate::SLE, IntPredicate::SGE)
            }  else {
                (IntPredicate::SLE, IntPredicate::SGT)
            };
            // TODO : replace with compile function like below ?
            let lower_cmp = compile_context.builder.build_int_compare(lower_predicate, matched_val.try_into().unwrap(), compile_context.context.i64_type().const_int(upper as u64, false), "match_int_cmp_range_lower").unwrap();
            let upper_cmp = compile_context.builder.build_int_compare(upper_predicate, matched_val.try_into().unwrap(), compile_context.context.i64_type().const_int(lower as u64, false), "match_int_cmp_range_upper").unwrap();
            
            // TODO : instead of creating a and, short circuit this with multiple branches ? (return a vec with the branches that need to be made ?)
            let combined_bool_val = compile_context.builder.build_and(lower_cmp, upper_cmp, "match_range_and").unwrap();
            combined_bool_val
        }
        Pattern::VarName(_) | Pattern::Underscore => compile_context.context.bool_type().const_int(true as u64, false),
        Pattern::List(pattern_list) => {
            
            let elem_type = match matched_expr_type {
                Type::List(e) => *e.to_owned(),
                _ => unreachable!(),   
            };

            compile_short_circuiting_match_static_list(compile_context, matched_val.into_pointer_value(), &pattern_list, &elem_type)
        },
        Pattern::String(s) => {
            let str_cmp_fun = compile_context.get_internal_function("__str_cmp");
            let pattern_str_val = create_string(compile_context, &s.get_str(&compile_context.rustaml_context.str_interner).to_owned());

            let args = vec![pattern_str_val.into(), matched_val.try_into().unwrap()];
            compile_context.builder.build_call(str_cmp_fun, &args, "pattern_match_str_cmp").unwrap().as_any_value_enum().into_int_value()
        },
        Pattern::ListDestructure(_, _) => compile_context.builder.build_int_compare(IntPredicate::NE, matched_val.into_pointer_value(), compile_context.context.ptr_type(AddressSpace::default()).const_null(), "cmp_destructure_not_empty").unwrap(),
        //p => panic!("unknown pattern {:?}", DebugWrapContext::new(p, compile_context.rustaml_context)),
    }
}


// init vars, etc
fn compile_pattern_match_prologue<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : PatternRef, matched_val : AnyValueEnum<'llvm_ctx>, matched_val_type : &Type){
    match pattern.get(&compile_context.rustaml_context.pattern_pool).clone() {
        Pattern::VarName(n) => {
            let matched_val_type_llvm = get_llvm_type(compile_context, matched_val_type);
            create_var(compile_context, n, matched_val, matched_val_type_llvm);
        }
        Pattern::ListDestructure(head, tail) => {
            let element_type = match matched_val_type {
                Type::List(e) => e.as_ref(),
                _ => unreachable!(),
            };

            let element_type_llvm = get_llvm_type(compile_context, element_type);
            let matched_val_list = matched_val.into_pointer_value();
            let head_val = load_list_val(compile_context, element_type, matched_val_list);
            create_var(compile_context, head, head_val.into(), element_type_llvm);
            let tail_val = load_list_tail(compile_context, matched_val_list);
            compile_pattern_match_prologue(compile_context, tail, tail_val.into(), matched_val_type);
        }
       _ => {}
    }
}

fn compile_pattern_match<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, pattern : PatternRef, matched_val : AnyValueEnum<'llvm_ctx>, matched_val_type : &Type, bb: BasicBlock<'llvm_ctx>, else_bb : BasicBlock<'llvm_ctx>){
    let bool_val = compile_pattern_match_bool_val(compile_context, pattern, matched_val, matched_val_type);

    create_br_conditional(compile_context, bool_val, bb, else_bb);
}

// for analyzing the ranges of match (make it smarter ?)
// TODO : use this function to check in the AST to have a warning for non exhaustive match
fn match_is_all_range(compile_context: &CompileContext<'_, '_, '_>, matched_val_type : &Type, patterns : &[(PatternRef, ASTRef)]) -> bool {
    match matched_val_type {
        Type::Bool => {
            let has_true = patterns.iter().any(|e| matches!(e.0.get(&compile_context.rustaml_context.pattern_pool), Pattern::Bool(true)));
            let has_false = patterns.iter().any(|e| matches!(e.0.get(&compile_context.rustaml_context.pattern_pool), Pattern::Bool(false)));
            has_true && has_false
        },
        Type::Integer => {
            let mut ranges : Vec<RangeInclusive<i64>> = Vec::new();

            for (p, _) in patterns {
                match p.get(&compile_context.rustaml_context.pattern_pool) {
                    Pattern::Integer(nb) => ranges.push(*nb..=*nb),
                    Pattern::Range(start, end, inclusivity) => {
                        dbg!((start, end, inclusivity));
                        if *inclusivity {
                            ranges.push(*start..=*end);
                        } else {
                            let end_exclusive = max(end-1, *start);
                            ranges.push(*start..=end_exclusive);
                        }
                    },
                    _ => {}
                }
            }

            let mut merged_range = ranges.get(0).cloned();

            ranges.sort_by_key(|e| *e.start());

            for range in ranges.into_iter().skip(1){

                if let Some(merged_range) = &mut merged_range {
                    if range.start() < merged_range.start(){
                        *merged_range = *range.start()..=*merged_range.end();
                    }

                    if range.end() > merged_range.end(){
                        *merged_range = *merged_range.start()..=*range.end();
                    }
                } else {
                    merged_range = Some(range);
                }
            }

            merged_range == Some(i64::MIN..=i64::MAX)
        }
        _ => false, // TODO
    }
}

fn match_has_enough_fallback_switch(compile_context: &CompileContext<'_, '_, '_>, matched_val_type : &Type, patterns : &[(PatternRef, ASTRef)]) -> bool {
    match_is_all_range(compile_context, matched_val_type, patterns) 
        ||  patterns.iter().filter(|(p, _)| matches!(p.get(&compile_context.rustaml_context.pattern_pool), Pattern::VarName(_) | Pattern::Underscore)).count() == 1
}

fn match_can_use_switch(compile_context: &CompileContext<'_, '_, '_>, matched_val_type : &Type, patterns : &[(PatternRef, ASTRef)]) -> bool {
    
    matches!(matched_val_type, Type::Integer | Type::Bool) // TODO : what other types match ? 
    // TODO : when guard are added, verify that there is no guard
        && patterns.iter().all(|(p, _)| matches!(p.get(&compile_context.rustaml_context.pattern_pool), Pattern::Integer(_) | Pattern::Bool(_) | Pattern::VarName(_) | Pattern::Underscore))
        && match_has_enough_fallback_switch(compile_context, matched_val_type, patterns)
}

fn match_switch_is_fallback(compile_context: &CompileContext<'_, '_, '_>, p : PatternRef) -> bool {
    matches!(p.get(&compile_context.rustaml_context.pattern_pool), Pattern::VarName(_) | Pattern::Underscore)
}

fn compile_match_switch<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, matched_val : AnyValueEnum<'llvm_ctx>, match_type_ret_llvm : AnyTypeEnum<'llvm_ctx>, matched_val_type : &Type, patterns : &[(PatternRef, ASTRef)]) -> AnyValueEnum<'llvm_ctx> {
    let function = get_current_function(compile_context.builder);
    
    let mut match_bbs = Vec::new();
    let mut fallback_bb = None;
    let mut normal_patterns = Vec::new();
    let mut fallback = None;
    for (p, a) in patterns {
        if match_switch_is_fallback(compile_context, *p){
            fallback = Some((*p, *a));
            fallback_bb = Some(compile_context.context.append_basic_block(function, "fallback_switch"));
        } else {
            let match_bb = compile_context.context.append_basic_block(function, "match_case");
            match_bbs.push(match_bb);

            normal_patterns.push((*p, *a));
        }
        
    }

    let fallback_bb = fallback_bb.unwrap_or_else(|| compile_context.context.append_basic_block(function, "fallback_switch"));

    let mut cases= Vec::new();

    for ((p, _a), bb) in normal_patterns.iter().zip(&match_bbs) {
        match p.get(&compile_context.rustaml_context.pattern_pool){
            Pattern::Integer(i) => {
                let int_val = compile_context.context.i64_type().const_int(*i as u64 , false);
                cases.push((int_val, *bb))
            },
            Pattern::Bool(b) => {
                let bool_val = compile_context.context.bool_type().const_int(*b as u64 , false);
                cases.push((bool_val, *bb))
            }
            _ => unreachable!(),
        }
    }

    compile_context.builder.build_switch(matched_val.into_int_value(), fallback_bb, &cases).unwrap();

    let after_match = compile_context.context.append_basic_block(function, "after_match");

    let mut pattern_vals = Vec::new();
    let mut match_phi_bbs = Vec::new();
    let mut has_br_bb_list = Vec::new();

    for ((_pattern, pattern_body), pattern_bb) in normal_patterns.iter().zip(&match_bbs) {
        move_bb_after_current(compile_context, *pattern_bb);
        compile_context.builder.position_at_end(*pattern_bb);
        let pattern_val = compile_expr(compile_context, *pattern_body);
        pattern_vals.push(pattern_val);
        match_phi_bbs.push(compile_context.builder.get_insert_block().unwrap());
        let has_br = create_br_unconditional(compile_context, after_match);
        has_br_bb_list.push(has_br);
    }

    move_bb_after_current(compile_context, fallback_bb);
    compile_context.builder.position_at_end(fallback_bb);

    let fallback_val= if let Some(fallback) = fallback {
        compile_pattern_match_prologue(compile_context, fallback.0, matched_val, matched_val_type);
        let fallback_val = compile_expr(compile_context, fallback.1);
        fallback_val
    } else {
        // no need for fallback because all the range is already used (it was already checked in match_can_use_switch)
        // just generate an unreachable
        compile_context.builder.build_unreachable().unwrap();
        get_void_val(compile_context.context)
    };

    let after_fallback_bb = compile_context.builder.get_insert_block().unwrap();

    let has_br_fallback = create_br_unconditional(compile_context, after_match);
    


    move_bb_after_current(compile_context, after_match);
    compile_context.builder.position_at_end(after_match);
    let phi_node = compile_context.builder.build_phi(TryInto::<BasicTypeEnum>::try_into(match_type_ret_llvm).unwrap(), "match_phi").unwrap();

    let mut incoming_phi = Vec::new();
    for ((val, bb), has_br) in pattern_vals.iter().zip(&match_phi_bbs).zip(has_br_bb_list) {
        if has_br {
            let basic_val = TryInto::<BasicValueEnum>::try_into(*val).unwrap();
            incoming_phi.push((basic_val, *bb));
        }
    }

    if has_br_fallback {
        let fallback_basic_val = TryInto::<BasicValueEnum>::try_into(fallback_val).unwrap();
        incoming_phi.push((fallback_basic_val, after_fallback_bb));
    }

    let incoming_phi = incoming_phi.iter().map(|(val, bb)| (val as &dyn BasicValue, *bb)).collect::<Vec<_>>();

    phi_node.add_incoming(&incoming_phi);
    
    phi_node.as_any_value_enum()
}

// TODO : test nested matchs for problem with bb placement (use move_bb_after_current ?)
pub fn compile_match<'llvm_ctx>(compile_context: &mut CompileContext<'_, '_, 'llvm_ctx>, match_node : ASTRef, matched_expr : ASTRef, patterns : &[(PatternRef, ASTRef)]) -> AnyValueEnum<'llvm_ctx> {
    
    let matched_val = compile_expr(compile_context, matched_expr);
    //let matched_val_type = matched_expr.get(&compile_context.rustaml_context.ast_pool).get_type(compile_context.rustaml_context, &compile_context.var_types).unwrap();
    let matched_val_type = matched_expr.get_type(&compile_context.rustaml_context.ast_pool).clone();

    let match_type = match_node.get_type(&compile_context.rustaml_context.ast_pool).clone();
    let match_range = match_node.get_range(&compile_context.rustaml_context.ast_pool);
    let match_type_llvm = get_llvm_type(compile_context, &match_type);
    

    if match_can_use_switch(compile_context, &matched_val_type, patterns) {
        return compile_match_switch(compile_context, matched_val, match_type_llvm, &matched_val_type, patterns);
    }

    let function = get_current_function(compile_context.builder);
    

    let mut match_bbs = Vec::new();
    for _ in patterns {
        let match_bb = compile_context.context.append_basic_block(function, "match_case");
        let match_else_bb = compile_context.context.append_basic_block(function, "match_else");
        match_bbs.push((match_bb, match_else_bb));
    }

    let after_match = compile_context.context.append_basic_block(function, "after_match");

    let mut pattern_vals = Vec::new();

    let mut match_phi_bbs = Vec::new();

    let mut has_br_bb_list = Vec::new();

    for (pattern, pattern_bbs) in patterns.iter().zip(&match_bbs) {
        let (pattern, pattern_body) = pattern;
        let (pattern_bb, pattern_else_bb) = pattern_bbs;
        compile_pattern_match(compile_context, *pattern, matched_val, &matched_val_type, *pattern_bb, *pattern_else_bb);
        move_bb_after_current(compile_context, *pattern_bb);
        compile_context.builder.position_at_end(*pattern_bb);
        compile_pattern_match_prologue(compile_context, *pattern, matched_val, &matched_val_type);
        let pattern_body_val = compile_expr(compile_context, *pattern_body);
        //compile_pattern_match_epilogue(compile_context, pattern);
        match_phi_bbs.push(compile_context.builder.get_insert_block().unwrap());
        let has_br = create_br_unconditional(compile_context, after_match);
        has_br_bb_list.push(has_br);
        pattern_vals.push(pattern_body_val);
        move_bb_after_current(compile_context, *pattern_else_bb);
        compile_context.builder.position_at_end(*pattern_else_bb);
    }

    // TODO : add line number ? 
    let line_col = get_debug_loc(compile_context.rustaml_context.content.as_ref().unwrap(), match_range);
    codegen_lang_runtime_error(compile_context, "no match branch was found", line_col);

    move_bb_after_current(compile_context, after_match);
    compile_context.builder.position_at_end(after_match);
    let phi_node = compile_context.builder.build_phi(TryInto::<BasicTypeEnum>::try_into(match_type_llvm).unwrap(), "match_phi").unwrap();
    let mut incoming_phi = Vec::new();
    for ((val, bb), has_br) in pattern_vals.iter().zip(&match_phi_bbs).zip(has_br_bb_list) {
        if has_br {
            let basic_val = TryInto::<BasicValueEnum>::try_into(*val).unwrap();
            incoming_phi.push((basic_val, *bb));
        }
    }

    let incoming_phi = incoming_phi.iter().map(|(val, bb)| (val as &dyn BasicValue, *bb)).collect::<Vec<_>>();

    phi_node.add_incoming(&incoming_phi);
    
    phi_node.as_any_value_enum()
}