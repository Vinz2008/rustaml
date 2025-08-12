/*use std::ops::Range;

use debug_with_context::DebugWrapContext;

use crate::{ast::{ASTNode, ASTRef, Pattern, Type}, debug_println, lexer::Operator, string_intern::StringRef, types::{get_function_type, TypeContext, _resolve_types}};

fn infer_var_type_pattern(type_context : &mut TypeContext, pattern: &Pattern, body : ASTRef, range : &Range<usize>) -> Option<Type> {
    match pattern {
        Pattern::Float(_) => Some(Type::Float),
        Pattern::Integer(_) | Pattern::Range(_, _, _) => Some(Type::Integer),
        Pattern::String(_) => Some(Type::Str),
        Pattern::List(l) => { 
            let elem_type = match l.first() {
                Some(first) => { 
                    match infer_var_type_pattern(type_context, first, body, range) {
                        Some(t) => t,
                        None => Type::Any,
                    }
                },
                None => Type::Any,
            };
            Some(Type::List(Box::new(elem_type)))
            
        },
        Pattern::ListDestructure(head_name, _tail_name) => { 
            let element_type = match _infer_var_type(type_context, *head_name, body, range) {
                Some(t) => t,
                None => Type::Any,
            };
            Some(Type::List(Box::new(element_type))) 
        },
        Pattern::Underscore => None,
        Pattern::VarName(var_name) => {
            // get the type of the var name in the body
            // TODO : remove the comment
            //_infer_var_type(type_context, *var_name, body, range)
            infer_var_type(type_context, *var_name, body, range).ok()
        }
    }
}

#[derive(Debug)]
pub struct TypeInferenceErr {
    pub arg_name : Box<String>,
    pub range : Range<usize>,
}

impl TypeInferenceErr {
    fn new(arg_name : String, range : Range<usize>) -> TypeInferenceErr {
        TypeInferenceErr {
            arg_name: Box::new(arg_name),
            range
        }
    }
}


fn eliminate_anys(first_type : Type, other_type : Type) -> (Type, Type) {
    match (first_type, other_type) {
        (Type::Any, t) | (t, Type::Any) => (t.clone(), t.clone()),
        (f, o) => (f, o),
    }
}

fn merge_lists(first_type : Type, other_type : Type) -> (Type, Type) {
    match (first_type, other_type){
        (Type::List(t), Type::List(t2)) => {
            let (t, t2) = eliminate_anys(*t, *t2);
            let (t, t2) = merge_lists(t, t2);
            (Type::List(Box::new(t)), Type::List(Box::new(t2)))
        },
        (f, o) => (f, o), 
    }
}

pub fn merge_types(first_type : Option<Type>, other_type : Option<Type>) -> Option<Type> {
    let (first_type, other_type) = match (first_type, other_type) {
        (Some(f), Some(o)) => (f, o),
        (Some(s), None) | (None, Some(s)) => return Some(s),
        (None, None) => return None,
    };

    if first_type == other_type {
        return Some(first_type);
    }

    let (first_type, other_type) = eliminate_anys(first_type, other_type);
    let (first_type, other_type) = merge_lists(first_type, other_type);

    if first_type != other_type {
        // not special case and not the same as other deduced type -> mismatched types
        panic!("Mismatched types {:?} and {:?}", &first_type, &other_type);
    }
    
    Some(first_type)
}

macro_rules! merge_types {
    ($first:expr, $second:expr $(, $rest:expr)+) => {
        merge_types($first, merge_types!($second $(, $rest)+))
    };
    ($a:expr, $b:expr) => {
        merge_types($a, $b)
    };
}

// TODO : return a result with a real error ?
// TODO : split this function into subfunctions
fn _infer_var_type(type_context : &mut TypeContext, var_name: StringRef, node: ASTRef, range: &Range<usize>) -> Option<Type> {
    let n = node.get(&type_context.rustaml_context.ast_pool).clone();
    let t = match n {
        ASTNode::TopLevel { nodes } => {
            for n in nodes {
                let type_inferred = _infer_var_type(type_context, var_name, n, range);
                if type_inferred.is_some() {
                    return type_inferred;
                }
            }
            None
        },
        ASTNode::FunctionDefinition { name, args: _, body, return_type: _ } => {
            _infer_var_type(type_context, name, body, range)
        }
        ASTNode::VarDecl { name, val, body, var_type : _ } => {
            // TODO : use var type ?
            let val_type_inferred = _infer_var_type(type_context, name, val, range);

            let body_type = if let Some(b) = body {
                _infer_var_type(type_context, name, b, range)
            } else { 
                None 
            };

            merge_types(val_type_inferred, body_type)
        },
        ASTNode::VarUse { name: _ } => None, // no infos on type in var use
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            let cond_type_inferred = _infer_var_type(type_context, var_name, cond_expr, range);
            let then_type_inferred = _infer_var_type(type_context, var_name, then_body, range);
            let else_type_inferred = _infer_var_type(type_context, var_name, else_body, range);
            merge_types!(cond_type_inferred, then_type_inferred, else_type_inferred)
        },
        ASTNode::MatchExpr { matched_expr, patterns } => {
            let matched_expr_type_inferred = _infer_var_type(type_context, var_name, matched_expr, range);
            let mut pattern_type_inferred = None;
            if matches!(matched_expr.get(&type_context.rustaml_context.ast_pool), ASTNode::VarUse { name: var_use_name } if *var_use_name == var_name){
                for pattern in &patterns {
                    let pattern_type = infer_var_type_pattern(type_context, &pattern.0, pattern.1, range);
                    println!("pattern_type (searching {:?}) : {:?}", DebugWrapContext::new(&var_name, type_context.rustaml_context), pattern_type);
                    pattern_type_inferred = merge_types(pattern_type_inferred, pattern_type);
                }
                println!("PATTERN TYPE INFERRED for {:?} : {:?}", DebugWrapContext::new(&var_name, type_context.rustaml_context), pattern_type_inferred);
                /*if let Some(p) = &pattern_type_inferred && !matches!(p, Type::Any) {
                    panic!();
                }*/
            }

            

            let mut pattern_body_type_inferred = None;

            for pattern in &patterns {
                let pattern_body_type = _infer_var_type(type_context, var_name, pattern.1, range);
                pattern_body_type_inferred = merge_types(pattern_body_type_inferred, pattern_body_type);
            }

            merge_types!(matched_expr_type_inferred, pattern_type_inferred, pattern_body_type_inferred)
        }
        ASTNode::Integer { nb: _ } => None,
        ASTNode::Float { nb: _ } => None,
        ASTNode::String { str: _ } => None,
        ASTNode::Boolean { b: _ } => None,
        ASTNode::List { list: _ } => None,
        ASTNode::Unit => None,
        ASTNode::BinaryOp { op, lhs, rhs } => {
            let is_left_var = match lhs.get(&type_context.rustaml_context.ast_pool) {
                ASTNode::VarUse { name } => *name == var_name, 
                _ => false,
            };

            let is_right_var = match rhs.get(&type_context.rustaml_context.ast_pool) {
                ASTNode::VarUse { name } => *name == var_name, 
                _ => false,
            };
            debug_println!(type_context.rustaml_context.is_debug_print, "is_left_var = {}", is_left_var);

            //dbg!(is_left_var);
            debug_println!(type_context.rustaml_context.is_debug_print, "is_right_var = {}", is_right_var);
            //dbg!(is_right_var);

            let t = if is_left_var || is_right_var {
                let other_operand_type = if is_left_var {
                    _resolve_types(type_context, rhs).ok()? // TODO : improve this error handling
                    //rhs.get_type(&type_context.rustaml_context.ast_pool)
                } else {
                    _resolve_types(type_context, lhs).ok()?
                };
                debug_println!(type_context.rustaml_context.is_debug_print, "other_operand_type : {:#?}", &other_operand_type);
                //dbg!(&other_operand_type);
                let operand_type = op.get_operand_type(is_left_var, &other_operand_type);
                debug_println!(type_context.rustaml_context.is_debug_print, "operand_type : {:#?}", &operand_type);
                //dbg!(&operand_type);
                debug_println!(type_context.rustaml_context.is_debug_print, "other_operand_type : {:#?}", &other_operand_type);
                //dbg!(&other_operand_type);
                // TODO : create prinln_context for these ?
                debug_println!(type_context.rustaml_context.is_debug_print, "var_name = {:#?}", DebugWrapContext::new(&var_name, type_context.rustaml_context));
                debug_println!(type_context.rustaml_context.is_debug_print, "node = {:#?}", DebugWrapContext::new(&node, type_context.rustaml_context));
                dbg!(Some(operand_type))
            } else {
                let lhs_inferred = _infer_var_type(type_context, var_name, lhs, range);
                let rhs_inferred = _infer_var_type(type_context, var_name, rhs, range); 
                merge_types(lhs_inferred, rhs_inferred)
            };

            /*if var_name.get_str(&type_context.rustaml_context.str_interner) == "s2_matched" && matches!(op, Operator::IsEqual){
                panic!("t : {:?}", t);
            }*/
            t
        },
        ASTNode::FunctionCall { name: function_name, args } => {
            let function_type = get_function_type(type_context, function_name).ok(); // replace this a better error handling
            // TODO : prevent problem with recursive function call ?
            match function_type {
                Some(Type::Function(a, _, _)) => {
                    for (arg, arg_type) in args.iter().zip(a) {
                        match arg.get(&type_context.rustaml_context.ast_pool) {
                            ASTNode::VarUse { name } if *name == var_name => {
                                return Some(arg_type.clone())
                            },
                            _ => {
                                let inferred_arg_type = _infer_var_type(type_context, var_name, *arg, range);
                                if inferred_arg_type.is_some() {
                                    return inferred_arg_type;
                                }
                            }
                        }
                    }
                    None
                },
                _ => None,
            }
        },

    };

    type_context.rustaml_context.dump_inference.borrow_mut().add_type_found(var_name, node, &t);

    t
}

// TODO : make this infallible (inference only gives infos, no need to crash if not found, only if they really need to be used we crash)
fn infer_var_type(type_context : &mut TypeContext, var_name: StringRef, node: ASTRef, range: &Range<usize>) -> Result<Type, TypeInferenceErr> {
    match _infer_var_type(type_context, var_name, node, range) {
        Some(t) => {
            type_context.rustaml_context.dump_inference.borrow_mut().add_var_inferred(var_name, t.clone());
            Ok(t)
        },
        None => Err(TypeInferenceErr::new(var_name.get_str(&type_context.rustaml_context.str_interner).to_owned(), range.clone()))
    }
}*/