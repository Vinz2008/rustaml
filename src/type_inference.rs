use std::ops::Range;

use rustc_hash::FxHashMap;

use crate::{ast::{ASTNode, ASTRef, Pattern, Type}, debug::DebugWrapContext, rustaml::RustamlContext, string_intern::StringRef};


// TODO : problem with type inference and Any types
// there could be cases where we find a type with an Any, then we return the type even though a more precise type could be deduced in the body
// solution : get all the types found instead of hot plugging, and put them in a vec, and do a .max() on it (need to implement ord on types)

fn infer_var_type_pattern(rustaml_context : &RustamlContext, vars : &FxHashMap<StringRef, Type>, pattern: &Pattern, body : ASTRef, range : &Range<usize>) -> Option<Type> {
    // TODO
    match pattern {
        Pattern::Float(_) => Some(Type::Float),
        Pattern::Integer(_) | Pattern::Range(_, _, _) => Some(Type::Integer),
        Pattern::String(_) => Some(Type::Str),
        Pattern::List(l) => { 
            let elem_type = match l.first() {
                Some(first) => { 
                    match infer_var_type_pattern(rustaml_context, vars, first, body, range) {
                        Some(t) => t,
                        None => Type::Any,
                    }
                },
                None => Type::Any,
            };
            Some(Type::List(Box::new(elem_type)))
            
        },
        Pattern::ListDestructure(_head_name, _tail_name) => Some(Type::List(Box::new(Type::Any))),
        Pattern::Underscore => None,
        Pattern::VarName(var_name) => {
            // get the type of the var name in the body
            _infer_var_type(rustaml_context, vars, *var_name, body, range)
        }
    }
}

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

// TODO : return a result with a real error ?
// TODO : split this function into subfunctions
pub fn _infer_var_type(rustaml_context : &RustamlContext, vars: &FxHashMap<StringRef, Type>, var_name: StringRef, node: ASTRef, range: &Range<usize>) -> Option<Type> {
    match node.get(&rustaml_context.ast_pool) {
        ASTNode::TopLevel { nodes } => {
            for n in nodes {
                let type_inferred = _infer_var_type(rustaml_context, vars, var_name, *n, range);
                if type_inferred.is_some() {
                    return type_inferred;
                }
            }
            None
        },
        ASTNode::FunctionDefinition { name, args: _, body, return_type: _ } => {
            _infer_var_type(rustaml_context, vars, *name, *body, range)
        }
        ASTNode::VarDecl { name, val, body } => {
            let val_type_inferred = _infer_var_type(rustaml_context, vars, *name, *val, range);
            if val_type_inferred.is_some() {
                return val_type_inferred
            }

            if let Some(b) = body {
                return _infer_var_type(rustaml_context, vars, *name, *b, range);
            } 
            None
        },
        ASTNode::VarUse { name: _ } => None, // no infos on type in var use
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            let cond_type_inferred = _infer_var_type(rustaml_context, vars, var_name, *cond_expr, range);
            if cond_type_inferred.is_some(){
                return cond_type_inferred;
            }
            let then_type_inferred = _infer_var_type(rustaml_context, vars, var_name, *then_body, range);
            if then_type_inferred.is_some() {
                return then_type_inferred;
            }
            return _infer_var_type(rustaml_context, vars, var_name, *else_body, range);
        },
        ASTNode::MatchExpr { matched_expr, patterns } => {
            let matched_expr_type_inferred = _infer_var_type(rustaml_context, vars, var_name, *matched_expr, range);
            if matched_expr_type_inferred.is_some(){
                return matched_expr_type_inferred;
            }
            if matches!(matched_expr.get(&rustaml_context.ast_pool), ASTNode::VarUse { name: var_use_name } if *var_use_name == var_name){
                for pattern in patterns {
                    let pattern_type_inferred = infer_var_type_pattern(rustaml_context, vars, &pattern.0, pattern.1, range);
                    if pattern_type_inferred.is_some() {
                        return pattern_type_inferred;
                    }
                }
            }

            for pattern in patterns {
                let pattern_body_type_inferred = _infer_var_type(rustaml_context, vars, var_name, pattern.1, range);
                if pattern_body_type_inferred.is_some() {
                    return pattern_body_type_inferred;
                }
            }

            None
        }
        ASTNode::Integer { nb: _ } => None,
        ASTNode::Float { nb: _ } => None,
        ASTNode::String { str: _ } => None,
        ASTNode::Boolean { b: _ } => None,
        ASTNode::List { list: _ } => None,
        ASTNode::BinaryOp { op, lhs, rhs } => {
            let is_left_var = match lhs .get(&rustaml_context.ast_pool) {
                ASTNode::VarUse { name } => *name == var_name, 
                _ => false,
            };

            let is_right_var = match rhs.get(&rustaml_context.ast_pool) {
                ASTNode::VarUse { name } => *name == var_name, 
                _ => false,
            };

            // create 2 ifs for when implementing where operators are not the same on each side (for example :: in ocaml)

            // TODO : fix the case car there is e :: l with e an integer, it should detect l as a list of integer and not as a list of any

            println!("is_left_var = {}", is_left_var);

            //dbg!(is_left_var);
            println!("is_right_var = {}", is_right_var);
            //dbg!(is_right_var);

            if is_left_var || is_right_var {
                let other_operand_type = if is_left_var {
                    rhs.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)
                } else {
                    lhs.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)
                };
                println!("other_operand_type : {:#?}", &other_operand_type);
                //dbg!(&other_operand_type);
                let operand_type = op.get_operand_type(is_left_var, &other_operand_type);
                println!("operand_type : {:#?}", &operand_type);
                //dbg!(&operand_type);
                println!("other_operand_type : {:#?}", &other_operand_type);
                //dbg!(&other_operand_type);
                // TODO : create prinln_context for these ?
                println!("var_name = {:#?}", DebugWrapContext::new(&var_name, rustaml_context));
                println!("node = {:#?}", DebugWrapContext::new(&node, rustaml_context));
                Some(operand_type)
            } else {
                let lhs_inferred = _infer_var_type(rustaml_context, vars, var_name, *lhs, range);
                if lhs_inferred.is_some() {
                    return lhs_inferred;
                }
                let rhs_inferred = _infer_var_type(rustaml_context, vars, var_name, *rhs, range);
                if rhs_inferred.is_some() {
                    return rhs_inferred;
                }
                None
            }
        },
        ASTNode::FunctionCall { name: function_name, args } => {
            match vars.get(function_name) {
                Some(Type::Function(a, _)) => {
                    for (arg, arg_type) in args.iter().zip(a) {
                        match arg.get(&rustaml_context.ast_pool) {
                            ASTNode::VarUse { name } if *name == var_name => {
                                return Some(arg_type.clone())
                            },
                            _ => {
                                let inferred_arg_type = _infer_var_type(rustaml_context, vars, var_name, *arg, range);
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

    }
}

// TODO : make this infallible (inference only gives infos, no need to crash if not found, only if they really need to be used we crash)
pub fn infer_var_type(rustaml_context : &RustamlContext, vars: &FxHashMap<StringRef, Type>, var_name: StringRef, node: ASTRef, range: &Range<usize>) -> Result<Type, TypeInferenceErr> {
    match _infer_var_type(rustaml_context, vars, var_name, node, range) {
        Some(t) => Ok(t),
        None => Err(TypeInferenceErr::new(var_name.get_str(&rustaml_context.str_interner).to_owned(), range.clone()))
    }
}