use std::ops::Range;

use crate::ast::{ASTNode, Parser, Pattern, Type};


fn infer_var_type_pattern(parser : &Parser, pattern: &Pattern, body : &ASTNode, range : &Range<usize>) -> Option<Type> {
    // TODO
    match pattern {
        Pattern::Float(_) => Some(Type::Float),
        Pattern::Integer(_) | Pattern::Range(_, _, _) => Some(Type::Integer),
        Pattern::String(_) => Some(Type::Str),
        Pattern::Underscore => None,
        Pattern::VarName(var_name) => {
            // get the type of the var name in the body
            _infer_var_type(parser, var_name, body, range)
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
pub fn _infer_var_type(parser : &Parser, var_name: &str, node: &ASTNode, range: &Range<usize>) -> Option<Type> {
    match node {
        ASTNode::TopLevel { nodes } => {
            for n in nodes {
                let type_inferred = _infer_var_type(parser, var_name, n, range);
                if type_inferred.is_some() {
                    return type_inferred;
                }
            }
            None
        },
        ASTNode::FunctionDefinition { name, args: _, body, return_type: _ } => {
            _infer_var_type(parser, name, body, range)
        }
        ASTNode::VarDecl { name, val, body } => {
            let val_type_inferred = _infer_var_type(parser, name, val.as_ref(), range);
            if val_type_inferred.is_some() {
                return val_type_inferred
            }

            if let Some(b) = body {
                return _infer_var_type(parser, name, b, range);
            } 
            None
        },
        ASTNode::VarUse { name } => None, // no infos on type in var use
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            let cond_type_inferred = _infer_var_type(parser, var_name, cond_expr, range);
            if cond_type_inferred.is_some(){
                return cond_type_inferred;
            }
            let then_type_inferred = _infer_var_type(parser, var_name, then_body, range);
            if then_type_inferred.is_some() {
                return then_type_inferred;
            }
            return _infer_var_type(parser, var_name, else_body, range);
        },
        ASTNode::MatchExpr { matched_expr, patterns } => {
            let matched_expr_type_inferred = _infer_var_type(parser, var_name, matched_expr, range);
            if matched_expr_type_inferred.is_some(){
                return matched_expr_type_inferred;
            }
            if matches!(matched_expr.as_ref(), ASTNode::VarUse { name: var_use_name } if var_use_name == var_name){
                for pattern in patterns {
                    let pattern_type_inferred = infer_var_type_pattern(parser, &pattern.0, &pattern.1, range);
                    if pattern_type_inferred.is_some() {
                        return pattern_type_inferred;
                    }
                }
            }

            for pattern in patterns {
                let pattern_body_type_inferred = _infer_var_type(parser, var_name, &pattern.1, range);
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
        ASTNode::BinaryOp { op, lhs, rhs } => {
            let is_left_var = match lhs.as_ref() {
                ASTNode::VarUse { name } => name == var_name, 
                _ => false,
            };

            let is_right_var = match rhs.as_ref() {
                ASTNode::VarUse { name } => name == var_name, 
                _ => false,
            };

            // create 2 ifs for when implementing where operators are not the same on each side (for example :: in ocaml)

            if is_left_var || is_right_var {
                let operand_type = op.get_operand_type();
                let op_type = match operand_type {
                    Some(op_type) => op_type,
                    None if is_left_var => rhs.get_type(parser),
                    None => lhs.get_type(parser),
                };
                Some(op_type)
            } else {
                None
            }
        },
        ASTNode::FunctionCall { name: function_name, args } => {
            match parser.vars.get(function_name) {
                Some(Type::Function(a, ret)) => {
                    for (arg, arg_type) in args.iter().zip(a) {
                        match arg {
                            ASTNode::VarUse { name } if name == var_name => {
                                return Some(arg_type.clone())
                            },
                            _ => {
                                let inferred_arg_type = _infer_var_type(parser, var_name, arg, range);
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

pub fn infer_var_type(parser : &Parser, var_name: &str, node: &ASTNode, range: &Range<usize>) -> Result<Type, TypeInferenceErr> {
    match _infer_var_type(parser, var_name, node, range) {
        Some(t) => Ok(t),
        None => Err(TypeInferenceErr::new(var_name.to_owned(), range.clone()))
    }
}