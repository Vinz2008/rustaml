use std::ops::Range;

use crate::ast::{ASTNode, Parser, Pattern, Type};


fn infer_var_type_pattern(name : &str, pattern: &Pattern) -> Result<Type, TypeInferenceErr> {
    // TODO
    
    todo!()
}

#[derive(Debug)]
pub enum TypeInferenceErrData {
    ArgTypeNotFound(Box<String>)
}

pub struct TypeInferenceErr {
    pub data : TypeInferenceErrData,
    pub range : Range<usize>,
}

impl TypeInferenceErr {
    fn new(data : TypeInferenceErrData, range : Range<usize>) -> TypeInferenceErr {
        TypeInferenceErr {
            data,
            range
        }
    }
}

// TODO : return a result with a real error ?
// TODO : split this function into subfunctions
pub fn infer_var_type(parser : &Parser, var_name: &str, node: &ASTNode, range: &Range<usize>) -> Result<Type, TypeInferenceErr> {
    match node {
        ASTNode::TopLevel { nodes } => {
            for n in nodes {
                let type_inferred = infer_var_type(parser, var_name, n, range);
                if type_inferred.is_ok() {
                    return type_inferred;
                }
            }
            Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(var_name.to_owned())), range.clone())) // TODO : doing allocations in every failed branch, maybe just return an option in the recusrsive function, then create a function "chapeau" which will return the err ? (will only work if there is only one typeInferenceError Type) 
        },
        ASTNode::FunctionDefinition { name, args: _, body, return_type: _ } => {
            infer_var_type(parser, name, body, range)
        }
        ASTNode::VarDecl { name, val, body } => {
            let val_type_inferred = infer_var_type(parser, name, val.as_ref(), range);
            if val_type_inferred.is_ok() {
                return val_type_inferred
            }

            if let Some(b) = body {
                return infer_var_type(parser, name, b, range);
            } 
            Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(name.to_owned())), range.clone()))
        },
        ASTNode::VarUse { name } => Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(name.to_owned())), range.clone())), // no infos on type in var use
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            let cond_type_inferred = infer_var_type(parser, var_name, cond_expr, range);
            if cond_type_inferred.is_ok(){
                return cond_type_inferred;
            }
            let then_type_inferred = infer_var_type(parser, var_name, then_body, range);
            if then_type_inferred.is_ok() {
                return then_type_inferred;
            }
            return infer_var_type(parser, var_name, else_body, range);
        },
        ASTNode::MatchExpr { matched_expr, patterns } => {
            let matched_expr_type_inferred = infer_var_type(parser, var_name, matched_expr, range);
            if matched_expr_type_inferred.is_ok(){
                return matched_expr_type_inferred;
            }

            for pattern in patterns {
                let pattern_type_inferred = infer_var_type_pattern(var_name, &pattern.0);
                if pattern_type_inferred.is_ok() {
                    return pattern_type_inferred;
                }

                let pattern_body_type_inferred = infer_var_type(parser, var_name, &pattern.1, range);
                if pattern_body_type_inferred.is_ok() {
                    return pattern_body_type_inferred;
                }
            }

            Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(var_name.to_owned())), range.clone()))
        }
        ASTNode::Integer { nb: _ } => Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(var_name.to_owned())), range.clone())),
        ASTNode::Float { nb: _ } => Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(var_name.to_owned())), range.clone())),
        ASTNode::String { str: _ } => Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(var_name.to_owned())), range.clone())),
        ASTNode::Boolean { b: _ } => Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(var_name.to_owned())), range.clone())),
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
                Ok(op_type)
            } else {
                Err(TypeInferenceErr::new(TypeInferenceErrData::ArgTypeNotFound(Box::new(var_name.to_owned())), range.clone()))
            }
        },
        ASTNode::FunctionCall { name: _, args: _ } => {
            todo!()
        },

    }
}