use std::{fs::File, io::Write, ops::Range, path::Path};

use rustc_hash::FxHashMap;

use crate::{ast::{ASTNode, ASTRef, Pattern, Type}, debug::DebugWrapContext, debug_println, rustaml::RustamlContext, string_intern::StringRef};


// TODO : put this in a type_inference_debug.rs ?
struct TypeFound {
    node : ASTRef,
    type_found : Type,
    var_name : StringRef,
}

impl TypeFound {
    fn new(node : ASTRef, type_found : Type, var_name : StringRef) -> TypeFound {
        TypeFound { node, type_found, var_name }
    }
}

struct DumpInferInner {
    vars_inferred : Vec<(StringRef, Type)>,
    types_found : Vec<TypeFound>, 
}

pub struct DumpInfer{ 
    inner : Option<DumpInferInner>, // TODO : add context (like the ast ref where the infer was done ?)
}


impl DumpInferInner {
    fn new() -> DumpInferInner {
        DumpInferInner {
            vars_inferred: Vec::new(),
            types_found:  Vec::new(),
        }
    }
}

impl DumpInfer {

    pub fn new(dump_inference : bool) -> DumpInfer {
        let inner = if dump_inference {
            Some(DumpInferInner::new())
        } else {
            None
        };
        DumpInfer { inner }
    }

    fn add_var_inferred(&mut self, var_name : StringRef, type_inferred : Type){
        if let Some(i) = &mut self.inner {
            i.vars_inferred.push((var_name, type_inferred));
        }
    }

    fn add_type_found(&mut self, var_name : StringRef, ast_node : ASTRef, t : &Option<Type>) {
        let t = match t {
            Some(t) => t,
            None => return,
        };
        
        if let Some(i) = &mut self.inner {
            i.types_found.push(TypeFound::new(ast_node, t.clone(), var_name))
        }
    }

    pub fn dump(&self, path : &Path, rustaml_context : &RustamlContext) -> std::io::Result<()>{
        if let Some(inner) = &self.inner {
            let mut file = File::create(path)?;

            for TypeFound { node: body, type_found, var_name } in &inner.types_found {
                writeln!(&mut file, "found type in {:?} for {} : {:?}", DebugWrapContext::new(body, rustaml_context), var_name.get_str(&rustaml_context.str_interner), type_found)?;
            }

            writeln!(&mut file)?;
            for (v_name, v_type) in &inner.vars_inferred {
                writeln!(&mut file, "{} : {:?}", v_name.get_str(&rustaml_context.str_interner), v_type)?;
            }
        }
    
        Ok(())
    }   
}

fn infer_var_type_pattern(rustaml_context : &RustamlContext, vars : &FxHashMap<StringRef, Type>, pattern: &Pattern, body : ASTRef, range : &Range<usize>) -> Option<Type> {
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
        Pattern::ListDestructure(head_name, _tail_name) => { 
            let element_type = match _infer_var_type(rustaml_context, vars, *head_name, body, range) {
                Some(t) => t,
                None => Type::Any,
            };
            Some(Type::List(Box::new(element_type))) 
        },
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


fn merge_types(first_type : Option<Type>, other_type : Option<Type>) -> Option<Type> {
    let (first_type, other_type) = match (first_type, other_type) {
        (Some(f), Some(o)) => (f, o),
        (Some(s), None) | (None, Some(s)) => return Some(s),
        (None, None) => return None,
    };

    if first_type == other_type {
        return Some(first_type);
    }

    match (&first_type, &other_type) {
        (Type::Any, t) | (t, Type::Any) => return Some(t.clone()),
        (Type::List(t), Type::List(other_t)) => {
            match (t.as_ref(), other_t.as_ref()) {
                (Type::Any, t) | (t, Type::Any) =>  {
                    return Some(Type::List(Box::new(t.clone())));
                },
                _ => {}
            }
        }
        _ => {}
    }

    // not special case and not the same as other deduced type -> mismatched types
    panic!("Mismatched types {:?} and {:?}", &first_type, &other_type);
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
pub fn _infer_var_type(rustaml_context : &RustamlContext, vars: &FxHashMap<StringRef, Type>, var_name: StringRef, node: ASTRef, range: &Range<usize>) -> Option<Type> {
    let t = match node.get(&rustaml_context.ast_pool) {
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
            /*if val_type_inferred.is_some() {
                return val_type_inferred
            }*/

            let body_type = if let Some(b) = body {
                _infer_var_type(rustaml_context, vars, *name, *b, range)
            } else { 
                None 
            };

            merge_types(val_type_inferred, body_type)
        },
        ASTNode::VarUse { name: _ } => None, // no infos on type in var use
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            let cond_type_inferred = _infer_var_type(rustaml_context, vars, var_name, *cond_expr, range);
            /*if cond_type_inferred.is_some(){
                return cond_type_inferred;
            }*/
            let then_type_inferred = _infer_var_type(rustaml_context, vars, var_name, *then_body, range);
            /*if then_type_inferred.is_some() {
                return then_type_inferred;
            }*/
            let else_type_inferred = _infer_var_type(rustaml_context, vars, var_name, *else_body, range);
            merge_types!(cond_type_inferred, then_type_inferred, else_type_inferred)
        },
        ASTNode::MatchExpr { matched_expr, patterns } => {
            let matched_expr_type_inferred = _infer_var_type(rustaml_context, vars, var_name, *matched_expr, range);
            /*if matched_expr_type_inferred.is_some(){
                return matched_expr_type_inferred;
            }*/
            let mut pattern_type_inferred = None;
            if matches!(matched_expr.get(&rustaml_context.ast_pool), ASTNode::VarUse { name: var_use_name } if *var_use_name == var_name){
                for pattern in patterns {
                    let pattern_type = infer_var_type_pattern(rustaml_context, vars, &pattern.0, pattern.1, range);
                    pattern_type_inferred = merge_types(pattern_type_inferred, pattern_type);
                    /*if pattern_type_inferred.is_some() {
                        return pattern_type_inferred;
                    }*/
                }
            }

            let mut pattern_body_type_inferred = None;

            for pattern in patterns {
                let pattern_body_type = _infer_var_type(rustaml_context, vars, var_name, pattern.1, range);
                pattern_body_type_inferred = merge_types(pattern_body_type_inferred, pattern_body_type);
                /*if pattern_body_type.is_some() {
                    return pattern_body_type;
                }*/
            }

            merge_types!(matched_expr_type_inferred, pattern_type_inferred, pattern_body_type_inferred)
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
            debug_println!(rustaml_context.is_debug_print, "is_left_var = {}", is_left_var);

            //dbg!(is_left_var);
            debug_println!(rustaml_context.is_debug_print, "is_right_var = {}", is_right_var);
            //dbg!(is_right_var);

            if is_left_var || is_right_var {
                let other_operand_type = if is_left_var {
                    rhs.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)
                } else {
                    lhs.get(&rustaml_context.ast_pool).get_type(rustaml_context, vars)
                };
                debug_println!(rustaml_context.is_debug_print, "other_operand_type : {:#?}", &other_operand_type);
                //dbg!(&other_operand_type);
                let operand_type = op.get_operand_type(is_left_var, &other_operand_type);
                debug_println!(rustaml_context.is_debug_print, "operand_type : {:#?}", &operand_type);
                //dbg!(&operand_type);
                debug_println!(rustaml_context.is_debug_print, "other_operand_type : {:#?}", &other_operand_type);
                //dbg!(&other_operand_type);
                // TODO : create prinln_context for these ?
                debug_println!(rustaml_context.is_debug_print, "var_name = {:#?}", DebugWrapContext::new(&var_name, rustaml_context));
                debug_println!(rustaml_context.is_debug_print, "node = {:#?}", DebugWrapContext::new(&node, rustaml_context));
                Some(operand_type)
            } else {
                let lhs_inferred = _infer_var_type(rustaml_context, vars, var_name, *lhs, range);
                /*if lhs_inferred.is_some() {
                    return lhs_inferred;
                }*/
                let rhs_inferred = _infer_var_type(rustaml_context, vars, var_name, *rhs, range);
                /*if rhs_inferred.is_some() {
                    return rhs_inferred;
                }
                None*/

                merge_types(lhs_inferred, rhs_inferred)
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

    };

    rustaml_context.dump_inference.borrow_mut().add_type_found(var_name, node, &t);

    t
}

// TODO : make this infallible (inference only gives infos, no need to crash if not found, only if they really need to be used we crash)
pub fn infer_var_type(rustaml_context : &RustamlContext, vars: &FxHashMap<StringRef, Type>, var_name: StringRef, node: ASTRef, range: &Range<usize>) -> Result<Type, TypeInferenceErr> {
    match _infer_var_type(rustaml_context, vars, var_name, node, range) {
        Some(t) => {
            rustaml_context.dump_inference.borrow_mut().add_var_inferred(var_name, t.clone());
            Ok(t)
        },
        None => Err(TypeInferenceErr::new(var_name.get_str(&rustaml_context.str_interner).to_owned(), range.clone()))
    }
}