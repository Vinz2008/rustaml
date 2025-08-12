use debug_with_context::DebugWrapContext;
use rustc_hash::FxHashMap;

use crate::{ast::{ASTNode, ASTRef, Pattern, Type}, debug_println, rustaml::RustamlContext, string_intern::StringRef, type_inference::{infer_var_type, merge_types, TypeInferenceErr}};


// refactor all this code:
// - make it faster (remove unnecessary work, cache everything possible)
// - make it smarter (make it constraints based ? see bidirectionnal type checking ?)

impl From<TypeInferenceErr> for TypesErr {
    fn from(value: TypeInferenceErr) -> Self {
        Self::TypeInference(value)
    }
}

#[derive(Debug)]
pub enum TypesErr {
    // make all the type inference err in the type err part ?
    TypeInference(TypeInferenceErr)
}

#[derive(Default)]
pub struct TypeInfos {
    pub vars_ast : FxHashMap<StringRef, ASTRef>,
    pub functions_ast : FxHashMap<StringRef, ASTRef>,
    pub vars_env : FxHashMap<StringRef, Type>, // TODO : replace these strings with vars indexes, then add a Hashmap from the AstRef of a VarUse to the var index
}

pub struct TypeContext<'a> {
    pub rustaml_context : &'a mut RustamlContext,
    type_infos : TypeInfos,
    functions_type_annotations : FxHashMap<StringRef, Type>,
    functions_arg_names : FxHashMap<StringRef, Vec<StringRef>>,
    current_function_name : Option<StringRef>,
}


fn std_functions_type_annotations(rustaml_context: &mut RustamlContext) -> FxHashMap<StringRef, Type> {
    let mut i = |s| rustaml_context.str_interner.intern_compiler(s);
    FxHashMap::from_iter([
        (i("print"), Type::Function(vec![Type::Any], Box::new(Type::Unit), false)),
        (i("rand"), Type::Function(vec![Type::Unit], Box::new(Type::Integer), false)),
        // TODO : add a rand_f ? or make the rand function generic with its return ?
        (i("format"), Type::Function(vec![Type::Str], Box::new(Type::Str), true)),
    ])
}

pub fn resolve_and_typecheck(rustaml_context: &mut RustamlContext, ast : ASTRef) -> Result<TypeInfos, TypesErr> {
    let functions_type_annotations= std_functions_type_annotations(rustaml_context);
    let mut context = TypeContext {
        rustaml_context,
        type_infos: TypeInfos::default(),
        functions_type_annotations,
        functions_arg_names: FxHashMap::default(),
        current_function_name: None,
    };
    resolve_types(&mut context, ast)?;
    typecheck_types(&mut context, ast)?;
    Ok(context.type_infos)
}


// TOD0 : deduplicate these functions ?


fn get_var_type(context: &mut TypeContext, name: StringRef) -> Result<Type, TypesErr> {
    debug_println!(context.rustaml_context.is_debug_print, "vars env : {:?}", DebugWrapContext::new(&context.type_infos.vars_env, context.rustaml_context));
    let t = match context.type_infos.vars_env.get(&name) {
        Some(t) => t.clone(),
        
        None => {
            let var_val_ast = *context.type_infos.vars_ast.get(&name).unwrap_or_else(|| panic!("Unknown var {:?}", DebugWrapContext::new(&name, context.rustaml_context))); // TODO : add better handling instead of unwrap (the var not found should be here ?)
            _resolve_types(context, var_val_ast)?
        }
    };
    Ok(t)
}


// TODO : how do you prevent this from happening multiple times ?
fn get_arg_types(context: &mut TypeContext, arg_names : &[StringRef], func_body : ASTRef) -> Result<Vec<Type>, TypesErr> {
    let mut arg_types = Vec::new();
    for arg_name in arg_names {
        let arg_type = match get_var_type(context, *arg_name)? {
            Type::Any => {
                let range = 0..0; // TODO
                let inferred_type = infer_var_type(context, *arg_name, func_body, &range).unwrap();
                debug_println!(context.rustaml_context.is_debug_print, "inferred {:?} to {:?}", DebugWrapContext::new(arg_name, context.rustaml_context), inferred_type);
                /*if let Type::Any = inferred_type {
                    panic!()
                }*/
                inferred_type
            },
            t => t,
        };
        context.type_infos.vars_env.insert(*arg_name, arg_type.clone());
                
        arg_types.push(arg_type);
    }
    Ok(arg_types)
}

pub fn get_function_type(context: &mut TypeContext, name: StringRef) -> Result<Type, TypesErr> {
    if let Some(f_name) = context.current_function_name && f_name == name {
        // recursive function call, do in second pass
        return Ok(Type::Any);
    }
    let t = match context.functions_type_annotations.get(&name).cloned() {
        Some(t) => t,
        None => {
            let func_body = *context.type_infos.functions_ast.get(&name).unwrap_or_else(|| panic!("function AST not found {:?}", DebugWrapContext::new(&name, context.rustaml_context))); // TODO : add better handling instead of unwrap 
            let ret_type = _resolve_types(context, func_body)?;
            
            let arg_names = context.functions_arg_names.get(&name).unwrap_or_else(|| panic!("arg_name not found {:?}", DebugWrapContext::new(&name, context.rustaml_context))).clone();

            // add the ret for thee type inference (TODO : refactor this ?)
            let args_any = (0..arg_names.len()).map(|_| Type::Any).collect();
            let t = Type::Function(args_any, Box::new(ret_type.clone()), false);
            context.functions_type_annotations.insert(name, t.clone());
            
            let arg_types = get_arg_types(context, &arg_names, func_body)?;
            
            let t = Type::Function(arg_types, Box::new(ret_type), false);
            context.functions_type_annotations.insert(name, t.clone());
            t
        }
    };
    
    Ok(t)
}

fn patch_recursive_calls(context : &mut TypeContext, function_body : ASTRef, ret_type : &Type){
    let a = function_body.get(&context.rustaml_context.ast_pool).clone(); // TODO : remove the clone (even though it is a shallow clone, it will do a shallow clone at every node in the AST !!)
    match a {
        ASTNode::TopLevel { nodes } => {
            for n in nodes {
                patch_recursive_calls(context, n, ret_type);
            }
        },
        ASTNode::FunctionDefinition { name: _, args: _, body: _, return_type: _ } => {}, // TODO : is it possible to do recursive calls to the outer function in functions in functions
        ASTNode::VarDecl { name: _, val, body, var_type: _ } => { 
            patch_recursive_calls(context, val, ret_type);
            if let Some(b) = body {
                patch_recursive_calls(context, b, ret_type);
            }
        },

        ASTNode::VarUse { name: _ } | ASTNode::Unit | ASTNode::Integer { nb: _ } | ASTNode::Float { nb: _ } | ASTNode::String { str: _ } | ASTNode::Boolean { b: _ } => {},
        ASTNode::List { list } => {
            for e in list {
                patch_recursive_calls(context, e, ret_type);
            }
        },
        ASTNode::BinaryOp { op: _, lhs, rhs } => {
            patch_recursive_calls(context, lhs, ret_type);
            patch_recursive_calls(context, rhs, ret_type);
            
        },
        ASTNode::FunctionCall { name, args } => {
            if let Some(f_name) = context.current_function_name && f_name == name {
                function_body.set_type(&mut context.rustaml_context.ast_pool, ret_type.clone());
            }

            for arg in args {
                patch_recursive_calls(context, arg, ret_type);
            }
        },
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            patch_recursive_calls(context, cond_expr, ret_type);
            patch_recursive_calls(context, then_body, ret_type);
            patch_recursive_calls(context, else_body, ret_type);
        }
        ASTNode::MatchExpr { matched_expr, patterns } => {
            patch_recursive_calls(context, matched_expr, ret_type);
            for (_, pattern_ast) in patterns {
                patch_recursive_calls(context, pattern_ast, ret_type);
            }
        }
        //t => panic!("Unknwown ast node : {:?}", DebugWrapContext::new(&t, context.rustaml_context)),
    };
}

fn register_pattern_vars(context: &mut TypeContext, pattern: &Pattern, matched_expr_type : Type) -> Result<(), TypesErr> {
    match pattern {
        Pattern::ListDestructure(e, l) => {
            let e_type = match &matched_expr_type {
                Type::List(e) => e.as_ref().clone(),
                Type::Any => Type::Any, // TODO : temporary workaround, remove this
                //_ => return Ok(()),
                t => panic!("trying to use list destructuring on what is not a list, type : {:?}", t), // TODO : better error handling
            };
            debug_println!(context.rustaml_context.is_debug_print, "REGISTER {:?}", DebugWrapContext::new(e, context.rustaml_context));
            context.type_infos.vars_env.insert(*e, e_type);
            register_pattern_vars(context, l.as_ref(), matched_expr_type)?;
        },
        Pattern::VarName(n) => {
            debug_println!(context.rustaml_context.is_debug_print, "REGISTER {:?}", DebugWrapContext::new(n, context.rustaml_context));
            context.type_infos.vars_env.insert(*n, matched_expr_type); // use type annotations to select match vars types (TODO : other way ?)

        },
        //p => println!("Unknown pattern : {:?}", DebugWrapContext::new(p, context.rustaml_context)),
        _ => {}
    }
    Ok(())
}

pub fn _resolve_types(context: &mut TypeContext, ast : ASTRef) -> Result<Type, TypesErr> {
    match context.rustaml_context.ast_pool.get_type(ast){
        Type::Any => {},
        t => return Ok(t.clone()),
    };

    // TODO : remove ensure_stack ?
    let a = ast.get(&context.rustaml_context.ast_pool).clone(); // TODO : remove the clone (even though it is a shallow clone, it will do a shallow clone at every node in the AST !!)
    let t = match a {
        ASTNode::TopLevel { nodes } => {
            let mut t = Type::Any;
            for n in nodes {
                t = _resolve_types(context, n)?;
            }
            t
        },
        ASTNode::FunctionDefinition { name, args, body, return_type } => {
            context.type_infos.functions_ast.insert(name, body);

            let mut arg_types = Vec::new();
            let mut arg_names = Vec::new();

            for arg in args {
                context.type_infos.vars_env.insert(arg.name, arg.arg_type.clone());
                arg_names.push(arg.name);
                arg_types.push(arg.arg_type);
            }

            context.functions_arg_names.insert(name, arg_names.clone());

            context.current_function_name = Some(name);

            if !matches!(return_type, Type::Any) && arg_types.iter().all(|e| !matches!(e, Type::Any)){
                // function annotation
                let function_type = Type::Function(arg_types, Box::new(return_type.clone()), false);
                context.functions_type_annotations.insert(name, function_type);
            } else {
                let var_types = get_arg_types(context, &arg_names, body)?;
                var_types.into_iter().zip(arg_names).for_each(|(ty, name)| { context.type_infos.vars_env.insert(name, ty); });
            }

            

            let body_type = _resolve_types(context, body)?;

            let return_type = match return_type {
                Type::Any => body_type,
                t => t,
            };


            patch_recursive_calls(context, body, &return_type);

            context.current_function_name.take();

            Type::Unit
        },
        ASTNode::VarDecl { name, val, body, var_type: _ } => { 
            // TODO : add the types (no need to infer the type in this case, should have a get_type function for vars that would : use the var type, if needed, and if not use the inferred type)
            // TODO : do this insertion before ?
            context.type_infos.vars_ast.insert(name, val);
            _resolve_types(context, val)?;
            let t = match body {
                Some(b) => {
                    _resolve_types(context, b)?
                },
                None => Type::Unit,
            };
            t
        },
        ASTNode::VarUse { name } => get_var_type(context, name)?,

        ASTNode::Unit => Type::Unit,
        ASTNode::Integer { nb: _ } => Type::Integer,
        ASTNode::Float { nb: _ } => Type::Float,
        ASTNode::String { str: _ } => Type::Str,
        ASTNode::Boolean { b: _ } => Type::Bool,
        ASTNode::List { list } => {
            let mut e_type = Type::Any; // TODO : how to know the type of the [] list (need to infer it, but will there be problems ?)
            for e in list {
                e_type = _resolve_types(context, e)?;
            }
                
            Type::List(Box::new(e_type))
        },
        ASTNode::BinaryOp { op, lhs, rhs } => {
            let lhs_type = _resolve_types(context, lhs)?;
            let rhs_type = _resolve_types(context, rhs)?;
            let e_type_optional = match rhs_type {
                Type::List(e_rhs_type) => {
                    merge_types(Some(lhs_type), Some(*e_rhs_type)) // TODO : add a custom merge type for this module
                }
                _ => None,
            };
            op.get_type(e_type_optional)
        },
        ASTNode::FunctionCall { name, args } => {
            let func_type = get_function_type(context, name)?;
            let t = match func_type {
                Type::Function(_, ret, _) => *ret,
                Type::Any => Type::Any, // TODO : remove ?
                _ => unreachable!(),
            };

            for arg in args {
                _resolve_types(context, arg)?;
            }

            t
        },
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            _resolve_types(context, cond_expr)?;
            let then_type = _resolve_types(context, then_body)?;
            let else_type = _resolve_types(context, else_body)?;
            match (then_type, else_type){
                (Type::Any, t) | (t, Type::Any) => t,
                (t1, t2) if t1 == t2 => t1,
                (t1, t2) => panic!("mismatched type branches in if : (then : {:?}, else : {:?})", t1, t2),
            }
        }
        ASTNode::MatchExpr { matched_expr, patterns } => {
            let matched_type = _resolve_types(context, matched_expr)?;
                
            let mut branch_types = Vec::new();
            for (pattern, pattern_ast) in patterns {
                register_pattern_vars(context, &pattern, matched_type.clone())?;
                let t = _resolve_types(context, pattern_ast)?;
                branch_types.push(t);
            }

            // TODO : typecheck match expr

            branch_types.into_iter().next().unwrap()
        }
        //t => panic!("Unknwown ast node : {:?}", DebugWrapContext::new(&t, context.rustaml_context)),
    };
    ast.set_type(&mut context.rustaml_context.ast_pool, t.clone());
    Ok(t)

    
}


fn resolve_types(context: &mut TypeContext, ast : ASTRef) -> Result<(), TypesErr> {
    _resolve_types(context, ast)?;
    // TODO : add a second pass for recursive function calls
    Ok(())
}


fn typecheck_types(context: &mut TypeContext, ast : ASTRef) -> Result<(), TypesErr> {
    // TODO : do all the typechecks not already done in the resolving (ex : function type args, lists)
    // check for any (optional flag ?)
    Ok(())
}