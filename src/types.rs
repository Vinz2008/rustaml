use debug_with_context::DebugWrapContext;
use rustc_hash::FxHashMap;

use crate::{ast::{ASTNode, ASTRef, Type}, rustaml::RustamlContext, string_intern::StringRef, type_inference::{infer_var_type, merge_types, TypeInferenceErr}};


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
    vars_ast : FxHashMap<StringRef, ASTRef>,
    functions_ast : FxHashMap<StringRef, ASTRef>,
}

pub struct TypeContext<'a> {
    pub rustaml_context : &'a mut RustamlContext,
    type_infos : TypeInfos,
    vars_type_annotations : FxHashMap<StringRef, Type>,
    functions_type_annotations : FxHashMap<StringRef, Type>,
    functions_arg_names : FxHashMap<StringRef, Vec<StringRef>>,
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
        vars_type_annotations: FxHashMap::default(),
        functions_type_annotations,
        functions_arg_names: FxHashMap::default(),
    };
    resolve_types(&mut context, ast)?;
    typecheck_types(&mut context, ast)?;
    Ok(context.type_infos)
}


// TOD0 : deduplicate these functions ?

fn get_var_type(context: &mut TypeContext, name: StringRef) -> Result<Type, TypesErr> {
    let t = match context.vars_type_annotations.get(&name) {
        Some(t) => t.clone(),
        
        None => {
            let var_val_ast = *context.type_infos.vars_ast.get(&name).unwrap_or_else(|| panic!("Unknown var {:?}", DebugWrapContext::new(&name, context.rustaml_context))); // TODO : add better handling instead of unwrap (the var not found should be here ?)
            _resolve_types(context, var_val_ast)?
        }
    };
    Ok(t)
}

pub fn get_function_type(context: &mut TypeContext, name: StringRef) -> Result<Type, TypesErr> {
    let t = match context.functions_type_annotations.get(&name).cloned() {
        Some(t) => t,
        None => {
            let func_body = *context.type_infos.functions_ast.get(&name).unwrap_or_else(|| panic!("function AST not found {:?}", DebugWrapContext::new(&name, context.rustaml_context))); // TODO : add better handling instead of unwrap 
            let ret_type = _resolve_types(context, func_body)?;
            
            let mut arg_types = Vec::new();
            for arg_name in context.functions_arg_names.get(&name).unwrap().clone() {
                let arg_type = match get_var_type(context, arg_name)? {
                    Type::Any => {
                        let range = 0..0; // TODO
                        infer_var_type(context, arg_name, func_body, &range).unwrap()
                    },
                    t => t,
                };
                
                arg_types.push(arg_type);
            }
            Type::Function(arg_types, Box::new(ret_type), false)
        }
    };
    Ok(t)
}

pub fn _resolve_types(context: &mut TypeContext, ast : ASTRef) -> Result<Type, TypesErr> {
    match context.rustaml_context.ast_pool.get_type(ast){
        Type::Any => {},
        t => return Ok(t.clone()),
    };

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

            for arg in args {
                context.vars_type_annotations.insert(arg.name, arg.arg_type.clone());
                arg_types.push(arg.arg_type);
            }

            if !matches!(return_type, Type::Any) && arg_types.iter().all(|e| !matches!(e, Type::Any)){
                // function annotation
                let function_type = Type::Function(arg_types, Box::new(return_type), false);
                context.functions_type_annotations.insert(name, function_type);
            };

            _resolve_types(context, body)?;

            Type::Unit
        },
        ASTNode::VarDecl { name, val, body, var_type } => { 
            // TODO : add the types (no need to infer the type in this case, should have a get_type function for vars that would : use the var type, if needed, and if not use the inferred type)
            // TODO : do this insertion before ?
            context.type_infos.vars_ast.insert(name, val);
            _resolve_types(context, val);
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
        ASTNode::Boolean { b } => Type::Bool,
        ASTNode::List { list } => {
            let e_type = match list.first(){
                Some(e) => _resolve_types(context, *e)?,
                None => Type::Any, // TODO : how to know the type of the [] list (need to infer it, but will there be problems ?)
            };
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
                _ => unreachable!(),
            };

            /*for arg in args {
                _resolve_types(context, arg)?;
            }*/

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
            todo!()
        }
        //t => panic!("Unknwown ast node : {:?}", DebugWrapContext::new(&t, context.rustaml_context)),
    };
    ast.set_type(&mut context.rustaml_context.ast_pool, t.clone());
    Ok(t)
}


fn resolve_recursive_function_calls(context: &mut TypeContext, ast : ASTRef) -> Result<(), TypesErr> {
    let a = ast.get(&context.rustaml_context.ast_pool).clone(); // TODO : remove the clone (even though it is a shallow clone, it will do a shallow clone at every node in the AST !!)
    match a {
        ASTNode::TopLevel { nodes } => {
            for n in nodes {
                resolve_recursive_function_calls(context, n)?;
            }
        },
        ASTNode::FunctionDefinition { name, args, body, return_type } => {

            _resolve_types(context, body)?;

            
        },
        ASTNode::VarDecl { name, val, body, var_type } => { 
            _resolve_types(context, val);
            let t = match body {
                Some(b) => {
                    _resolve_types(context, b)?;
                },
                None => {},
            };
        },
        ASTNode::VarUse { name } => { get_var_type(context, name)?; },

        ASTNode::Unit | ASTNode::Integer { nb: _ } | ASTNode::Float { nb: _ } | ASTNode::String { str: _ } | ASTNode::Boolean { b: _ } => {},
        ASTNode::List { list } => {
            match list.first(){
                Some(e) => { _resolve_types(context, *e)?; },
                None => {},
            };
        },
        ASTNode::BinaryOp { op, lhs, rhs } => {
            _resolve_types(context, lhs)?;
            _resolve_types(context, rhs)?;
            
        },
        ASTNode::FunctionCall { name, args } => {
            let func_type = get_function_type(context, name)?;
            match func_type {
                Type::Function(_, ret, _) => *ret,
                _ => unreachable!(),
            };
            for arg in args {
                _resolve_types(context, arg)?;
            }
        },
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            _resolve_types(context, cond_expr)?;
            _resolve_types(context, then_body)?;
            _resolve_types(context, else_body)?;
        }
        ASTNode::MatchExpr { matched_expr, patterns } => {
            todo!()
        }
        //t => panic!("Unknwown ast node : {:?}", DebugWrapContext::new(&t, context.rustaml_context)),
    };
    Ok(())
}

fn resolve_types(context: &mut TypeContext, ast : ASTRef) -> Result<(), TypesErr> {
    _resolve_types(context, ast)?;
    // TODO : add a second pass for recursive function calls
    Ok(())
}


fn typecheck_types(context: &mut TypeContext, ast : ASTRef) -> Result<(), TypesErr> {
    // TODO : do all the typechecks not already done in the resolving (ex : function type args, lists)
    Ok(())
}