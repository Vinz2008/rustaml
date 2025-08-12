use debug_with_context::DebugWrapContext;
use rustc_hash::FxHashMap;

use crate::{ast::{ASTNode, ASTRef, Pattern, Type}, rustaml::RustamlContext, string_intern::StringRef};

#[derive(Debug)]
pub enum TypesErr {
    
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

    // new fields (TODO : remove all we can from before this ones)

    table : TypeVarTable,
    constraints : Vec<TypeConstraint>,
    node_type_vars : FxHashMap<ASTRef, TypeVarId>,
    
    // they are the reverse of each other, (TODO : use this https://docs.rs/bidirectional-map/latest/bidirectional_map/struct.Bimap.html ?)
    vars_type_vars : FxHashMap<StringRef, TypeVarId>,
    vars_types_vars_names : FxHashMap<TypeVarId, StringRef>,

    functions_type_vars : FxHashMap<StringRef, TypeVarId>,
}

// unknown type
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct TypeVarId(u32);

enum TypeConstraint {
    SameType(TypeVarId, TypeVarId),   // var1 == var2
    IsType(TypeVarId, Type),   // var == Int, Float, Function, ...
    FunctionType { fun_type_var: TypeVarId, args_type_vars: Vec<TypeVarId>, ret_type_var: TypeVarId }, // check if the function type is good (for calls)
    // TODO : can I merge these list constraints ?
    ListType(TypeVarId), // var is list(Any)
    IsElementOf { element: TypeVarId, list : TypeVarId }
}

#[derive(Default)]
struct TypeVarTable {
    type_var_parents : Vec<TypeVarId>,
    real_types : Vec<Option<Type>>,
}

impl TypeVarTable {
    fn new_type_var(&mut self) -> TypeVarId {
        let id = TypeVarId(self.type_var_parents.len() as u32);
        self.type_var_parents.push(id);
        self.real_types.push(None);
        id
    }

    fn find_root(&mut self, var : TypeVarId) -> TypeVarId {
        let parent = self.type_var_parents[var.0 as usize];
        if parent == var {
            var
        } else {
            let root = self.find_root(parent);
            self.type_var_parents[var.0 as usize] = root;
            root
        }
    }


    fn resolve_type(&mut self, tv : TypeVarId) -> Type {
        let tv_root = self.find_root(tv);
        self.real_types[tv_root.0 as usize].clone().unwrap_or(Type::Any) // TODO : try to replace unwrap_or with unwrap ?
    }
}

fn get_var_type_var(context : &TypeContext, name: StringRef) -> TypeVarId {
    *context.vars_type_vars.get(&name).unwrap()
}

fn get_function_type_var(context : &TypeContext, name: StringRef) -> TypeVarId {
    *context.functions_type_vars.get(&name).unwrap()
}

fn create_var(context : &mut TypeContext, name : StringRef, val_type_var : TypeVarId){
    context.vars_type_vars.insert(name, val_type_var);
    context.vars_types_vars_names.insert(val_type_var, name);
}

fn collect_constraints_pattern(context : &mut TypeContext, matched_type_var : TypeVarId, pattern: &Pattern) {
    match pattern {
        Pattern::Integer(_) | Pattern::Range(_, _, _) => context.constraints.push(TypeConstraint::IsType(matched_type_var, Type::Integer)),
        Pattern::Float(_) => context.constraints.push(TypeConstraint::IsType(matched_type_var, Type::Float)),
        Pattern::String(_) => context.constraints.push(TypeConstraint::IsType(matched_type_var, Type::Str)), 
        Pattern::List(_) => todo!(),
        Pattern::ListDestructure(e, l) => {
            let element_type_var = context.table.new_type_var();
            create_var(context, *e, element_type_var);
            // TODO : add constraints

            let list_pattern_var = collect_constraints_pattern(context, matched_type_var, pattern);
            context.constraints.push(TypeConstraint::IsElementOf { element: element_type_var, list: todo!() });
        }
        Pattern::VarName(n) => { 
            let var_type_var = context.table.new_type_var();
            create_var(context, *n, var_type_var);
            context.constraints.push(TypeConstraint::SameType(var_type_var, matched_type_var));
        }, 
        Pattern::Underscore => {}, // no constraints
    }
}

fn collect_constraints(context: &mut TypeContext, ast : ASTRef) -> TypeVarId {
    if let Some(&tv) = context.node_type_vars.get(&ast) {
        return tv;
    }
    let new_type_var = context.table.new_type_var();
    context.node_type_vars.insert(ast, new_type_var);

    let a = ast.get(&context.rustaml_context.ast_pool).clone();

    match a {
        ASTNode::TopLevel { nodes } => {
            nodes.iter().for_each(|&e|{ collect_constraints(context, e); });
        },
        ASTNode::Integer { .. } => context.constraints.push(TypeConstraint::IsType(new_type_var, Type::Integer)),
        ASTNode::Float { .. } => context.constraints.push(TypeConstraint::IsType(new_type_var, Type::Float)),
        ASTNode::String { .. } => context.constraints.push(TypeConstraint::IsType(new_type_var, Type::Str)),
        ASTNode::Boolean { .. } => context.constraints.push(TypeConstraint::IsType(new_type_var, Type::Bool)),
        ASTNode::List { list } => {
            todo!()
        }
        ASTNode::VarUse { name } => {
            let var_type_var = get_var_type_var(context, name);
            context.constraints.push(TypeConstraint::SameType(new_type_var, var_type_var));
        }

        ASTNode::BinaryOp { op, lhs, rhs } => {
            let lhs_type_var = collect_constraints(context, lhs);
            let rhs_type_var = collect_constraints(context, rhs);
            // TODO : make it more complex (for now just need to be the same type)
            context.constraints.push(TypeConstraint::SameType(lhs_type_var, rhs_type_var));

            // TODO : refactor this (inline this ?)
            let res_type = op.get_type(None);
            context.constraints.push(TypeConstraint::IsType(new_type_var, res_type));
        }

        ASTNode::FunctionDefinition { name, args, body, return_type } => {
            context.functions_type_vars.insert(name, new_type_var);
            
            let mut arg_vars = Vec::new();
            for arg in args {
                let arg_type_var = context.table.new_type_var();
                context.type_infos.vars_env.insert(arg.name, Type::Any); // placeholder (TODO ? remove ?)
                arg_vars.push(arg_type_var);
                if !matches!(arg.arg_type, Type::Any){
                    context.constraints.push(TypeConstraint::IsType(arg_type_var, arg.arg_type));
                }
            }

            let ret_type_var = context.table.new_type_var();
            if !matches!(return_type, Type::Any){
                context.constraints.push(TypeConstraint::IsType(ret_type_var, return_type));
            }

            let body_type_var = collect_constraints(context, body);
            context.constraints.push(TypeConstraint::SameType(body_type_var, ret_type_var));
            //context.constraints.push(TypeConstraint::IsType(new_type_var, Type::Function(arg_vars.iter().map(|_| Type::Any).collect(), Box::new(Type::Any), false))); // The Any are replaced later
        
            context.constraints.push(TypeConstraint::FunctionType { fun_type_var: new_type_var, args_type_vars: arg_vars, ret_type_var });
        }

        ASTNode::FunctionCall { name, args } => {
            let fun_type_var = get_function_type_var(context, name);

            // TODO : replace the unwrap_or ?
            /*if let Type::Function(arg_types, ret, _) = context.functions_type_annotations.get(&name).unwrap_or(&Type::Any) {
                context.constraints.push(TypeConstraint::IsType(new_type_var, *ret.clone()));
            }*/

            let ret_type_var = context.table.new_type_var();

            let args_type_vars = args.iter().map(|&e| collect_constraints(context, e)).collect::<Vec<_>>();

            context.constraints.push(TypeConstraint::FunctionType { fun_type_var, args_type_vars, ret_type_var });

            for arg in args {
                collect_constraints(context, arg);
            }
        }

        ASTNode::VarDecl { name, val, body, var_type } => {
            let val_type_var = collect_constraints(context, val);

            if let Some(v_t) = var_type {
                context.constraints.push(TypeConstraint::IsType(val_type_var, v_t));
            }
            
            create_var(context, name, val_type_var);

            let body_var_type = if let Some(b) = body {
                let body_var_type = collect_constraints(context, b);
                
                body_var_type
            } else {
                let body_var_type = context.table.new_type_var();
                context.constraints.push(TypeConstraint::IsType(body_var_type, Type::Unit));
                body_var_type
            };

            context.constraints.push(TypeConstraint::SameType(new_type_var, body_var_type));

        }

        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            let cond_type_var = collect_constraints(context, cond_expr);
            context.constraints.push(TypeConstraint::IsType(cond_type_var, Type::Bool));

            let then_type_var = collect_constraints(context, then_body);
            let else_type_var = collect_constraints(context, else_body);
            context.constraints.push(TypeConstraint::SameType(then_type_var, else_type_var));
        }

        ASTNode::MatchExpr { matched_expr, patterns } => {
            let matched_type_var = collect_constraints(context, matched_expr);

            // put all the constraints to one root to improve performance (test ?)
            let mut first_branch = None;
            for (pattern, pattern_ast) in patterns {
                collect_constraints_pattern(context, matched_type_var, &pattern);

                let pattern_ast_type_var = collect_constraints(context, pattern_ast);
                if let Some(f) = first_branch {
                    context.constraints.push(TypeConstraint::SameType(f, pattern_ast_type_var));
                } else {
                    first_branch = Some(pattern_ast_type_var);
                }
            }

        }
        t => panic!("Unknown ast node : {:?}", DebugWrapContext::new(&t, context.rustaml_context)),
        _ => todo!(),
    }

    new_type_var
}

fn merge_types(t1 : &Type, t2: &Type) -> Option<Type> {
    match (t1, t2){
        (t, Type::Any) | (Type::Any, t) => Some(t.clone()),
        (t1, t2) if t1 == t2 => Some(t1.clone()),
        (Type::List(e1), Type::List(e2)) => {
            let e = merge_types(e1.as_ref(), e2.as_ref())?;
            Some(Type::List(Box::new(e)))
        }
        _ => None,
    }
}

// TODO : return result
fn solve_constraints(table: &mut TypeVarTable, constraints : &[TypeConstraint]){
    for c in constraints {
        match c {
            // TODO : add support for the Never type in these constraints
            TypeConstraint::IsType(tv, t) => {
                let root_tv = table.find_root(*tv);
                match &table.real_types[root_tv.0 as usize] {
                    Some(tv_type) => { 
                        if /*tv_type != t*/ let None = merge_types(tv_type, t) {
                            // type checking error (TODO)
                            panic!("Error type checking, expected : {:?}, got : {:?}", t, tv_type);
                        }
                    },
                    None => table.real_types[root_tv.0 as usize] = Some(t.clone()),
                }
            },
            TypeConstraint::SameType(tv1, tv2) => {
                let root_tv1 = table.find_root(*tv1);
                let root_tv2 = table.find_root(*tv2);
                if root_tv1 != root_tv2 {

                    let tv1_type = table.real_types[root_tv1.0 as usize].clone();
                    let tv2_type = table.real_types[root_tv2.0 as usize].clone();
                    match (tv1_type, tv2_type){
                        (Some(t1), Some(t2)) => {
                            if let Some(merged_type) = merge_types(&t1, &t2){
                                table.real_types[tv2.0 as usize] = Some(merged_type);
                                table.real_types[tv1.0 as usize] = None;
                            } else {
                                // type checking error (TODO)
                                panic!("Error type checking, these types are incompatible : {:?} and {:?}", t1, t2);
                            }
                        },
                        _ => {} // no concrete types
                    }

                    // tv2 becomes the parent of tv1
                    table.type_var_parents[tv1.0 as usize] = *tv2;
                }
            },
            TypeConstraint::FunctionType { fun_type_var, args_type_vars, ret_type_var } => {
                todo!()
            }

            TypeConstraint::ListType(l) => todo!(),
            TypeConstraint::IsElementOf { element, list } => todo!(),
        }
    }
}

fn apply_types_to_ast(context : &mut TypeContext){
    for (node, tv) in &context.node_type_vars {
        let t = context.table.resolve_type(*tv);
        if let Some(name) = context.vars_types_vars_names.get(tv) {
            // var is associated to value of typevar, so set the real type
            context.type_infos.vars_env.insert(*name, t.clone());
        } 
        node.set_type(&mut context.rustaml_context.ast_pool, t);
    }
}

pub fn resolve_and_typecheck(rustaml_context: &mut RustamlContext, ast : ASTRef) -> Result<TypeInfos, TypesErr> {
    let functions_type_annotations= std_functions_type_annotations(rustaml_context);
    let mut context = TypeContext {
        rustaml_context,
        type_infos: TypeInfos::default(),
        functions_type_annotations,
        functions_arg_names: FxHashMap::default(),
        current_function_name: None,
        table: TypeVarTable::default(),
        constraints: Vec::new(),
        node_type_vars: FxHashMap::default(),
        vars_type_vars: FxHashMap::default(),
        vars_types_vars_names: FxHashMap::default(),
        functions_type_vars: FxHashMap::default(),
    };

    collect_constraints(&mut context, ast);
    solve_constraints(&mut context.table, &context.constraints);
    apply_types_to_ast(&mut context);

    /*resolve_types(&mut context, ast)?;
    typecheck_types(&mut context, ast)?;*/
    Ok(context.type_infos)
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