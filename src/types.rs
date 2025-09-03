use debug_with_context::{DebugWithContext, DebugWrapContext};
use enum_tags::Tag;
use rustc_hash::FxHashMap;
// TODO : replace these with use std::range::Range when https://github.com/rust-lang/rust/issues/125687 is added without a feature, then remove all the ranges clones because the new version is Copy
use std::ops::Range;

use crate::{ast::{ASTNode, ASTRef, Pattern, PatternRef, Type}, debug_println, lexer::Operator, rustaml::{nearest_string, RustamlContext}, string_intern::StringRef};


// TODO : make this part generic for every err (something like Ranged<TypesErr>)
#[derive(Debug)]
pub struct TypesErr {
    pub err_data: Box<TypesErrData>,
    pub range : Range<usize>,
}

impl TypesErr {
    fn new(err_data : TypesErrData, range : Range<usize>) -> TypesErr {
        TypesErr { range, err_data: Box::new(err_data) }
    }
}

#[derive(Debug, Tag)]
pub enum TypesErrData {
    VarNotFound {
        name : String,
        nearest_var_name : Option<String>,
    },
    WrongType {
        expected_type : Type,
        got_type : Type
    },
    IncompatibleTypes {
        type1 : Type,
        type2 : Type,
    },
    // TODO : is this one really needed (can it be replaced by the use of another one ?)
    FunctionTypeExpected {
        wrong_type : Type,
    },
    WrongArgNb {
        function_name : String,
        expected_nb : usize,
        got_nb : usize,
    },
    WrongArgType {
        function_name : String,
        expected_type : Type,
        got_type : Type,
    },
    WrongRetType {
        function_name : String,
        expected_type : Type,
        got_type : Type,
    },
    ListTypeExpected {
        wrong_type : Type,
    }
}

// TODO : test to use typerefs to interned types instead of types to speed up type comparisons ? (ex : could store somewhere the Type::Any index to compare it for the merge type ?)

#[derive(Default)]
pub struct TypeInfos {
    pub vars_env : FxHashMap<VarId, Type>,
    pub ast_var_ids : FxHashMap<ASTRef, VarId>
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(u32);

impl DebugWithContext<RustamlContext> for VarId {
    // just print the var id
    fn fmt_with_context(&self, f: &mut std::fmt::Formatter, _context: &RustamlContext) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
struct Var {
    var_id : VarId,
    is_global_function : bool,
}

impl Var {
    fn new(var_id: VarId, is_global_function : bool) -> Var {
        Var { var_id, is_global_function }
    }
}

pub struct TypeContext<'a> {
    pub rustaml_context : &'a mut RustamlContext,
    type_infos : TypeInfos,

    table : TypeVarTable,
    constraints : Vec<Constraint>,
    constraints_ranges : Vec<Range<usize>>,

    node_type_vars : FxHashMap<ASTRef, TypeVarId>,

    current_vars : FxHashMap<StringRef, Vec<Var>>,
    max_var_id : u32,

    generic_type_idx: u32,

    vars_type_vars : FxHashMap<VarId, TypeVarId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericIdx(u32);

impl<'a> TypeContext<'a> {


    fn new_generic_type(&mut self) -> GenericIdx {
        let id = self.generic_type_idx;
        self.generic_type_idx += 1;
        GenericIdx(id)
    }

    fn push_var(&mut self, name : StringRef, is_global : bool) -> VarId {
        let new_id = self.max_var_id;
        self.max_var_id += 1;
        let ret = VarId(new_id);

        let new_var = Var::new(ret, is_global);

        if self.current_vars.contains_key(&name) {
            self.current_vars.get_mut(&name).unwrap().push(new_var);
        } else {
            self.current_vars.insert(name, vec![new_var]);
        }

        ret
    } 

    fn remove_var(&mut self, name : StringRef){
        self.current_vars.get_mut(&name).unwrap().pop().unwrap();
    }

    // when entering an anonymous function, that can't keep the local vars
    fn remove_current_local_vars(&mut self) -> FxHashMap<StringRef, Vec<Var>> {
        let ret = self.current_vars.clone();

        let mut emptied_var_name = Vec::new();

        for (name, vars) in self.current_vars.iter_mut() {
            let mut elements_to_remove = Vec::new();

            for var in vars.iter() {
                // for now only not remove global functions, because globals in rustaml are just local variables of the main function, so it either need to either 1. become real globals 2. be automatically passed in a struct like in closure 
                if !var.is_global_function {
                    elements_to_remove.push(var.clone());
                }
            }

            for element in elements_to_remove {
                let idx = vars.iter().position(|e| e == &element).unwrap();
                vars.remove(idx);
            }

            if vars.is_empty() {
                emptied_var_name.push(*name);
            }
        }

        for name in emptied_var_name {
            self.current_vars.remove(&name);
        }

        ret
    }

    fn readd_local_vars(&mut self, vars_to_add : FxHashMap<StringRef, Vec<Var>> ) {
        self.current_vars = vars_to_add;
    }

    fn push_constraint(&mut self, constraint : Constraint, range : Range<usize>){
        self.constraints.push(constraint);
        self.constraints_ranges.push(range);
    }
}

// unknown type
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, DebugWithContext)]
#[debug_context(RustamlContext)]
struct TypeVarId(u32);

// TODO : rename just Constraint ?
#[derive(Debug)]
enum Constraint {
    SameType(TypeVarId, TypeVarId),   // var1 == var2
    IsType(TypeVarId, Type),   // var == Int, Float, Function, ...
    // TODO : replace function_name with a stringref ?
    FunctionType { fun_type_var: TypeVarId, args_type_vars: Box<[TypeVarId]>, ret_type_var: TypeVarId, is_variadic: bool, function_name : Option<String> }, // check if the function type is good (for calls)
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
        self.real_types[tv_root.0 as usize].clone().unwrap_or(Type::Any)
    }
}

fn get_var_id(context : &TypeContext, name : StringRef, range : Range<usize>) -> Result<VarId, TypesErr> {
    debug_println!(context.rustaml_context.is_debug_print, "{:?}", DebugWrapContext::new(&context.current_vars, context.rustaml_context));
    match context.current_vars.get(&name){
        Some(vars) => {
            Ok(vars
                .last()
                .unwrap_or_else(|| panic!("var id not found : {:?}", name.get_str(&context.rustaml_context.str_interner)))
                .var_id
            )
        },
        None => {
            let nearest_var_name = get_nearest_var(context, name);
            Err(TypesErr::new(TypesErrData::VarNotFound { name: name.get_str(&context.rustaml_context.str_interner).to_owned(), nearest_var_name }, range))
        }
    }
}

fn get_nearest_var(context : &TypeContext, name : StringRef) -> Option<String> {
    nearest_string(name.get_str(&context.rustaml_context.str_interner), context.current_vars.keys().map(|s| s.get_str(&context.rustaml_context.str_interner)), None).map(|s| s.to_owned())
}

fn get_var_type_var(context : &TypeContext, var_id: VarId) -> TypeVarId {
    match context.vars_type_vars.get(&var_id){
        Some(t) => *t,
        None => unreachable!(),
    }
}

fn create_var(context : &mut TypeContext, name : StringRef, is_global_function : bool, val_type_var : TypeVarId) -> VarId {
    let var_id = context.push_var(name, is_global_function);
    context.vars_type_vars.insert(var_id, val_type_var);
    var_id
}

fn remove_var(context : &mut TypeContext, name : StringRef){
    context.remove_var(name);
}

fn create_function(context : &mut TypeContext, name : StringRef, val_type_var : TypeVarId) -> VarId {
    let is_global_function = true; // TODO ?
    create_var(context, name, is_global_function, val_type_var)
}

fn is_underscore(rustaml_context: &RustamlContext, name : StringRef) -> bool {
    name.get_str(&rustaml_context.str_interner) == "_"
}


// TODO : add ranges to patterns ? (add a pattern pool and add a vec of ranges in it ?)
fn collect_constraints_pattern(context : &mut TypeContext, matched_type_var : TypeVarId, pattern: PatternRef) {
    let range = pattern.get_range(&context.rustaml_context.pattern_pool);
    // TODO : remove clone
    match pattern.get(&context.rustaml_context.pattern_pool).clone() {
        Pattern::Integer(_) | Pattern::Range(_, _, _) => context.push_constraint(Constraint::IsType(matched_type_var, Type::Integer), range),
        Pattern::Float(_) => context.push_constraint(Constraint::IsType(matched_type_var, Type::Float), range),
        Pattern::Bool(_) => context.push_constraint(Constraint::IsType(matched_type_var, Type::Bool), range),
        Pattern::String(_) => context.push_constraint(Constraint::IsType(matched_type_var, Type::Str), range), 
        Pattern::List(pattern_list) => {
            context.push_constraint(Constraint::ListType(matched_type_var), range.clone());
            for p in pattern_list {
                let element_type_var = context.table.new_type_var();
                context.push_constraint(Constraint::IsElementOf { element: element_type_var, list: matched_type_var }, range.clone());
                collect_constraints_pattern(context, element_type_var, p);
            }
        },
        Pattern::ListDestructure(e, l) => {
            let element_type_var = context.table.new_type_var();
            let is_var_underscore= is_underscore(context.rustaml_context, e); 
            if !is_var_underscore {
                create_var(context, e, false, element_type_var);
            }

            context.push_constraint(Constraint::IsElementOf { element: element_type_var, list: matched_type_var }, range.clone());
            collect_constraints_pattern(context, matched_type_var, l);
        }
        Pattern::VarName(n) => { 
            let var_type_var = context.table.new_type_var();
            if !is_underscore(context.rustaml_context, n) {
                debug_println!(context.rustaml_context.is_debug_print, "add pattern var {:?}", DebugWrapContext::new(&n, context.rustaml_context));
                create_var(context, n, false, var_type_var);
            }
            
            context.push_constraint(Constraint::SameType(var_type_var, matched_type_var), range);
        }, 
        Pattern::Underscore => {}, // no constraints
    }
}

fn remove_vars_pattern(context : &mut TypeContext, pattern: PatternRef){
    // TODO : remove this clone ?
    match pattern.get(&context.rustaml_context.pattern_pool).clone() {
        Pattern::ListDestructure(e, l) => {
            if !is_underscore(context.rustaml_context, e) {
                remove_var(context, e);
            }
            remove_vars_pattern(context, l);
        }
        Pattern::VarName(n) => {
            if !is_underscore(context.rustaml_context, n) {
                remove_var(context, n);
            }
        }
        _ => {}
    }
}

fn collect_constraints(context: &mut TypeContext, ast : ASTRef) -> Result<TypeVarId, TypesErr> {
    if let Some(&tv) = context.node_type_vars.get(&ast) {
        return Ok(tv);
    }
    let new_type_var = context.table.new_type_var();
    context.node_type_vars.insert(ast, new_type_var);

    let a = ast.get(&context.rustaml_context.ast_pool).clone();
    let range = ast.get_range(&context.rustaml_context.ast_pool);

    match a {
        ASTNode::TopLevel { nodes } => {
            for n in nodes {
                collect_constraints(context, n)?;
            }
        },
        ASTNode::Unit => context.push_constraint(Constraint::IsType(new_type_var, Type::Unit), range),
        ASTNode::Integer { .. } => context.push_constraint(Constraint::IsType(new_type_var, Type::Integer), range),
        ASTNode::Float { .. } => context.push_constraint(Constraint::IsType(new_type_var, Type::Float), range),
        ASTNode::String { .. } => context.push_constraint(Constraint::IsType(new_type_var, Type::Str), range),
        ASTNode::Boolean { .. } => context.push_constraint(Constraint::IsType(new_type_var, Type::Bool), range),
        ASTNode::List { list } => {
            context.push_constraint(Constraint::ListType(new_type_var), range.clone());

            let mut first_element = None;
            for e in list {
                let element_var_type = collect_constraints(context, e)?;

                if let Some(f) = first_element {
                    context.push_constraint(Constraint::SameType(element_var_type, f), range.clone());
                } else {
                    first_element = Some(element_var_type);
                }
            }
            if let Some(f) = first_element {
                context.push_constraint(Constraint::IsElementOf { element: f, list: new_type_var }, range);
            }
            
        }

        ASTNode::Cast { to_type, expr } => {
            collect_constraints(context, expr)?;
            context.push_constraint(Constraint::IsType(new_type_var, to_type), range);
        }

        ASTNode::VarUse { name } => {
            let var_id = get_var_id(context, name, range.clone())?;

            // use the var id instead of the name here
            let var_type_var = get_var_type_var(context, var_id);
            context.type_infos.ast_var_ids.insert(ast, var_id);
            context.push_constraint(Constraint::SameType(new_type_var, var_type_var), range);
        }

        ASTNode::BinaryOp { op, lhs, rhs } => {
            let lhs_type_var = collect_constraints(context, lhs)?;
            let rhs_type_var = collect_constraints(context, rhs)?;

            match op {
                Operator::Plus | Operator::Minus | Operator::Mult | Operator::Div => {
                    context.push_constraint(Constraint::IsType(lhs_type_var, Type::Integer), range.clone());
                    context.push_constraint(Constraint::IsType(rhs_type_var, Type::Integer), range.clone());
                },
                Operator::PlusFloat | Operator::MinusFloat | Operator::MultFloat | Operator::DivFloat => {
                    context.push_constraint(Constraint::IsType(lhs_type_var, Type::Float), range.clone());
                    context.push_constraint(Constraint::IsType(rhs_type_var, Type::Float), range.clone());
                },
                Operator::IsEqual | Operator::IsNotEqual | Operator::SuperiorOrEqual | Operator::InferiorOrEqual | Operator::Superior | Operator::Inferior => {
                    context.push_constraint(Constraint::SameType(lhs_type_var, rhs_type_var), range.clone());
                },
                Operator::And | Operator::Or => {
                    context.push_constraint(Constraint::IsType(lhs_type_var, Type::Bool), range.clone());
                    context.push_constraint(Constraint::IsType(rhs_type_var, Type::Bool), range.clone());
                },
                Operator::StrAppend => {
                    context.push_constraint(Constraint::IsType(lhs_type_var, Type::Str), range.clone());
                    context.push_constraint(Constraint::IsType(rhs_type_var, Type::Str), range.clone());
                },
                Operator::ListAppend => {
                    context.push_constraint(Constraint::IsElementOf { element: lhs_type_var, list: rhs_type_var }, range.clone());
                },
                Operator::ListMerge => {
                    context.push_constraint(Constraint::SameType(lhs_type_var, rhs_type_var), range.clone());
                    context.push_constraint(Constraint::ListType(lhs_type_var), range.clone());
                    context.push_constraint(Constraint::ListType(rhs_type_var), range.clone());
                },
                Operator::Not => unreachable!(),
            }

            match op {
                Operator::Plus | Operator::Minus | Operator::Mult | Operator::Div => {
                    context.push_constraint(Constraint::IsType(new_type_var, Type::Integer), range);
                },
                Operator::PlusFloat | Operator::MinusFloat | Operator::MultFloat | Operator::DivFloat => {
                    context.push_constraint(Constraint::IsType(new_type_var, Type::Float), range);
                },
                Operator::IsEqual | Operator::IsNotEqual | Operator::SuperiorOrEqual | Operator::InferiorOrEqual | Operator::Superior | Operator::Inferior => {
                    context.push_constraint(Constraint::IsType(new_type_var, Type::Bool), range);
                },
                Operator::And | Operator::Or => {
                    context.push_constraint(Constraint::IsType(new_type_var, Type::Bool), range);
                },
                Operator::StrAppend => {
                    context.push_constraint(Constraint::IsType(new_type_var, Type::Str), range);
                },
                Operator::ListAppend => {
                    context.push_constraint(Constraint::SameType(new_type_var, rhs_type_var), range);
                },
                Operator::ListMerge => {
                    context.push_constraint(Constraint::SameType(new_type_var, lhs_type_var), range.clone());
                    context.push_constraint(Constraint::SameType(new_type_var, rhs_type_var), range);
                },
                Operator::Not => unreachable!(),
            }
            
        }

        ASTNode::UnaryOp { op, expr } => {
            let expr_type_var = collect_constraints(context, expr)?;

            match op {
                Operator::Minus => {
                    // TODO : change this if floats become supported with minus unary
                    context.push_constraint(Constraint::IsType(new_type_var, Type::Integer), range.clone());
                    context.push_constraint(Constraint::IsType(expr_type_var, Type::Integer), range);
                },
                Operator::Not => {
                    context.push_constraint(Constraint::IsType(new_type_var, Type::Bool), range.clone());
                    context.push_constraint(Constraint::IsType(expr_type_var, Type::Bool), range);
                }
                _ => unreachable!(),
            }
        }

        ASTNode::FunctionDefinition { name, args, body, type_annotation } => {
            let function_id = create_function(context, name, new_type_var);
            context.type_infos.ast_var_ids.insert(ast, function_id);

            let (return_type, arg_type_annotations) = match type_annotation {
                Some(Type::Function(args, ret, _)) => (Some(*ret), Some(args)),
                Some(t) => return Err(TypesErr::new(TypesErrData::FunctionTypeExpected { wrong_type: t }, range)),
                _ => (None, None),
            };

            let mut arg_vars = Vec::new();
            for (arg_idx, arg_name) in args.iter().enumerate() {
                let arg_type_var = context.table.new_type_var();
                arg_vars.push(arg_type_var);

                if let Some(arg_type_annotations) = &arg_type_annotations {
                    let arg_type = arg_type_annotations[arg_idx].clone();
                    if !matches!(arg_type, Type::Any){
                        context.push_constraint(Constraint::IsType(arg_type_var, arg_type), range.clone());
                    }
                }
                

                if !is_underscore(context.rustaml_context, *arg_name) {
                    create_var(context, *arg_name, false, arg_type_var);
                }
            }

            let ret_type_var = context.table.new_type_var();
            if let Some(return_type) = &return_type {
                if !matches!(return_type, Type::Any){
                    context.push_constraint(Constraint::IsType(ret_type_var, return_type.clone()), range.clone());
                }
            }

            

            let body_type_var = collect_constraints(context, body)?;
            context.push_constraint(Constraint::SameType(body_type_var, ret_type_var), range.clone());
        
            for arg in args {
                if !is_underscore(context.rustaml_context, arg) {
                    remove_var(context, arg);
                }
            }

            let function_name = Some(name.get_str(&context.rustaml_context.str_interner).to_owned());

            context.push_constraint(Constraint::FunctionType { 
                fun_type_var: new_type_var, 
                args_type_vars: arg_vars.into_boxed_slice(), 
                ret_type_var, 
                is_variadic: false, 
                function_name,
            }, range);
        }

        ASTNode::AnonFunc { args: arg_names, body, type_annotation } => {

            let old_vars = context.remove_current_local_vars();

            let mut arg_vars = Vec::new();

            let (return_type, arg_type_annotations) = match type_annotation {
                Some(Type::Function(args, ret, _)) => (Some(*ret), Some(args)),
                Some(t) => return Err(TypesErr::new(TypesErrData::FunctionTypeExpected { wrong_type: t }, range)),
                _ => (None, None),
            };

            for (arg_idx, arg_name) in arg_names.iter().enumerate() {
                let arg_type_var = context.table.new_type_var();
                arg_vars.push(arg_type_var);
                if let Some(arg_type_annotations) = &arg_type_annotations {
                    let arg_type = arg_type_annotations[arg_idx].clone();
                    if !matches!(arg_type, Type::Any){
                        context.push_constraint(Constraint::IsType(arg_type_var, arg_type), range.clone());
                    }
                }

                if !is_underscore(context.rustaml_context, *arg_name) {
                    create_var(context, *arg_name, false, arg_type_var);
                }
            }

            let ret_type_var = context.table.new_type_var();
            if let Some(return_type) = return_type {
                context.push_constraint(Constraint::IsType(ret_type_var, return_type), range.clone());
            }

            let body_type_var = collect_constraints(context, body)?;
            context.push_constraint(Constraint::SameType(body_type_var, ret_type_var), range.clone());

            for arg_name in arg_names {
                if !is_underscore(context.rustaml_context, arg_name) {
                    remove_var(context, arg_name);
                }
            }

            context.readd_local_vars(old_vars);

            context.push_constraint(Constraint::FunctionType { 
                fun_type_var: new_type_var, 
                args_type_vars: arg_vars.into_boxed_slice(), 
                ret_type_var, 
                is_variadic: false, 
                function_name: None,
            }, range);
        }

        ASTNode::ExternFunc { name, type_annotation, lang: _, so_str: _ } => {
            let function_id = create_function(context, name, new_type_var);
            context.type_infos.ast_var_ids.insert(ast, function_id);

            let (function_args_types, ret, is_variadic) = match type_annotation {
                Type::Function(args, ret, is_variadic) => (args, ret, is_variadic),
                _ => return Err(TypesErr::new(TypesErrData::FunctionTypeExpected { wrong_type: type_annotation }, range)),
            };

            let ret_type_var = context.table.new_type_var();
            context.push_constraint(Constraint::IsType(ret_type_var, *ret), range.clone());

            let arg_vars = function_args_types.into_iter().map(|e| {
                let arg_tv = context.table.new_type_var();
                context.push_constraint(Constraint::IsType(arg_tv, e), range.clone());
                arg_tv
            }).collect::<Vec<_>>();

            context.push_constraint(Constraint::FunctionType { 
                fun_type_var: new_type_var, 
                args_type_vars: arg_vars.into_boxed_slice(), 
                ret_type_var, 
                is_variadic, 
                function_name: None,
            }, range);
        }

        ASTNode::FunctionCall { callee, args } => {
            let callee_type_var = collect_constraints(context, callee)?;

            let ret_type_var = new_type_var;

            let args_type_vars = args.iter().map(|&e| collect_constraints(context, e)).collect::<Result<Vec<_>, TypesErr>>()?;

            let function_name = if let ASTNode::VarUse { name } = callee.get(&context.rustaml_context.ast_pool) {
                Some(name.get_str(&context.rustaml_context.str_interner).to_owned())
            } else {
                None
            };

            context.push_constraint(Constraint::FunctionType { 
                fun_type_var: callee_type_var, 
                args_type_vars: args_type_vars.into_boxed_slice(), 
                ret_type_var, 
                is_variadic: false,
                function_name, 
            }, range);
        }

        ASTNode::VarDecl { name, val, body, var_type } => {
            let val_type_var = collect_constraints(context, val)?;

            if let Some(v_t) = var_type {
                context.push_constraint(Constraint::IsType(val_type_var, v_t), range.clone());
            }

            
            if !is_underscore(context.rustaml_context, name){
                let var_id = create_var(context, name, false, val_type_var);
                context.type_infos.ast_var_ids.insert(ast, var_id);
            }

            let body_var_type = if let Some(b) = body {
                let body_var_type = collect_constraints(context, b)?;
                
                body_var_type
            } else {
                let body_var_type = context.table.new_type_var();
                context.push_constraint(Constraint::IsType(body_var_type, Type::Unit), range.clone());
                body_var_type
            };

            context.push_constraint(Constraint::SameType(new_type_var, body_var_type), range);

        }

        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            let cond_type_var = collect_constraints(context, cond_expr)?;
            context.push_constraint(Constraint::IsType(cond_type_var, Type::Bool), range.clone());

            let then_type_var = collect_constraints(context, then_body)?;
            let else_type_var = collect_constraints(context, else_body)?;
            context.push_constraint(Constraint::SameType(new_type_var, then_type_var), range.clone());
            context.push_constraint(Constraint::SameType(new_type_var, else_type_var), range);
            
        }

        ASTNode::MatchExpr { matched_expr, patterns } => {
            let matched_type_var = collect_constraints(context, matched_expr)?;

            // put all the constraints to one root to improve performance (test ?)
            let mut first_branch = None;
            for (pattern, pattern_ast) in patterns {
                collect_constraints_pattern(context, matched_type_var, pattern);

                let pattern_ast_type_var = collect_constraints(context, pattern_ast)?;
                if let Some(f) = first_branch {
                    context.push_constraint(Constraint::SameType(pattern_ast_type_var, f), range.clone());
                } else {
                    first_branch = Some(pattern_ast_type_var);
                }
                remove_vars_pattern(context, pattern);
            }

            if let Some(f) = first_branch {
                context.push_constraint(Constraint::SameType(new_type_var, f), range);
            }

        }
        //t => panic!("Unknown ast node : {:?}", DebugWrapContext::new(&t, context.rustaml_context)),
    }

    Ok(new_type_var)
}

fn merge_types(t1 : &Type, t2: &Type) -> Option<Type> {
    let merged_type = match (t1, t2){
        (t, Type::Never) | (Type::Never, t) => Some(t.clone()),
        (t, Type::Any) | (Type::Any, t) => Some(t.clone()),
        (t1, t2) if t1 == t2 => Some(t1.clone()),
        (Type::List(e1), Type::List(e2)) => {
            let e = merge_types(e1.as_ref(), e2.as_ref())?;
            Some(Type::List(Box::new(e)))
        },
        (Type::Function(args1, ret1, variadic1), Type::Function(args2, ret2, variadic2)) => {
            let variadic = *variadic1 || *variadic2;
            let ret = merge_types(ret1.as_ref(), ret2.as_ref())?;
            let mut args = Vec::new();
            for (arg1, arg2) in args1.iter().zip(args2){
                args.push(merge_types(arg1, arg2)?);
            }
            Some(Type::Function(args.into_boxed_slice(), Box::new(ret), variadic))
        },
        (_, t_g) | (t_g, _) if matches!(t_g, Type::Generic(_)) => Some(t_g.clone()), // TODO ?
        _ => None,
    };

    //println!("merging type {:?} {:?} -> {:?}", t1, t2, merged_type.as_ref());
    merged_type
}

fn set_type_with_changed(type_mut : &mut Option<Type>, t: Type, changed : &mut bool) {
    let new_type = Some(t);
    let old_type = type_mut.take();
    if old_type != new_type {
        *changed = true;
    }

    *type_mut = new_type;
    
}

const ANON_FUNC_NAME : &'static str = "";

fn solve_constraints(table: &mut TypeVarTable, constraints : &[Constraint], constraints_ranges : &[Range<usize>]) -> Result<(), TypesErr> {
    //println!("constraints: {:?}", constraints);
    let mut changed = true;
    while changed {
        changed = false;
        for (idx, c) in constraints.iter().enumerate() {
            //println!("solving constraint : {:?}", c);
            let range = constraints_ranges[idx].clone();
            //dbg!(c);
            match c {
                Constraint::IsType(tv, t) => {
                    let root_tv = table.find_root(*tv);
                    let merged_type = match &table.real_types[root_tv.0 as usize] {
                        Some(tv_type) => { 
                            if let Some(merged_type) = merge_types(tv_type, t) {
                                merged_type
                            } else {
                                return Err(TypesErr::new(TypesErrData::WrongType { expected_type: t.clone(), got_type: tv_type.clone() }, range));
                            }
                        },
                        None => t.clone(),
                    };

                    //println!("real type becomes with isType {:?}", &merged_type);
                    set_type_with_changed(&mut table.real_types[root_tv.0 as usize], merged_type, &mut changed);
                    //table.real_types[root_tv.0 as usize] = Some(merged_type);

                },
                Constraint::SameType(tv1, tv2) => {
                    let root_tv1 = table.find_root(*tv1);
                    let root_tv2 = table.find_root(*tv2);
                    if root_tv1 != root_tv2 {
                        let tv1_type = table.real_types[root_tv1.0 as usize].clone();
                        let tv2_type = table.real_types[root_tv2.0 as usize].clone();
                        let merged_type = match (tv1_type, tv2_type) {
                            (Some(t1), Some(t2)) => { 
                                let merged_type = merge_types(&t1, &t2);
                                match merged_type {
                                    Some(t) => Some(t),
                                    None => return Err(TypesErr::new(TypesErrData::IncompatibleTypes { type1: t1, type2: t2 } , range)),
                                }
                            },
                            (Some(t), None) | (None, Some(t)) => Some(t),
                            (None, None) => {
                                // tv2 becomes the parent of tv1
                                table.type_var_parents[tv1.0 as usize] = *tv2;
                                changed = true;
                                None
                            }
                        };

                        if let Some(merged_type) = merged_type {
                            //println!("real type becomes with SameType {:?}", &merged_type);
                            table.real_types[root_tv2.0 as usize] = Some(merged_type);
                            table.real_types[root_tv1.0 as usize] = None;
                            
                            // tv2 becomes the parent of tv1
                            table.type_var_parents[root_tv1.0 as usize] = root_tv2;
                            changed = true;
                        }

                        
                    }
                },
                // TODO : add in this constraint if it a function call or decl for message error
                Constraint::FunctionType { fun_type_var, args_type_vars, ret_type_var, is_variadic, function_name } => {
                    let fun_root = table.find_root(*fun_type_var);
                    let fun_type = table.real_types[fun_root.0 as usize].clone(); 

                    // TODO : remove these resolve_type ?

                    let passed_args_types = args_type_vars.iter().map(|&e| table.resolve_type(e)).collect::<Vec<_>>();

                    let ret_var_root = table.find_root(*ret_type_var);
                    let ret_type = table.real_types[ret_var_root.0 as usize].clone().unwrap_or(Type::Any);

                    let fun_type_tuple = match fun_type {
                        Some(Type::Function(args, ret, v)) => Some((args, ret, v)),
                        Some(t) => return Err(TypesErr::new(TypesErrData::FunctionTypeExpected { wrong_type: t }, range)),
                        None => {
                            set_type_with_changed(&mut table.real_types[fun_root.0 as usize], Type::Function(passed_args_types.clone().into_boxed_slice(), Box::new(ret_type.clone()), *is_variadic), &mut changed);
                            None
                        },
                    };

                    if let Some((actual_args, actual_ret, variadic)) = fun_type_tuple {
                        //let is_arg_nb_wrong = (function_name != &Some("print".to_owned()) && !variadic && passed_args_types.len() != actual_args.len()) || (function_name == &Some("print".to_owned()) && passed_args_types.len() != 1);
                        let is_arg_nb_wrong = !variadic && passed_args_types.len() != actual_args.len();

                        if is_arg_nb_wrong {
                            // TODO : add better anon func name (with a get_anon_func_name function ?)
                            return Err(TypesErr::new(TypesErrData::WrongArgNb { function_name: function_name.clone().unwrap_or_else(|| ANON_FUNC_NAME.to_owned()) , expected_nb: actual_args.len(), got_nb: passed_args_types.len() }, range));
                        }

                        //dbg!((&actual_ret, &actual_args));

                        let mut merged_arg_types = Vec::new();
                        //dbg!(&passed_args_types);
                        //dbg!(&actual_args);
                        for (passed_arg, actual_arg) in passed_args_types.iter().zip(&actual_args) {
                            if let Some(merged_art_type) = merge_types(passed_arg, actual_arg){
                                merged_arg_types.push(merged_art_type);
                            } else {
                                return Err(TypesErr::new(TypesErrData::WrongArgType { function_name: function_name.clone().unwrap_or_else(|| ANON_FUNC_NAME.to_owned()), expected_type: actual_arg.clone(), got_type: passed_arg.clone() }, range));
                            }
                        }

                        let merged_ret = if let Some(merged_ret) = merge_types(actual_ret.as_ref(), &ret_type) {
                            merged_ret
                        } else {
                            return Err(TypesErr::new(TypesErrData::WrongRetType { function_name: function_name.clone().unwrap_or_else(|| ANON_FUNC_NAME.to_owned()), expected_type: *actual_ret.clone(), got_type: ret_type }, range))
                        };

                        for (arg_tv, arg_type) in args_type_vars.iter().zip(&merged_arg_types) {
                            if !arg_type.contains_generic() {
                                let arg_root = table.find_root(*arg_tv);
                                set_type_with_changed(&mut table.real_types[arg_root.0 as usize], arg_type.clone(), &mut changed);
                            }
                        }

                        let func_merged_contains_generics = merged_ret.contains_generic() || merged_arg_types.iter().any(|e| e.contains_generic());

                        if !func_merged_contains_generics {
                            set_type_with_changed(&mut table.real_types[fun_root.0 as usize], Type::Function(merged_arg_types.into_boxed_slice(), Box::new(merged_ret.clone()), variadic), &mut changed);
                        }
                            
                        if !ret_type.contains_generic() {
                            set_type_with_changed(&mut table.real_types[ret_var_root.0 as usize], merged_ret, &mut changed);
                        }    
                    }


                    
                }

                Constraint::ListType(tv_l) => {
                    let tv_l_root = table.find_root(*tv_l);
                    if let Some(tv_type) = &table.real_types[tv_l_root.0 as usize] {
                        if !matches!(tv_type, Type::List(_)) && !matches!(tv_type, Type::Generic(_)){
                            return Err(TypesErr::new(TypesErrData::ListTypeExpected { wrong_type: tv_type.clone() }, range))
                        }
                    } else {
                        set_type_with_changed(&mut table.real_types[tv_l_root.0 as usize], Type::List(Box::new(Type::Any)), &mut changed);
                    }
                },
                Constraint::IsElementOf { element, list } => {
                    let element_root = table.find_root(*element);
                    let list_root = table.find_root(*list);
                    let element_type = table.real_types[element_root.0 as usize].clone();
                    let list_type = table.real_types[list_root.0 as usize].clone();
                    //dbg!((&list_type, &element_type));

                    let merged_element_type = match (&list_type, &element_type) {
                        (Some(Type::List(list_element_type)), element_type) => {
                            if let Some(element_type) = element_type {
                                let merged_element_type = merge_types(list_element_type.as_ref(), element_type);
                                if merged_element_type.is_none() {
                                    // TODO : replace this to make it more clear that is not list_element_type and element_type that are incompatible, but it is that appending a element_type to a list_type (which is List(list_element_type)) that is invalid
                                    return Err(TypesErr::new(TypesErrData::IncompatibleTypes { type1: *list_element_type.clone(), type2: element_type.clone() }, range))
                                }
                                merged_element_type
                            } else {
                                Some(list_element_type.as_ref().clone())
                            }
                        },
                        (Some(Type::Generic(_)), element_type) => element_type.clone(),
                        (Some(t), _) => return Err(TypesErr::new(TypesErrData::ListTypeExpected { wrong_type: t.clone() }, range)),
                        (None, Some(t)) => Some(t.clone()),
                        (None, None) => None,
                    };

                    if let Some(merged_element_type) = merged_element_type {
                        // we have infos about the element type
                        let merged_list_type = Type::List(Box::new(merged_element_type.clone()));

                        set_type_with_changed(&mut table.real_types[element_root.0 as usize], merged_element_type, &mut changed);
                        set_type_with_changed(&mut table.real_types[list_root.0 as usize], merged_list_type, &mut changed);
                    } else {
                        // the only info is that obviously the list is a list
                        // stolen from IsList to remove the need of IsList and IsElementOf because if there is elements, it is alist
                        set_type_with_changed(&mut table.real_types[list_root.0 as usize], Type::List(Box::new(Type::Any)), &mut changed);
                    }
                },
            }
        }
    }
    Ok(())
    
}

fn apply_types_to_ast(context : &mut TypeContext){


    for (&var_id, &var_tv) in &context.vars_type_vars {
        let t = context.table.resolve_type(var_tv);
        context.type_infos.vars_env.insert(var_id, t);
    }

    debug_println!(context.rustaml_context.is_debug_print, "all_vars : {:?}", DebugWrapContext::new(&context.vars_type_vars, context.rustaml_context));
    debug_println!(context.rustaml_context.is_debug_print, "vars_env : {:?}", DebugWrapContext::new(&context.type_infos.vars_env, context.rustaml_context));

    for (node, tv) in &context.node_type_vars {
        let t = context.table.resolve_type(*tv);

        //debug_println!(context.rustaml_context.is_debug_print, "set type of node {:?} to {:?}", DebugWrapContext::new(node, context.rustaml_context), DebugWrapContext::new(&t, context.rustaml_context));

        node.set_type(&mut context.rustaml_context.ast_pool, t);
    }
}

fn std_function_constraint(context : &mut TypeContext, name : &'static str, args : Vec<Type>, ret: Type, is_variadic : bool){
    let fun_type_var = context.table.new_type_var();
    let function_name = context.rustaml_context.str_interner.intern_compiler(name);
    create_function(context, function_name, fun_type_var);

    let args_type_vars = args.iter().map(|e|{
        let arg_type_var = context.table.new_type_var();
        context.push_constraint(Constraint::IsType(arg_type_var, e.clone()), 0..0); // Can't have ranges, TODO ? (use an option ?)
        arg_type_var
    }).collect::<Vec<_>>();

    let ret_type_var =  context.table.new_type_var();
    context.push_constraint(Constraint::IsType(ret_type_var, ret), 0..0);

    context.push_constraint(Constraint::FunctionType { 
        fun_type_var, 
        args_type_vars: args_type_vars.into_boxed_slice(), 
        ret_type_var, 
        is_variadic, 
        function_name: Some(name.to_owned()), 
    }, 0..0);
}

fn std_functions_constraints_types(context : &mut TypeContext) {
    let print_type = Type::Generic(0);
    std_function_constraint(context, "print", vec![print_type], Type::Unit, false);
    std_function_constraint(context, "rand", vec![Type::Unit], Type::Integer, false);
    std_function_constraint(context, "format", vec![Type::Str], Type::Str, true);
    std_function_constraint(context, "panic", vec![Type::Str], Type::Never, true);
    let generic_type_elem_map_input = Type::Generic(0);
    let generic_type_elem_map_output = Type::Generic(1);
    std_function_constraint(context, "map", vec![Type::List(Box::new(generic_type_elem_map_input.clone())), Type::Function(Box::new([generic_type_elem_map_input]), Box::new(generic_type_elem_map_output.clone()), false)], Type::List(Box::new(generic_type_elem_map_output)), false);
    
    
    let generic_type_elem_filter = Type::Generic(0);
    std_function_constraint(context, "filter", vec![Type::List(Box::new(generic_type_elem_filter.clone())), Type::Function(Box::new([generic_type_elem_filter.clone()]), Box::new(Type::Bool), false)], Type::List(Box::new(generic_type_elem_filter)), false);
    // TODO : add a rand_f ? or make the rand function generic with its return ?
}

pub fn resolve_and_typecheck(rustaml_context: &mut RustamlContext, ast : ASTRef) -> Result<TypeInfos, TypesErr> {
    let mut context = TypeContext {
        rustaml_context,
        type_infos: TypeInfos::default(),
        table: TypeVarTable::default(),
        constraints: Vec::new(),
        constraints_ranges: Vec::new(),
        node_type_vars: FxHashMap::default(),
        current_vars: FxHashMap::default(),
        max_var_id: 0,
        generic_type_idx: 0,
        vars_type_vars: FxHashMap::default(),
    };

    std_functions_constraints_types(&mut context);

    collect_constraints(&mut context, ast)?;

    debug_println!(context.rustaml_context.is_debug_print, "constraints ranges : {:?}", context.constraints_ranges);
    debug_println!(context.rustaml_context.is_debug_print, "var ids with names : {:?}", DebugWrapContext::new(&context.current_vars.iter().enumerate().collect::<Vec<_>>(), context.rustaml_context ));
    debug_println!(context.rustaml_context.is_debug_print, "var ids with type vars : {:?}", DebugWrapContext::new(&context.vars_type_vars, context.rustaml_context));

    solve_constraints(&mut context.table, &context.constraints, &context.constraints_ranges)?;

    apply_types_to_ast(&mut context);

    Ok(context.type_infos)
}