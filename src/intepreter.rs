use core::panic;
use std::{cmp::Ordering, collections::HashMap};

use crate::{ast::{ASTNode, Type}, lexer::Operator};

#[derive(Debug, Clone, PartialEq)]
enum Val {
    Number(i64),
    Float(f64),
    Bool(bool),
    Unit,
}

/*impl Ord for Val {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Val::Number(nb_self), Val::Number(nb_other)) => nb_self.cmp(nb_other),
            _ => unreachable!(), // should do typechecking to avoid this
        }
    }
}*/

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Val::Number(nb_self), Val::Number(nb_other)) => Some(nb_self.cmp(nb_other)),
            (Val::Float(nb_self), Val::Float(nb_other)) => nb_self.partial_cmp(nb_other),
            _ => unreachable!(), // should do typechecking to avoid this
        }
    }
}

impl Val {
    fn get_type(&self) -> Type {
        match self {
            Val::Number(_) => Type::Integer,
            Val::Float(_) => Type::Float,
            Val::Bool(_) => Type::Bool,
            Val::Unit => Type::Unit, 
        }
    }
}

#[derive(Debug, Clone)]
struct FunctionDef {
    name : String,
    args : Vec<String>,
    body : Box<ASTNode>,
    return_type : Type,
}

#[derive(Debug)]
struct InterpretContext {
    functions : HashMap<String, FunctionDef>,
    vars: HashMap<String, Val>,
}

// TODO : gc allocator (https://crates.io/crates/gc)

fn interpret_binop_nb(context: &mut InterpretContext, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_nb = match lhs_val {
        Val::Number(nb) => nb,
        _ => panic!("Expected number in left-side of binary operation"),
    };

    let rhs_nb = match rhs_val {
        Val::Number(nb) => nb,
        _ => panic!("Expected number in left-side of binary operation"),
    };
    // TODO : do unchecked operations ?
    let res_nb = match op {
        Operator::Plus => {
            lhs_nb + rhs_nb
        },
        Operator::Minus => {
            lhs_nb - rhs_nb
        },
        Operator::Mult => {
            lhs_nb * rhs_nb
        },
        Operator::Div => {
            lhs_nb / rhs_nb
        },
        Operator::Equal | Operator::InferiorOrEqual | Operator::IsEqual => unreachable!(), // impossible to have a alone equal, because it is only in let exprs
    };

    Val::Number(res_nb)
}

fn interpret_binop_bool(context: &mut InterpretContext, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_val_type = lhs_val.get_type();
    let rhs_val_type = rhs_val.get_type();
    if rhs_val.get_type() != lhs_val.get_type() {
        panic!("Not the same types around operators (lhs : {:?}, rhs : {:?})", lhs_val_type, rhs_val_type)
    }
    
    match op {
        Operator::IsEqual => Val::Bool(lhs_val == rhs_val),
        Operator::InferiorOrEqual => Val::Bool(lhs_val <= rhs_val),
        _ => unreachable!()
    }
}

fn interpret_binop(context: &mut InterpretContext, op : Operator, lhs : Box<ASTNode>, rhs : Box<ASTNode>) -> Val {
    let lhs_val = interpret_node(context, *lhs);
    let rhs_val = interpret_node(context, *rhs);

    match op.get_type() {
        Type::Integer => interpret_binop_nb(context, op, lhs_val, rhs_val),
        Type::Bool => interpret_binop_bool(context, op, lhs_val, rhs_val),
        _ => unreachable!(),
    }

    
}

fn interpret_function_call(context: &mut InterpretContext, name : String, args : Vec<ASTNode>) -> Val {
    let args_val = args.into_iter().map(|e| interpret_node(context, e)).collect::<Vec<_>>();
    // TODO : remove clone
    let func_def = context.functions.get(&name).unwrap().clone();
    let mut old_vals : Vec<(String, Val)> = Vec::new();
    for (arg_name, arg_val) in (&func_def.args).iter().zip(&args_val) {
        if let Some(old_val) = context.vars.get(arg_name) {
            old_vals.push((arg_name.clone(), old_val.clone()));
        }
        context.vars.insert(arg_name.clone(), arg_val.clone());
    }

    let res_val = interpret_node(context, *func_def.body.clone());

    for arg_name in &func_def.args {
        context.vars.remove(arg_name);
    }
    for (old_name, old_val) in old_vals {
        context.vars.insert(old_name, old_val);
    }
    res_val
}

fn interpret_if_expr(context: &mut InterpretContext, cond_expr : Box<ASTNode>, then_body : Box<ASTNode>, else_body : Box<ASTNode>) -> Val {
    let cond_expr_val = match interpret_node(context, *cond_expr) {
        Val::Bool(b) => b,
        _ => unreachable!(),
    };

    if cond_expr_val {
        interpret_node(context, *then_body)
    } else {
        interpret_node(context, *else_body)
    }
}

fn interpret_node(context: &mut InterpretContext, ast: ASTNode) -> Val {
    match ast {
        ASTNode::TopLevel { nodes } => {
            for node in nodes {
                interpret_node(context, node);
            }
            Val::Unit
        }
        ASTNode::FunctionDefinition { name, args, body, return_type } => {
            let func_def = FunctionDef { 
                name: name.clone(), 
                args: args.into_iter().map(|arg| arg.name).collect(),
                body,
                return_type,
            };
            context.functions.insert(name, func_def);
            Val::Unit
        },
        ASTNode::Float { nb } => Val::Float(nb),
        ASTNode::Integer { nb } => Val::Number(nb),
        ASTNode::Boolean { b } => Val::Bool(b),
        ASTNode::VarDecl { name, val } => {
            let val_node = interpret_node(context, *val);
            context.vars.insert(name, val_node);
            Val::Unit
        },
        ASTNode::VarUse { name } => context.vars.get(&name).unwrap_or_else(|| panic!("BUG interpreter : unknown var {}", &name)).clone(),
        ASTNode::BinaryOp { op, lhs, rhs } => interpret_binop(context, op, lhs, rhs),
        ASTNode::FunctionCall { name, args } => interpret_function_call(context, name, args),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => interpret_if_expr(context, cond_expr, then_body, else_body),
        //n => panic!("unexpected ast node when interpreting : {:?}", n),
    }
}

pub fn interpret(ast: ASTNode) {
    let mut context = InterpretContext {
        vars: HashMap::new(),
        functions: HashMap::new(),
    };

    interpret_node(&mut context, ast);

    dbg!(context);
}
