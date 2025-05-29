use core::panic;
use std::collections::HashMap;

use crate::{ast::ASTNode, lexer::Operator};

#[derive(Debug, Clone)]
enum Val {
    Number(i64),
    // for now have a void expr, TODO : remove it, because let will have a in expr that it will return (see ocaml)
    Void,
}

#[derive(Debug, Clone)]
struct FunctionDef {
    name : String,
    args : Vec<String>,
    body : Box<ASTNode>,
}

#[derive(Debug)]
struct InterpretContext {
    vars: HashMap<String, Val>,
    functions : HashMap<String, FunctionDef>,
}

fn is_number(val : &Val) -> bool {
    match val {
        Val::Number(_) => true,
        _ => false,
    }
}

fn interpret_binop(context: &mut InterpretContext, op : Operator, lhs : Box<ASTNode>, rhs : Box<ASTNode>) -> Val {
    let lhs_val = interpret_node(context, *lhs);
    let rhs_val = interpret_node(context, *rhs);

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
        }
    };

    Val::Number(res_nb)
}

fn interpret_function_call(context: &mut InterpretContext, name : String, args : Vec<ASTNode>) -> Val {
    let args_val = args.into_iter().map(|e| interpret_node(context, e)).collect::<Vec<_>>();
    // TODO : remove clone
    let func_def = context.functions.get(&name).unwrap().clone();
    for (arg_name, arg_val) in (&func_def.args).iter().zip(&args_val) {
        context.vars.insert(arg_name.clone(), arg_val.clone());
    }

    let res_val = interpret_node(context, *func_def.body.clone());

    for arg_name in &func_def.args {
        context.vars.remove(arg_name);
    }
    res_val
}

fn interpret_node(context: &mut InterpretContext, ast: ASTNode) -> Val {
    match ast {
        ASTNode::TopLevel { nodes } => {
            for node in nodes {
                interpret_node(context, node);
            }
            Val::Void
        }
        ASTNode::FunctionDefinition { name, args, body } => {
            let func_def = FunctionDef { 
                name: name.clone(), 
                args, 
                body, 
            };
            context.functions.insert(name, func_def);
            Val::Void
        },
        ASTNode::Number { nb } => Val::Number(nb),
        ASTNode::VarDecl { name, val } => {
            let val_node = interpret_node(context, *val);
            context.vars.insert(name, val_node);
            Val::Void
        },
        ASTNode::BinaryOp { op, lhs, rhs } => interpret_binop(context, op, lhs, rhs),
        ASTNode::FunctionCall { name, args } => interpret_function_call(context, name, args),
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
