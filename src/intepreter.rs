use core::panic;
use std::collections::HashMap;

use crate::ast::ASTNode;


#[derive(Debug)]
enum Val {
    Number(i64),
    // for now have a void expr, TODO : remove it, because let will have a in expr that it will return (see ocaml)
    Void,
}

#[derive(Debug)]
struct InterpretContext {
    vars : HashMap<String, Val>
}

fn interpret_node(ast : ASTNode, context : &mut InterpretContext) -> Val {
    match ast {
        ASTNode::TopLevel { nodes } => {
            for node in nodes {
                interpret_node(node, context);
            }
            Val::Void
        }
        ASTNode::Number { nb } => Val::Number(nb),
        ASTNode::VarDecl { name, val } => {
            let val_node = interpret_node(*val, context); 
            context.vars.insert(name, val_node);
            Val::Void
        },
        _ => panic!("unexpected ast node when interpreting"),
    }
}

pub fn interpret(ast : ASTNode){
    let mut context = InterpretContext {
        vars: HashMap::new(),
    };

    interpret_node(ast, &mut context);


    dbg!(context);
    
}