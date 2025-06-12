use rustc_hash::FxHashMap;
use core::panic;
use std::{cmp::Ordering, process::ExitCode};
use std::fmt::{Debug, Formatter};

use crate::{ast::{ASTNode, Type, Pattern}, lexer::Operator};


#[derive(Clone, PartialEq)]
enum List {
    None,
    Node(Val, Box<List>),
}


impl List {
    // intepret nodes here instead of doing before the call and passing a Vec<Val> to avoid not necessary allocations
    fn new(context: &mut InterpretContext, v : &Vec<ASTNode>) -> List {
        let mut l = List::None;
        for e in v {
            let val = interpret_node(context, e);
            l.append(val);
        }
        
        l
    }

    fn append(&mut self, val : Val){
        let mut current: &mut List = self;
        while let List::Node(_, next) = current {
            current = next.as_mut();   
        }
        *current = List::Node(val, Box::new(List::None));

    }

    fn iter(self : &List) -> ListIter<'_> {
        ListIter { current: self }
    }
}


struct ListIter<'a> {
    current : &'a List
}

// TODO : is it needed ?
impl<'a> Iterator for ListIter<'a> {
    type Item = &'a Val;

    fn next(&mut self) -> Option<Self::Item> { 
        match self.current {
            List::None => None,
            List::Node(v, next) => {
                let current = v;
                self.current = next;
                Some(current)
            }
        }
    }
}

impl Debug for List {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> { 
        let mut current: &List = self;
        let mut iter_nb = 0;
        while let List::Node(v, next) = current {
            if iter_nb != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", v)?;
            current = next.as_ref();
            iter_nb += 1;   
        }

        Ok(())
    }
}


#[derive(Debug, Clone, PartialEq)]
enum Val {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(Box<String>), // TODO : replace the boxed string with a index to an interned string
    List(Box<List>),
    Unit,
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Val::Integer(nb_self), Val::Integer(nb_other)) => Some(nb_self.cmp(nb_other)),
            (Val::Float(nb_self), Val::Float(nb_other)) => nb_self.partial_cmp(nb_other),
            (Val::String(str_self), Val::String(str_other)) => str_self.as_ref().partial_cmp(&str_other),
            _ => unreachable!(), // should do typechecking to avoid this
        }
    }
}

impl Val {
    fn get_type(&self) -> Type {
        match self {
            Val::Integer(_) => Type::Integer,
            Val::Float(_) => Type::Float,
            Val::Bool(_) => Type::Bool,
            Val::String(_) => Type::Str,
            Val::List(l) => {
                let elem_type = match l.as_ref() {
                    List::Node(v, _next) => v.get_type(),
                    List::None => Type::Any,
                };
                Type::List(Box::new(elem_type))
            },
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
    functions : FxHashMap<String, FunctionDef>,
    vars: FxHashMap<String, Val>,
}

// TODO : gc allocator (https://crates.io/crates/gc)

fn interpret_binop_nb(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_nb = match lhs_val {
        Val::Integer(nb) => nb,
        _ => panic!("Expected number in left-side of binary operation"),
    };

    let rhs_nb = match rhs_val {
        Val::Integer(nb) => nb,
        _ => panic!("Expected number in right-side of binary operation"),
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
        _ => unreachable!(),
    };

    Val::Integer(res_nb)
}

fn interpret_binop_bool(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_val_type = lhs_val.get_type();
    let rhs_val_type = rhs_val.get_type();
    if rhs_val.get_type() != lhs_val.get_type() {
        panic!("Not the same types around operators (lhs : {:?}, rhs : {:?})", lhs_val_type, rhs_val_type)
    }
    
    match op {
        Operator::IsEqual => Val::Bool(lhs_val == rhs_val),
        Operator::SuperiorOrEqual => Val::Bool(lhs_val >= rhs_val),
        Operator::InferiorOrEqual => Val::Bool(lhs_val <= rhs_val),
        Operator::Superior => Val::Bool(lhs_val > rhs_val),
        Operator::Inferior => Val::Bool(lhs_val < rhs_val),
        _ => unreachable!()
    }
}

fn interpret_binop_str(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_str = match lhs_val {
        Val::String(s) => *s,
        _ => panic!("Expected string in left-side of binary operation"),
    };

    let rhs_str = match rhs_val {
        Val::String(s) => *s,
        _ => panic!("Expected string in left-side of binary operation"),
    };
    
    match op {
        Operator::StrAppend => Val::String(Box::new(lhs_str + &rhs_str)),
        _ => unreachable!()
    }
}

fn interpret_binop(context: &mut InterpretContext, op : Operator, lhs : &ASTNode, rhs : &ASTNode) -> Val {
    let lhs_val = interpret_node(context, lhs);
    let rhs_val = interpret_node(context, rhs);

    match op.get_type() {
        Type::Integer => interpret_binop_nb(op, lhs_val, rhs_val),
        Type::Bool => interpret_binop_bool(op, lhs_val, rhs_val),
        Type::Str => interpret_binop_str(op, lhs_val, rhs_val),
        _ => unreachable!(),
    }

    
}

fn interpret_function_call(context: &mut InterpretContext, name : &String, args : Vec<ASTNode>) -> Val {
    let args_val = args.into_iter().map(|e| interpret_node(context, &e)).collect::<Vec<_>>();
    // TODO : remove clone
    let func_def = context.functions.get(name).unwrap().clone(); // TODO : remove the clone ?
    let mut old_vals : Vec<(String, Val)> = Vec::new();
    for (arg_name, arg_val) in func_def.args.iter().zip(&args_val) {
        if let Some(old_val) = context.vars.get(arg_name) {
            old_vals.push((arg_name.clone(), old_val.clone()));
        }
        context.vars.insert(arg_name.clone(), arg_val.clone());
    }

    let res_val = interpret_node(context, &func_def.body);

    for arg_name in &func_def.args {
        context.vars.remove(arg_name);
    }
    for (old_name, old_val) in old_vals {
        context.vars.insert(old_name, old_val);
    }
    res_val
}

fn interpret_if_expr(context: &mut InterpretContext, cond_expr : &ASTNode, then_body : &ASTNode, else_body : &ASTNode) -> Val {
    let cond_expr_val = match interpret_node(context, cond_expr) {
        Val::Bool(b) => b,
        _ => unreachable!(),
    };

    if cond_expr_val {
        interpret_node(context, then_body)
    } else {
        interpret_node(context, else_body)
    }
}

fn interpret_match(context: &mut InterpretContext, matched_expr : &ASTNode, patterns : &[(Pattern, ASTNode)]) -> Val {
    let matched_expr_val = interpret_node(context, matched_expr);
    for (pattern, pattern_expr) in patterns {
        match pattern {
            Pattern::VarName(s) => {
                context.vars.insert(s.clone(), matched_expr_val.clone());
                let res_val = interpret_node(context, pattern_expr);
                context.vars.remove(s);

                return res_val;
            },
            Pattern::Underscore => return interpret_node(context, pattern_expr),
            Pattern::Integer(nb) => {
                match matched_expr_val {
                    Val::Integer(matched_nb) => {
                        dbg!((*nb, matched_nb));
                        if *nb == matched_nb {
                            return interpret_node(context, pattern_expr);
                        }
                    },
                    _ => panic!("matching an expression that is not an integer with an integer pattern"),
                }
            },
            Pattern::Float(nb) => {
                match matched_expr_val {
                    Val::Float(matched_nb) => {
                        if *nb == matched_nb {
                            return interpret_node(context, pattern_expr);
                        }
                    },
                    _ => panic!("matching an expression that is not a float with a float pattern"),
                }
            },
            Pattern::Range(start, end, inclusivity) => {
                match matched_expr_val {
                    Val::Integer(matched_nb) => {
                        if *inclusivity {
                            if *start <= matched_nb && matched_nb <= *end {
                                return interpret_node(context, pattern_expr);
                            }
                        } else {
                            if *start < matched_nb && matched_nb < *end {
                                return interpret_node(context, pattern_expr);
                            }
                        }
                    },
                    _ => panic!("matching an expression that is not an integer with an range integer pattern"),
                }
            },
            Pattern::String(s) => {
                match matched_expr_val {
                    Val::String(ref matched_str) => {
                        if s == matched_str.as_ref() {
                            return interpret_node(context, pattern_expr);
                        }
                    },
                    _ => panic!("matching an expression that is not an integer with an integer pattern"),
                }
            },
            Pattern::List(l) => {
                let matched_expr_list = match matched_expr_val {
                    Val::List(ref l) => l,
                    _ => panic!("matching an expression that is not a list with a list pattern"),
                };
                if l.len() == 0 && matches!(matched_expr_list.as_ref(), List::None){
                    return interpret_node(context, pattern_expr);
                }
                
                // TODO : refactor this if it is a performance problem (profile it ?)
                for (p, v) in l.iter().zip(matched_expr_list.iter()) {
                    // can't compare pattern and vals !!
                    todo!()
                }
            },
            Pattern::ListDestructure(_, _) => todo!(),
        }
    }

    panic!("No pattern was matched in match expressions (not exhaustive match)")
}

fn interpret_node(context: &mut InterpretContext, ast: &ASTNode) -> Val {
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
                args: args.iter().map(|arg| arg.name.clone()).collect(),
                body: body.clone(),
                return_type: return_type.clone(),
            };
            context.functions.insert(name.clone(), func_def);
            Val::Unit
        },
        ASTNode::Float { nb } => Val::Float(*nb),
        ASTNode::Integer { nb } => Val::Integer(*nb),
        ASTNode::Boolean { b } => Val::Bool(*b),
        ASTNode::VarDecl { name, val, body } => {
            let val_node = interpret_node(context, val.as_ref());
            context.vars.insert(name.clone(), val_node);
            match body {
                Some(b) => {
                    let body_val = interpret_node(context, b.as_ref());
                    context.vars.remove(name);
                    body_val
                },
                None => {
                    Val::Unit
                }
            }
            
        },
        ASTNode::VarUse { name } => context.vars.get(name).unwrap_or_else(|| panic!("BUG interpreter : unknown var {}", &name)).clone(),
        ASTNode::BinaryOp { op, lhs, rhs } => interpret_binop(context, *op, lhs.as_ref(), rhs.as_ref()),
        ASTNode::FunctionCall { name, args } => interpret_function_call(context, name, args.clone()),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => interpret_if_expr(context, cond_expr, then_body, else_body),
        ASTNode::MatchExpr { matched_expr, patterns } => interpret_match(context, matched_expr.as_ref(), patterns.as_slice()),
        ASTNode::String { str } => Val::String(Box::new(str.clone())),
        ASTNode::List { list } => Val::List(Box::new(List::new(context, list))),
        //n => panic!("unexpected ast node when interpreting : {:?}", n),
    }
}

pub fn interpret(ast: ASTNode) -> ExitCode {
    let mut context = InterpretContext {
        vars: FxHashMap::default(),
        functions: FxHashMap::default(),
    };

    interpret_node(&mut context, &ast);

    dbg!(context);

    ExitCode::SUCCESS
}
