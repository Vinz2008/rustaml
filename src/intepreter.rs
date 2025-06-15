use rustc_hash::FxHashMap;
use std::{cmp::Ordering, process::ExitCode};
use std::fmt::{self, Debug};

use crate::ast::{ASTPool, ASTRef};
use crate::string_intern::{StrInterner, StringRef, DebugWithContext};
use crate::{ast::{ASTNode, Type, Pattern}, lexer::Operator};


#[derive(Clone, PartialEq)]
enum List {
    None,
    Node(Val, Box<List>),
}


impl List {
    
    // intepret nodes here instead of doing before the call and passing a Vec<Val> to avoid not necessary allocations
    fn new(context: &mut InterpretContext, v : &Vec<ASTRef>) -> List {
        let mut l = List::None;
        for e in v {
            let val = interpret_node(context, *e);
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

    fn len(&self) -> usize {
        let mut count = 0;
        let mut current: &List = self;
        while let List::Node(_, next) = current {
            current = next.as_ref();
            count += 1;
        }
        count
    }

    fn empty(&self) -> bool {
        match self {
            List::None => true,
            List::Node(_, _) => false
        }
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

impl DebugWithContext for List {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, interner: &StrInterner, ast_pool : &ASTPool) -> fmt::Result {
        let mut current: &List = self;
        let mut iter_nb = 0;
        while let List::Node(v, next) = current {
            if iter_nb != 0 {
                write!(f, ", ")?;
            }
            v.fmt_with_context(f, interner, ast_pool)?;
            //write!(f, "{:?}", v)?;
            current = next.as_ref();
            iter_nb += 1;   
        }
        Ok(())
    }

}


#[derive(Clone, PartialEq)]
enum Val {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(StringRef),
    List(Box<List>),
    Unit,
}

impl DebugWithContext for Val {
    
    fn fmt_with_context(&self, f: &mut fmt::Formatter, interner: &StrInterner, ast_pool : &ASTPool) -> fmt::Result {
        match self {
            Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field_with(|fmt| arg0.fmt_with_context(fmt, interner, ast_pool)).finish(),
            Self::List(arg0) => f.debug_tuple("List").field_with(|fmt| arg0.fmt_with_context(fmt, interner, ast_pool)).finish(),
            Self::Unit => write!(f, "Unit"),
        }
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Val::Integer(nb_self), Val::Integer(nb_other)) => Some(nb_self.cmp(nb_other)),
            (Val::Float(nb_self), Val::Float(nb_other)) => nb_self.partial_cmp(nb_other),
            (Val::String(str_self), Val::String(str_other)) => str_self.partial_cmp(str_other),
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

#[derive(Clone)]
struct FunctionDef {
    name : StringRef,
    args : Vec<StringRef>,
    body : ASTRef,
    return_type : Type,
}

impl DebugWithContext for FunctionDef {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, interner: &StrInterner, ast_pool : &ASTPool) -> fmt::Result {
        f.debug_struct("FunctionDef").field("name", &self.name.get_str(interner)).field_with("args", |fmt| self.args.fmt_with_context(fmt, interner, ast_pool)).field_with("body", |fmt| self.body.fmt_with_context(fmt, interner, ast_pool)).field("return_type", &self.return_type).finish()
    }
}


struct InterpretContext<'refs> {
    functions : FxHashMap<StringRef, FunctionDef>,
    vars: FxHashMap<StringRef, Val>,
    pub str_interner : &'refs mut StrInterner,
    pub ast_pool : &'refs mut ASTPool,
}


impl<'intern> Debug for InterpretContext<'intern> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("InterpretContext")
            .field_with("functions", |fmt| self.functions.fmt_with_context(fmt, &self.str_interner, &self.ast_pool))
            .field_with("vars", |fmt | self.vars.fmt_with_context(fmt, &self.str_interner, &self.ast_pool))
            .finish()
    }
}

// TODO
/*impl<'intern> DebugWithInterner for InterpretContext<'intern> {
    #[inline]
    fn fmt_with_interner(&self, f: &mut fmt::Formatter, str_interner: &StrInterner) -> fmt::Result {
        f.debug_struct("InterpretContext")
            .field_with("functions", |fmt| self.functions.fmt_with_interner(fmt, str_interner))
            .field_with("vars", |fmt| self.vars.fmt_with_interner(fmt, str_interner))
            .finish()
    }
}*/

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

fn interpret_binop_str(context: &mut InterpretContext, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_str = match lhs_val {
        Val::String(s) => s,
        _ => panic!("Expected string in left-side of binary operation"),
    };

    let rhs_str = match rhs_val {
        Val::String(s) => s,
        _ => panic!("Expected string in right-side of binary operation"),
    };
    
    match op {
        Operator::StrAppend => Val::String(lhs_str.add(rhs_str, context.str_interner)),
        _ => unreachable!()
    }
}

fn interpret_binop_list(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {

    let rhs_type = rhs_val.get_type();

    let rhs_list = match rhs_val {
        Val::List(l) => *l,
        _ => panic!("Expected list in right-side of binary operation"),
    };

    let rhs_elem_type = match rhs_type {
        Type::List(e_t) => *e_t,  
        _ => unreachable!(),
    };

    dbg!(lhs_val.get_type(), &rhs_elem_type);

    if !rhs_list.empty() && lhs_val.get_type() != rhs_elem_type {
        panic!("Trying to add to an array of a type an element of another type");
    }

    match op {
        Operator::ListAppend => Val::List(Box::new(List::Node(lhs_val, Box::new(rhs_list)))),
        _ => unreachable!(),
    }
}

fn interpret_binop(context: &mut InterpretContext, op : Operator, lhs : ASTRef, rhs : ASTRef) -> Val {
    let lhs_val = interpret_node(context, lhs);
    let rhs_val = interpret_node(context, rhs);

    match op.get_type() {
        Type::Integer => interpret_binop_nb(op, lhs_val, rhs_val),
        Type::Bool => interpret_binop_bool(op, lhs_val, rhs_val),
        Type::Str => interpret_binop_str(context, op, lhs_val, rhs_val),
        Type::List(_) => interpret_binop_list(op, lhs_val, rhs_val),
        _ => unreachable!(),
    }

}

fn interpret_function_call(context: &mut InterpretContext, name : StringRef, args : Vec<ASTRef>) -> Val {

    let func_def = context.functions.get(&name).unwrap().clone(); // TODO : remove the clone ?

    if args.len() != func_def.args.len() {
        panic!("Invalid args number in function call, expected {}, got {}", func_def.args.len(), args.len());
    }

    let args_val = args.into_iter().map(|e| interpret_node(context, e)).collect::<Vec<_>>();
    
    let mut old_vals : Vec<(StringRef, Val)> = Vec::new();
    for (arg_name, arg_val) in func_def.args.iter().zip(&args_val) {
        if let Some(old_val) = context.vars.get(arg_name) {
            old_vals.push((*arg_name, old_val.clone()));
        }
        context.vars.insert(arg_name.clone(), arg_val.clone());
    }

    let res_val = interpret_node(context, func_def.body);

    for arg_name in &func_def.args {
        context.vars.remove(arg_name);
    }
    for (old_name, old_val) in old_vals {
        context.vars.insert(old_name, old_val);
    }
    res_val
}

fn interpret_if_expr(context: &mut InterpretContext, cond_expr : ASTRef, then_body : ASTRef, else_body : ASTRef) -> Val {
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

fn interpret_match_pattern(matched_val : &Val, pattern : &Pattern) -> bool {
    match pattern {
        Pattern::VarName(_) | Pattern::Underscore => true,
        Pattern::Integer(nb) => {
            match matched_val {
                Val::Integer(matched_nb) => {
                    //dbg!((*nb, matched_nb));
                    *nb == *matched_nb
                },
                _ => panic!("matching an expression that is not an integer with an integer pattern"),
            }
        },
        Pattern::Float(nb) => {
            match matched_val {
                Val::Float(matched_nb) => {
                    *nb == *matched_nb
                },
                _ => panic!("matching an expression that is not a float with a float pattern"),
            }
        },
        Pattern::Range(start, end, inclusivity) => {
            match matched_val {
                Val::Integer(matched_nb) => {
                    if *inclusivity {
                        *start <= *matched_nb && matched_nb <= end
                    } else {
                        *start < *matched_nb && matched_nb < end
                    }
                },
                _ => panic!("matching an expression that is not an integer with an range integer pattern"),
            }
        },
        Pattern::String(s) => {
            match matched_val {
                Val::String(matched_str) => {
                    s == matched_str
                },
                _ => panic!("matching an expression that is not an integer with an integer pattern"),
            }
        },
        Pattern::List(l) => {
            let matched_expr_list = match matched_val {
                Val::List(l) => l,
                _ => panic!("matching an expression that is not a list with a list pattern"),
            };
            if l.is_empty() && matches!(matched_expr_list.as_ref(), List::None){
                return true;
            }
                
            // TODO : refactor this if it is a performance problem (profile it ?)
            let mut pattern_matched_nb = 0;
            for (p, v) in l.iter().zip(matched_expr_list.iter()) {
                if !interpret_match_pattern(v, p){
                    return false;
                }
                pattern_matched_nb += 1;
            }


            if pattern_matched_nb == l.len() && pattern_matched_nb == matched_expr_list.len(){
                return true;
            }
            false
        },
        Pattern::ListDestructure(_, _) => todo!(),
    }
}

fn interpret_match(context: &mut InterpretContext, matched_expr : ASTRef, patterns : &[(Pattern, ASTRef)]) -> Val {
    let matched_expr_val = interpret_node(context, matched_expr);
    for (pattern, pattern_expr) in patterns {

        if interpret_match_pattern(&matched_expr_val, pattern) {
            match pattern {
                Pattern::VarName(s) => { 
                    context.vars.insert(s.clone(), matched_expr_val.clone());
                },
                _ => {},
            }
            let res_val = interpret_node(context, *pattern_expr);
            match pattern {
                Pattern::VarName(s) => { 
                    context.vars.remove(s);
                },
                _ => {},
            }

            return res_val;
        }
        /*match pattern {
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
                if l.is_empty() && matches!(matched_expr_list.as_ref(), List::None){
                    return interpret_node(context, pattern_expr);
                }
                
                // TODO : refactor this if it is a performance problem (profile it ?)
                for (p, v) in l.iter().zip(matched_expr_list.iter()) {
                    // can't compare pattern and vals !!
                    todo!()
                }
            },
            Pattern::ListDestructure(_, _) => todo!(),
        }*/
    }

    panic!("No pattern was matched in match expressions (not exhaustive match)")
}

fn interpret_node(context: &mut InterpretContext, ast: ASTRef) -> Val {
    let ast_node = ast.get(context.ast_pool).clone(); // remove the clone ? (because there are indexes in the ast node, the clone is not a deep copy)
    match &ast_node {
        ASTNode::TopLevel { nodes } => {
            for node in nodes {
                interpret_node(context, *node);
            }
            Val::Unit
        }
        ASTNode::FunctionDefinition { name, args, body, return_type } => {
            let func_def = FunctionDef { 
                name: *name, 
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
            let val_node = interpret_node(context, *val);
            context.vars.insert(name.clone(), val_node);
            match body {
                Some(b) => {
                    let body_val = interpret_node(context, *b);
                    context.vars.remove(name);
                    body_val
                },
                None => {
                    Val::Unit
                }
            }
            
        },
        ASTNode::VarUse { name } => context.vars.get(name).unwrap_or_else(|| panic!("BUG interpreter : unknown var {}", name.get_str(context.str_interner))).clone(),
        ASTNode::BinaryOp { op, lhs, rhs } => interpret_binop(context, *op, *lhs, *rhs),
        ASTNode::FunctionCall { name, args } => interpret_function_call(context, *name, args.clone()),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => interpret_if_expr(context, *cond_expr, *then_body, *else_body),
        ASTNode::MatchExpr { matched_expr, patterns } => interpret_match(context, *matched_expr, patterns.as_slice()),
        ASTNode::String { str } => Val::String(*str),
        ASTNode::List { list } => Val::List(Box::new(List::new(context, list))),
        //n => panic!("unexpected ast node when interpreting : {:?}", n),
    }
}

pub fn interpret(ast: ASTRef, str_interner: &mut StrInterner, ast_pool : &mut ASTPool) -> ExitCode {
    let mut context = InterpretContext {
        vars: FxHashMap::default(),
        functions: FxHashMap::default(),
        str_interner,
        ast_pool,
    };

    interpret_node(&mut context, ast);

    dbg!(context);

    ExitCode::SUCCESS
}
