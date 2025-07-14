use rustc_hash::FxHashMap;
use std::{cmp::Ordering, process::ExitCode};
use std::fmt::{self, Debug};

use crate::ast::{ASTRef};
use crate::debug::DebugWithContext;
use crate::debug_println;
use crate::rustaml::RustamlContext;
use crate::string_intern::{StrInterner, StringRef};
use crate::{ast::{ASTNode, Type, Pattern}, lexer::Operator};


pub struct ListPool(Vec<List>);

impl ListPool {
    pub fn new() -> ListPool {
        ListPool(Vec::new())
    }

    fn get(&self, list_node : ListRef) -> &List {
        &self.0[list_node.0 as usize]
    }

    fn get_mut(&mut self, list_node : ListRef) -> &mut List {
        &mut self.0[list_node.0 as usize]
    } 

    fn push(&mut self, node : List) -> ListRef {
        let idx = self.0.len();
        self.0.push(node);
        ListRef(idx.try_into().expect("too many list nodes in the pool"))
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct ListRef(u32);

impl ListRef {
    pub fn get(self, list_pool : &ListPool) -> &List {
        list_pool.get(self)
    }

    pub fn get_mut(self, list_pool : &mut ListPool) -> &mut List {
        list_pool.get_mut(self)
    } 
}

impl DebugWithContext for ListRef {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        self.get(&rustaml_context.list_node_pool).fmt_with_context(f, rustaml_context)
    }
}


#[derive(Clone, PartialEq)]
pub enum List {
    None,
    Node(Val, ListRef),

}


impl List {
    // intepret nodes here instead of doing before the call and passing a Vec<Val> to avoid not necessary allocations
    fn new(context: &mut InterpretContext, v : &Vec<ASTRef>) -> ListRef {
        let mut l = List::None;
        for e in v {
            let val = interpret_node(context, *e);
            l.append(&mut context.rustaml_context.list_node_pool, val);
        }
        
        context.rustaml_context.list_node_pool.push(l)
    }

    fn append(&mut self, list_pool: &mut ListPool, val : Val){
        let new_node = list_pool.push(List::None);
        let mut current: &mut List = self;
        while let List::Node(_, next) = current {
            current = next.get_mut(list_pool);
        }
        
        *current = List::Node(val, new_node);

    }

    fn iter<'a>(&'a self, list_pool : &'a ListPool) -> ListIter<'a> {
        ListIter { current: self, list_pool }
    }

    fn len(&self, list_pool : &ListPool) -> usize {
        let mut count = 0;

        let mut current: &List = self;
        while let List::Node(_, next) = current {
            current = next.get(list_pool);
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
    current : &'a List,
    list_pool : &'a ListPool,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = &'a Val;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            List::None => None,
            List::Node(v, next) => {
                let current = v;
                self.current = next.get(self.list_pool);
                Some(current)
            }
        }
    }
}

impl DebugWithContext for List {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        let mut current = self;
        let mut iter_nb = 0;

        while let List::Node(v, next) = current {
            if iter_nb != 0 {
                write!(f, ", ")?;
            }

            v.fmt_with_context(f, rustaml_context)?;
            current = next.get(&rustaml_context.list_node_pool);
            iter_nb += 1;
        }

        Ok(())
    }

}


#[derive(Clone, PartialEq)]
pub enum Val {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(StringRef),
    List(ListRef),
    Unit,
}

impl DebugWithContext for Val {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        match self {
            Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field_with(|fmt| arg0.fmt_with_context(fmt, rustaml_context)).finish(),
            Self::List(arg0) => f.debug_tuple("List").field_with(|fmt| arg0.fmt_with_context(fmt, rustaml_context)).finish(),
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
    fn get_type(&self, list_pool: &ListPool) -> Type {
        match self {
            Val::Integer(_) => Type::Integer,
            Val::Float(_) => Type::Float,
            Val::Bool(_) => Type::Bool,
            Val::String(_) => Type::Str,
            Val::List(l) => {
                let elem_type = match l.get(list_pool) {
                    List::Node(v, _next) => v.get_type(list_pool),
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
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        f.debug_struct("FunctionDef").field("name", &self.name.get_str(&rustaml_context.str_interner)).field_with("args", |fmt| self.args.fmt_with_context(fmt, rustaml_context)).field_with("body", |fmt| self.body.fmt_with_context(fmt, rustaml_context)).field("return_type", &self.return_type).finish()
    }
}


struct InterpretContext<'context> {
    functions : FxHashMap<StringRef, FunctionDef>,
    vars: FxHashMap<StringRef, Val>,
    pub rustaml_context : &'context mut RustamlContext,
}


impl<'context> Debug for InterpretContext<'context> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("InterpretContext")
            .field_with("functions", |fmt| self.functions.fmt_with_context(fmt, self.rustaml_context))
            .field_with("vars", |fmt | self.vars.fmt_with_context(fmt, self.rustaml_context))
            .finish()
    }
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
            // TODO : check if 0, have a special error message in this case (return a result), then use unchecked_div to remove the panic check in the assembly 
            lhs_nb / rhs_nb
        },
        _ => unreachable!(),
    };

    Val::Integer(res_nb)
}

fn interpret_binop_bool(list_pool:  &ListPool, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_val_type = lhs_val.get_type(list_pool);
    let rhs_val_type = rhs_val.get_type(list_pool);
    if rhs_val.get_type(list_pool) != lhs_val.get_type(list_pool) {
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

fn interpret_binop_str(str_interner : &mut StrInterner, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_str = match lhs_val {
        Val::String(s) => s,
        _ => panic!("Expected string in left-side of binary operation"),
    };

    let rhs_str = match rhs_val {
        Val::String(s) => s,
        _ => panic!("Expected string in right-side of binary operation"),
    };
    
    match op {
        Operator::StrAppend => Val::String(lhs_str.add(rhs_str, str_interner)),
        _ => unreachable!()
    }
}

fn interpret_binop_list(list_pool : &mut ListPool, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {

    let rhs_type = rhs_val.get_type(list_pool);

    let rhs_list = match rhs_val {
        Val::List(l) => l,
        _ => panic!("Expected list in right-side of binary operation"),
    };

    let rhs_elem_type = match rhs_type {
        Type::List(e_t) => *e_t,  
        _ => unreachable!(),
    };

    println!("lhs_val.get_type(list_pool) : {:#?}", lhs_val.get_type(list_pool));
    //dbg!(lhs_val.get_type(list_pool))
    println!("rhs_elem_type : {:#?}", &rhs_elem_type);
    //dbg!(&rhs_elem_type);

    if !rhs_list.get(list_pool).empty() && lhs_val.get_type(list_pool) != rhs_elem_type {
        panic!("Trying to add to an array of a type an element of another type");
    }

    match op {
        // use the already existing subtree, should it be clone ?
        Operator::ListAppend => Val::List(list_pool.push(List::Node(lhs_val, rhs_list))),
        _ => unreachable!(),
    }
}

fn interpret_binop(context: &mut InterpretContext, op : Operator, lhs : ASTRef, rhs : ASTRef) -> Val {
    let lhs_val = interpret_node(context, lhs);
    let rhs_val = interpret_node(context, rhs);

    match op.get_type() {
        Type::Integer => interpret_binop_nb(op, lhs_val, rhs_val),
        Type::Bool => interpret_binop_bool(&context.rustaml_context.list_node_pool, op, lhs_val, rhs_val),
        Type::Str => interpret_binop_str(&mut context.rustaml_context.str_interner, op, lhs_val, rhs_val),
        Type::List(_) => interpret_binop_list(&mut context.rustaml_context.list_node_pool, op, lhs_val, rhs_val),
        _ => unreachable!(),
    }

}

fn interpret_function_call(context: &mut InterpretContext, name : StringRef, args : Vec<ASTRef>) -> Val {

    let func_def = context.functions.get(&name).unwrap_or_else(|| panic!("Function {} not found", name.get_str(&context.rustaml_context.str_interner))).clone();

    if args.len() != func_def.args.len() {
        panic!("Invalid args number in function call, expected {}, got {}", func_def.args.len(), args.len());
    }

    let args_val = args.into_iter().map(|e| interpret_node(context, e)).collect::<Vec<_>>();
    
    let mut old_vals : Vec<(StringRef, Val)> = Vec::new();
    for (arg_name, arg_val) in func_def.args.iter().zip(&args_val) {
        if let Some(old_val) = context.vars.get(arg_name) {
            old_vals.push((*arg_name, old_val.clone()));
        }
        context.vars.insert(*arg_name, arg_val.clone());
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

fn interpret_match_pattern(list_pool:  &ListPool, matched_val : &Val, pattern : &Pattern) -> bool {
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
            if l.is_empty() && matches!(matched_expr_list.get(list_pool), List::None){
                return true;
            }
                
            // TODO : refactor this if it is a performance problem (profile it ?)
            let mut pattern_matched_nb = 0;
            for (p, v) in l.iter().zip(matched_expr_list.get(list_pool).iter(list_pool)) {
                if !interpret_match_pattern(list_pool, v, p){
                    return false;
                }
                pattern_matched_nb += 1;
            }


            if pattern_matched_nb == l.len() && pattern_matched_nb == matched_expr_list.get(list_pool).len(list_pool) {
                return true;
            }
            false
        },
        Pattern::ListDestructure(_, tail_pattern) => {
            let matched_expr_list = match matched_val {
                Val::List(l) => l,
                _ => panic!("matching an expression that is not a list with a list destructure pattern"),
            };

            if matched_expr_list.get(list_pool).empty(){
                return false;
            }

            let tail = match matched_expr_list.get(list_pool) {
                List::Node(_, next) => next,
                List::None => unreachable!(),
            };

            let tail_val = Val::List(*tail);
            if !interpret_match_pattern(list_pool, &tail_val, tail_pattern.as_ref()){
                return false;
            }

            return true;
        },
    }
}

fn handle_match_pattern_start(context: &mut InterpretContext, pattern : &Pattern, matched_expr_val : &Val){
    match pattern {
        Pattern::VarName(s) => { 
            context.vars.insert(*s, matched_expr_val.clone());
        },
        Pattern::ListDestructure(head_name, tail_pattern) => {
            let matched_expr_list = match matched_expr_val {
                Val::List(l) => l,
                _ => panic!("matching an expression that is not a list with a list destructure pattern"),
            };
            let (head_val, tail) = match matched_expr_list.get(&context.rustaml_context.list_node_pool) {
                List::Node(val, next) => (val, next),
                List::None => unreachable!(),
            };
            context.vars.insert(*head_name, head_val.clone());
            let tail_val = Val::List(*tail);
            handle_match_pattern_start(context, tail_pattern.as_ref(), &tail_val);
        }
        _ => {},
    }
}

fn handle_match_pattern_end(context: &mut InterpretContext, pattern : &Pattern){
    match pattern {
        Pattern::VarName(s) => { 
            context.vars.remove(s);
        },
        Pattern::ListDestructure(head_name, tail_pattern) => {
            context.vars.remove(head_name);
            handle_match_pattern_end(context, tail_pattern);
        },
        _ => {},
    }
}

fn interpret_match(context: &mut InterpretContext, matched_expr : ASTRef, patterns : &[(Pattern, ASTRef)]) -> Val {
    let matched_expr_val = interpret_node(context, matched_expr);
    for (pattern, pattern_expr) in patterns {

        if interpret_match_pattern(&context.rustaml_context.list_node_pool, &matched_expr_val, pattern) {
            handle_match_pattern_start(context, pattern, &matched_expr_val);
            let res_val = interpret_node(context, *pattern_expr);
            handle_match_pattern_end(context, pattern);
            return res_val;
        }
    }

    panic!("No pattern was matched in match expressions (not exhaustive match)")
}

fn interpret_node(context: &mut InterpretContext, ast: ASTRef) -> Val {
    let ast_node = ast.get(&context.rustaml_context.ast_pool).clone(); // remove the clone ? (because there are indexes in the ast node, the clone is not a deep copy)
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
                args: args.iter().map(|arg| arg.name).collect(),
                body: *body,
                return_type: return_type.clone(),
            };
            context.functions.insert(*name, func_def);
            Val::Unit
        },
        ASTNode::Float { nb } => Val::Float(*nb),
        ASTNode::Integer { nb } => Val::Integer(*nb),
        ASTNode::Boolean { b } => Val::Bool(*b),
        ASTNode::VarDecl { name, val, body } => {
            let val_node = interpret_node(context, *val);
            context.vars.insert(*name, val_node);
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
        ASTNode::VarUse { name } => context.vars.get(name).unwrap_or_else(|| panic!("BUG interpreter : unknown var {}", name.get_str(&context.rustaml_context.str_interner))).clone(),
        ASTNode::BinaryOp { op, lhs, rhs } => interpret_binop(context, *op, *lhs, *rhs),
        ASTNode::FunctionCall { name, args } => interpret_function_call(context, *name, args.clone()),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => interpret_if_expr(context, *cond_expr, *then_body, *else_body),
        ASTNode::MatchExpr { matched_expr, patterns } => interpret_match(context, *matched_expr, patterns.as_slice()),
        ASTNode::String { str } => Val::String(*str),
        ASTNode::List { list } => Val::List(List::new(context, list)),
        //n => panic!("unexpected ast node when interpreting : {:?}", n),
    }
}

pub fn interpret(ast: ASTRef, rustaml_context: &mut RustamlContext) -> ExitCode {
    let mut context = InterpretContext {
        vars: FxHashMap::default(),
        functions: FxHashMap::default(),
        rustaml_context 
    };

    interpret_node(&mut context, ast);

    
    debug_println!(context.rustaml_context.is_debug_print, "content = {:#?}", context);
    //dbg!(context);

    ExitCode::SUCCESS
}
