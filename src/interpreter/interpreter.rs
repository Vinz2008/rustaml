use rustc_hash::FxHashMap;
use std::cmp::max;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display};
use std::panic;
use debug_with_context::DebugWithContext;
use rand::prelude::*;

use crate::ast::ASTRef;
use crate::ast::PatternRef;
use crate::debug_println;

use crate::interpreter::gc::{try_gc_collect, Gc, GcContext};
use crate::rustaml::ensure_stack;
use crate::rustaml::RustamlContext;
use crate::string_intern::StringRef;
use crate::{ast::{ASTNode, Type, Pattern}, lexer::Operator};

#[cfg(feature = "gc-test-collect")] 
use crate::interpreter::gc::collect_gc;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(target_arch = "wasm32")]{
        use crate::ast::ExternLang;

        fn ffi_not_supported_wasm() -> ! {
            panic!("FFI not supported on wasm");
        }

        #[derive(Clone, PartialEq, DebugWithContext)]
        #[debug_context(RustamlContext)]
        struct FFIFunc;
        
        fn call_ffi_function(context : &mut InterpretContext, ffi_func : &FFIFunc, args : &[Val]) -> Val {
            ffi_not_supported_wasm()
        }
        fn get_ffi_func(context : &mut InterpretContext, name: StringRef, func_type : Type, external_lang : ExternLang, so_str : Option<StringRef>) -> FFIFunc {
            ffi_not_supported_wasm()
        }
    } else {
        use crate::interpreter::ffi::{call_ffi_function, get_ffi_func, FFIFunc};
    }
}

// None values are freed lists that can be reused
#[derive(Clone)]
pub struct ListPool(pub Vec<Option<Gc<List>>>);

impl ListPool {
    pub fn new() -> ListPool {
        ListPool(Vec::new())
    }

    // TODO : test if unwrap has better performance when replaced with unwrap_unchecked here
    fn get(&self, list_node : ListRef) -> &List {
        &self.0[list_node.0 as usize].as_ref().unwrap().data
    }

    fn get_mut(&mut self, list_node : ListRef) -> &mut List {
        &mut self.0[list_node.0 as usize].as_mut().unwrap().data
    }
    
    /*fn get_gc(&self, list_node : ListRef) -> &Gc<List> {
        self.0[list_node.0 as usize].as_ref().unwrap()
    }*/

    fn get_gc_mut(&mut self, list_node : ListRef) -> &mut Gc<List> {
        self.0[list_node.0 as usize].as_mut().unwrap()
    }

    fn free(&mut self, list_node : ListRef) {
        let freed_node = self.0[list_node.0 as usize].take();

        let _freed_node = match freed_node {
            Some(n) => n,
            None => panic!("gc tried to free a None list node"),
        };
        
        // TODO : drop internal vals ? (would need to have a free function on vals)

    }

    fn push(&mut self, node : List) -> ListRef {
        for (idx, e) in self.0.iter_mut().enumerate() {
            if e.is_none() {
                *e = Some(Gc::new(node));
                return ListRef(idx.try_into().unwrap());
            }
        }


        let idx = self.0.len();
        self.0.push(Some(Gc::new(node)));
        ListRef(idx.try_into().expect("too many list nodes in the pool"))
    }


    pub fn nb_used_nodes(&self) -> usize {
        return self.0.iter().filter(|e| e.is_some()).count();
    }

    pub fn nb_free_nodes(&self) -> usize {
        return self.0.len() - self.nb_used_nodes();
    }

    pub fn nb_free_at_end(&self) -> usize {
        return self.0.iter().rev().take_while(|l| l.is_none()).count();
    }

    // TODO : heuristics for this
    pub fn shrink_end(&mut self, free_at_end : usize){
        let old_len = self.0.len();
        // TODO : multiply this by a factor(1.2 ? 1.5) to keep a certain capacity more than the length
        let end_length = max(old_len - free_at_end, 20);
        //println!("end_length : {}", end_length);
        // keep at least 20 None
        if end_length == 0 {
            self.0.clear();
            self.0.shrink_to(old_len/3);
        } else {
            self.0.truncate(end_length);
            let end_capacity = (end_length as f64 * 1.3) as usize;
            self.0.shrink_to(end_capacity);
        }
        
    }
}

// does not use macro for opti (not printing the entire list, just the node)
impl DebugWithContext<RustamlContext> for ListPool {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        f.debug_tuple("ListPool").field_with(|f| {
            let mut debug_l = f.debug_list();
            for e in &self.0 {
                match e {
                    Some(l) => /*l.data.fmt_with_context(f, rustaml_context)?*/ {
                        match &l.data {
                            List::None => debug_l.entry(&"None"),
                            List::Node(val, next) => debug_l.entry_with(|f| {
                                f.debug_tuple("Node").field_with(|f| val.fmt_with_context(f, rustaml_context)).field(&next.0).finish()
                            }),
                        };
                    },
                    None => { 
                        debug_l.entry(&None::<()>);
                    }
                };
            }
            debug_l.finish()?;
            fmt::Result::Ok(())
        }).finish()
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct ListRef(u32);

impl ListRef {
    /// # Safety
    ///
    /// This function should only be called with known good indexes from the list pool
    pub unsafe fn new_unchecked(idx : u32) -> ListRef {
        ListRef(idx)
    }

    pub fn get(self, list_pool : &ListPool) -> &List {
        list_pool.get(self)
    }

    pub fn get_mut(self, list_pool : &mut ListPool) -> &mut List {
        list_pool.get_mut(self)
    }

    /*pub fn get_gc(self, list_pool : &ListPool) -> &Gc<List> {
        list_pool.get_gc(self)
    }*/
    
    pub fn get_gc_mut(self, list_pool : &mut ListPool) -> &mut Gc<List> {
        list_pool.get_gc_mut(self)
    }

    pub fn free(self, list_pool : &mut ListPool) {
        list_pool.free(self)
    }
}

impl DebugWithContext<RustamlContext> for ListRef {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        self.get(&rustaml_context.list_node_pool).fmt_with_context(f, rustaml_context)
    }
}


// TODO : rework the layout ? (see https://rust-unofficial.github.io/too-many-lists/)
#[derive(Clone)]
pub enum List {
    None,
    Node(Val, ListRef)
}


impl List {
    fn new(val : Val, next : ListRef) -> List {
        List::Node(val, next)
    }

    // intepret nodes here instead of doing before the call and passing a Vec<Val> to avoid not necessary allocations
    fn new_from(context: &mut InterpretContext, v : &[ASTRef]) -> ListRef {
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
        while let List::Node(_, next ) = current {
            current = next.get_mut(list_pool);
        }
        
        *current = List::new(val, new_node);

    }

    pub fn iter<'a>(&'a self, list_pool : &'a ListPool) -> ListIter<'a> {
        ListIter { current: self, list_pool }
    }

    fn len(&self, list_pool : &ListPool) -> usize {
        let mut count = 0;

        let mut current: &List = self;
        while let List::Node(_, next ) = current {
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

    pub fn deep_clone(&self, list_pool : &mut ListPool) -> List {
        match self {
            List::Node(val, l) => {
                let cloned_tail = l.get(list_pool).clone().deep_clone(list_pool);
                List::Node(val.clone(), list_pool.push(cloned_tail))
            }
            List::None => List::None,
        }
    }
}


pub struct ListIter<'a> {
    current : &'a List,
    list_pool : &'a ListPool,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = &'a Val;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            List::None => None,
            List::Node(v, next ) => {
                let current = v;
                self.current = next.get(self.list_pool);
                Some(current)
            }
        }
    }
}

impl DebugWithContext<RustamlContext> for List {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        let mut current = self;
        let mut iter_nb = 0;

        while let List::Node(v, next ) = current {
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

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub struct SumTypeVal {
    // TODO : are there other ways to represent this ? (do I really need the sum_type_name and variant_nb ?)
    sum_type_name : StringRef,
    variant_nb : usize,
    variant_name : StringRef,
    // TODO : add val
}

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub enum Val {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(StringRef),
    Char(char),
    List(ListRef),
    Function(FunctionDef),
    SumType(SumTypeVal),
    Unit,
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

pub struct ValWrapDisplay<'a> {
    val : &'a Val,
    rustaml_context: &'a RustamlContext,
}

fn _display_list(l : ListRef, rustaml_context: &RustamlContext, f: &mut fmt::Formatter<'_>, is_first : bool) -> fmt::Result {
    match l.get(&rustaml_context.list_node_pool){
        List::Node(e, l) => {
            if !is_first {
                write!(f, ", ")?;
            }
            let e_wrap = ValWrapDisplay {
                val: e,
                rustaml_context,
            };

            write!(f, "{}", e_wrap)?;

            _display_list(*l, rustaml_context, f, false)
        },
        List::None => fmt::Result::Ok(()),
    }
}

fn display_list(l : ListRef, rustaml_context: &RustamlContext, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match l.get(&rustaml_context.list_node_pool){
        List::None => write!(f, "[]"),
        _ => {
            write!(f, "[")?;
            _display_list(l, rustaml_context, f, true)?;
            write!(f, "]")
        },
    }
}

impl Display for ValWrapDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.val {
            Val::Integer(i) => write!(f, "{}", i),
            Val::Float(fl) => write!(f, "{}", fl),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Char(c) => write!(f, "{}", c),
            Val::String(s) => write!(f, "{}", s.get_str(&self.rustaml_context.str_interner)),
            Val::List(l) => display_list(*l, self.rustaml_context, f),
            Val::Function(_) => write!(f, "function"), // TODO ?
            Val::SumType(_) => todo!(), // TODO
            Val::Unit => write!(f, "()"),
        }
    }
}

impl Val {
    pub fn display<'a>(&'a self, rustaml_context: &'a RustamlContext) -> ValWrapDisplay<'a> {
        ValWrapDisplay { 
            val: self, 
            rustaml_context  
        }
    }
}

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub enum FunctionBody {
    Ast(ASTRef),
    Ffi(FFIFunc), // TODO : should it be a Rc to prevent cloning it from being very costly and to reduce the size of the enum
}

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub struct FunctionDef {
    name : StringRef,
    args : Box<[StringRef]>,
    pub body : FunctionBody,
    pub function_def_ast : Option<ASTRef>,
}

impl FunctionDef {
    pub fn new_ffi(context : &mut InterpretContext, ffi_func : FFIFunc) -> FunctionDef {
        FunctionDef { 
            name: context.rustaml_context.str_interner.intern_runtime("<FFI function>"), 
            args: vec![].into_boxed_slice(), // TODO ? 
            body: FunctionBody::Ffi(ffi_func), 
            function_def_ast: None, 
        }
    }
}

pub struct InterpretContext<'context> {
    //functions : FxHashMap<StringRef, FunctionDef>,
    pub vars: FxHashMap<StringRef, Val>,
    pub rustaml_context : &'context mut RustamlContext,
    pub gc_context : GcContext,
    rng : ThreadRng
}


impl<'context> Debug for InterpretContext<'context> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("InterpretContext")
            //.field_with("functions", |fmt| self.functions.fmt_with_context(fmt, self.rustaml_context))
            .field_with("vars", |fmt | self.vars.fmt_with_context(fmt, self.rustaml_context))
            .finish()
    }
}

// TODO : replace all these panics in interpret_binop_* with unreachables ?

fn interpret_binop_int(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_nb = match lhs_val {
        Val::Integer(nb) => nb,
        _ => unreachable!(),
    };

    let rhs_nb = match rhs_val {
        Val::Integer(nb) => nb,
        _ => unreachable!(),
    };
    let res_nb = match op {
        Operator::Plus => {
            match lhs_nb.checked_add(rhs_nb){
                Some(res) => res,
                None => runtime_error("Overflow when adding"),
            }
            //lhs_nb + rhs_nb
        },
        Operator::Minus => {
            match lhs_nb.checked_sub(rhs_nb){
                Some(res) => res,
                None => runtime_error("Overflow when substracting"),
            }
            //lhs_nb - rhs_nb
        },
        Operator::Mult => {
            match lhs_nb.checked_mul(rhs_nb){
                Some(res) => res,
                None => runtime_error("Overflow when multiplying")
            }
            //lhs_nb * rhs_nb
        },
        Operator::Div => {
            match lhs_nb.checked_div(rhs_nb){
                Some(res) => res,
                None => if rhs_nb == 0 {
                    runtime_error("Division by zero")
                } else {
                    runtime_error("Overflow when dividing")
                },
            }
            //lhs_nb / rhs_nb
        },
        Operator::Rem => {
            match lhs_nb.checked_rem(rhs_nb){
                Some(res) => res,
                None => if rhs_nb == 0 {
                    runtime_error("Calculating remainder with zero")
                } else {
                    runtime_error("Overflow when calculating remainder")
                },
            }
        }
        _ => unreachable!(),
    };

    Val::Integer(res_nb)
}

fn interpret_binop_float(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    //println!("got {:?} for lhs", DebugWrapContext::new(&lhs_val, rustaml_context));
    let lhs_nb = match lhs_val {
        Val::Float(nb) => nb,
        _ => unreachable!(),
    };

    let rhs_nb = match rhs_val {
        Val::Float(nb) => nb,
        _ => unreachable!(),
    };

    let res_nb = match op {
        Operator::PlusFloat => {
            lhs_nb + rhs_nb
        },
        Operator::MinusFloat => {
            lhs_nb - rhs_nb
        },
        Operator::MultFloat => {
            lhs_nb * rhs_nb
        },
        Operator::DivFloat => {
            // TODO : check if 0, have a special error message in this case (return a result), then use unchecked_div to remove the panic check in the assembly 
            lhs_nb / rhs_nb
        },
        Operator::RemFloat => {
            lhs_nb % rhs_nb
        }
        _ => unreachable!(),
    };

    Val::Float(res_nb)
}

fn interpret_binop_bool_logical(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    
    let lhs_bool = match lhs_val {
        Val::Bool(b) => b,
        _ => unreachable!(),
    };

    let rhs_bool = match rhs_val {
        Val::Bool(b) => b,
        _ => unreachable!(),
    };

    let ret_bool = match op {
        Operator::And => lhs_bool && rhs_bool,
        Operator::Or => lhs_bool || rhs_bool,
        _ => unreachable!(),
    };

    Val::Bool(ret_bool)
}

fn interpret_binop_bool(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let b = match op {
        Operator::IsEqual => lhs_val == rhs_val,
        Operator::IsNotEqual => lhs_val != rhs_val,
        Operator::SuperiorOrEqual => lhs_val >= rhs_val,
        Operator::InferiorOrEqual => lhs_val <= rhs_val,
        Operator::Superior => lhs_val > rhs_val,
        Operator::Inferior => lhs_val < rhs_val,
        Operator::And | Operator::Or => return interpret_binop_bool_logical(op, lhs_val, rhs_val),
        _ => unreachable!()
    };

    Val::Bool(b)
}

fn interpret_binop_str(context: &mut InterpretContext, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_str = match lhs_val {
        Val::String(s) => s,
        _ => unreachable!(),
    };

    let rhs_str = match rhs_val {
        Val::String(s) => s,
        _ => unreachable!(),
    };
    
    let v = match op {
        Operator::StrAppend => {
            let str_ref = lhs_str.add(rhs_str, &mut context.rustaml_context.str_interner);
            context.gc_context.add_allocation(str_ref.len(&context.rustaml_context.str_interner));
            Val::String(str_ref)
        },
        _ => unreachable!()
    };

    // TODO : activate this
    //try_gc_collect(context); // try gc collect because these operators create big allocations

    v
}

fn interpret_binop_list(context: &mut InterpretContext, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {

    let rhs_list = match rhs_val {
        Val::List(l) => l,
        _ => unreachable!(),
    };

    let v = match op {
        // TODO : use the already existing subtree, should it be clone ?
        // TODO : call add_allocation for gc in these cases
        Operator::ListAppend => Val::List(context.rustaml_context.list_node_pool.push(List::new(lhs_val, rhs_list))),
        Operator::ListMerge => {
            // TODO : optimize this ?
            let lhs_list = match lhs_val {
                Val::List(l) => l,
                _ => unreachable!(),
            };

            let mut cloned_lhs = lhs_list.get(&context.rustaml_context.list_node_pool).clone().deep_clone(&mut context.rustaml_context.list_node_pool);

            let mut rhs_vals = Vec::new();
            for v in rhs_list.get(&context.rustaml_context.list_node_pool).iter(&context.rustaml_context.list_node_pool) {
                rhs_vals.push(v.clone()); // TODO : add a deep clone for vals for situations like this (where val is cloned, but need to deep clone for immutability)
            }

            for v in rhs_vals {
                cloned_lhs.append(&mut context.rustaml_context.list_node_pool, v);
            }

            let cloned_lhs_ref = context.rustaml_context.list_node_pool.push(cloned_lhs);

            Val::List(cloned_lhs_ref)
        }
        _ => unreachable!(),
    };
    // TODO : activate this
    //try_gc_collect(context);
    
    v
}

fn interpret_binop(context: &mut InterpretContext, op : Operator, lhs : ASTRef, rhs : ASTRef) -> Val {
    // TODO : add a short circuiting for bool ops
    let lhs_val = interpret_node(context, lhs);
    let rhs_val = interpret_node(context, rhs);

    match op.get_type() {
        Type::Integer => interpret_binop_int(op, lhs_val, rhs_val),
        Type::Float => interpret_binop_float(op, lhs_val, rhs_val),
        Type::Bool => interpret_binop_bool(op, lhs_val, rhs_val),
        Type::Str => interpret_binop_str(context, op, lhs_val, rhs_val),
        Type::List(_) => interpret_binop_list(context, op, lhs_val, rhs_val),
        _ => unreachable!(),
    }

}

fn interpret_unop(context : &mut InterpretContext, op : Operator, expr : ASTRef) -> Val {
    match (op, expr.get(&context.rustaml_context.ast_pool)){
        (Operator::Minus, ASTNode::Integer { nb }) => return Val::Integer((-nb).try_into().unwrap()),
        _ => {}
    }

    let expr_val = interpret_node(context, expr);

    match op {
        Operator::Minus => {
            // TODO : make it work with float ?
            let expr_nb = match expr_val {
                Val::Integer(nb) => nb,
                _ => unreachable!(),
            };
            Val::Integer(-expr_nb)
        },
        Operator::Not => {
            let expr_bool = match expr_val {
                Val::Bool(b) => b,
                _ => unreachable!(),
            };

            Val::Bool(!expr_bool)
        }
        _ => unreachable!(),
    }
}

fn runtime_terminate() -> ! {
    // set hook to deactivate printing
    panic::set_hook(Box::new(|_| {
        // do nothing
    }));
    panic!()
}

// TODO : add line number and file ?
fn runtime_error(message : &str) -> ! {
    eprintln!("LANG RUNTIME ERROR : {}", message);
    runtime_terminate()
}


// TODO : add line number and file ?
fn rustaml_panic(message : &str) -> ! {
    eprintln!("PANIC in rustaml code : {}", message);
    runtime_terminate()
}

#[derive(DebugWithContext)]
#[debug_context(RustamlContext)]
enum FormatChunk {
    Arg(Val), // TODO ? : put ref instead
    Str(String), // TODO ? : put ref instead
}

#[derive(DebugWithContext)]
#[debug_context(RustamlContext)]
struct FormatChunks {
    format : Vec<FormatChunk>,
}

impl FormatChunks {
    fn new() -> FormatChunks {
        FormatChunks {
            format: Vec::new()
        }
    }

    fn append(&mut self, c : char){
        match self.format.last_mut() {
            Some(f_c) => {
                match f_c {
                    FormatChunk::Arg(_) => self.format.push(FormatChunk::Str(c.to_string())),
                    FormatChunk::Str(s) => s.push(c),
                }
            }
            None => self.format.push(FormatChunk::Str(c.to_string())),
        }
    }
}

fn interpret_format(context: &mut InterpretContext, arg_format_str: StringRef, args_format : &[Val]) -> Val {
    let mut format_chunks = FormatChunks::new();

    let arg_format_chars = arg_format_str.get_str(&context.rustaml_context.str_interner).chars().collect::<Vec<_>>();
    let mut pos = 0;

    let mut arg_pos = 0;
    while pos < arg_format_chars.len() {
        match arg_format_chars[pos] {
            '{' => {
                if let Some('}') = arg_format_chars.get(pos+1){
                    format_chunks.format.push(FormatChunk::Arg(args_format[arg_pos].clone()));
                    arg_pos += 1;
                    pos += 1; // pass '}'
                } else {
                    format_chunks.append('{');
                }
                
            },
            c => format_chunks.append(c),
        }
        pos += 1;
    }

    //dbg!(DebugWrapContext::new(&format_chunks, context.rustaml_context));

    let mut formatted_str = String::new();

    for f_c in format_chunks.format {
        match f_c {
            FormatChunk::Str(s) => formatted_str.push_str(&s),
            FormatChunk::Arg(v) => formatted_str.push_str(&format!("{}", v.display(context.rustaml_context))),
        }
    }

    Val::String(context.rustaml_context.str_interner.intern_runtime(&formatted_str))
}

fn interpret_map(context: &mut InterpretContext, list_val : Val, fun_val : Val) -> Val {
    let list = match list_val {
        Val::List(l) => l,
        _ => unreachable!(),
    };

    let fun_val = match fun_val {
        Val::Function(f) => f,
        _ => unreachable!(),
    };

    let mut vals= Vec::new();

    {
        let mut current = list.get(&context.rustaml_context.list_node_pool);

        while let List::Node(val, next ) = current { 
            vals.push(val.clone());
            current = next.get(&context.rustaml_context.list_node_pool);
        }
    }
    
    let mut new_list = List::None;

    
    // TODO : create a function which will be another new_from to create from a val slice to not go throught the whole list at each append ?
    for v in vals {
        let new_val = call_function(context, &fun_val, vec![v]);
        new_list.append(&mut context.rustaml_context.list_node_pool, new_val);
    }

    let new_list_ref = context.rustaml_context.list_node_pool.push(new_list);

    Val::List(new_list_ref)
}


fn interpret_filter(context: &mut InterpretContext, list_val : Val, fun_val : Val) -> Val {
    let list = match list_val {
        Val::List(l) => l,
        _ => unreachable!(),
    };

    let fun_val = match fun_val {
        Val::Function(f) => f,
        _ => unreachable!(),
    };

    let mut new_list = List::None;

    let mut vals= Vec::new();
    {
        let mut current = list.get(&context.rustaml_context.list_node_pool);

        while let List::Node(val, next ) = current { 
            vals.push(val.clone());
            current = next.get(&context.rustaml_context.list_node_pool);
        }
    }

    
    // TODO : create a function which will be another new_from to create from a val slice to not go throught the whole list at each append ?
    for v in vals {
        let should_append = call_function(context, &fun_val, vec![v.clone()]);
        let should_append_bool = match should_append {
            Val::Bool(b) => b,
            _ => unreachable!(),
        };

        if should_append_bool {
            new_list.append(&mut context.rustaml_context.list_node_pool, v);
        }
    }

    let new_list_ref = context.rustaml_context.list_node_pool.push(new_list);

    Val::List(new_list_ref)
}

fn interpret_chars(context : &mut InterpretContext, str : Val) -> Val {
    let str = match str {
        Val::String(s) => s,
        _ => unreachable!(),
    };
    let mut new_list = List::None;
    for c in str.get_str(&context.rustaml_context.str_interner).chars() {
        new_list.append(&mut context.rustaml_context.list_node_pool, Val::Char(c));
    }
    let new_list_ref = context.rustaml_context.list_node_pool.push(new_list);
    Val::List(new_list_ref)
}

const STD_FUNCTIONS : &[&str] = &[
    "print",
    "rand",
    "format",
    "panic",
    "map",
    "filter",
    "chars"
];

fn interpret_std_function(context: &mut InterpretContext, name : StringRef, args_val : Vec<Val>) -> Val {
    // TODO : better error handling for wrong nb of args
    match name.get_str(&context.rustaml_context.str_interner) {
        "print" => {
            // TODO : verification before when parsing ?
            assert_eq!(args_val.len(), 1);
            println!("{}", args_val[0].display(context.rustaml_context));
            Val::Unit
        },
        "rand" => {
            assert_eq!(args_val.len(), 1);
            assert!(matches!(args_val[0], Val::Unit));
            let rand_nb = context.rng.random::<i64>();
            Val::Integer(rand_nb)
        },
        "format" => {
            let (arg_first, args_format) = args_val.split_first().unwrap();
            let arg_format_str = match arg_first {
                Val::String(s) => *s,
                _ => panic!("expected string for format"),
            };

            interpret_format(context, arg_format_str, args_format)
        }
        "panic" => {
            assert_eq!(args_val.len(), 1);
            let message = format!("{}", args_val[0].display(context.rustaml_context)) ;
            rustaml_panic(&message)
        }
        // TODO : remove these clones
        "map" => {
            assert_eq!(args_val.len(), 2);
            let list = args_val[0].clone();
            let fun = args_val[1].clone();
            interpret_map(context, list, fun)
        }
        "filter" => {
            assert_eq!(args_val.len(), 2);
            let list = args_val[0].clone();
            let fun = args_val[1].clone();
            interpret_filter(context, list, fun)
        }
        "chars" => {
            assert_eq!(args_val.len(), 1);
            let s = args_val[0].clone();
            interpret_chars(context, s)
        }
        _ => unreachable!()
    }
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

pub fn call_function(context: &mut InterpretContext, func_def : &FunctionDef, args_val : Vec<Val>) -> Val {
    match &func_def.body {
        FunctionBody::Ast(a) => {
            let mut old_vals : Vec<(StringRef, Val)> = Vec::new();
            context.vars.reserve(func_def.args.len());
            for (arg_name, arg_val) in func_def.args.iter().zip(args_val) {
                let old_val = context.vars.insert(*arg_name, arg_val);
                if let Some(old_val) = old_val {
                    old_vals.push((*arg_name, old_val));
                }
            }
            let res_val = ensure_stack(|| interpret_node(context, *a));
            for arg_name in &func_def.args {
                context.vars.remove(arg_name);
            }
            for (old_name, old_val) in old_vals {
                context.vars.insert(old_name, old_val);
            }
            res_val
        },
        
        FunctionBody::Ffi(f) => call_ffi_function(context, f, &args_val),
    }    
}

fn interpret_function_call(context: &mut InterpretContext, callee : ASTRef, args : &[ASTRef]) -> Val {

    let args_val = args.iter().map(|e| interpret_node(context, *e)).collect::<Vec<_>>();

    if let ASTNode::VarUse { name } = callee.get(&context.rustaml_context.ast_pool) 
            && STD_FUNCTIONS.contains(&name.get_str(&context.rustaml_context.str_interner)){
        return interpret_std_function(context, *name, args_val);
    }

    let callee_val = interpret_node(context, callee);

    let func_def = match callee_val {
        Val::Function(f) => f,
        _ => unreachable!(),
    };    
    
    call_function(context, &func_def, args_val)
}

fn interpret_match_pattern(context: &mut InterpretContext, matched_val : &Val, pattern : PatternRef) -> bool {
    // TODO : remove this clone
    match pattern.get(&context.rustaml_context.pattern_pool).clone() {
        Pattern::VarName(_) | Pattern::Underscore => true,
        Pattern::Integer(nb) => {
            match matched_val {
                Val::Integer(matched_nb) => {
                    //dbg!((*nb, matched_nb));
                    nb == *matched_nb
                },
                _ => unreachable!(),
            }
        },
        Pattern::Float(nb) => {
            match matched_val {
                Val::Float(matched_nb) => {
                    nb == *matched_nb
                },
                _ => unreachable!(),
            }
        },
        Pattern::Bool(b) => {
            match matched_val {
                Val::Bool(matched_b) => {
                    b == *matched_b
                }
                _ => unreachable!(),
            }
        }
        Pattern::Range(start, end, inclusivity) => {
            match matched_val {
                Val::Integer(matched_nb) => {
                    if inclusivity {
                        start <= *matched_nb && *matched_nb <= end
                    } else {
                        start <= *matched_nb && *matched_nb < end
                    }
                },
                _ => unreachable!(),
            }
        },
        Pattern::String(s) => {
            match matched_val {
                Val::String(matched_str) => {
                    s == *matched_str
                },
                _ => unreachable!(),
            }
        },
        Pattern::Char(c) => {
            match matched_val {
                Val::Char(matched_char) => {
                    c == *matched_char
                }
                _ => unreachable!(),
            }
        }
        Pattern::SumTypeVariant(n) => {
            match matched_val {
                Val::SumType(s) => {
                    n == s.variant_name
                }
                _ => unreachable!(),
            }
        }
        Pattern::List(l) => {
            let matched_expr_list = match matched_val {
                Val::List(l) => l,
                _ => unreachable!(),
            };

            let matched_expr_list_node = matched_expr_list.get(&context.rustaml_context.list_node_pool);

            // if both empty
            if l.is_empty() && matches!(matched_expr_list_node, List::None){
                return true;
            }

            // TODO : maybe put len in the node to improve performance/create a cache for length ? (benchmark it/ add it as a feature ?)
            let matched_expr_list_len = matched_expr_list_node.len(&context.rustaml_context.list_node_pool);

            if matched_expr_list_len != l.len(){
                return false;
            }
                
            // TODO : refactor this if it is a performance problem (profile it ?)
            let mut pattern_matched_nb = 0;
            // TODO : remove these clones -need these because we can't borrow as mut context while borrowing those vals)
            let matched_list = matched_expr_list_node.iter(&context.rustaml_context.list_node_pool).cloned().collect::<Vec<_>>();
            for (&p, v) in l.iter().zip(matched_list) {
                if !interpret_match_pattern(context, &v, p){
                    return false;
                }
                pattern_matched_nb += 1;
            }


            // TODO : this len is hot code (optimize it)
            return pattern_matched_nb == l.len()
        },
        Pattern::ListDestructure(head_pattern, tail_pattern) => {
            let matched_expr_list = match matched_val {
                Val::List(l) => l,
                _ => unreachable!(),
            };

            if matched_expr_list.get(&context.rustaml_context.list_node_pool).empty(){
                return false;
            }

            let (head_val, tail) = match matched_expr_list.get(&context.rustaml_context.list_node_pool) {
                List::Node(head_val, next ) => (head_val.clone(), *next),
                List::None => unreachable!(),
            };

            if !interpret_match_pattern(context, &head_val, head_pattern){
                return false;
            }

            let tail_val = Val::List(tail);
            if !interpret_match_pattern(context, &tail_val, tail_pattern){
                return false;
            }

            return true;
        },
    }
}

fn handle_match_pattern_start(context: &mut InterpretContext, pattern : PatternRef, matched_expr_val : &Val){
    match pattern.get(&context.rustaml_context.pattern_pool) {
        Pattern::VarName(s) => { 
            context.vars.insert(*s, matched_expr_val.clone());
        },
        &Pattern::ListDestructure(head_pattern, tail_pattern) => {
            let matched_expr_list = match matched_expr_val {
                Val::List(l) => l,
                _ => unreachable!(),
            };
            let (head_val, tail) = match matched_expr_list.get(&context.rustaml_context.list_node_pool) {
                List::Node(val, next ) => (val.clone(), *next),
                List::None => unreachable!(),
            };
            handle_match_pattern_start(context, head_pattern, &head_val);
            let tail_val = Val::List(tail);
            handle_match_pattern_start(context, tail_pattern, &tail_val);
        }
        _ => {},
    }
}

fn handle_match_pattern_end(context: &mut InterpretContext, pattern : PatternRef){
    match pattern.get(&context.rustaml_context.pattern_pool) {
        Pattern::VarName(s) => { 
            context.vars.remove(s);
        },
        &Pattern::ListDestructure(head_pattern, tail_pattern) => {
            handle_match_pattern_end(context, head_pattern);
            handle_match_pattern_end(context, tail_pattern);
        },
        _ => {},
    }
}

fn interpret_match(context: &mut InterpretContext, matched_expr : ASTRef, patterns : &[(PatternRef, ASTRef)]) -> Val {
    let matched_expr_val = interpret_node(context, matched_expr);
    for (pattern, pattern_expr) in patterns {

        if interpret_match_pattern(context, &matched_expr_val, *pattern) {
            handle_match_pattern_start(context, *pattern, &matched_expr_val);
            let res_val = interpret_node(context, *pattern_expr);
            handle_match_pattern_end(context, *pattern);
            return res_val;
        }
    }

    panic!("No pattern was matched in match expressions (not exhaustive match)")
}

fn interpret_cast(context : &mut InterpretContext, _to_type : Type, expr : ASTRef) -> Val {
    let val = interpret_node(context, expr);
    // TODO
    val
}

fn interpret_variant(context : &mut InterpretContext, name : StringRef, _arg : Option<ASTRef>) -> Val {
    let mut sum_type_name_variant_nb = None;
    for (k, t) in &context.rustaml_context.type_aliases {
        match t {
            Type::SumType(sum_type) => {
                for (idx, v) in sum_type.variants.iter().enumerate() {
                    if v.name.as_ref() == name.get_str(&context.rustaml_context.str_interner){
                        sum_type_name_variant_nb = Some((*k, idx));
                    }
                }
            },
            _ => {},
        }
    }
    let (sum_type_name, variant_nb) = sum_type_name_variant_nb.unwrap();
    let sum_type_val = SumTypeVal { 
        sum_type_name, 
        variant_nb, 
        variant_name: name 
    };
    Val::SumType(sum_type_val)
}

// TODO: add a real call to collect_gc

pub fn interpret_node(context: &mut InterpretContext, ast: ASTRef) -> Val {
    let ast_node = ast.get(&context.rustaml_context.ast_pool).clone(); // remove the clone ? (because there are indexes in the ast node, the clone is not a deep copy)
    match ast_node {
        ASTNode::TopLevel { nodes } => {
            let mut last_node = Val::Unit;
            for node in nodes {
                last_node = interpret_node(context, node);

                #[cfg(feature = "gc-test-collect")]
                collect_gc(context, false);
            }
            last_node
        }
        ASTNode::FunctionDefinition { name, args, body, type_annotation: _ } => {
            let func_def = FunctionDef { 
                name, 
                args,
                body: FunctionBody::Ast(body),
                function_def_ast: Some(ast),
            };
            context.vars.insert(name, Val::Function(func_def));
            //context.functions.insert(name, func_def);
            Val::Unit
        },
        ASTNode::AnonFunc { args, body, type_annotation: _ } => {
            let func_def = FunctionDef {
                name: context.rustaml_context.str_interner.intern_compiler("anon_func"), // add an index to not have the same name for all closures ?
                args,
                body: FunctionBody::Ast(body),
                function_def_ast: Some(ast),
            };
            Val::Function(func_def)
        }
        ASTNode::ExternFunc { name, type_annotation, lang, so_str } => {
            //call_function_ffi(context, name, type_annotation, lang)
            let ffi_fun = get_ffi_func(context, name, type_annotation, lang, so_str);
            let func_def = FunctionDef { 
                name, 
                args: Box::new([]), // unused (TODO ?, not need to pass this ?)
                body: FunctionBody::Ffi(ffi_fun),
                function_def_ast: Some(ast),
            };
            context.vars.insert(name, Val::Function(func_def));
            Val::Unit
        }
        ASTNode::Float { nb } => Val::Float(nb),
        ASTNode::Integer { nb } => Val::Integer(nb.try_into().unwrap()),
        ASTNode::Boolean { b } => Val::Bool(b),
        ASTNode::Char { c } => Val::Char(c),
        ASTNode::VarDecl { name, val, body, var_type: _ } => {
            let val_node = interpret_node(context, val);
            let is_underscore = name.get_str(&context.rustaml_context.str_interner) == "_";
            if !is_underscore {
                context.vars.insert(name, val_node);
            }
            try_gc_collect(context);
            match body {
                Some(b) => {
                    let body_val = interpret_node(context, b);
                    if !is_underscore {
                        context.vars.remove(&name);
                    }
                    body_val
                },
                None => {
                    Val::Unit
                }
            }
            
        },
        ASTNode::VarUse { name } => context.vars.get(&name).unwrap_or_else(|| panic!("BUG interpreter : unknown var {}", name.get_str(&context.rustaml_context.str_interner))).clone(),
        ASTNode::BinaryOp { op, lhs, rhs } => interpret_binop(context, op, lhs, rhs),
        ASTNode::UnaryOp { op, expr } => interpret_unop(context, op, expr),
        ASTNode::FunctionCall { callee, args } => interpret_function_call(context, callee, &args),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => interpret_if_expr(context, cond_expr, then_body, else_body),
        ASTNode::MatchExpr { matched_expr, patterns } => interpret_match(context, matched_expr, patterns.as_ref()),
        ASTNode::String { str } => Val::String(str),
        ASTNode::List { list } => Val::List(List::new_from(context, &list)),
        ASTNode::Cast { to_type, expr } => interpret_cast(context, to_type, expr),
        ASTNode::Variant { name, arg } => interpret_variant(context, name, arg),
        ASTNode::TypeAlias { name: _, type_alias: _ } => {
            Val::Unit
        },
        ASTNode::Unit => Val::Unit,
        //n => panic!("unexpected ast node when interpreting : {:?}", n),
    }
}

pub fn interpret_with_val(ast: ASTRef, rustaml_context: &mut RustamlContext) -> Val {
    let mut context = InterpretContext {
        vars: FxHashMap::default(),
        // functions: FxHashMap::default(),
        rustaml_context,
        gc_context: GcContext::new(),
        rng: rand::rng(),
    };

    let v = interpret_node(&mut context, ast);

    
    debug_println!(context.rustaml_context.is_debug_print, "content = {:#?}", context);
    //dbg!(context);

    v
}

pub fn interpret(ast: ASTRef, rustaml_context: &mut RustamlContext){
    rustaml_context.start_section("interpreter");
    interpret_with_val(ast, rustaml_context);
    rustaml_context.end_section("interpreter");
}