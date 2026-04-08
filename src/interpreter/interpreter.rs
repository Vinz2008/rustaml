use nohash::IntMap;
use regex::Regex;
use smallvec::SmallVec;
use smallvec::smallvec;
use std::cmp::max;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display};
use std::rc::Rc;
use std::panic;
use debug_with_context::DebugWithContext;
use rand::prelude::*;

use crate::ast::ASTRef;
use crate::ast::PatternRef;
use crate::ast::TypeTag;
use crate::debug_println;

use crate::interpreter::gc::{try_gc_collect, Gc, GcContext};

use crate::rustaml::ensure_stack;
use crate::rustaml::RustamlContext;
use crate::string_intern::StringRef;
use crate::types::TypeInfos;
use crate::{ast::{ASTNode, Type, Pattern}, lexer::Operator};

// TODO : have trampolines to help with recursion ? is it even needed because there is JIT ?


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

cfg_if! {
    if #[cfg(feature = "jit")]{
        use crate::interpreter::jit::{update_jit_heuristics_function_start_call, should_use_jit_function, call_jit_function};
        use crate::interpreter::jit::JitContext;
        use std::time::Instant;
    }
}

// None values are freed lists that can be reused
#[derive(Clone)]
pub(crate) struct ListPool(pub(crate) Vec<Option<Gc<ListNode>>>);

impl ListPool {
    pub(crate) fn new() -> ListPool {
        ListPool(Vec::new())
    }

    fn get(&self, list_node : ListNodeRef) -> &ListNode {
        &self.0[list_node.0 as usize].as_ref().unwrap().data
    }

    fn get_mut(&mut self, list_node : ListNodeRef) -> &mut ListNode {
        &mut self.0[list_node.0 as usize].as_mut().unwrap().data
    }

    fn get_gc_mut(&mut self, list_node : ListNodeRef) -> &mut Gc<ListNode> {
        self.0[list_node.0 as usize].as_mut().unwrap()
    }

    fn free(&mut self, list_node : ListNodeRef) {
        let freed_node = self.0[list_node.0 as usize].take();

        let _freed_node = match freed_node {
            Some(n) => n,
            None => panic!("gc tried to free a None list node"),
        };
    }

    fn push(&mut self, node : ListNode) -> ListNodeRef {
        for (idx, e) in self.0.iter_mut().enumerate() {
            if e.is_none() {
                *e = Some(Gc::new(node));
                return ListNodeRef(idx.try_into().unwrap());
            }
        }


        let idx = self.0.len();
        self.0.push(Some(Gc::new(node)));
        ListNodeRef(idx.try_into().expect("too many list nodes in the pool"))
    }

    cfg_if! {
        if #[cfg(feature = "gc-test-print")] {

            pub(crate) fn nb_used_nodes(&self) -> usize {
                return self.0.iter().filter(|e| e.is_some()).count();
            }

            pub(crate) fn nb_free_nodes(&self) -> usize {
                return self.0.len() - self.nb_used_nodes();
            }
        }
    }


    pub(crate) fn nb_free_at_end(&self) -> usize {
        return self.0.iter().rev().take_while(|l| l.is_none()).count();
    }

    // TODO : heuristics for this
    pub(crate) fn shrink_end(&mut self, free_at_end : usize){
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
                        debug_l.entry_with(|f| {
                            f.debug_tuple("Node").field_with(|f| l.data.val.fmt_with_context(f, rustaml_context)).field(&l.data.next.as_ref().map(|e| e.0)).finish()
                        })
                    },
                    None => { 
                        debug_l.entry(&None::<()>)
                    }
                };
            }
            debug_l.finish()?;
            fmt::Result::Ok(())
        }).finish()
    }
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) struct ListNodeRef(u32); // >TODO : use NonZero to improve Option (look at also other refs, need to look if there is a lot of option, but would need to addd a -1 when really accessing data)

impl ListNodeRef {
    /// # Safety
    ///
    /// This function should only be called with known good indexes from the list pool
    pub(crate) unsafe fn new_unchecked(idx : u32) -> ListNodeRef {
        ListNodeRef(idx)
    }

    pub(crate) fn get(self, list_pool : &ListPool) -> &ListNode {
        list_pool.get(self)
    }

    pub(crate) fn get_mut(self, list_pool : &mut ListPool) -> &mut ListNode {
        list_pool.get_mut(self)
    }

    /*pub(crate) fn get_gc(self, list_pool : &ListPool) -> &Gc<List> {
        list_pool.get_gc(self)
    }*/
    
    pub(crate) fn get_gc_mut(self, list_pool : &mut ListPool) -> &mut Gc<ListNode> {
        list_pool.get_gc_mut(self)
    }

    pub(crate) fn free(self, list_pool : &mut ListPool) {
        list_pool.free(self)
    }
}

impl DebugWithContext<RustamlContext> for ListNodeRef {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        List {
            head: Some(*self),
        }.fmt_with_context(f, rustaml_context)
    }
}

// TODO : rework the layout ? (see https://rust-unofficial.github.io/too-many-lists/)
/*#[derive(Clone, Default)]
pub(crate) enum List {
    #[default]
    None,
    Node(Val, ListRef)
}*/


#[derive(Clone)]
pub(crate) struct ListNode {
    pub val: Val,
    pub next : Option<ListNodeRef>,
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) struct List {
    pub head: Option<ListNodeRef>,
}

impl List {
    // intepret nodes here instead of doing before the call and passing a Vec<Val> to avoid not necessary allocations
    fn new_from(context: &mut InterpretContext, v : &[ASTRef]) -> List {
        let mut list_ref = None;
        for e in v.iter().rev() {
            let val = interpret_node(context, *e);
            list_ref = Some(list_push_start(&mut context.rustaml_context.list_node_pool, val, list_ref));
            
        }
        List {
            head: list_ref,
        }
    }

    #[cfg(feature = "jit")]
    pub(crate) fn new_from_vals(context: &mut InterpretContext, vals : Vec<Val>) -> List {
        let mut list_ref = None;
        for val in vals.into_iter().rev() {
            list_ref = Some(list_push_start(&mut context.rustaml_context.list_node_pool, val, list_ref));
        }
        List {
            head: list_ref,
        }
    }

    pub(crate) fn len(&self, list_pool : &ListPool) -> usize {
        match self.head {
            Some(head) => head.get(list_pool).len(list_pool),
            None => 0,
        }
    }

    fn add_list_at_end(&mut self, list_pool : &mut ListPool, list : List){
        let mut current_ref = match self.head {
            Some(head_ref) => {
                head_ref
            }
            None => {
                self.head = list.head;
                return;
            }
        };

        while let Some(next_ref) = current_ref.get(list_pool).next {
            current_ref = next_ref;
        }

        let current = current_ref.get_mut(list_pool);
        current.next = list.head;
    }

    pub(crate) fn iter<'a>(self, list_pool : &'a ListPool) -> ListIter<'a> {
        ListIter { current: self.head, list_pool }
    }

    pub(crate) fn deep_clone(&self, list_pool : &mut ListPool) -> List {
        let head = match self.head {
            Some(l) => Some(l.get(list_pool).clone().deep_clone(list_pool)), // TODO : remove the additional clone ?
            None => None,
        };
        List { head }
    }
}


impl ListNode {
    fn new(val : Val, next : ListNodeRef) -> ListNode {
        ListNode {
            val,
            next: Some(next), 
        }
    }


    pub(crate) fn len(&self, list_pool : &ListPool) -> usize {
        let mut count = 1;

        let mut current: &ListNode = self;
        while let Some(next_ref) = current.next {
            count += 1;
            current = next_ref.get(list_pool);
        }
        count
    }

    pub(crate) fn deep_clone(&self, list_pool : &mut ListPool) -> ListNodeRef {
        let cloned_val = self.val.clone();
        let new_node = list_pool.push(ListNode { val: cloned_val, next: None });
        let head = new_node;
        let mut last_node = new_node;
        let mut current_node = self;
        while let Some(next_ref) = current_node.next {
            let cloned_val = next_ref.get(list_pool).val.clone();
            let new_node = list_pool.push(ListNode { val: cloned_val, next: None });
            last_node.get_mut(list_pool).next = Some(new_node);
            last_node = new_node;
            current_node = next_ref.get(list_pool);
        }

        head
    }
}


pub(crate) struct ListIter<'a> {
    current : Option<ListNodeRef>,
    list_pool : &'a ListPool,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = &'a Val;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            Some(current) => {
                let val = &current.get(self.list_pool).val;
                let next = current.get(self.list_pool).next;
                self.current = next;
                Some(val)
            }
            None => None,
        }
    }
}

impl DebugWithContext<RustamlContext> for List {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        let mut current = self.head;
        let mut iter_nb = 0;


        while let Some(node_ref) = current {
            if iter_nb != 0 {
                write!(f, ", ")?;
            }
            let node = node_ref.get(&rustaml_context.list_node_pool);
            node.val.fmt_with_context(f, rustaml_context)?;
            current = node.next;
            iter_nb += 1;
        }

        Ok(())
    }

}

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub(crate) struct SumTypeVal {
    // TODO : are there other ways to represent this ? (do I really need the sum_type_name and variant_nb ?)
    sum_type_name : StringRef,
    variant_nb : u32,
    variant_name : StringRef,
    // TODO : add val
}

#[derive(Clone)]
pub(crate) struct RegexWrapper(Regex);

impl RegexWrapper {
    fn new(re : &str) -> Result<RegexWrapper, regex::Error> {
        Ok(RegexWrapper(Regex::new(re)?))
    }
}

impl PartialEq for RegexWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl DebugWithContext<RustamlContext> for RegexWrapper {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, _context: &RustamlContext) -> fmt::Result {
        f.write_str(self.0.as_str())
    }
}


// TODO : transform this struct into an enum with predefined simd types as variants ? or just transform into a simd variant when doing the operations (add, etc)
#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub(crate) struct VecVal {
    vec : Box<[Val]>,
}


// TODO : instead of boxed function def and regex, use Refs to a pool ?
// TODO : also pooled vecs ? or only for non inlined (see the TODO on top of the VecVal struct)

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub(crate) enum Val {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(StringRef),
    Char(char),
    List(List),
    Function(Rc<FunctionDef>),
    SumType(SumTypeVal),
    Regex(Box<RegexWrapper>),
    Vec(VecVal),
    Tuple(Box<[Val]>),
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

pub(crate) type ArgsVec = SmallVec<[Val; 4]>;

pub(crate) struct ValWrapDisplay<'a> {
    val : &'a Val,
    rustaml_context: &'a RustamlContext,
}

// TODO : make this iterative instead of recursive (would make it only 1 function with aux func, but is it already optimized as tail call ? but on debug mode could it blow the stack ?)
fn _display_list(l : Option<ListNodeRef>, rustaml_context: &RustamlContext, f: &mut fmt::Formatter<'_>, is_first : bool) -> fmt::Result {
    match l {
        Some(head_ref) => {
            if !is_first  {
                write!(f, ", ")?;
            }
            let head_node = head_ref.get(&rustaml_context.list_node_pool);
            let next = head_node.next;
            let e = &head_node.val;
            let e_wrap = ValWrapDisplay {
                val: e,
                rustaml_context,
            };
            write!(f, "{}", e_wrap)?;
            
            _display_list(next, rustaml_context, f, false)
        },
        None => fmt::Result::Ok(()),
    }
}

fn display_list(l : List, rustaml_context: &RustamlContext, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[")?;
    _display_list(l.head, rustaml_context, f, true)?;
    write!(f, "]")
}

fn display_vec(v : &VecVal, rustaml_context: &RustamlContext, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "vec[")?;
    let mut first = true;
    for e in &v.vec {
        if !first {
            write!(f, ", ")?;
        }
        let e_wrap = ValWrapDisplay {
            val: e,
            rustaml_context,
        };
        write!(f, "{}", e_wrap)?;

        first = false;
    }
    write!(f, "]")
}

fn display_tuple(tuple : &[Val], rustaml_context: &RustamlContext, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    let mut first = true;
    for e in tuple {
        if !first {
            write!(f, ", ")?;
        }
        let e_wrap = ValWrapDisplay {
            val: e,
            rustaml_context,
        };
        write!(f, "{}", e_wrap)?;

        first = false;
    }
    write!(f, ")")
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
            Val::Vec(v) => display_vec(v, self.rustaml_context, f),
            Val::Regex(re) => write!(f, "regex({})", re.0.as_str()),
            Val::Tuple(tuple) => display_tuple(tuple, self.rustaml_context, f),
            Val::Function(_) => write!(f, "function"), // TODO ?
            Val::SumType(_) => todo!(), // TODO
            Val::Unit => write!(f, "()"),
        }
    }
}

impl Val {
    pub(crate) fn display<'a>(&'a self, rustaml_context: &'a RustamlContext) -> ValWrapDisplay<'a> {
        ValWrapDisplay { 
            val: self, 
            rustaml_context  
        }
    }
}

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub(crate) enum FunctionBody {
    Ast(ASTRef),
    Ffi(FFIFunc), // TODO : should it be a Rc to prevent cloning it from being very costly and to reduce the size of the enum
}

#[derive(Clone, PartialEq, DebugWithContext)]
#[debug_context(RustamlContext)]
pub(crate) struct FunctionDef {
    pub name : StringRef,
    pub(crate) args : Box<[StringRef]>,
    pub(crate) body : FunctionBody,
    pub(crate) function_def_ast : Option<ASTRef>,
}

impl FunctionDef {
    pub(crate) fn new_ffi(context : &mut InterpretContext, ffi_func : FFIFunc) -> FunctionDef {
        FunctionDef { 
            name: context.rustaml_context.str_interner.intern_runtime("<FFI function>"), 
            args: vec![].into_boxed_slice(), // TODO ? 
            body: FunctionBody::Ffi(ffi_func), 
            function_def_ast: None, 
        }
    }
}

pub(crate) struct InterpretContext<'context> {
    pub(crate) vars: IntMap<StringRef, Val>,
    pub(crate) rustaml_context : &'context mut RustamlContext,
    pub(crate) gc_context : GcContext,
    rng : ThreadRng,

    #[cfg(feature = "jit")]
    pub(crate) jit_context : JitContext,
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


fn interpret_binop_bool_short_circuiting(context: &mut InterpretContext, op : Operator, lhs : ASTRef, rhs : ASTRef) -> Val {

    let lhs_bool = {
        // put lhs_val in a scope like that to ensure tail cal
        let lhs_val = interpret_node(context, lhs);
        match lhs_val {
            Val::Bool(b) => b,
            _ => unreachable!(),
        }
    };

    match op {
        Operator::And => {
            if !lhs_bool {
                return Val::Bool(false);
            }
        }
        Operator::Or => {
            if lhs_bool {
                return Val::Bool(true);
            }
        }
        _ => unreachable!(),
    }


    interpret_node(context, rhs) // if no short circuiting, just verify if the rhs is true
}

fn interpret_binop_bool(op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let b = match op {
        Operator::IsEqual => lhs_val == rhs_val,
        Operator::IsNotEqual => lhs_val != rhs_val,
        Operator::SuperiorOrEqual => lhs_val >= rhs_val,
        Operator::InferiorOrEqual => lhs_val <= rhs_val,
        Operator::Superior => lhs_val > rhs_val,
        Operator::Inferior => lhs_val < rhs_val,
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

fn list_push_start(list_node_pool : &mut ListPool, lhs_val : Val, rhs_list : Option<ListNodeRef>) -> ListNodeRef {
    list_node_pool.push(ListNode { 
        val: lhs_val, 
        next: rhs_list 
    })
}

fn interpret_binop_list(context: &mut InterpretContext, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {

    let rhs_list = match rhs_val {
        Val::List(l) => l,
        _ => unreachable!(),
    };

    let v = match op {
        // TODO : call add_allocation for gc in these cases
        Operator::ListAppend => {
            let head = list_push_start(&mut context.rustaml_context.list_node_pool, lhs_val, rhs_list.head);
            Val::List(List { head: Some(head) })
        },
        Operator::ListMerge => {
            // TODO : optimize this ?
            let lhs_list = match lhs_val {
                Val::List(l) => l,
                _ => unreachable!(),
            };

            let mut cloned_lhs = lhs_list.deep_clone(&mut context.rustaml_context.list_node_pool);
            cloned_lhs.add_list_at_end(&mut context.rustaml_context.list_node_pool, rhs_list);


            Val::List(cloned_lhs)
        }
        _ => unreachable!(),
    };
    // TODO : activate this
    //try_gc_collect(context);
    
    v
}

fn interpret_binop_vec(context: &mut InterpretContext, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    let lhs_vec = match lhs_val {
        Val::Vec(v) => v,
        _ => unreachable!(),
    };
    let rhs_vec = match rhs_val {
        Val::Vec(v) => v,
        _ => unreachable!(),
    };

    assert_eq!(lhs_vec.vec.len(), rhs_vec.vec.len());
    let vec_len = lhs_vec.vec.len();

    let scalar_op = match (op, lhs_vec.vec.first().unwrap()) {
        (Operator::PlusVec, Val::Integer(_)) => Operator::Plus,
        (Operator::PlusVec, Val::Float(_)) => Operator::PlusFloat,
        (Operator::MinusVec, Val::Integer(_)) => Operator::Minus,
        (Operator::MinusVec, Val::Float(_)) => Operator::MinusFloat,
        (Operator::MultVec, Val::Integer(_)) => Operator::Mult,
        (Operator::MultVec, Val::Float(_)) => Operator::MultFloat,
        (Operator::DivVec, Val::Integer(_)) => Operator::Div,
        (Operator::DivVec, Val::Float(_)) => Operator::DivFloat,
        _ => unreachable!(),
    };

    let mut res_vec = Vec::with_capacity(vec_len);
    for (e1, e2) in lhs_vec.vec.into_iter().zip(rhs_vec.vec) {
        let res_val = interpret_binop_val(context, scalar_op, e1, e2);
        res_vec.push(res_val);
    }
    Val::Vec(VecVal { vec: res_vec.into_boxed_slice() })
}

fn interpret_binop_val(context: &mut InterpretContext, op : Operator, lhs_val : Val, rhs_val : Val) -> Val {
    match op.get_res_type() {
        TypeTag::Integer => interpret_binop_int(op, lhs_val, rhs_val),
        TypeTag::Float => interpret_binop_float(op, lhs_val, rhs_val),
        TypeTag::Bool => interpret_binop_bool(op, lhs_val, rhs_val),
        TypeTag::Str => interpret_binop_str(context, op, lhs_val, rhs_val),
        TypeTag::List => interpret_binop_list(context, op, lhs_val, rhs_val),
        TypeTag::Vec => interpret_binop_vec(context, op, lhs_val, rhs_val),
        _ => unreachable!(),
    }

}

fn interpret_binop(context: &mut InterpretContext, op : Operator, lhs : ASTRef, rhs : ASTRef) -> Val {
    match op {
        Operator::And | Operator::Or => return interpret_binop_bool_short_circuiting(context, op, lhs, rhs),
        _ => {}
    }

    let lhs_val = interpret_node(context, lhs);
    let rhs_val = interpret_node(context, rhs);

    interpret_binop_val(context, op, lhs_val, rhs_val)
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

//#[derive(DebugWithContext)]
//#[debug_context(RustamlContext)]
enum FormatChunk<'a> {
    Arg(&'a Val),
    Str(String), // TODO ? : put ref instead
}

//#[derive(DebugWithContext)]
//#[debug_context(RustamlContext)]
struct FormatChunks<'a> {
    format : Vec<FormatChunk<'a>>,
}

impl<'a> FormatChunks<'a> {
    fn new() -> FormatChunks<'a> {
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

    let mut pos = 0;

    let formatted_str_ref = if !arg_format_str.get_str(&context.rustaml_context.str_interner).contains("{"){
        // just a string
        arg_format_str
    } else {

        // TODO : optimize this to not do a lot of append ?
        let arg_format_chars = arg_format_str.get_str(&context.rustaml_context.str_interner).chars().collect::<Vec<_>>();
        let mut arg_pos = 0;
        while pos < arg_format_chars.len() {
            match arg_format_chars[pos] {
                '{' => {
                    if let Some('}') = arg_format_chars.get(pos+1){
                        format_chunks.format.push(FormatChunk::Arg(&args_format[arg_pos]));
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
        context.rustaml_context.str_interner.intern_runtime(&formatted_str)
    };

    Val::String(formatted_str_ref)
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

    let vals= list.iter(&context.rustaml_context.list_node_pool).cloned().collect::<Vec<_>>();
    
    let mut new_list_ref = None;

    
    // TODO : create a function which will be another new_from to create from a val slice to not go throught the whole list at each append ?
    for v in vals.into_iter().rev() {
        let new_val = call_function(context, &fun_val, smallvec![v]);
        new_list_ref = Some(list_push_start(&mut context.rustaml_context.list_node_pool, new_val, new_list_ref));
    }


    Val::List(List {
        head: new_list_ref,
    })
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

    let vals = list.iter(&context.rustaml_context.list_node_pool).cloned().collect::<Vec<_>>();

    let mut new_list_ref = None;    

    // TODO : create a function which will be another new_from to create from a val slice to not go throught the whole list at each append ?
    for v in vals.into_iter().rev() {
        let should_append = call_function(context, &fun_val, smallvec![v.clone()]);
        let should_append_bool = match should_append {
            Val::Bool(b) => b,
            _ => unreachable!(),
        };

        if should_append_bool {
            new_list_ref = Some(list_push_start(&mut context.rustaml_context.list_node_pool, v, new_list_ref));
        }
    }

    Val::List(List {
        head: new_list_ref
    })
}

fn interpret_chars(context : &mut InterpretContext, str : Val) -> Val {
    let str = match str {
        Val::String(s) => s,
        _ => unreachable!(),
    };
    let mut new_list_ref = None;
    for c in str.get_str(&context.rustaml_context.str_interner).chars().rev() {
        new_list_ref = Some(list_push_start(&mut context.rustaml_context.list_node_pool, Val::Char(c), new_list_ref));
    }
    Val::List(List {
        head: new_list_ref,
    })
}

fn interpret_regex_create(context : &InterpretContext, str : Val) -> Val {
    let s = match str {
        Val::String(s) => s,
        _ => unreachable!(),
    };
    let re = match RegexWrapper::new(s.get_str(&context.rustaml_context.str_interner)){
        Ok(re) => re,
        Err(e) => runtime_error(&format!("Error when creating regex : {}", e)),
    };
    Val::Regex(Box::new(re))
}

fn interpret_regex_has_match(context : &InterpretContext, re : Val, str : Val) -> Val {
    let re = match re {
        Val::Regex(re) => re,
        _ => unreachable!(),
    };
    let s = match str {
        Val::String(s) => s,
        _ => unreachable!(),
    };
    let b = re.0.is_match(s.get_str(&context.rustaml_context.str_interner));
    Val::Bool(b)
}

pub(crate) const STD_FUNCTIONS : &[&str] = &[
    "print",
    "rand",
    "format",
    "panic",
    "map",
    "filter",
    "chars",
    "regex_create",
    "regex_has_match",
    "black_box",
];

fn interpret_std_function(context: &mut InterpretContext, name : StringRef, args_val : ArgsVec) -> Val {
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
                _ => panic!("expected string for format"), // TODO : replace the panics, expects, etc, with runtime_error
            };

            interpret_format(context, arg_format_str, args_format)
        }
        "panic" => {
            assert_eq!(args_val.len(), 1);
            let message = format!("{}", args_val[0].display(context.rustaml_context)) ;
            rustaml_panic(&message)
        }
        // TODO : remove these clones (do let [a, b] = args_val.try_into())
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
        "regex_create" => {
            assert_eq!(args_val.len(), 1);
            let s = args_val[0].clone();
            interpret_regex_create(context, s)
        }
        "regex_has_match" => {
            assert_eq!(args_val.len(), 2);
            let re = args_val[0].clone();
            let s = args_val[1].clone();
            interpret_regex_has_match(context, re, s)
        }
        "black_box" => {
            Val::Unit
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

pub(crate) fn call_function(context: &mut InterpretContext, func_def : &FunctionDef, args_val : ArgsVec) -> Val {
    match &func_def.body {
        FunctionBody::Ast(a) => {
            cfg_if! {
                if #[cfg(feature = "jit")]{
                    if should_use_jit_function(context, func_def){

                        return call_jit_function(context, func_def, args_val);
                    }

                    
                    update_jit_heuristics_function_start_call(context, *a);
                    let start_time = Instant::now();
                }
            }
            
            let mut new_vars : Vec<StringRef> = Vec::with_capacity(func_def.args.len()); // new vars vecs to known which are needed to be removed (which are the one that have no old_vals so they will not be replaced)
            let mut old_vals : Vec<(StringRef, Val)> = Vec::with_capacity(func_def.args.len());
            context.vars.reserve(func_def.args.len());
            for (arg_name, arg_val) in func_def.args.iter().zip(args_val) {
                let old_val = context.vars.insert(*arg_name, arg_val);
                match old_val {
                    Some(old_val) => old_vals.push((*arg_name, old_val)),
                    None => new_vars.push(*arg_name),
                }
            }

            let res_val = ensure_stack(|| interpret_node(context, *a));
            
             

            cfg_if! {
                if #[cfg(feature = "jit")]{
                    use crate::interpreter::jit::update_jit_heuristics_function_end_call;
                    let duration = Instant::now()-start_time;
                    update_jit_heuristics_function_end_call(context, *a, duration);
                }
            }

            for arg_name in new_vars {
                context.vars.remove(&arg_name);
            }
            for (old_name, old_val) in old_vals {
                context.vars.insert(old_name, old_val);
            }
            res_val
        },
        
        FunctionBody::Ffi(f) => call_ffi_function(context, f, args_val),
    } 
}

fn interpret_function_call(context: &mut InterpretContext, callee : ASTRef, args : Box<[ASTRef]>) -> Val {

    let args_val = args.iter().map(|e| interpret_node(context, *e)).collect::<ArgsVec>();

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

            // if both empty
            if l.is_empty() && matched_expr_list.head.is_none() {
                return true;
            }

            // TODO : maybe put len in the node to improve performance/create a cache for length ? (benchmark it/ add it as a feature ?)
            let matched_expr_list_len = matched_expr_list.len(&context.rustaml_context.list_node_pool);

            if matched_expr_list_len != l.len(){
                return false;
            }
                
            // TODO : refactor this if it is a performance problem (profile it ?)
            let mut pattern_matched_nb = 0;
            // TODO : remove these clones -need these because we can't borrow as mut context while borrowing those vals)
            let matched_list = matched_expr_list.iter(&context.rustaml_context.list_node_pool).cloned().collect::<Vec<_>>();
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
                Val::List(l) => *l,
                _ => unreachable!(),
            };

            let (head_val, tail) = match matched_expr_list.head {
                Some(node_ref) => {
                    let node = node_ref.get(&context.rustaml_context.list_node_pool);
                    (node.val.clone(), node.next)
                }
                None => return false,
            };

            if !interpret_match_pattern(context, &head_val, head_pattern){
                return false;
            }

            let tail_val = Val::List(List { head: tail });
            interpret_match_pattern(context, &tail_val, tail_pattern)
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

            let (head_val, tail) = match matched_expr_list.head {
                Some(node_ref) => {
                    let node = node_ref.get(&context.rustaml_context.list_node_pool);
                    (node.val.clone(), node.next)
                }
                None => unreachable!(),
            };


            handle_match_pattern_start(context, head_pattern, &head_val);
            let tail_val = Val::List(List { head: tail });
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

fn interpret_match(context: &mut InterpretContext, matched_expr : ASTRef, patterns : Box<[(PatternRef, ASTRef)]>) -> Val {
    let matched_expr_val = interpret_node(context, matched_expr);
    for (pattern, pattern_expr) in patterns {

        if interpret_match_pattern(context, &matched_expr_val, pattern) {
            handle_match_pattern_start(context, pattern, &matched_expr_val);
            let res_val = interpret_node(context, pattern_expr);
            handle_match_pattern_end(context, pattern);
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

fn interpret_variant(context : &InterpretContext, sum_type_name : StringRef, variant_name : StringRef, _arg : Option<ASTRef>) -> Val {
    let enum_ref = context.rustaml_context.type_aliases.get(&sum_type_name).unwrap();
    let variant_nb = match enum_ref {
        Type::SumType(sum_type) => {

            // TODO : should variants be a hashmap ? (benchmark it)
            // do intern compiler because it should already be in it (use a get string ref function ?)
            sum_type.variants.iter()
            .position(|var| var.get_name() == variant_name.get_str(&context.rustaml_context.str_interner)).unwrap().try_into().unwrap()

        }
        _ => panic!("trying to use a not enum type to create an enum variant"),
    };
    /*let mut sum_type_name_variant_nb = None;

    for (k, t) in &context.rustaml_context.type_aliases {
        match t {
            Type::SumType(sum_type) => {
                for (idx, v) in sum_type.variants.iter().enumerate() {
                    if v.get_name() == name.get_str(&context.rustaml_context.str_interner){
                        sum_type_name_variant_nb = Some((*k, idx));
                    }
                }
            },
            _ => {},
        }
    }
    let (sum_type_name, variant_nb) = sum_type_name_variant_nb.unwrap();*/
    let sum_type_val = SumTypeVal { 
        sum_type_name, 
        variant_nb, 
        variant_name, 
    };
    Val::SumType(sum_type_val)
}

fn interpret_static_vec(context: &mut InterpretContext, vec : Box<[ASTRef]>) -> Val {
    let vec_val = VecVal { 
        vec: vec.iter().map(|e| interpret_node(context, *e)).collect::<Box<[_]>>(), 
    };
    Val::Vec(vec_val)
}

fn interpret_tuple(context : &mut InterpretContext, tuple : Box<[ASTRef]>) -> Val {
    let tuple_vals = tuple.iter().map(|e| interpret_node(context, *e)).collect::<Box<[_]>>();
    Val::Tuple(tuple_vals)
}

// TODO: add a real call to collect_gc

pub(crate) fn interpret_node(context: &mut InterpretContext, ast: ASTRef) -> Val {
    match ast.get(&context.rustaml_context.ast_pool) {
        ASTNode::TopLevel { nodes } => {
            let nodes = nodes.clone();
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
                name: *name, 
                args: args.clone(),
                body: FunctionBody::Ast(*body),
                function_def_ast: Some(ast),
            };
            context.vars.insert(*name, Val::Function(Rc::new(func_def)));
            Val::Unit
        },
        ASTNode::AnonFunc { args, body, type_annotation: _ } => {
            let func_def = FunctionDef {
                name: context.rustaml_context.str_interner.intern_compiler("anon_func"), // add an index to not have the same name for all closures ?
                args: args.clone(),
                body: FunctionBody::Ast(*body),
                function_def_ast: Some(ast),
            };
            Val::Function(Rc::new(func_def))
        }
        ASTNode::ExternFunc { name, type_annotation, lang, so_str } => {
            let (name, type_annotation, lang, so_str) = (*name, type_annotation.clone(), *lang, *so_str);
            let ffi_fun = get_ffi_func(context, name, type_annotation, lang, so_str);
            let func_def = FunctionDef { 
                name, 
                args: Box::new([]), // unused (TODO ?, not need to pass this ?)
                body: FunctionBody::Ffi(ffi_fun),
                function_def_ast: Some(ast),
            };
            context.vars.insert(name, Val::Function(Rc::new(func_def)));
            Val::Unit
        }
        ASTNode::Float { nb } => Val::Float(*nb),
        ASTNode::Integer { nb } => Val::Integer((*nb).try_into().unwrap()),
        ASTNode::Boolean { b } => Val::Bool(*b),
        ASTNode::Char { c } => Val::Char(*c),
        ASTNode::VarDecl { name, val, body, var_type: _ } => {
            let (name, val, body) = (*name, *val, *body);
            let val_node = interpret_node(context, val);
            let is_underscore = name.get_str(&context.rustaml_context.str_interner) == "_";
            let old_val = if !is_underscore {
                context.vars.insert(name, val_node)
            } else {
                None
            };
            try_gc_collect(context);
            match body {
                Some(b) => {
                    let body_val = interpret_node(context, b);
                    if !is_underscore {
                        match old_val {
                            Some(old_val) => context.vars.insert(name, old_val),
                            None => context.vars.remove(&name),
                        };
                    }
                    body_val
                },
                None => {
                    Val::Unit
                }
            }
            
        },
        ASTNode::VarUse { name } => context.vars.get(name).unwrap_or_else(|| panic!("BUG interpreter : unknown var {}", name.get_str(&context.rustaml_context.str_interner))).clone(),
        ASTNode::BinaryOp { op, lhs, rhs } => interpret_binop(context, *op, *lhs, *rhs),
        ASTNode::UnaryOp { op, expr } => interpret_unop(context, *op, *expr),
        ASTNode::FunctionCall { callee, args } => interpret_function_call(context, *callee, args.clone()),
        ASTNode::IfExpr { cond_expr, then_body, else_body } => interpret_if_expr(context, *cond_expr, *then_body, *else_body),
        ASTNode::MatchExpr { matched_expr, patterns } => {
            let (matched_expr, patterns) = (*matched_expr, patterns.clone());
            interpret_match(context, matched_expr, patterns)
        },
        ASTNode::String { str } => Val::String(*str),
        ASTNode::List { list } => Val::List(List::new_from(context, &list.clone())),
        ASTNode::Vec { vec } => interpret_static_vec(context, vec.clone()),
        ASTNode::Cast { to_type, expr } => interpret_cast(context, to_type.clone(), *expr),
        ASTNode::Variant { sum_type_name: enum_name, variant_name, arg } => interpret_variant(context, *enum_name, *variant_name, *arg),
        ASTNode::Tuple { tuple_vals } => interpret_tuple(context, tuple_vals.clone()),
        ASTNode::TypeAlias { name: _, type_alias: _ } => {
            Val::Unit
        },
        ASTNode::Unit => Val::Unit,
        //n => panic!("unexpected ast node when interpreting : {:?}", n),
    }
}

pub(crate) fn interpret_with_val(ast: ASTRef, rustaml_context: &mut RustamlContext, type_infos : Option<TypeInfos>, dump_jit_ir : bool, dump_jit_asm : bool) -> Val {
    let mut context = InterpretContext {
        vars: IntMap::default(),
        rustaml_context,
        gc_context: GcContext::new(),
        rng: rand::rng(),

        #[cfg(feature = "jit")]
        jit_context: JitContext::new(type_infos, dump_jit_ir, dump_jit_asm),
    };

    #[cfg(not(feature = "jit"))]{
        let _ = (dump_jit_ir, dump_jit_asm, type_infos);
    }

    let v = interpret_node(&mut context, ast);

    
    debug_println!(context.rustaml_context.is_debug_print, "content = {:#?}", context);
    //dbg!(context);

    v
}

pub(crate) fn interpret(ast: ASTRef, rustaml_context: &mut RustamlContext, type_infos : Option<TypeInfos>, dump_jit_ir: bool, dump_jit_asm : bool){
    rustaml_context.start_section("interpreter");
    interpret_with_val(ast, rustaml_context, type_infos, dump_jit_ir, dump_jit_asm);
    rustaml_context.end_section("interpreter");
}