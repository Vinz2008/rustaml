use std::collections::VecDeque;
use debug_with_context::{DebugWithContext, DebugWrapContext};

use crate::{interpreter::{InterpretContext, List, ListPool, ListRef, Val}, rustaml::RustamlContext, string_intern::{StrInterned, StrInterner, StringRef}};

pub struct GcContext {
    bytes_allocated : usize,
    bytes_for_next_gc : usize,
}

impl GcContext {
    const GC_HEAP_GROW_FACTOR : usize = 2; // TODO : change ?

    pub const fn new() -> GcContext {
        GcContext { 
            bytes_allocated: 0, 
            bytes_for_next_gc: 1024, // TODO : change ?
        }
    }

    pub fn add_allocation(&mut self, allocated_size : usize){
        self.bytes_allocated += allocated_size;
    }
}

#[derive(Debug, DebugWithContext)]
#[debug_context(RustamlContext)]
pub struct Gc<T> {
    pub data : T,
    pub is_marked : bool,
}

impl<T> Gc<T> {
    pub fn new(data : T) -> Gc<T> {
        Gc {
            data,
            is_marked: false,
        }
    }
}

pub fn collect_gc(context : &mut InterpretContext, update_next_limit : bool){
    #[cfg(feature = "gc-test-print")]
    println!("-- COLLECT GC START (done at {} Mb allocated)", context.gc_context.bytes_allocated/1_000_000);

    mark_and_sweep_list_nodes(context);

    mark_and_sweep_strings(context);

    if update_next_limit {
        context.gc_context.bytes_for_next_gc = context.gc_context.bytes_allocated * GcContext::GC_HEAP_GROW_FACTOR;
    }

    #[cfg(feature = "gc-test-print")]
    println!("-- COLLECT GC END (next at {} Mb allocated)", context.gc_context.bytes_for_next_gc/1_000_000);
}

// TODO : create a iter_roots iterator for iterating roots ?


fn shrink_list_pool_if_needed(list_node_pool : &mut ListPool){
    let free_at_end = list_node_pool.nb_free_at_end();
    // TODO : do heuristics for the 1/3 part ? (what should be the ratio ?)
    if free_at_end > list_node_pool.0.len()/3 {
        list_node_pool.shrink_end(free_at_end);
    }
}

fn does_list_contain_lists(l : &List) -> bool {
    match l {
        List::Node(val, _) => matches!(val, Val::List(_)),
        List::None => false,
    }
}

// only call this on val from a list that you have checked the first element is a list (with does_list_contain_lists), so all elements are
fn val_to_list_ref_unchecked(val: &Val) -> ListRef {
    match val {
        Val::List(l) => *l,
        _ => unreachable!(),
    }
}

fn mark_list_ref(list_node_pool : &mut ListPool, grey_stack : &mut VecDeque<ListRef>, l : ListRef){
    {
        let l_gc = l.get_gc_mut(list_node_pool);
        l_gc.is_marked = true;
        match l_gc.data {
            List::Node(_, next) => {
                grey_stack.push_back(next);
            },
            List::None => {}
        }
    }

    // is node a list
    // only recursion in case of lists of lists (there is rarely nesting of types in a very deep way, so it is safe)
    if does_list_contain_lists(l.get(list_node_pool)){
        // TODO : refactor to not have this vec ?
        let val_list_refs = l.get(list_node_pool).iter(list_node_pool).map(val_to_list_ref_unchecked).collect::<Vec<_>>();
        for val_list_ref in val_list_refs {
            mark_list_ref(list_node_pool, grey_stack, val_list_ref);
        }
        
    }
}

#[cfg(feature = "gc-test-print")]
fn helper_print_lists(list_node_pool : &ListPool, is_before : bool){
    let when = if is_before { "BEFORE" } else { "AFTER" };
    println!("GC : LIST NODES {} -> {} (used : {}, free : {}, capacity : {})", when, list_node_pool.0.len(), list_node_pool.nb_used_nodes(), list_node_pool.nb_free_nodes(), list_node_pool.0.capacity());
}

fn mark_and_sweep_list_nodes(context : &mut InterpretContext){
    
    #[cfg(feature = "gc-test-print")]
    helper_print_lists(&context.rustaml_context.list_node_pool, true);

    let mut grey_stack = VecDeque::new();

    // mark
    for var_val in context.vars.values() {
        if let Val::List(l) = var_val {
            mark_list_ref(&mut context.rustaml_context.list_node_pool, &mut grey_stack, *l);
        }
    }

    
    while !grey_stack.is_empty(){
        //println!("{:?}", DebugWrapContext::new(&context.rustaml_context.list_node_pool, context.rustaml_context));
        let grey_node = grey_stack.pop_front().unwrap();
        mark_list_ref(&mut context.rustaml_context.list_node_pool, &mut grey_stack, grey_node);
    }

    #[cfg(feature = "gc-test-print")]
    println!("after marking : {:?}", DebugWrapContext::new(&context.rustaml_context.list_node_pool, context.rustaml_context));

    // sweep

    let mut lists_to_free = Vec::new();
    for (idx, list) in context.rustaml_context.list_node_pool.0.iter_mut().enumerate() {
        if let Some(l) = list {
            if !l.is_marked {
                let list_ref = unsafe { ListRef::new_unchecked(idx.try_into().unwrap()) };
                lists_to_free.push(list_ref);
            } else {
                // to make at the end the every node unmarked
                l.is_marked = false;
            }
        }
    }

    for l in lists_to_free {
        l.free(&mut context.rustaml_context.list_node_pool);
    }


    // TODO : if a certain part of the end of the list pool is None (for example one third), shrink the vec

    shrink_list_pool_if_needed(&mut context.rustaml_context.list_node_pool);

    #[cfg(feature = "gc-test-print")]
    helper_print_lists(&context.rustaml_context.list_node_pool, false);
}

fn mark_str_ref(str_interner : &mut StrInterner, s : StringRef){
    let gc_str = s.get_gc_mut(str_interner);
    gc_str.is_marked = true;

}

fn shrink_string_pool_if_needed(string_interner : &mut StrInterner){
    let free_at_end = string_interner.nb_free_at_end();
    // TODO : do heuristics for the 1/3 part ? (what should be the ratio ?)
    if free_at_end > string_interner.len()/3 {
        string_interner.shrink_end(free_at_end);
    }
}

#[cfg(feature = "gc-test-print")]
fn helper_print_strings(str_interner : &StrInterner, is_before : bool){
    let when = if is_before { "BEFORE" } else { "AFTER" };
    println!("GC : LIST STRINGS {} -> {} (used by runtime : {}, used by compile time : {}, free : {}, capacity : {})", when, str_interner.len(), str_interner.runtime_nb(), str_interner.compiler_nb(), str_interner.free_nb(), str_interner.capacity());
}

fn mark_and_sweep_strings(context : &mut InterpretContext){
    #[cfg(feature = "gc-test-print")]
    helper_print_strings(&context.rustaml_context.str_interner, true);


    // mark
    for var_val in context.vars.values() {
        if let Val::String(s) = var_val {
            mark_str_ref(&mut context.rustaml_context.str_interner, *s);
        }
    }

    #[cfg(feature = "gc-test-print")]
    println!("after marking : {:?}", DebugWrapContext::new(&context.rustaml_context.str_interner, context.rustaml_context));

    // sweep

    let mut strs_to_free = Vec::new();
    for (idx, str) in context.rustaml_context.str_interner.strs.iter_mut().enumerate() {
        match str {
            StrInterned::Compiler(_) => {},
            StrInterned::Runtime(str) => {
                if let Some(s) = str {
                    if !s.is_marked {
                        let str_ref = unsafe { StringRef::new_unchecked(idx.try_into().unwrap()) };
                        strs_to_free.push(str_ref);
                    } else {
                        s.is_marked = false;
                    }
                }
            }
        }
    }

    for str_to_free in strs_to_free {
        str_to_free.free(&mut context.rustaml_context.str_interner);
    }

    shrink_string_pool_if_needed(&mut context.rustaml_context.str_interner);

    #[cfg(feature = "gc-test-print")]
    helper_print_strings(&context.rustaml_context.str_interner, false);
}

pub fn try_gc_collect(context : &mut InterpretContext){
    if context.gc_context.bytes_allocated > context.gc_context.bytes_for_next_gc {
        collect_gc(context, true);
    }
}