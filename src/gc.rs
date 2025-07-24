use std::collections::VecDeque;

use crate::{debug::DebugWrapContext, interpreter::{InterpretContext, List, ListPool, ListRef, Val}};

#[derive(Debug)]
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

pub fn collect_gc(context : &mut InterpretContext){
    #[cfg(feature = "gc-test")]
    println!("-- COLLECT GC START");

    mark_and_sweep_list_nodes(context);

    #[cfg(feature = "gc-test")]
    println!("-- COLLECT GC END");
}

// TODO : create a iter_roots iterator for iterating roots ?


fn shrink_list_pool_if_needed(list_node_pool : &mut ListPool){
    let free_at_end = list_node_pool.nb_free_at_end();
    println!("{}", free_at_end);
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
        let val_list_refs = l.get(list_node_pool).iter(list_node_pool).map(|e| val_to_list_ref_unchecked(e)).collect::<Vec<_>>();
        for val_list_ref in val_list_refs {
            mark_list_ref(list_node_pool, grey_stack, val_list_ref);
        }
        
    }
}

fn mark_and_sweep_list_nodes(context : &mut InterpretContext){
    
    #[cfg(feature = "gc-test")]
    println!("GC : LIST NODES BEFORE -> {} (used : {}, free : {}, capacity : {})", context.rustaml_context.list_node_pool.0.len(), context.rustaml_context.list_node_pool.nb_used_nodes(), context.rustaml_context.list_node_pool.nb_free_nodes(), context.rustaml_context.list_node_pool.0.capacity());

    let mut grey_stack = VecDeque::new();

    // mark
    for (_, var_val) in &context.vars {
        match var_val {
            Val::List(l) => mark_list_ref(&mut context.rustaml_context.list_node_pool, &mut grey_stack, *l),
            _ => {}
        }
    }

    
    while !grey_stack.is_empty(){
        //println!("{:?}", DebugWrapContext::new(&context.rustaml_context.list_node_pool, context.rustaml_context));
        let grey_node = grey_stack.pop_front().unwrap();
        mark_list_ref(&mut context.rustaml_context.list_node_pool, &mut grey_stack, grey_node);
    }

    #[cfg(feature = "gc-test")]
    println!("after marking : {:?}", DebugWrapContext::new(&context.rustaml_context.list_node_pool, context.rustaml_context));

    // sweep

    let mut lists_to_free = Vec::new();
    for (idx, list) in context.rustaml_context.list_node_pool.0.iter_mut().enumerate() {
        if let Some(l) = list {
            if !l.is_marked {
                let list_ref = ListRef(idx.try_into().unwrap());
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

    #[cfg(feature = "gc-test")]
    println!("GC : LIST NODES AFTER -> {} (used : {}, free : {}, capacity : {})", context.rustaml_context.list_node_pool.0.len(), context.rustaml_context.list_node_pool.nb_used_nodes(), context.rustaml_context.list_node_pool.nb_free_nodes(), context.rustaml_context.list_node_pool.0.capacity());
}