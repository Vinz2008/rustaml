use std::{cmp::max, fmt};

use rustc_hash::FxHashMap;

use crate::{debug::DebugWithContext, gc::Gc, rustaml::RustamlContext};

// TODO : optimize this (https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html)

pub struct StrInterner {
    map : FxHashMap<String, u32>,
    pub strs : Vec<StrInterned>,
}

#[derive(Debug)]
pub enum StrInterned {
    Compiler(String),
    Runtime(Option<Gc<String>>)
}

impl StrInterned {
    fn as_str(&self) -> &str {
        match self {
            StrInterned::Compiler(s) => s,
            StrInterned::Runtime(gc_s) => &gc_s.as_ref().unwrap().data,
        }
    }

    fn is_runtime(&self) -> bool  {
        matches!(self, StrInterned::Runtime(_))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct StringRef(u32);

impl StringRef {

    pub unsafe fn new_unchecked(idx : u32) -> StringRef {
        StringRef(idx)
    }

    pub fn get_str(self, str_interner : &StrInterner) -> &str {
        str_interner.lookup(self)
    }

    pub fn get_gc_mut(self, str_interner : &mut StrInterner) -> &mut Gc<String> {
        str_interner.lookup_gc_mut(self)
    }

    // create a new string
    pub fn add(self, rhs : StringRef, str_interner : &mut StrInterner) -> StringRef {
        let lhs_str = str_interner.lookup(self);
        let rhs_str = str_interner.lookup(rhs);
        let new_str = lhs_str.to_owned() + rhs_str;
        str_interner.intern(&new_str, true)
    }

    pub fn len(self, str_interner : &StrInterner) -> usize {
        str_interner.lookup(self).len()
    }

    pub fn free(self, str_interner : &mut StrInterner){
        str_interner.free(self);
    }
}



impl DebugWithContext for StringRef {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context : &RustamlContext) -> fmt::Result {
        write!(f, "{}", rustaml_context.str_interner.lookup(*self))
    }
}

// TODO : shrink the string interner in gc when it is full of None


impl StrInterner {
    pub fn new() -> StrInterner {
        StrInterner { 
            map: FxHashMap::default(), 
            strs: Vec::new() 
        }
    }

    fn intern(&mut self, name : &str, is_runtime : bool) -> StringRef {

        if let Some(idx) = self.map.get(name) {
            return StringRef(*idx);
        }

        let idx = self.strs.len() as u32;
        self.map.insert(name.to_owned(), idx);
        let intern_str = if is_runtime {
            StrInterned::Runtime(Some(Gc::new(name.to_owned())))
        } else {
            StrInterned::Compiler(name.to_owned())
        };
        self.strs.push(intern_str);

        StringRef(idx)
    }

    pub fn intern_compiler(&mut self, name : &str) -> StringRef {
        self.intern(name, false)
    }

    pub fn intern_runtime(&mut self, name : &str) -> StringRef {
        self.intern(name, true)
    }

    fn lookup_interned(&self, idx : StringRef) -> &StrInterned {
        &self.strs[idx.0 as usize]
    }

    fn lookup_interned_mut(&mut self, idx : StringRef) -> &mut StrInterned {
        &mut self.strs[idx.0 as usize]
    }

    pub fn lookup(&self, idx : StringRef) -> &str {
        self.lookup_interned(idx).as_str()
    }

    pub fn lookup_gc_mut(&mut self, idx : StringRef) -> &mut Gc<String> {
        match self.lookup_interned_mut(idx){
            StrInterned::Runtime(o) => {
                match o {
                    Some(gc_str) => gc_str,
                    None => unreachable!(),
                }
            }
            StrInterned::Compiler(_) => unreachable!(),
        }
    }

    pub fn len(&self) -> usize {
        self.strs.len()
    }

    pub fn runtime_nb(&self) -> usize {
        self.strs.iter().filter(|s| {
            match s {
                StrInterned::Runtime(Some(_)) => true,
                _ => false,
            }
        }).count()
    }

    pub fn compiler_nb(&self) -> usize {
        self.strs.iter().filter(|s| !s.is_runtime()).count()
    }

    pub fn free_nb(&self) -> usize {
        self.strs.iter().filter(|s| {
            match s {
                StrInterned::Runtime(None) => true,
                _ => false,
            }
        }).count()
    }

    pub fn capacity(&self) -> usize {
        self.strs.capacity()
    }

    pub fn shrink_end(&mut self, free_at_end : usize){
        let old_len = self.len();
        let end_length = max(old_len - free_at_end, 10 + self.compiler_nb());
        if end_length == 0 {
            self.strs.clear();
            self.strs.shrink_to(old_len/3);
        } else {
            self.strs.truncate(end_length);
            let end_capacity = (end_length as f64 * 1.3) as usize;
            self.strs.shrink_to(end_capacity);
        }
    }

    pub fn nb_free_at_end(&self) -> usize {
        self.strs.iter().rev().take_while(|e| {
            match e {
                StrInterned::Runtime(s) => {
                    s.is_none()
                }
                StrInterned::Compiler(_) => false,
            }
        }).count()
    }

    pub fn free(&mut self, idx : StringRef){
        match self.lookup_interned_mut(idx) {
            StrInterned::Runtime(s) =>  {
                let freed_str = s.take();
                let freed_str = match freed_str {
                    Some(s) => s,
                    None => panic!("gc tried to free a None string"),
                };
                self.map.remove(&freed_str.data);
                // at the end of this scope, the freed string is dropped
            }
            StrInterned::Compiler(_) => unreachable!(),
        }
        
    }
}

impl DebugWithContext for StrInterner {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, _rustaml_context: &RustamlContext) -> fmt::Result {
        f.debug_tuple("StrInterner").field_with(|f| {
            let mut debug_l = f.debug_list();
            for str_interned in &self.strs {
                debug_l.entry(str_interned);
            }
            debug_l.finish()?;
            fmt::Result::Ok(())
        }).finish()
    }
}