use std::fmt;

use rustc_hash::FxHashMap;

use crate::{debug::DebugWithContext, rustaml::RustamlContext};

// TODO : optimize this (https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html)

pub struct StrInterner {
    map : FxHashMap<String, u32>,
    strs : Vec<String>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct StringRef(u32);

impl StringRef {
    pub fn get_str(self, str_interner : &StrInterner) -> &str {
        str_interner.lookup(self)
    }

    // create a new string
    pub fn add(self, rhs : StringRef, str_interner : &mut StrInterner) -> StringRef {
        let new_str = str_interner.lookup(self).to_owned() + str_interner.lookup(rhs);
        str_interner.intern(&new_str)
    }
}



impl DebugWithContext for StringRef {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context : &RustamlContext) -> fmt::Result {
        write!(f, "{}", rustaml_context.str_interner.lookup(*self))
    }
}

impl StrInterner {
    pub fn new() -> StrInterner {
        StrInterner { 
            map: FxHashMap::default(), 
            strs: Vec::new() 
        }
    }

    pub fn intern(&mut self, name : &str) -> StringRef {
        if let Some(idx) = self.map.get(name) {
            return StringRef(*idx);
        }

        let idx = self.strs.len() as u32;
        self.map.insert(name.to_owned(), idx);
        self.strs.push(name.to_owned());

        StringRef(idx)
    }

    pub fn lookup(&self, idx : StringRef) -> &str {
        self.strs[idx.0 as usize].as_str()
    }
}