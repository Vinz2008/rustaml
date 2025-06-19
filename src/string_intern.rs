use std::fmt::{self, Debug};

use rustc_hash::FxHashMap;

use crate::rustaml::RustamlContext;

// TODO : use this (will need code for debug displaying, look at the code for the flatten AST, because they have the same problem)
// TODO : optimize this (https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html)

pub struct StrInterner {
    map : FxHashMap<String, u32>,
    strs : Vec<String>,
}

// TODO : remove Debug and add proper debug pringing (URGENT !! )
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

pub trait DebugWithContext {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result;
}

pub struct DebugWrapContext<'a, T> {
    value: &'a T,
    context: &'a RustamlContext
}

impl <'a, T> DebugWrapContext<'a, T> {
    pub fn new(value: &'a T, context: &'a RustamlContext) -> DebugWrapContext<'a, T> {
        DebugWrapContext { value, context }
    }
}

impl<'a, T> Debug for DebugWrapContext<'a, T>
where
    T: DebugWithContext,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt_with_context(f, self.context)
    }
}

impl<'a, T> DebugWithContext for &'a T where T: DebugWithContext {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        (*self).fmt_with_context(f, rustaml_context)
    }
}

impl DebugWithContext for StringRef {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context : &RustamlContext) -> fmt::Result {
        write!(f, "{}", rustaml_context.str_interner.lookup(*self))
    }
}

impl <T1, T2> DebugWithContext for (T1, T2)
where 
    T1 : DebugWithContext,
    T2 : DebugWithContext
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        f.debug_tuple("").field_with(|fmt| self.0.fmt_with_context(fmt, rustaml_context)).field_with(|fmt| self.1.fmt_with_context(fmt, rustaml_context)).finish()
    }
}

impl<T> DebugWithContext for Vec<T>
where
    T: DebugWithContext,
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        f.debug_list()
            .entries(self.iter().map(|item| DebugWrapContext { value: item, context: rustaml_context }))
            .finish()
    }
}

impl<T> DebugWithContext for Option<T>
where
    T: DebugWithContext,
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        match self {
            Some(s) => s.fmt_with_context(f, rustaml_context),
            None => None::<()>.fmt(f),
        }
    }
}

impl<T> DebugWithContext for Box<T>
where
    T: DebugWithContext,
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        self.as_ref().fmt_with_context(f, rustaml_context)
    }
}

impl<K, V> DebugWithContext for FxHashMap<K, V> 
where 
    K : DebugWithContext,
    V : DebugWithContext,
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, rustaml_context: &RustamlContext) -> fmt::Result {
        f.debug_map()
            .entries(self.iter().map(|(k, v)| (
                DebugWrapContext::new(k, rustaml_context), DebugWrapContext::new(v, rustaml_context)
            )))
            .finish()
    }
}

#[macro_export]
macro_rules! dbg_intern {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        eprintln!("[{}:{}:{}]", file!(), line!(), column!())
    };
    ($val:expr, $context:expr) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                eprintln!("[{}:{}:{}] {} = {:#?}",
                    file!(), line!(), column!(), stringify!($val),  $crate::string_intern::DebugWrapContext::new(&tmp, $context));
                tmp
            }
        }
    };
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