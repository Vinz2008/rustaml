use std::fmt::{self, Debug};

use rustc_hash::FxHashMap;

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

pub trait DebugWithInterner {
    fn fmt_with_interner(&self, f: &mut fmt::Formatter, interner: &StrInterner) -> fmt::Result;
}

pub struct DebugWrapInterner<'a, T> {
    value: &'a T,
    interner: &'a StrInterner,
}

impl <'a, T> DebugWrapInterner<'a, T> {
    pub fn new(value: &'a T, interner: &'a StrInterner) -> DebugWrapInterner<'a, T> {
        DebugWrapInterner { value, interner }
    }
}

impl<'a, T> Debug for DebugWrapInterner<'a, T>
where
    T: DebugWithInterner,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt_with_interner(f, self.interner)
    }
}

impl<'a, T> DebugWithInterner for &'a T where T: DebugWithInterner {
    fn fmt_with_interner(&self, f: &mut fmt::Formatter, interner: &StrInterner) -> fmt::Result {
        (*self).fmt_with_interner(f, interner)
    }
}

impl DebugWithInterner for StringRef {
    fn fmt_with_interner(&self, f: &mut fmt::Formatter, interner: &StrInterner) -> fmt::Result {
        write!(f, "{}", interner.lookup(*self))
    }
}

impl <T1, T2> DebugWithInterner for (T1, T2)
where 
    T1 : DebugWithInterner,
    T2 : DebugWithInterner
{
    fn fmt_with_interner(&self, f: &mut fmt::Formatter, interner: &StrInterner) -> fmt::Result {
        f.debug_tuple("").field_with(|fmt| self.0.fmt_with_interner(fmt, interner)).field_with(|fmt| self.1.fmt_with_interner(fmt, interner)).finish()
    }
}

impl<T> DebugWithInterner for Vec<T>
where
    T: DebugWithInterner,
{
    fn fmt_with_interner(&self, f: &mut fmt::Formatter, interner: &StrInterner) -> fmt::Result {
        f.debug_list()
            .entries(self.iter().map(|item| DebugWrapInterner { value: item, interner }))
            .finish()
    }
}

impl<T> DebugWithInterner for Option<Box<T>>
where
    T: DebugWithInterner,
{
    fn fmt_with_interner(&self, f: &mut fmt::Formatter, interner: &StrInterner) -> fmt::Result {
        match self {
            Some(s) => s.fmt_with_interner(f, interner),
            None => None::<()>.fmt(f),
        }
    }
}

impl<K, V> DebugWithInterner for FxHashMap<K, V> 
where 
    K : DebugWithInterner,
    V : DebugWithInterner,
{
    fn fmt_with_interner(&self, f: &mut fmt::Formatter, interner: &StrInterner) -> fmt::Result {
        f.debug_map()
            .entries(self.iter().map(|(k, v)| (
                DebugWrapInterner::new(k, interner), DebugWrapInterner::new(v, interner)
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
    ($val:expr, $interner:expr) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                eprintln!("[{}:{}:{}] {} = {:#?}",
                    file!(), line!(), column!(), stringify!($val),  $crate::string_intern::DebugWrapInterner::new(&tmp, $interner));
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