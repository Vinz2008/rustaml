// TODO : put this on crates.io (in another github repo then add a submodule ?)

#![feature(debug_closure_helpers)]

use std::{collections::HashMap, fmt::{self, Debug}};

pub use debug_with_context_macros::DebugWithContext;

pub trait DebugWithContext<C> {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, context: &C) -> fmt::Result;
}

pub struct DebugWrapContext<'a, C, T> {
    value: &'a T,
    context: &'a C,
}

impl <'a, C, T> DebugWrapContext<'a, C, T> {
    pub fn new(value: &'a T, context: &'a C) -> DebugWrapContext<'a, C, T> {
        DebugWrapContext { value, context }
    }
}

impl<'a, C, T> Debug for DebugWrapContext<'a, C, T>
where
    T: DebugWithContext<C>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt_with_context(f, self.context)
    }
}

macro_rules! debug_with_context_debug {
    ($t:ty) => {
        impl <C> DebugWithContext<C> for $t {
            fn fmt_with_context(&self, f: &mut fmt::Formatter, _context: &C) -> fmt::Result {
                write!(f, "{:?}", self)
            }
        }
    };
    ($t1:ty, $($ts:ty),+) => {
        debug_with_context_debug!($t1);
        debug_with_context_debug!($($ts),+);
    }
}

// TODO : add types here ?
debug_with_context_debug!(u8, i8, u16, i16, u32, i32, u64, i64, usize, isize, &str, String);


// TODO : use a macro to create tuples for example to a bigger size
impl <C, T1, T2> DebugWithContext<C> for (T1, T2)
where 
    T1 : DebugWithContext<C>,
    T2 : DebugWithContext<C>
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, context: &C) -> fmt::Result {
        f.debug_tuple("").field_with(|fmt| self.0.fmt_with_context(fmt, context)).field_with(|fmt| self.1.fmt_with_context(fmt, context)).finish()
    }
}

impl<C, T> DebugWithContext<C> for Vec<T>
where
    T: DebugWithContext<C>,
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, context: &C) -> fmt::Result {
        f.debug_list()
            .entries(self.iter().map(|item| DebugWrapContext { value: item, context }))
            .finish()
    }
}

impl<C, T> DebugWithContext<C> for Option<T>
where
    T: DebugWithContext<C>,
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, context: &C) -> fmt::Result {
        match self {
            Some(s) => s.fmt_with_context(f, context),
            None => None::<()>.fmt(f),
        }
    }
}

impl<C, K, V, S> DebugWithContext<C> for HashMap<K, V, S> 
where 
    K : DebugWithContext<C>,
    V : DebugWithContext<C>,
{
    fn fmt_with_context(&self, f: &mut fmt::Formatter, context: &C) -> fmt::Result {
        f.debug_map()
            .entries(self.iter().map(|(k, v)| (
                DebugWrapContext::new(k, context), DebugWrapContext::new(v, context)
            )))
            .finish()
    }
}

/*#[macro_export]
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
                    file!(), line!(), column!(), stringify!($val),  $crate::debug::DebugWrapContext::new(&tmp, $context));
                tmp
            }
        }
    };
}*/

impl<C, T> DebugWithContext<C> for &'_ T where T: DebugWithContext<C> {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, context: &C) -> fmt::Result {
        (*self).fmt_with_context(f, context)
    }
}

impl<C, T> DebugWithContext<C> for &'_ mut T where T: DebugWithContext<C> {
    fn fmt_with_context(&self, f: &mut fmt::Formatter, context: &C) -> fmt::Result {
        (**self).fmt_with_context(f, context)
    }
}