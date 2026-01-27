pub(crate) mod compiler;
pub(crate) mod compiler_utils;
mod compile_match;
pub(crate) mod debuginfo; // pub(crate)for using ContentLoc in rustaml.rs
mod internal_monomorphized;
pub(crate) mod linker;
pub(crate) mod cast;

// reexports to not have compiler::compiler
pub(crate) use compiler::*;