pub(crate) mod interpreter;
pub(crate) mod gc;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) mod ffi;

pub(crate) use interpreter::*; 