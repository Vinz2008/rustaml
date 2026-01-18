pub(crate) mod interpreter;
pub(crate) mod gc;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) mod ffi;

#[cfg(feature = "jit")]
mod jit;

pub(crate) use interpreter::*; 