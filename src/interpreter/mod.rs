pub mod interpreter;
pub mod gc;
#[cfg(not(target_arch = "wasm32"))]
pub mod ffi;

pub use interpreter::*; 