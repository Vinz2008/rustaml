#[macro_export]
macro_rules! debug_println {
    ($is_debug_print:expr) => {
        if $is_debug_print {
            println!()
        }
    };
    ($is_debug_print:expr, $($arg:tt)*) => {
        if $is_debug_print {
            println!($($arg)*);
        }
    };
}