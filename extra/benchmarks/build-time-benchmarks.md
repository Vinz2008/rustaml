# Build time benchmarks
The benchmark are run on a system with a ryzen 9 5900X, 32go of ram and running Manjaro with linux 6.12.
Rustaml is built in release mode with fat LTO, panic as abort and these features enabled : native, cache, repl, human-panic, stack-expand, dot-format, jit, build-bdwgc, musl 


LLVM 22.1.0 release mode with all targets enabled : 845.91s  

Rustaml (not including LLVM) : 28.83s  
Rustaml (including LLVM) : 845.91 + 28.83 = 874.74s  
Rust 1.95.0-nightly with ThinLTO (not including LLVM) : 380.08s  
Rust 1.95.0-nightly with ThinLTO (including LLVM) : 845.91 + 380.08 = 1225.99s  
Clang 22.1.0 release mode (not including LLVM) : 487.99s  
Clang 22.1.0 release mode (including LLVM) : 845.91 + 487.99 = 1333.9s  
Zig 0.16.0-dev (not including LLVM):  974.73s  
Zig 0.16.0-dev (including LLVM): 845.91 + 974.73 = 1820.64s  
GCC 15.2.0 with languages C and C++ (and also LTO) : 2009.54s  
Go 1.26rc3 : 51.503s  
NodeJS v26.0.0 : 983.53s  
Haskwell GHC : TODO
Ocaml : TODO
Python : TODO