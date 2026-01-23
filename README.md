# rustaml

A simple functional language (still very in progress) inspired by ocaml and written in rust

## Functionnalities
- Tree-walking Interpreter and Compiler with a LLVM Backend
- JIT support in the interpreter
- Debuginfos support
- REPL
- First-class functions (with builtin-functions map, filter, etc)
- Pattern Matching
- Immutability
- Static types with Hindley–Milner type system
- Generic functions
- Good error messages
- UTF-8 strings support by default
- Builtin regex support with Rust Regex syntax

## Parts that are Done
- [x] Lexer
- [x] Parser (AST)
- [x] Interpreter
- [x] Codegen
- [x] Repl