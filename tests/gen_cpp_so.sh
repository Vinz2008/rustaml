#!/bin/bash
clang++ -c -fPIC cpp_ffi.cpp -o cpp_ffi.o
clang++ -shared -o libcpp_ffi.so cpp_ffi.o
rm -f cpp_ffi.o