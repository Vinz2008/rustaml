# Benchmarks compared to other languages

[The benchmarks](https://github.com/Vinz2008/Language-benchmarks)

The benchmark are run on a system with a ryzen 9 5900X, 32go of ram and running Manjaro with linux 6.12.  
In all the results using rustaml, it is using LLVM 21.1 

Finding the 50th Fibonacci number using a recursive function  
rustaml interpreter : really too long (to the 48th number is more than 4 hours)  
rustaml interpreter with JIT : 256.83s  
rustaml compiler -O0 : 328.428s  
rustaml compiler -O3 : 70.337s  
gcc -O0 v15.2.1 : 131.583s  
gcc -O3 v15.2.1 : 38.218s  
clang -O0 v21.1.6 : 133.159s  
clang -O3 v21.1.6 : 56.974s  
rustc debug 1.95.0-nightly : 222.946s  
rustc release 1.95.0-nightly : 57.514s  
Go v1.25.6 : 131.091s  
NodeJS v20.0.0 : 307.181s  
Haskwell GHC -O0 v9.6.7 : TODO
Haskwell GHC -O3 v9.6.7 : 137.152s  
Ocaml interpreter v5.4.0 : TODO
OCaml ocamlopt v5.4.0 : TODO
Python 3.13.2 : TODO  
PyPy : TODO
Java : TODO

![Fibonacci benchmark](fibonacci.png)

Finding the factors of 2,000,000,000  
rustaml interpreter : Not enough memory (crashes)  
rustaml interpreter with JIT : 2.963s  
rustaml compiler -O0 : 4.309s  
rustaml compiler -O3 : 2.51s  
gcc -O0 v15.2.1 : 2.963s  
gcc -O3 v15.2.1 : 2.936s  
clang -O0 v21.1.6 : 2.987s  
clang -O3 v21.1.6 : 2.520s  
rustc debug 1.95.0-nightly : 9.539s  
rustc release 1.95.0-nightly : 2.528s  
Go v1.25.6 : 2.982s  
NodeJS v20.0.0 : 2.566s  
Haskwell GHC -O0 v9.6.7 : 209.424s  
Haskwell GHC -O3 v9.6.7 : 3.863s  
Ocaml interpreter v5.4.0 : 21,050s  
OCaml ocamlopt v5.4.0 : 2.988s  
Python 3.13.2 : TODO  
PyPy : TODO
Java : TODO


Finding all prime numbers up to 15,000  
rustaml interpreter : 248.594s  
rustaml interpreter with JIT : 371.6ms  
rustaml compiler -O0 : 413.8ms  
rustaml compiler -O3 : 145.2ms  
gcc -O0 v15.2.1 : 146.4ms  
gcc -O3 v15.2.1 : 143.7ms  
clang -O0 v21.1.6 : 147.7ms  
clang -O3 v21.1.6 : 143.8ms  
rustc debug 1.95.0-nightly : 775.0ms  
rustc release 1.95.0-nightly : 145.7ms  
Go v1.25.6 : 171.6ms  
NodeJS v20.0.0 : 175.7ms  
Haskwell GHC -O0 v9.6.7 : 2.401s  
Haskwell GHC -O3 v9.6.7 : 203.0ms  
Ocaml interpreter v5.4.0 : TODO
OCaml ocamlopt v5.4.0 : TODO
Python 3.13.2 : 6.63s   
PyPy : TODO
Java : TODO
