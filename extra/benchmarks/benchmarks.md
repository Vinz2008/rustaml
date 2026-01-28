# Benchmarks compared to other languages

[The benchmarks](https://github.com/Vinz2008/Language-benchmarks)

The benchmark are run on a system with a ryzen 9 5900X, 32go of ram and running Manjaro with linux 6.12.67-1.
In all the results using rustaml, it is using LLVM 

Finding the 50th Fibonacci number using a recursive function
rustaml interpreter : 
rustaml interpreter with JIT : 256,83s
rustaml compiler -O0 : 
rustaml compiler -O3 : 233,00s



Finding the factors of 2,000,000,000
rustaml interpreter : Not enough memory (crashes)
rustaml interpreter with JIT : 2.963s
rustaml compiler -O0 : 4.309s
rustaml compiler -O3 : 2,51s