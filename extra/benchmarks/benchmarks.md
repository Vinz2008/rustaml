# Benchmarks compared to other languages

[The benchmarks](https://github.com/Vinz2008/Language-benchmarks)

The benchmark are run on a system with a ryzen 9 5900X, 32go of ram and running Manjaro with linux 6.12.67-1.  
In all the results using rustaml, it is using LLVM  

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

Finding all prime numbers up to 15,000
TODO