Hopt
----

Hopt is the result of a course project in Compiler Optimizations at the University
of California in San Diego.  The goal of hopt is to implement a modular compiler 
framework that can begin optimizing a program before the full program is available 
to the compiler.  For example, to start rendering the screen while still downloading 
and decompressing a program.

hopt attempts to optimize programs by incrementally parsing input and generating all
potential optimized programs as it parses.  As the compiler acquires new information
about the program, it discards optimizations that are not possible.  At any time,
the application may flush the compiler.  Upon a flush command, hopt spits out
a program that is optimized as best it can given the partial input.

The interface to the command-line executable 'opt' aims to be identical to LLVM's 'opt'.
Example usage:

 $ echo -e "%0 = 42\n ret %0" | opt -S -constprop

Produces:

 ret 42

Note: At the time of this writing, the syntax parsed and produced by Hopt is not
      exactly legitimate LLVM - not even a subset!  But stay tuned...

