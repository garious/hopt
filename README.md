Hopt
----

![alt text](https://travis-ci.org/garious/hopt.png "Build Status")

Hopt is an experimental compiler for a course project in Compiler Optimizations at the 
University of California in San Diego.  The goal of hopt is to implement a modular compiler 
framework that can begin optimizing a program before the full program is available 
to the compiler.  For example, to start rendering the screen while still downloading 
and decompressing a program.

Hopt attempts to optimize programs by incrementally parsing input and generating all
potential optimized programs as it parses.  As the compiler acquires new information
about the program, it discards optimizations that are not possible.  At any time,
the application may flush the compiler.  Upon a flush command, hopt spits out
a program that is optimized as best it can given the partial input.

The interface to the command-line executable 'hopt' aims to be identical to LLVM's 'opt'.

Example usage:

    $ echo "
      define i32 @test() {
         %1 = add i32 21, 21
         ret i32 %1
      }
      " | hopt -S -constprop

Produces:

    define i32 @test() {
      ret i32 42
    }

