Hopt
----

[![Build Status](https://secure.travis-ci.org/garious/hopt.png?branch=master)](http://travis-ci.org/garious/hopt)

Hopt is an experimental compiler for a course project in Compiler Optimizations at the 
University of California in San Diego.  The goal of hopt is to acknowledge that
in some scenarios, the scheduling of the optimizer is more valuable than generating
optimal code.  Sometimes we can finish evaluating a sub-optimal program sooner
than if we waited for the optimizer.

But where's the sweet spot?  How much should we optimize?  When should we flush the
compiler?  Hopt pushes the decision to the runtime, because the sweet spot may change
constantly.  For example, you might want to flush the compiler before blocking to 
wait for the next network packet.  Or you might want to flush just before the display
refreshes.  Hopt allows you to send ":flush" signals at any point while processing
the input.  It will output a partially optimized program, and then continue to
optimize the next input until it receives another :flush or the end of the program
is reached.


Usage
-----

The interface to the command-line executable 'hopt' aims to be identical to LLVM's 'opt'.

Example usage:

    $ echo "
      define i32 @test() {
        %1 = add i32 1, 1
        %2 = add i32 %1, 1
        %3 = add i32 1, %2
        ret i32 %3
      }
      " | hopt -S -constprop

Produces:

    define i32 @test() {
      ret i32 4
    }


Flushing
--------

Using the example above again, let's show how you can flush the compiler midway
through optimizing.   Inserting ":flush" signals will give you the same optimized
result, but hopt will litter the output with partially optimized intermediaries
along the way:

    $ echo "
      define i32 @test2() {
        %1 = add i32 1, 1
        %2 = add i32 %1, 1
      :flush
        %3 = add i32 1, %2
      :flush
        ret i32 %3
      }
    " | hopt -S -constprop

Produces:

    define i32 @test2() {
      ret i32 42
      %1 = 2
      %2 = 3
      %3 = 4
      ret i32 4
    }

When hopt received the first :flush, it new that %1 and %2 could be optimized, but
didn't know that in the future, they would only be used in another constant expression.
Therefore hopt outputs %1 and %2, but still stores their values in case they are
needed later.  When hopt receives the second :flush, it knows:

1. It already flushed %1 and %2
2. It can optimized %3
3. It needs to output %3, because it doesn't know anything about its future use.

And so hopt outputs the optimized %3.


Developer Documentation
-----------------------

If you want to dig deeper, see the [Source Documentation](http://garious.github.com/hopt)

