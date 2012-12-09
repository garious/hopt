Hopt
----

[![Build Status](https://secure.travis-ci.org/garious/hopt.png?branch=master)](http://travis-ci.org/garious/hopt)

Hopt is an experimental compiler for a course project in Compiler Optimizations at the 
University of California in San Diego.  The goal of hopt is to implement a compiler
under the assumption that scheduling optimizations is sometimes more valuable than
generating optimal code.  While a traditional compiler tries to generate optimal code in a
reasonable about of time, and an interpreter tries to compile and execute as quickly as
possible, hopt takes an approach that sits somewhere in between.

Hopt will continue to optimize so long as there is time available.  When the user requires
a result, you can send hopt a flush signal and it will output a partially-optimized 
fragment as quickly as possible.  If hopt is told to flush immediately, it behaves like an
interpreter and only performs some minimal set of optimizations.  On the other hand, if hopt
is never flushed, it behaves like a traditional compiler, and tries its best to generate
optimal code.

Hopt serves up optimized code incrementally and does so under the assumption that it can
be interpreted incrementally.  Consider the following scenario:

  1. Web browser downloads the first nine of ten packets for JavaScript file.
  2. Each of those packets is fed to hopt.
  3. The screen is ready to refresh, and the tenth packet has not yet arrived.

What can we do?  Hopt gives the browser two choices that traditional compilers do not.
Firstly, it will have already output any code it is done optimizing.  Secondly, if the
browser signals hopt to flush, it will output partially-optimized code for all available
input.  If a lot of optimized code is ready for execution, the browser may choose to
start executing and hope that when ready for more input, that tenth packet has arrived.
If still waiting on the packet, the browser can flush the compiler and execute the
suboptimal fragment as well.  What's most important is that when that tenth packet
finally arrives, the compiler and browser will have already executed everything else!


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

Lessons Learned
===============

Flushable optimization passes are harder
----------------------------------------

After having implemented three basic optimizations for hopt, constant propagation,
copy propagation, and dead instruction elimination, we have learned that implementing
optimization passes that can be flushed adds significant complexity.  Whereas a
traditional optimization pass need only hold state to perform the optimization, hopt
must hold information about what can be flushed, and what has already been flushed.
If you browse the code, you see that each unit test requires 4 pieces:

  1. Does the optimization work as expected?
  2. When we send a flush signal, is all input processed?
  3. After flushing, does the compiler retain what it learned from earlier input?
  4. When we send a second flush signal, does it mistakenly flush anything from the
     first flush.

The trickiest part was managing #3 and #4 above.  In Constant Propagation, for example,
if we read "%1 = add i32 1 1" and then flush and then read "%2 = add i32 %1 %1", do we
still know that %1 is 2?  We should!  And if we do and receive a second signal to flush,
do we know to output only %2 and not %1?  We should as well!  So each unit test has a
case for the optimization itself, the optimization when flushed midway through, and the
optimization if flushed midway a second time.  What a pain!


Incremental optimization depends on preprocessing
-------------------------------------------------

The value of processing input and generating output incrementally makes a huge assumption
about the control flow graph.  To make use of this time of optimizer, we require the
server to tweak the layout of the program such that the functions most likely to execute
first are passed across the network first.


Mid-block flush is expensive
----------------------------

The granularity of the flush feature is important.  In this prototype, we allow the user
to send a flush signal after any statement anywhere in the program.  This was a bad
design decision.  It added lots of book-keeping, forced me to traverse the program
top-down, and prevented me from leveraging existing frameworks.  In hindsight, this
level of granularity was unnecessary and too costly.  A better choice would have been
to only allow flushing between either basic blocks, functions, or modules.  The
decision should be based on how much input is typically processed before a flush.
If 5 network packets are received between screen refreshes, and each packet contains
10 functions, then it is probably sufficient to only enable flushing between modules.
The coarser the granularity, the more you can avoid extra bookkeeping and use
traditional compiler techniques.

