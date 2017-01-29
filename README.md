Making a Just-In-Time compiler is complex, a large source of security issues, and is a price
which is frequently paid to have better performance results.

HolyJIT is made to remove this trade-off! Simplicity and Security should no longer be exhanged for
performance reasons.

== HolyJit

HolyJit is a high-level Just-In-Time compiler. It uses a Rust compiler plugin to convert the code
of an interpreter written in Rust to build a custom JIT compiler for the same interpreted language.

HolyJit aims at being:
 * trivial.
 * secure.
 * fast.

=== Trivial

The compiler plugin used by HolyJIT gives the ability to the Rust compiler to re-use its internal
MIR representation and convert it into a representation which fit the constraints and optimizations
opportunities of the HolyJIT compiler.

This implies that as a user, you literaly have to write an interpreter, and annotate it properly to
teach the JIT what can be optimized by the compiler.

As opposed to many JIT libraries, the user do not have to know anything about assembly to start
using HolyJIT on projects.

=== Secure

The user has to keep the code of an interpreter around, the Rust compiler will enforce all the
lifetime rules proper to the Rust language to the code of the interpreter.

The code of the interpreter is the only source of thruth, the assembly instructions which would be
produced by the JIT compiler are generated out of the instructions of the interpreter. Thus, no
developer has to duplicate the code of the interpreter manually and convert it into assembly, as
this job is done by the plugin compiler.

As an interpreter implementation is mandatorry, the correctness of the JIT compiler can be checked
against the interpreter by using a differential testing fuzzer.

=== Fast

The HolyJIT compiler uses a new design of compiler which is not yet in your text book, which aims at
taking advantage of multiple core of your CPU if possible.

The speed of the compiler enables it to make more assumption based optimizations, which has the
potential to offer faster performance sooner.

