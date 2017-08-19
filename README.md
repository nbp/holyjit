Making a Just-In-Time compiler is complex, a large source of security
issues, and is a price which is frequently paid to have better performance
results.

HolyJIT is made to remove this trade-off! Simplicity and Security should no
longer be exhanged for Performance reasons.

== HolyJit

HolyJit is a high-level Just-In-Time compiler. It extends the Rust compiler
to convert the code of an interpreter written in Rust to tune a JIT compiler
to handle the same interpreted language.

HolyJit aims at being:
 * trivial.
 * secure.
 * fast.

=== Trivial

HolyJIT extends the Rust compiler to copy its internal representation of
functions and convert it into a representation which can be consumed by the
Jit compiler provided by HolyJit library.

As a user, this implies that to inline a function in JIT compiled code, one
just need to annotate it with the `jit!` macro:

```rust
jit!{ fn eval(jc: &JitContext, script: &JSScript, args: &[Value]) -> Result<Value, Error> = eval_impl }
fn eval_impl(_jc: &JitContext, script: &JSScript, args: &[Value]) -> Result<Value, Error> {
    ...
}
```

Thus, you basically have to write an interpreter, and annotate it properly
to teach the JIT compiler what can be optimized by the compiler.

No assembly knowledge is required to start instrumenting your code to make
it available to the JIT compiler set of known functions.

=== Secure

Security issues from JIT compilers are coming from:
* Duplication of the runtime into a set of MacroAssembler functions.
* Correctness of the compiler optimization.

As HolyJiy extends the Rust compiler to extract the effective knowledge of
the compiler, there is no more risk of having correctness issues caused by
the duplication of code.

Moreover, the code which is given to the JIT compiler is as safe as the code
users wrote in the Rust language.

As HolyJit aims at being a JIT library which can easily be added into other
projects, correctness of the compiler optimizations should be caught by the
community of users. Thus leaving less bugs for you to find out.

=== Fast

Fast is a tricky question when dealing with a JIT compiler, as the cost of
the compilation is part of the equation.

When a compilation assumption fails, such failure is as costly as the extra
time spent in the lower tier of the compiler.  An assumption failure cost is
proportional to the cost of the compilation.

Thus, HolyJit compiler aims to reduce the time taken by the compiler, to
make it as fast as possible.  Thus opening the room for more assumption to
be used during the compilation.

The advantage of assumption failure, is that they do not have to be prooven,
they only have to verified.  As long as the verification cost does not
overcome the benefit of the optimization, we should be able to optimize much
futher than what static compilers can do.

== Using HolyJit

At the moment HolyJit relies on a patched version of rustc, which allow to
create compiler plugins.  To use holyjit, you will have to compile this
custom rustc from the following repository:

https://github.com/nbp/rust/tree/register_opt_mir_pass

Use the above rustc to compile the HolyJit library and plugin, as well as
whatever project which depends on HolyJit.

If you are using HolyJit library, but you are not using the plugin, then the
JIT would not be enabled as no data stored in the binary to be consumed by
the JIT compiler.

To run tests, you can either run the test of the library with `cargo test`,
or run the examples of HolyJit with:

```sh
$ cargo run --example brainfuck --verbose
```

== HolyJit Roadmap for 0.1.0

The current goal is to make a proof of concept which highlight the main
feature, i-e being trivial to integrate into an existing code base and to
have a running JIT compiler.

As of today, HolyJit contains a draft of what the interface might look like,
and is not able to generate any code yet.

- [ ] Create Rust library
  - [x] Allocate pages and map them as executable.
  - [x] Add a way call either a dynamically compiled function or a statically
        compiled function.
  - [x] Add a `jit!` macro, to make calls transparents, from the usage point
        of view.
  - [x] Create a JitContext class, and use it to request JIT compiled code.
  - [ ] Create a graph representation.
  - [ ] Consume the graph to generate code.

- [ ] Create a Mir plugin
  - [x] Detect location which have to be patched.
  - [x] Find functions which have to be converted.
  - [x] Inject a generated vector in the binary content.
  - [x] Inject static variable as a tuple.
  - [ ] collect static variable references.
  - [ ] Convert the Mir (from the Rust compiler) to the library graph representation.
