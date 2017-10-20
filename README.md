Making a Just-In-Time compiler is complex, a large source of security
issues, and is a price which is frequently paid to have better performance
results.

HolyJIT is made to remove this trade-off! Simplicity and Security should no
longer be exhanged for Performance reasons.

# HolyJit

HolyJit is a high-level Just-In-Time compiler. It extends the Rust compiler
to convert the code of an interpreter written in Rust to tune a JIT compiler
to handle the same interpreted language.

HolyJit aims at being:
 * Easy.
 * Safe.
 * Fast.

### Easy

HolyJIT extends the Rust compiler to copy its internal representation of
functions and convert it into a representation which can be consumed by the
Jit compiler provided by HolyJit library.

As a user, this implies that to inline a function in JIT compiled code, one
just need to annotate it with the `jit!` macro:

```rust
jit!{
    fn eval(script: &Script, args: &[Value]) -> Result<Value, Error>
    = eval_impl
    in script.as_ref()
}

fn eval_impl(script: &Script, args: &[Value]) -> Result<Value, Error> {
    // ...
    // ... A few hundred lines of ordinary Rust code later ...
    // ...
}

fn main() {
    let script = ...;
    let args = ...;
    // Call it as any ordinary function.
    let res = eval(&script, &args);
    println!("Result: {}", res);
}
```

Thus, you basically have to write an interpreter, and annotate it properly
to teach the JIT compiler what can be optimized by the compiler.

No assembly knowledge is required to start instrumenting your code to make
it available to the JIT compiler set of known functions.

### Safe

Security issues from JIT compilers are coming from:
* Duplication of the runtime into a set of MacroAssembler functions.
* Correctness of the compiler optimization.

As HolyJit extends the Rust compiler to extract the effective knowledge of
the compiler, there is no more risk of having correctness issues caused by
the duplication of code.

Moreover, the code which is given to the JIT compiler is as safe as the code
users wrote in the Rust language.

As HolyJit aims at being a JIT library which can easily be embedded into
other projects, correctness of the compiler optimizations should be caught
by the community of users and fuzzers. Thus leaving less bugs for you to
find out.

### Fast

Fast is a tricky question when dealing with a JIT compiler, as the cost of
the compilation is part of the equation.

HolyJit aims at reducing the start-up time, based on annotation made out of
macros, to guide the early tiers of the compilers for unrolling loops and
generating inline caches.

For final compilation tiers, it uses special types/traits to wrap the data
in order to instrument and monitor the values which are being used, such
that guard can later be converted into constraints.

## Using HolyJit

At the moment HolyJit relies on a patched version of rustc, which allow to
create compiler plugins.  To use holyjit, you will have to compile this
custom rustc from the following repository:

https://github.com/nbp/rust/tree/register_opt_mir_pass

Use the above rustc to compile the HolyJit library and plugin, as well as
whatever project which depends on HolyJit.

If you are using HolyJit library, but you are not using the plugin, then the
JIT would not be enabled as would no data stored in the binary to be
consumed by the JIT compiler at runtime.

To run tests, you can either run the test of the library with `cargo test`,
or run the examples of HolyJit with:

```sh
$ cargo run --example brainfuck --verbose
```

At the moment, HolyJit is far from being yet ready for production! This is
currently at a prototype stage, and most of the code & dependencies present
today were made only as a proof of concept and not as a definitive
implementation design.

## HolyJit Roadmap for 0.0.0

The current goal is to make a proof of concept which highlight the main
feature, i-e being trivial to integrate into an existing code base and to
have a running JIT compiler.

As of today, HolyJit contains a draft of what the interface might look like,
and is able to generate code for the example present in the repository.

- [x] Create Rust library
  - [x] Allocate pages and map them as executable.
  - [x] Add a way call either a dynamically compiled function or a statically
        compiled function.
  - [x] Add a `jit!` macro, to make calls transparents, from the usage point
        of view.
  - [x] Create a JitContext class, and use it to request JIT compiled code.
  - [x] Create a graph representation.
  - [x] Consume the graph to generate code.

- [x] Create a Mir plugin
  - [x] Detect location which have to be patched.
  - [x] Find functions which have to be converted.
  - [x] Inject a generated vector in the binary content.
  - [x] Inject static variable as a tuple.
  - [x] collect static variable references.
  - [x] Convert the Mir (from the Rust compiler) to the library graph representation.
