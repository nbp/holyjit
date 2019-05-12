// TODO: Use #![feature(fn_traits)] once this accepted in the compiler, as
// this would allow to move the call in an Fn<Args, Output> trait for the
// ExecutableMemory, making it callable.

#![feature(associated_type_defaults)]   // Used for adding a type Output to HolyJitFn
#![feature(unboxed_closures)]
#![feature(fn_traits)]

// For dynasm plugin.
#![feature(plugin)]
#![plugin(dynasm)]

pub use std::marker::PhantomData;
use std::rc::Rc;

// dynasm is "currently" used by the compiler as a way to generate code
// without the burden of implementing yet another macro-assembler.
#[macro_use]
extern crate dynasmrt;

// Serde is used for serializing and deserializing the LIR which is stored
// by the plugin in a constant, and deserialized by the JIT compiler in
// order to be manipulated.
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate bincode;

extern crate holyjit_lir as lir;
extern crate holyjit_codegen as codegen;
mod context;

pub use context::JitContext;
pub use codegen::JitCode;

/// This trait should be implemented by every function that we want to be able to Jit. This trait
/// implements the Fn trait to make this function callable, and to make it a potential entry point
/// for the Jit.  When functions from this class are called, HolyJit will decided whether it should
/// be called or inlined.  When a caller of such function is Jitted, the Jit compiler would be able
/// to decide whether it might be interesting to call or inline this function in the Jitted code.
///
/// The structure which are implementing the hj_call function, with the macro named [TODO] would be
/// used as a place-holder space for being populated by the HolyJit plugin.  If the plugin is not
/// used, then the program should still work as-is but the Jit would be disabled.

/// CurryN types are made to wrap the native function pointer and provide
/// a FnOnce, FnMut, and Fn traits implementation.
pub enum Curry0<Output> {
    Native(fn() -> Output),
    // Use an Rc in order to ensure that the JitCode is kept alive as long
    // as the function is running on the stack.
    Jit(Rc<JitCode>)
}
impl<Output> FnOnce<()> for Curry0<Output> {
    type Output = Output;
    extern "rust-call" fn call_once(self, _: ()) -> Output {
        match self {
            Curry0::Native(fun) => fun(),
            Curry0::Jit(jit) => {
                let fun : fn() -> Output = unsafe {
                    std::mem::transmute(jit.as_ptr())
                };
                fun()
            }
        }
    }
}
impl<Output> FnMut<()> for Curry0<Output> {
    extern "rust-call" fn call_mut(&mut self, _: ()) -> Output {
        match self {
            &mut Curry0::Native(ref mut fun) => fun(),
            &mut Curry0::Jit(ref mut jit) => {
                let fun : fn() -> Output = unsafe {
                    std::mem::transmute(jit.as_ptr())
                };
                fun()
            }
        }
    }
}
impl<Output> Fn<()> for Curry0<Output> {
    extern "rust-call" fn call(&self, _: ()) -> Output {
        match self {
            &Curry0::Native(ref fun) => fun(),
            &Curry0::Jit(ref jit) => {
                let fun : fn() -> Output = unsafe {
                    std::mem::transmute(jit.as_ptr())
                };
                fun()
            }
        }
    }
}

macro_rules! curry_call {
    ($fun:expr => $arg:ident : ($a0:ty)) => { $fun($arg.0) };
    ($fun:expr => $arg:ident : ($a0:ty,$a1:ty)) => { $fun($arg.0,$arg.1)  };
    ($fun:expr => $arg:ident : ($a0:ty,$a1:ty,$a2:ty)) => { $fun($arg.0,$arg.1,$arg.2)  };
    ($fun:expr => $arg:ident : ($a0:ty,$a1:ty,$a2:ty,$a3:ty)) => { $fun($arg.0,$arg.1,$arg.2,$arg.3)  };
}

#[macro_export]
macro_rules! fn_ty {
    () => { () };
    ($a0:ty) => { ($a0,) };
    ($a0:ty,$($as:ty),*) => { ($a0,$($as),*)  };
}

macro_rules! curry_decl {
    ($name:ident<($($arg:ident),*) -> $ret:ident>) => {
        pub enum $name<$($arg,)* $ret> {
            Native(fn($($arg),*) -> $ret),
            Jit(Rc<JitCode>)
        }
        impl<$($arg,)* $ret> FnOnce<fn_ty!{$($arg),*}> for $name<$($arg,)* $ret> {
            type Output = $ret;
            extern "rust-call" fn call_once(self, args: fn_ty!{$($arg),*}) -> $ret {
                match self {
                    $name::Native(fun) => curry_call!{ fun => args: ($($arg),*) },
                    $name::Jit(jit) => {
                        let fun : fn($($arg),*) -> $ret = unsafe {
                            std::mem::transmute(jit.as_ptr())
                        };
                        curry_call!{ fun => args: ($($arg),*) }
                    }
                }
            }
        }
        impl<$($arg,)* $ret> FnMut<fn_ty!{$($arg),*}> for $name<$($arg,)* $ret> {
            extern "rust-call" fn call_mut(&mut self, args: fn_ty!{$($arg),*}) -> $ret {
                match self {
                    &mut $name::Native(ref mut fun) => curry_call!{ fun => args: ($($arg),*) },
                    &mut $name::Jit(ref mut jit) => {
                        let fun : fn($($arg),*) -> $ret = unsafe {
                            std::mem::transmute(jit.as_ptr())
                        };
                        curry_call!{ fun => args: ($($arg),*) }
                    }
                }
            }
        }
        impl<$($arg,)* $ret> Fn<fn_ty!{$($arg),*}> for $name<$($arg,)* $ret> {
            extern "rust-call" fn call(&self, args: fn_ty!{$($arg),*}) -> $ret {
                match self {
                    &$name::Native(ref fun) => curry_call!{ fun => args: ($($arg),*) },
                    &$name::Jit(ref jit) => {
                        let fun : fn($($arg),*) -> $ret = unsafe {
                            std::mem::transmute(jit.as_ptr())
                        };
                        curry_call!{ fun => args: ($($arg),*) }
                    }
                }
            }
        }
    }
}

curry_decl!{ Curry1<(A0) -> Output>  }
curry_decl!{ Curry2<(A0,A1) -> Output>  }
curry_decl!{ Curry3<(A0,A1,A2) -> Output>  }
curry_decl!{ Curry4<(A0,A1,A2,A3) -> Output>  }

/// This trait defines get_fn function which is used to select which
/// function is going to be executed.  Based on various criteria, such as
/// the number of hit-counts, this function will select either the function
/// statically compiled by rustc, or the result of the compilation performed
/// by HolyJit.
pub trait HolyJitFn<Args> {
    type Curry;
    type Output;
    fn get_fn(&self) -> Self::Curry;
    fn from_code(jit: Rc<JitCode>) -> Self::Curry where Self : Sized;
    fn get_jc<'a>(&self, args: &'a Args) -> &'a JitContext;
}

/// Structure made for wrapping both the function pointer and the serialized
/// bytes of the LIR graph.  The first parameter defines the tuple of
/// arguments type, and the second parameter defines Output of the function.
/// The last parameter T corresponds to the structure which is wrapping the
/// function pointer. This structure should implement the HolyJitFn trait.
pub struct HolyJitFnWrapper<Args, Output, Curry, T>
    where T : HolyJitFn<Args, Curry = Curry, Output = Output>
{
    /// Reference to the statically compiled function.
    pub fun: T,

    /// Serialized LIR graph, of the statically compiled function.
    pub bytes: &'static [u8],

    /// Statically compiled function might contain references to other
    /// symbols. At the time of the plugin compilation, we do not know what
    /// these symbols addresses would be, as they would be decided by LLVM
    /// assembler, and the dynamic-linker.
    ///
    /// To carry this information, we use a tuple, as not all symbols have
    /// the same type. Moreover, there is no single cast target possible for
    /// all. The previous vector of bytes, thus know how to interpret the
    /// content of this pointer.
    pub defs: *const (),

    // Keep track of the types.
    pub _proto: PhantomData<HolyJitFn<Args, Curry = Curry, Output = Output>>
}

impl<Args, Output, Curry, T> HolyJitFnWrapper<Args, Output, Curry, T>
    where T : HolyJitFn<Args, Curry = Curry, Output = Output>
{
    /// Given a list of argument, extract a JitContext from it, and look for
    /// existing compiled code.  Otherwise, either return the static
    /// function, or the newly JIT compiled function.
    fn select_fn<'ctx>(&'ctx self, args: &Args) -> Curry {
        let jc = self.fun.get_jc(args);
        match jc.lookup(self.bytes, self.defs) {
            Some(code) => T::from_code(code),
            None => self.fun.get_fn()
        }
    }
}

impl<Args, Output, Curry, T> FnOnce<Args>
    for HolyJitFnWrapper<Args, Output, Curry, T>
    where T : HolyJitFn<Args, Curry = Curry, Output = Output>,
          Curry : FnOnce<Args, Output = Output>
{
    type Output = Output;
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output {
        let f = {
            let ra = &args;
            self.select_fn(ra)
        };
        f.call_once(args)
    }
}
impl<Args, Output, Curry, T : HolyJitFn<Args, Curry = Curry>> FnMut<Args>
    for HolyJitFnWrapper<Args, Output, Curry, T>
    where T : HolyJitFn<Args, Curry = Curry, Output = Output>,
          Curry : FnMut<Args, Output = Output>
{
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output {
        let mut f = {
            let ra = &args;
            self.select_fn(ra)
        };
        f.call_mut(args)
    }
}
impl<Args, Output, Curry, T : HolyJitFn<Args, Curry = Curry>> Fn<Args>
    for HolyJitFnWrapper<Args, Output, Curry, T>
    where T : HolyJitFn<Args, Curry = Curry, Output = Output>,
          Curry : Fn<Args, Output = Output>
{
    extern "rust-call" fn call(&self, args: Args) -> Self::Output {
        let f = {
            let ra = &args;
            self.select_fn(ra)
        };
        f.call(args)
    }
}

#[macro_export]
macro_rules! curry {
    (() -> $ret_type:ty) => { $crate::Curry0<$ret_type> };
    (($a0:ty) -> $ret_type:ty) => { $crate::Curry1<$a0,$ret_type> };
    (($a0:ty,$a1:ty) -> $ret_type:ty) => { $crate::Curry2<$a0,$a1,$ret_type> };
    (($a0:ty,$a1:ty,$a2:ty) -> $ret_type:ty) => { $crate::Curry3<$a0,$a1,$a2,$ret_type> };
    (($a0:ty,$a1:ty,$a2:ty,$a3:ty) -> $ret_type:ty) => { $crate::Curry4<$a0,$a1,$a2,$a3,$ret_type> };

    (() -> $ret_type:ty = $e:ident ) => { $crate::Curry0::Native( $e ) };
    (($a0:ty) -> $ret_type:ty = $e:ident ) => { $crate::Curry1::Native( $e ) };
    (($a0:ty,$a1:ty) -> $ret_type:ty = $e:ident ) => { $crate::Curry2::Native( $e ) };
    (($a0:ty,$a1:ty,$a2:ty) -> $ret_type:ty = $e:ident ) => { $crate::Curry3::Native( $e ) };
    (($a0:ty,$a1:ty,$a2:ty,$a3:ty) -> $ret_type:ty = $e:ident ) => { $crate::Curry4::Native( $e ) };

    (() -> $ret_type:ty = < $e:ident > ) => { $crate::Curry0::Jit( $e ) };
    (($a0:ty) -> $ret_type:ty = < $e:ident > ) => { $crate::Curry1::Jit( $e ) };
    (($a0:ty,$a1:ty) -> $ret_type:ty = < $e:ident > ) => { $crate::Curry2::Jit( $e ) };
    (($a0:ty,$a1:ty,$a2:ty) -> $ret_type:ty = < $e:ident > ) => { $crate::Curry3::Jit( $e ) };
    (($a0:ty,$a1:ty,$a2:ty,$a3:ty) -> $ret_type:ty = < $e:ident > ) => { $crate::Curry4::Jit( $e ) };
}

#[macro_export]
macro_rules! match_ref_args {
    () => { () };
    ($a0:ident) => { (ref $a0,) };
    ($a0:ident,$($as:ident),*) => { (ref $a0,$(ref $as),*)  };
}

// TODO: Add support arguments, and their types: $($arg_name:ident: $arg_type:ty),* ?

/// The jit! macro is used to define function which is compiled statically
/// and which can be compiled dynamically too. This function definitions
/// aliases an existing function definition which has an identical
/// signature.
#[macro_export]
macro_rules! jit {
    (fn $fun:ident ($($arg:ident: $typ:ty),*) -> $ret_type:ty = $delegate:ident
     in $jc:expr ;
    ) => {
        // WARNING: The $fun identifier is used both for the struct name,
        // which holds the graph representation of the $delegate function,
        // and for the constant of type HolyJitFnWrapper. As there is no
        // proper way to generate new identifier, we use this aliasing
        // technique to avoid conflicting hard-coded names.
        #[allow(non_upper_case_globals)]
        const $fun : $crate::HolyJitFnWrapper<fn_ty!{$($typ),*}, $ret_type, curry!{ ($($typ),*) -> $ret_type }, $fun> = $crate::HolyJitFnWrapper{
            fun   : $fun{ curry: curry!{ ($($typ),*) -> $ret_type = $delegate } },
            bytes : &[ 0 /* placeholder for holyjit plugin */ ],
            defs  : &(1u8, 2u8) as *const _ as *const (),
            _proto: $crate::PhantomData,
        };
        #[allow(non_camel_case_types)]
        struct $fun { curry: curry!{ ($($typ),*) -> $ret_type } }
        impl $crate::HolyJitFn<fn_ty!{$($typ),*}> for $fun {
            type Curry = curry!{ ($($typ),*) -> $ret_type };
            type Output = $ret_type;
            fn get_fn(&self) -> Self::Curry {
                // Return the statically compiled function.
                if let curry!{ ($($typ),*) -> $ret_type = fun } = self.curry {
                    curry!{ ($($typ),*) -> $ret_type = fun }
                } else {
                    panic!("The impossible happened!")
                }
            }
            fn from_code(jit: std::rc::Rc<$crate::JitCode>) -> Self::Curry {
                // Return the dynamically compiled function.
                curry!{($($typ),*) -> $ret_type = < jit > }
            }
            fn get_jc<'a>(&self, args: &'a fn_ty!{$($typ),*}) -> &'a $crate::JitContext {
                // Use a match statement to fake the arguments bindings of
                // the function.
                #[allow(unused_variables)]
                match args {
                    &match_ref_args!{$($arg),*} => $jc
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Add wrappers for the test functions, and check that we can call them
    // without any syntax overhead. Check that we can use a function to get
    // the JitContext from a global.
    jit!{ fn test0() -> i32 = test0_impl in global_jc(); }
    fn test0_impl() -> i32 { 42 }

    jit!{ fn test1(x: i32) -> i32 = test1_impl in global_jc(); }
    fn test1_impl(x: i32) -> i32 { x }

    jit!{ fn test2(x: i32, y: i32) -> i32 = test2_impl in global_jc(); }
    fn test2_impl(x: i32, y: i32) -> i32 { x + y }

    // Check that we can use one of the argument to extract a JitContext.
    jit!{ fn test3(jc: JitContext, x: i32, y: i32) -> i32 = test3_impl in jc; }
    fn test3_impl(_jc: JitContext, x: i32, y: i32) -> i32 {
        x + y
    }

    static mut JC: Option<*const JitContext> = None;
    fn global_jc() -> &'static JitContext {
        // Good enough for a test case?
        unsafe {
            if JC == None {
                let jc : Box<JitContext> = Box::new(Default::default());
                JC = Some(std::mem::transmute(jc));
            };
            std::mem::transmute(JC.unwrap())
        }
    }

    #[test]
    fn check_transparent_wrappers() {
        assert_eq!(test0(), 42);
        assert_eq!(test1(51), 51);
        assert_eq!(test2(21, 21), 42);
        let jc : JitContext = Default::default();
        assert_eq!(test3(jc, 21, 21), 42);
    }
}
