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

#[macro_use]
extern crate dynasmrt;

pub mod lir;
mod compile;

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
pub struct Curry0<Output> { pub fun: fn() -> Output }
impl<Output> FnOnce<()> for Curry0<Output> {
    type Output = Output;
    extern "rust-call" fn call_once(self, _: ()) -> Output {
        (self.fun)()
    }
}
impl<Output> FnMut<()> for Curry0<Output> {
    extern "rust-call" fn call_mut(&mut self, _: ()) -> Output {
        (self.fun)()
    }
}
impl<Output> Fn<()> for Curry0<Output> {
    extern "rust-call" fn call(&self, _: ()) -> Output {
        (self.fun)()
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
        pub struct $name<$($arg,)* $ret> { pub fun: fn($($arg),*) -> $ret }
        impl<$($arg,)* $ret> FnOnce<fn_ty!{$($arg),*}> for $name<$($arg,)* $ret> {
            type Output = $ret;
            extern "rust-call" fn call_once(self, args: fn_ty!{$($arg),*}) -> $ret {
                curry_call!{ (self.fun) => args: ($($arg),*) }
            }
        }
        impl<$($arg,)* $ret> FnMut<fn_ty!{$($arg),*}> for $name<$($arg,)* $ret> {
            extern "rust-call" fn call_mut(&mut self, args: fn_ty!{$($arg),*}) -> $ret {
                curry_call!{ (self.fun) => args: ($($arg),*) }
            }
        }
        impl<$($arg,)* $ret> Fn<fn_ty!{$($arg),*}> for $name<$($arg,)* $ret> {
            extern "rust-call" fn call(&self, args: fn_ty!{$($arg),*}) -> $ret {
                curry_call!{ (self.fun) => args: ($($arg),*) }
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
pub trait HolyJitFn<Args, Output> {
    // fn get_fn(&self) -> fn(Args) -> Output; // single argument function :/
    fn get_fn(&self) -> &Fn<Args, Output = Output>;
}

/// Structure made for wrapping both the function pointer and the serialized
/// bytes of the MIR graph.  The first parameter defines the tuple of
/// arguments type, and the second parameter defines Output of the function.
/// The last parameter T corresponds to the structure which is wrapping the
/// function pointer. This structure should implement the HolyJitFn trait.
pub struct HolyJitFnWrapper<Args, Output, T: HolyJitFn<Args, Output>>{
    pub fun: T,
    #[allow(dead_code)]
    pub bytes: &'static [u8],
    pub _proto: PhantomData<HolyJitFn<Args, Output>>
}

impl<Args, Output, T : HolyJitFn<Args, Output>> FnOnce<Args> for HolyJitFnWrapper<Args, Output, T> {
    type Output = Output;
    extern "rust-call" fn call_once(self, args: Args) -> Self::Output {
        self.fun.get_fn().call_once(args)
        // (self.fun.get_fn())(args)
    }
}
impl<Args, Output, T : HolyJitFn<Args, Output>> FnMut<Args> for HolyJitFnWrapper<Args, Output, T> {
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output {
        self.fun.get_fn().call_mut(args)
        // (self.fun.get_fn())(args)
    }
}
impl<Args, Output, T : HolyJitFn<Args, Output>> Fn<Args> for HolyJitFnWrapper<Args, Output, T> {
    extern "rust-call" fn call(&self, args: Args) -> Self::Output {
        self.fun.get_fn().call(args)
        // (self.fun.get_fn())(args)
    }
}

#[macro_export]
macro_rules! curry {
    (() -> $ret_type:ty) => { $crate::Curry0<$ret_type> };
    (($a0:ty) -> $ret_type:ty) => { $crate::Curry1<$a0,$ret_type> };
    (($a0:ty,$a1:ty) -> $ret_type:ty) => { $crate::Curry2<$a0,$a1,$ret_type> };
    (($a0:ty,$a1:ty,$a2:ty) -> $ret_type:ty) => { $crate::Curry3<$a0,$a1,$a2,$ret_type> };
    (($a0:ty,$a1:ty,$a2:ty,$a3:ty) -> $ret_type:ty) => { $crate::Curry4<$a0,$a1,$a2,$a3,$ret_type> };

    (() -> $ret_type:ty = $e:ident ) => { $crate::Curry0{ fun: $e } };
    (($a0:ty) -> $ret_type:ty = $e:ident ) => { $crate::Curry1{ fun: $e } };
    (($a0:ty,$a1:ty) -> $ret_type:ty = $e:ident ) => { $crate::Curry2{ fun: $e } };
    (($a0:ty,$a1:ty,$a2:ty) -> $ret_type:ty = $e:ident ) => { $crate::Curry3{ fun: $e } };
    (($a0:ty,$a1:ty,$a2:ty,$a3:ty) -> $ret_type:ty = $e:ident ) => { $crate::Curry4{ fun: $e } };
}

// TODO: Add support arguments, and their types: $($arg_name:ident: $arg_type:ty),* ?

/// The jit! macro is used to define function which is compiled statically
/// and which can be compiled dynamically too. This function definitions
/// aliases an existing function definition which has an identical
/// signature.
#[macro_export]
macro_rules! jit {
    (fn $fun:ident ($($arg:ident: $typ:ty),*) -> $ret_type:ty = $delegate:ident) => {
        // WARNING: The $fun identifier is used both for the struct name,
        // which holds the graph representation of the $delegate function,
        // and for the constant of type HolyJitFnWrapper. As there is no
        // proper way to generate new identifier, we use this aliasing
        // technique to avoid conflicting hard-coded names.
        #[allow(non_upper_case_globals)]
        const $fun : $crate::HolyJitFnWrapper<fn_ty!{$($typ),*}, $ret_type, $fun> = $crate::HolyJitFnWrapper{
            fun : $fun{ curry: curry!{ ($($typ),*) -> $ret_type = $delegate } },
            bytes : &[ 0 /* placeholder for holyjit plugin */ ],
            _proto: $crate::PhantomData,
        };
        #[allow(non_camel_case_types)]
        struct $fun { curry: curry!{ ($($typ),*) -> $ret_type } }
        impl $crate::HolyJitFn<fn_ty!{$($typ),*}, $ret_type> for $fun {
            fn get_fn(&self) -> &Fn<fn_ty!{$($typ),*}, Output = $ret_type> {
                // TODO: Replace this line by an argument with a JitContext,
                // which can be used to map a graph to a given compilation.
                // Also, add a function which converts the given set of
                // arguments into a JitContext.

                // use std::sync::{Arc, Mutex};
                // static mut choice: Arc<Mutex<bool>> = Arc::new(Mutex::new(true));
                &self.curry
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Add wrappers for the test functions, and check that we can call them
    // without any syntax overhead.
    jit!{ fn test0() -> i32 = test0_impl }
    fn test0_impl() -> i32 { 42 }

    jit!{ fn test1(x: i32) -> i32 = test1_impl }
    fn test1_impl(x: i32) -> i32 { x }

    jit!{ fn test2(x: i32, y: i32) -> i32 = test2_impl }
    fn test2_impl(x: i32, y: i32) -> i32 { x + y }

    #[test]
    fn check_transparent_wrappers() {
        assert_eq!(test0(), 42);
        assert_eq!(test1(51), 51);
        assert_eq!(test2(21, 21), 42);
    }
}
