// error-pattern:yummy
#![feature(box_syntax)]
#![feature(rustc_private)]
#![feature(decl_macro)]

//extern crate holyjit_plugin;
extern crate getopts;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_plugin;
extern crate rustc_trans_utils;
extern crate rustc_mir;
extern crate syntax;

extern crate holyjit_plugin;

use rustc_mir::transform::dump_mir;
use rustc::hir::def_id::DefId;
use rustc::mir::Mir;
use rustc::ty::TyCtxt;
use rustc::ty::maps::Providers;
use rustc_driver::{driver, Compilation, CompilerCalls, RustcDefaultCalls};
use rustc_trans_utils::trans_crate::TransCrate;
use rustc::session::{config, Session};
use rustc::session::config::{ErrorOutputType, Input};
use std::path::PathBuf;
use syntax::ast;
use std::env;
use std::process::Command;
use rustc_mir::transform::{MirPass, MirSource};

struct HolyJitCompilerCalls {
    default: RustcDefaultCalls,

    // Whether we should instrument or not the code with HolyJIT.
    add_jit: bool,
}

impl HolyJitCompilerCalls {
    fn new(add_jit: bool) -> Self {
        Self {
            default: RustcDefaultCalls,
            add_jit,
        }
    }
}

impl<'a> CompilerCalls<'a> for HolyJitCompilerCalls {
    fn early_callback(
        &mut self,
        matches: &getopts::Matches,
        sopts: &config::Options,
        cfg: &ast::CrateConfig,
        descriptions: &rustc_errors::registry::Registry,
        output: ErrorOutputType,
    ) -> Compilation {
        self.default
            .early_callback(matches, sopts, cfg, descriptions, output)
    }

    fn no_input(
        &mut self,
        matches: &getopts::Matches,
        sopts: &config::Options,
        cfg: &ast::CrateConfig,
        odir: &Option<PathBuf>,
        ofile: &Option<PathBuf>,
        descriptions: &rustc_errors::registry::Registry,
    ) -> Option<(Input, Option<PathBuf>)> {
        self.default
            .no_input(matches, sopts, cfg, odir, ofile, descriptions)
    }

    fn late_callback(
        &mut self,
        trans_crate: &TransCrate,
        matches: &getopts::Matches,
        sess: &Session,
        crate_stores: &rustc::middle::cstore::CrateStore,
        input: &Input,
        odir: &Option<PathBuf>,
        ofile: &Option<PathBuf>,
    ) -> Compilation {
        self.default
            .late_callback(trans_crate, matches, sess, crate_stores, input, odir, ofile)
    }

    fn build_controller(&mut self, sess: &Session, matches: &getopts::Matches) -> driver::CompileController<'a> {
        let mut controller = self.default.build_controller(sess, matches);

        // Extend rustc default driver, with the extra pass provided by the
        // holyjit driver. This extra pass is registered at the end of the
        // optimization made on the Mir, such that we have a desugared version
        // of Rust before it goes into the trans phase.
        if self.add_jit {
            // nbp-note: controller.provide_extern = ?
            //   https://github.com/rust-lang/rust/blob/1670a532dd769763f1d6ad9e5d624ec31361a098/src/librustc_driver/driver.rs#L1003

            // https://github.com/rust-lang/rust/blob/1670a532dd769763f1d6ad9e5d624ec31361a098/src/librustc_driver/driver.rs#L328
            //
            // controller.provide = box |providers| providers.optimized_mir = |tcx, def_id| { let mut mir = transform::optimized_mir(tcx, def_id).clone(); /* mutate mir */ tcx.alloc_mir(mir) };


            // instead of transform::optimized_mir...
            //   let mut p = ty::maps::Providers::default(); driver::default_provide(&mut p); (p.optimized_mir)(tcx, def_id)
            let old_provide = std::mem::replace(&mut controller.provide, box |_| {});
            controller.provide = box (move |providers| {
                old_provide(providers);
                // Note: this erases the default one, but we re-create the
                // default Provider within optimized_mir function in order to
                // execute all other optimization passes.
                *providers = Providers {
                    optimized_mir,
                    ..*providers
                };
            });
        }

        controller
    }
}

pub macro run_passes($tcx:ident, $mir:ident, $def_id:ident, $suite_index:expr; $($pass:expr,)*) {{
    let suite_index: usize = $suite_index;
    let run_passes = |mir: &mut _, promoted| {
        let source = MirSource {
            def_id: $def_id,
            promoted
        };
        let mut index = 0;
        let mut run_pass = |pass: &MirPass| {
            let run_hooks = |mir: &_, index, is_after| {
                dump_mir::on_mir_pass($tcx, &format_args!("{:03}-{:03}", suite_index, index),
                                      &pass.name(), source, mir, is_after);
            };
            run_hooks(mir, index, false);
            pass.run_pass($tcx, source, mir);
            run_hooks(mir, index, true);

            index += 1;
        };
        $(run_pass(&$pass);)*
    };

    run_passes(&mut $mir, None);

    for (index, promoted_mir) in $mir.promoted.iter_enumerated_mut() {
        run_passes(promoted_mir, Some(index));

        // Let's make sure we don't miss any nested instances
        assert!(promoted_mir.promoted.is_empty());
    }
}}

fn optimized_mir<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, def_id: DefId) -> &'tcx Mir<'tcx> {
    let mut p = Providers::default();
    driver::default_provide(&mut p);
    let optmir = p.optimized_mir;
    let mut mir = optmir(tcx, def_id).clone();
    // TODO
    run_passes![tcx, mir, def_id, 3;
        holyjit_plugin::AttachFnGraphOnCst,
    ];
    tcx.alloc_mir(mir)
}

fn extract_sysroot() -> String {
    // If we attempt to compile without a sysroot, then the core library would
    // not be found which would prevent the compilation of some crates. This
    // code is a work-around which either relies on the environment variables,
    // or on the original rustc compiler to extra the sysroot path.
    option_env!("SYSROOT")
        .map(String::from)
        .or_else(|| env::var("SYSROOT").ok())
        .or_else(|| {
            let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
            let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
            home.and_then(|home| toolchain.map(|toolchain| format!("{}/toolchains/{}", home, toolchain)))
        })
        .or_else(|| {
            Command::new("rustc")
                .arg("--print")
                .arg("sysroot")
                .output()
                .ok()
                .and_then(|out| String::from_utf8(out.stdout).ok())
                .map(|s| s.trim().to_owned())
        })
        .expect("need to specify SYSROOT env var during holyjit compilation, or use rustup or multirust")
}

pub fn main() {
    let mut args: Vec<String> = env::args().collect();


    // Setting RUSTC_WRAPPER causes Cargo to pass 'rustc' as the first argument.
    // We're invoking the compiler programmatically, so we ignore this.
    if args.len() <= 1 {
        std::process::exit(1);
    }
    if args[1] == "rustc" {
        // we still want to be able to invoke it normally though
        args.remove(1);
    }

    let args = if args.iter().any(|s| s == "--sysroot") {
        // User provides the --sysroot on the command line.
        args
    } else {
        // HolyJit provide the --sysroot from the environment or extracted from
        // rustc.
        args.into_iter()
            .chain(Some("--sysroot".to_owned()))
            .chain(Some(extract_sysroot()))
            .collect()
    };

    let holyjit_enabled = true;
    let mut holyjit_cc = HolyJitCompilerCalls::new(holyjit_enabled);
    rustc_driver::run(move || {
        rustc_driver::run_compiler(&args, &mut holyjit_cc, None, None)
    });
}
