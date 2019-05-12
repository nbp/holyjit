{ stdenv, rust, python }:

stdenv.mkDerivation rec {
  version = "0.0.0";
  name = "holyjit-${version}";
  buildInputs = [ rust python ];
  shellHook = "
    export RUSTC_WRAPPER=$PWD/rustc.sh
    export RUST_BACKTRACE=1
  ";
}
