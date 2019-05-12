#!/bin/sh

debug=false

# CLI: rustc --crate name path/name.rs
wrap=false;
case $4 in
    tests/*) wrap=true;;
    examples/*) wrap=true;;
    benches/*) wrap=true;;
esac

if $wrap; then
    # Apparently, on MacOS/X we seems to fail to link against rustc libraries.
    # In the mean time, just add rust libraries to the LD_LIBRARY_PATH.
    #export LD_LIBRARY_PATH=$(rustc --print sysroot)/lib
    for arg; do
        case $arg in
            # .../target/debug/deps/libholyjit_lib-abcdef.rlib
            holyjit_lib=*) holyjit_lib=${arg#*=};;
        esac
    done

    test -z "$holyjit_lib" && \
        echo 1>&2 "Unable to find holyjit_lib on the command line."

    holyjit=$(dirname $(dirname $holyjit_lib))/holyjit
    test \! -x $holyjit && \
        echo 1>&2 "Unable to find $holyjit binary."

    if $debug; then
        log_dir=$(dirname $holyjit)/log
        mkdir -p $log_dir
        exec $holyjit "$@" -Z dump-mir=all -Z dump-mir-dir=$log_dir
    else
        exec $holyjit "$@"
    fi
fi

exec "$@"

