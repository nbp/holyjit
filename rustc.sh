#!/bin/sh

debug=true

# CLI: rustc --crate name path/name.rs
wrap=false;
case $4 in
    tests/*) wrap=true;;
    examples/*) wrap=true;;
    benches/*) wrap=true;;
esac

if $wrap; then
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

