#![feature(custom_attribute)]
#![feature(unboxed_closures)]
#[macro_use] extern crate holyjit_lib as hj;

jit!{ fn eval(jc: hj::JitContext, program: String) -> Result<(), ()> = eval_impl in jc; }
fn eval_impl(_jc: hj::JitContext, program: String) -> Result<(), ()> {
    let prog = program.as_bytes();
    let mut pc : usize = 0;
    let mut ptr : usize = 0;
    let mut mem : Vec<u8> = Vec::with_capacity(256);
    mem.resize(256, 0);
    loop {
        if pc >= prog.len() {
            return Ok(());
        }
        match prog[pc] {
            b'>' => {
                ptr += 1;
                if ptr >= mem.len() {
                    mem.push(0);
                }
            }
            b'<' => { ptr = ptr.saturating_sub(1); }
            b'-' => { mem[ptr] = mem[ptr].wrapping_sub(1); }
            b'+' => { mem[ptr] = mem[ptr].wrapping_add(1); }
            b'.' => { panic!("putchar: NYI"); }
            b',' => { panic!("getchar: NYI"); }
            b'[' => {
                if mem[ptr] == 0 {
                    let mut iter = (pc + 1, 0);
                    loop {
                        iter = match (iter, prog[iter.0]) {
                            ((p, 0), b']') => {
                                pc = p + 1;
                                break;
                            },
                            ((p, d), b'[') => (p + 1, d + 1),
                            ((p, d), b']') => (p + 1, d - 1),
                            ((p, d), _) => (p + 1, d)
                        }
                    }
                    continue; // skip pc increment
                }
            }
            b']' => {
                let mut iter = (pc - 1, 0);
                loop {
                    iter = match (iter, prog[iter.0]) {
                        ((p, 0), b'[') => {
                            pc = p;
                            break;
                        },
                        ((p, d), b'[') => (p - 1, d + 1),
                        ((p, d), b']') => (p - 1, d - 1),
                        ((p, d), _) => (p - 1, d)
                    }
                }
                continue; // skip pc increment
            }
            _ => { panic!("Unknown Symbol"); }
        }
        pc += 1;
    }
}

fn main() {
    let prog = "++";
    // Run without the Jit.
    let jc : hj::JitContext = Default::default();
    eval_impl(jc, prog.into()).unwrap();
    // Run with the Jit.
    let jc : hj::JitContext = Default::default();
    eval(jc, prog.into()).unwrap();
}
