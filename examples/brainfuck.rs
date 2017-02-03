#![feature(plugin, custom_attribute)]
#![plugin(holyjit_plugin)]
#![feature(unboxed_closures)]

#[macro_use] extern crate holyjit_lib as hj;

struct JitContext;

jit!{ fn eval(jc: JitContext, program: String) -> Result<(), ()> = eval_impl }
fn eval_impl(_jc: JitContext, program: String) -> Result<(), ()> {
    let prog = program.as_bytes();
    let mut pc : usize = 0;
    let mut val : u8 = 0;
    let mut mem : Vec<u8> = Vec::with_capacity(256);
    mem.resize(256, 0);
    loop {
        if pc >= prog.len() {
            return Ok(());
        }
        match prog[pc] {
            b'>' => { val += 1; }
            b'<' => { val -= 1; }
            b'-' => { mem[val as usize] += 1; }
            b'+' => { mem[val as usize] -= 1; }
            b'.' => { panic!("putchar: NYI"); }
            b',' => { panic!("getchar: NYI"); }
            b'[' => {
                if val == 0 {
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
            }
            _ => { panic!("Unknown Symbol"); }
        }
        pc += 1;
    }
}

fn main() {
    let jc = JitContext{};
    eval(jc, "".into()).unwrap()
}
