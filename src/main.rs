use compiler_experiments::*;

use std::collections::HashSet;

use builder;

pub fn test_ssa(program: &str) {
    let parsed = ast::customlang::stmt(program);

    let mut builder = builder::Builder::new(vec![], HashSet::new());

    if let Err(err) = builder.gen_stmt(parsed.clone().unwrap()) {
        builder::show_builder_error(program, err);
        return;
    }

    println!("program: \n{}", program);

    let mut cfg = builder.cfg();

    println!("cfg (no-ssa): \n{}", cfg);

    let mut dom = dominance::Dominance::new(&cfg);
    dom.run(&cfg);

    for (label, _) in cfg.iter_blocks() {
        print!("idom({}) := ", label);
        if dom.reachable(label) {
            println!("{}", dom.idom(label));
        } else {
            println!("_");
        }
    }

    for (label, _) in cfg.iter_blocks() {
        print!("frontier({}) := ", label);
        if dom.reachable(label) {
            for b in dom.frontier(label).iter() {
                print!("{} ", b);
            }
            println!("");
        } else {
            println!("_");
        }
    }

    let mut to_ssa = into_ssa::IntoSsaTransform::new(&cfg);
    to_ssa.run(&mut cfg);
    //into_ssa::into_ssa(&mut cfg);
    println!("cfg (ssa): \n{}", cfg);

    let mut mem2reg = mem_to_reg::MemToReg::new(&cfg);
    mem2reg.run(&mut cfg);

    println!("cfg (ssa): \n{}", cfg);

    let mut simplifier = simplify_ssa::Simplifier::new(&cfg);
    simplifier.run(&mut cfg);

    println!("cfg (ssa): \n{}", cfg);

    cfg.gc();
    println!("cfg (ssa): \n{}", cfg);


    let mut copy = copy_prop::CopyProp::new(&cfg);
    copy.run(&mut cfg);

    println!("cfg (ssa): \n{}", cfg);

    let mut conv = out_of_ssa::Conventionalize::new(&cfg);
    conv.run(&mut cfg);

    println!("cfg (ssa): \n{}", cfg);

    out_of_ssa::out_of_ssa(&mut cfg);

    println!("cfg (ssa): \n{}", cfg);

}

pub fn optimize(table: &mut ssa::SymbolTable<ssa::Instr>) {
    for (_, section) in table.symbols.iter_mut() {
        match section {
            ssa::Section::Text(cfg) => {
                let mut to_ssa = into_ssa::IntoSsaTransform::new(&cfg);
                to_ssa.run(cfg);

                let mut mem2reg = mem_to_reg::MemToReg::new(&cfg);
                mem2reg.run(cfg);

                let mut simplifier = simplify_ssa::Simplifier::new(&cfg);
                simplifier.run(cfg);

                let mut copy = copy_prop::CopyProp::new(&cfg);
                copy.run(cfg);

                cfg.gc();

                println!("cfg (ssa): \n{}", cfg);

                let mut conv = out_of_ssa::Conventionalize::new(&cfg);
                conv.run(cfg);

                out_of_ssa::out_of_ssa(cfg);
            }
            _ => {}
        }
    }
}

pub fn fibo(x: i32) -> i32 {
    if x < 2 { x }
    else { fibo(x-1) + fibo(x-2) }
}

fn main() {

    let foo: &str = "
var A = 42;

var buf[100] = 0;

def fibo_mem() {
    let y = 2;
    buf[0] = 0;
    buf[1] = 1;

    while y != 100 {
        buf[y] = buf[y-1] + buf[y-2];
        y = y + 1;
    }
}

def fibo(x) {
    if (x == 0) | (x == 1) {
        return x;
    } else {
        return fibo(x-1) + fibo(x-2);
    }
}

def foo(x) {
    let counter = 0;
    while 1 {
        if x == 0 {
            break;
        } else {
            x = x - 1;
            counter = counter + 1;
        }
    }

    return x;
}

def bar(x) {
    while x != 0 {
        x = x - 1;
    }

    return x + foo(A);
}

def update(x) {
    *x = 53;
    return 0;
}

def main() {
    let _ = fibo_mem();
    let x = buf[50];
    let _ = print_i32(x);
    *(&x) = 42;
    let _ = print_i32(x);
    let _ = update(&x);
    let _ = print_i32(x);

    return bar(2);
}
    ";

    let parsed = ast::customlang::decl(foo);

    println!("{:?}", parsed);

    let mut table =
        match builder::build(parsed.unwrap()) {
            Ok(table) => table,
            Err(err) => {
                builder::show_builder_error(foo, err);
                return;
            }
        };

    println!("{foo}");
    println!("{table}");
    optimize(&mut table);

    println!("{table}");

    let mut interp = interpreter::Interpreter::new(&table);
    interp.interpret_function();

    println!("instret: {} loads: {} stores: {}", interp.instret, interp.loads, interp.stores);

    //println!("{}", fibo(50));

    //  let program: &str = "
    //  let x = 2;
    //  let y = 4;
    //  while x != 3 {
    //      if x == 2 {
    //          x = 1;
    //      } else {
    //          x = 3;
    //      }
    //  }

    //  x = 42;

    //  while x != 0 {
    //      x = x - 1;
    //  }

    //  x = x + y;
    //  return x;
    //  ";

    //  test_ssa(program);
}
