use compiler_experiments::*;

use compiler_experiments::builder;

pub fn test_ssa(program: &str) {
    let parsed = ast::customlang::stmt(program);

    let mut builder = builder::Builder::new();
    builder.gen_stmt(parsed.clone().unwrap());

    println!("program: \n{}", program);

    let mut cfg = builder.cfg();

    println!("cfg (no-ssa): \n{}", cfg);

    let mut dom = compiler_experiments::dominance::Dominance::new(&cfg);
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

    compiler_experiments::into_ssa::into_ssa(&mut cfg);
    println!("cfg (ssa): \n{}", cfg);

    let mut gvn = compiler_experiments::gvn::Gvn::new();
    gvn.run_analyse(&cfg);

    gvn.show();
}

fn main() {

    let program: &str = "
    x := 2;
    y := 4;
    while x != 3 {
        if x == 2 then {
            x := 1;
        } else {
            x := 3;
        }
    }

    x := 42;

    while x != 0 {
        x := x - 1;
    }

    x := x + y;
    return x;
    ";

    test_ssa(program);
}
