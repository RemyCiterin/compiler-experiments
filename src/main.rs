use compiler_experiments::*;

use builder;

pub fn test_ssa(program: &str) {
    let parsed = ast::customlang::stmt(program);

    let mut builder = builder::Builder2::new();
    builder.gen_stmt(parsed.clone().unwrap());

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

    // let mut simplifier = simplify_ssa::Simplifier::new(&cfg);
    // simplifier.run(&mut cfg);

    // println!("cfg (ssa): \n{}", cfg);


    // let mut gvn = gvn::Gvn::new();
    // gvn.run_analyse(&cfg);

    // gvn.show();

    // cfg.gc();

    // let mut liveness = liveness::Liveness::new(&cfg);
    // liveness.run(&cfg);

    // let mut matrix = interference::InterferenceGraph::new(&cfg);
    // matrix.run(&cfg, &liveness);

    // for (var, _) in cfg.iter_vars() {
    //     print!("interference({}) := [", var);
    //     for x in matrix[var].iter() {
    //         print!(" {}", x);
    //     } println!(" ]");
    // }
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
