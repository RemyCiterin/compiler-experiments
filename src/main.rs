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
            }
            _ => {}
        }
    }
}

fn main() {

    let foo: &str = "
var A := 42;

def foo(x) {
    let x = 4;
    while 1 {
        if x == 0 {
            break;
        } else {
            x = x - 1;
        }
    }

    return x;
}

def bar(x) {
    let x = 42;
    while x != 0 {
        x = x - 1;
    }

    return x + foo(A);
}
    ";

    let parsed = ast::customlang::decl(foo);
    println!("parse {foo} into {:?}", parsed);

    let mut table =
        match builder::build(parsed.unwrap()) {
            Ok(table) => table,
            Err(err) => {
                builder::show_builder_error(foo, err);
                return;
            }
        };

    println!("{table}");
    optimize(&mut table);

    println!("{table}");

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
