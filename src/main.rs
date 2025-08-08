use compiler_experiments::*;

use std::io::prelude::*;

use builder;

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

                let mut gvn = gvn::ValueTable::new();
                gvn.run(cfg);

                let mut copy = copy_prop::CopyProp::new(&cfg);
                copy.run(cfg);

                cfg.gc();

                // let translator = codegen::Translator::new(cfg);
                // let mut cfg = translator.translate(cfg);

                // let mut conv = out_of_ssa::Conventionalize::new(&cfg);
                // conv.run(&mut cfg);

                // out_of_ssa::out_of_ssa(&mut cfg);

                // println!("cfg (ssa): \n{}", cfg);
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
    let file_name = std::env::args().nth(1).unwrap();
    let mut file = std::fs::File::open(file_name).unwrap();

    let mut program: String = String::new();

    file.read_to_string(&mut program).unwrap();

    let parsed = ast::customlang::decl(&program);

    match &parsed {
        Err(peg::error::ParseError{location, expected}) => {
            let msg = format!("unexpected token, expect {}", expected);
            ast::show_error(&msg, &program, *location, *location);
            return;
        }
        _ => {}
    }

    let mut table =
        match builder::build(parsed.unwrap()) {
            Ok(table) => table,
            Err(err) => {
                builder::show_builder_error(&program, err);
                return;
            }
        };

    println!("{program}");
    //println!("{table}");
    optimize(&mut table);

    println!("{table}");

    let mut interp = interpreter::Interpreter::new(&table);
    interp.interpret_function();

    println!("instret: {} loads: {} stores: {} calls: {}",
        interp.instret, interp.loads, interp.stores, interp.calls);

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
