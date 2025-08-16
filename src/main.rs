use compiler_experiments::*;

use std::collections::HashMap;

use std::io::prelude::*;

use builder;

pub fn into_ssa(table: &mut ssa::SymbolTable<ssa::Instr>) {
    for (_, section) in table.symbols.iter_mut() {
        match section {
            ssa::Section::Text(cfg) => {
                let mut to_ssa = into_ssa::IntoSsaTransform::new(&cfg);
                to_ssa.run(cfg);
            }
            _ => {}
        }
    }
}

pub fn optimize(table: &mut ssa::SymbolTable<ssa::Instr>) {
    for (name, section) in table.symbols.iter_mut() {
        match section {
            ssa::Section::Text(cfg) => {
                let mut mem2reg = mem_to_reg::MemToReg::new(&cfg);
                mem2reg.run(cfg);

                let mut simplifier = simplify_ssa::Simplifier::new(&cfg);
                simplifier.run(cfg);

                instcombine::combine_instructions(cfg);

                let mut gvn = gvn::ValueTable::new();
                gvn.run(cfg);

                tail_call_elim::tail_call_elim(name, cfg);

                let mut copy = copy_prop::CopyProp::new(&cfg);
                copy.run(cfg);

                //licm::licm(cfg);

                cfg.gc();
            }
            _ => {}
        }
    }
}

pub fn translate(table: ssa::SymbolTable<ssa::Instr>) -> ssa::SymbolTable<rtl::rv32::RvInstr> {
    let mut symbols = HashMap::new();

    for (name, section) in table.symbols.into_iter() {
        match section {
            ssa::Section::Text(cfg) => {

                let mut cfg = rtl::rv32::translate(cfg);

                let mut gvn = rtl::gvn::ValueTable::new();
                gvn.run(&mut cfg);

                let mut dce = rtl::dce::Dce::new();
                dce.run(&mut cfg);

                let mut conv = out_of_ssa::Conventionalize::new(&cfg);
                conv.run(&mut cfg);

                out_of_ssa::out_of_ssa(&mut cfg);

                symbols.insert(name, ssa::Section::Text(cfg));
            }
            ssa::Section::Data(v) =>
                _ = symbols.insert(name, ssa::Section::Data(v.clone())),
        }
    }

    ssa::SymbolTable{symbols}
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

    let parsed = parser::customlang::decl(&program);

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

    //println!("{program}");
    //println!("{table}");
    into_ssa(&mut table);
    optimize(&mut table);

    //table.pp_text();

    let mut interp = interpreter::Interpreter::new(&table);
    interp.interpret_function();
    println!("{}", interp.stats);

    let rtl_table = translate(table);

    //rtl_table.pp_text();

    let ltl_table: ltl::LtlSymbolTable<rtl::rv32::RvArch>
        = ltl::LtlSymbolTable::new(rtl_table);

    let mut interp =
        ltl::interpreter::Interpreter::new(&ltl_table);
    interp.interpret_function();

    println!("{}", interp.stats);

    let file_name = std::env::args().nth(2).unwrap();
    let mut file = std::fs::File::create(file_name).unwrap();

    file.write_all(format!("{ltl_table}").as_bytes()).unwrap();
}
