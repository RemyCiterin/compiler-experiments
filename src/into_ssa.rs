//! This module is used to transform a control flow graph into a Static Single Assignment form,
//! it does it by taking as argument a CFG and returning a new equivalent CSF in SSA form.

use crate::ssa::*;
use std::collections::HashMap;
use crate::liveness::*;

pub fn into_ssa(cfg: &mut Cfg) {
    // Used to compute the translation as ssa of the output variables of each block
    let mut globals: HashMap<Label, HashMap<Var, Var>> = HashMap::new();

    // Phi expressions to insert in each block
    let mut phis: HashMap<Label, Vec<Instr>> = HashMap::new();

    // Liveness analysis
    let mut liveness = Liveness::new(cfg);
    liveness.run(cfg);

    let order = cfg.preorder().clone();

    // Compute phis and rename variables (except in phi expressions)
    for &block in order.iter() {
        // Start by creating a fresh variable for each live variable at input of the block
        let mut map: HashMap<Var, Var> = HashMap::new();
        let mut stmt = vec![];

        for &var in liveness[block].inputs.iter() {
            let v = cfg.fresh_var();
            map.insert(var, v);
            stmt.push(
                Instr::Phi(v, cfg.callers(block).iter().map(|l| (var, *l)).collect())
            );
        }

        phis.insert(block, stmt);

        // Now rename each variable of the block
        let mut stmt = cfg[block].stmt.clone();

        for instr in stmt.iter_mut() {
            for x in instr.operands_mut() { *x = map[x]; }
            if let Some(x) = instr.destination_mut() {
                let y = cfg.fresh_var();
                map.insert(*x, y);
                *x = y;
            }
        }

        globals.insert(block, map);
        cfg.set_block_stmt(block, stmt);
    }

    // Rename phi variables and add phis to blocks
    for &block in order.iter() {
        let mut stmt = phis.remove(&block).unwrap();

        for phi in stmt.iter_mut() {
            match phi {
                Instr::Phi(_, vars) => {
                    for (v, l) in vars.iter_mut() {
                        *v = globals[l][v];
                    }
                }
                _ => {}
            }
        }

        // Add phis expressions to each blocks
        stmt.extend(cfg[block].stmt.iter().cloned());
        cfg.set_block_stmt(block, stmt);
    }

    // Start to compute the block of definition of each variable
    cfg.start_ssa();
}
