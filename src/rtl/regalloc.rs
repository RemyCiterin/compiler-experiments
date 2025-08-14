use std::collections::{BTreeSet, HashMap};

use crate::interference::*;
use crate::liveness::*;
use crate::ssa::*;
use super::*;

use slotmap::*;

pub type Coloring = SparseSecondaryMap<Var, usize>;

pub fn show_coloring(color: &Coloring) {
    for (v, c) in color {
        println!("\tcolor({v}) := {c}");
    }
}

/// Search "preferences" in register allocation: each times we see a ```move x, y```, and if in the
/// coloring algotithm the color of `x` is available, then we choose this color in priority
pub fn coalessing<A: Arch>(cfg: &Rtl<A::Op, A::Cond>) -> SparseSecondaryMap<Var, BTreeSet<Var>> {
    let mut map = SparseSecondaryMap::<Var, BTreeSet<Var>>::new();

    for (var, _) in cfg.iter_vars() {
        map.insert(var, BTreeSet::new());
    }

    for (_, block) in cfg.iter_blocks() {
        for instr in block.stmt.iter() {
            if let RInstr::Move(dst, Lit::Var(src)) = instr {
                map[*dst].insert(*src);
                map[*src].insert(*dst);
            }
        }
    }

    map
}

/// Prepare coloring, in particular it introduce copies and precolor registers for
/// calls/return instructions
pub fn prepare_coloring<A: Arch>(cfg: &mut Rtl<A::Op, A::Cond>) -> Coloring {
    let mut color: Coloring = Coloring::new();

    let args: HashMap<Var, Var> =
        cfg.args.clone().into_iter()
        .map(|v| (v, cfg.fresh_var()))
        .collect();

    for i in 0..cfg.args.len() {
        color.insert(cfg.args[i], A::arg_regs()[i].0);
    }

    for block in cfg.labels() {
        let mut stmt: Vec<RInstr<A::Op, A::Cond>> = vec![];

        if block == cfg.entry() {
            for (&old, &new) in args.iter() {
                stmt.push(RInstr::Move(new, Lit::Var(old)));
            }
        }

        for mut instr in cfg[block].stmt.clone() {
            for x in instr.operands_mut() {
                if let Some(y) = args.get(x) {
                    *x = *y;
                }
            }

            if let Some(x) = instr.destination_mut() {
                if let Some(y) = args.get(x) {
                    *x = *y;
                }
            }

            match instr {
                RInstr::Call(dest, name, args) => {
                    let new_args: Vec<Var> = args.iter().map(|_| cfg.fresh_var()).collect();
                    let new_dest = cfg.fresh_var();

                    for i in 0..args.len() {
                        stmt.push(RInstr::Move(new_args[i], Lit::Var(args[i])));
                        color.insert(new_args[i], A::arg_regs()[i].0);
                    }

                    stmt.push(RInstr::Call(new_dest, name, new_args));

                    color.insert(new_dest, A::ret_reg().0);
                    stmt.push(RInstr::Move(dest, Lit::Var(new_dest)));

                }
                RInstr::Return(var) => {
                    let id = cfg.fresh_var();
                    stmt.push(RInstr::Move(id, Lit::Var(var)));
                    stmt.push(RInstr::Return(id));
                    color.insert(id, A::ret_reg().0);
                }
                _ => stmt.push(instr),
            }
        }

        cfg.set_block_stmt(block, stmt);
    }

    color
}

pub fn solve_coloring<A: Arch>(
    cfg: &Cfg<RInstr<A::Op, A::Cond>>,
    coloring: &mut Coloring,
    heuristic: SparseSecondaryMap<Var, BTreeSet<Var>>
) -> BTreeSet<Var> {
    let mut liveness = Liveness::new(cfg);
    liveness.run(cfg);

    let mut graph = InterferenceGraph::new(cfg);
    graph.run(cfg, &liveness);

    let avail: BTreeSet<usize> =
        A::callee_saved().into_iter()
        .chain(A::caller_saved().into_iter())
        .map(|x|x.0)
        .collect();

    let mut spill_set: BTreeSet<Var> = BTreeSet::new();

    let mut worklist: Vec<Var> = cfg.iter_vars().map(|(v,_)| v).collect();

    'main_loop: while let Some(var) = worklist.pop() {
        if coloring.contains_key(var) { continue; }

        let others: BTreeSet<usize> =
            graph[var].iter()
            .filter_map(|v| {
                coloring.get(*v).cloned()
            }).collect();

        for v in heuristic[var].iter() {
            if let Some(c) = coloring.get(*v) && !others.contains(c) {
                coloring.insert(var, *c);
                continue 'main_loop;
            }
        }


        // Allocation succede without spilling the variable
        if let Some(c) = avail.difference(&others).into_iter().next() {
            coloring.insert(var, *c);
            continue;
        }

        // Allocation fail, we need to introduce a new register
        spill_set.insert(var);
    }

    spill_set
}

pub fn spill_vars<A: Arch>(cfg: &mut Rtl<A::Op, A::Cond>, spill: BTreeSet<Var>) {

    let slots: HashMap<Var, Slot> =
        spill.iter()
        .map(|v| (*v, cfg.fresh_stack_var(4)))
        .collect();

    for block in cfg.labels() {
        let mut stmt: Vec<RInstr<A::Op, A::Cond>> = vec![];

        for mut instr in cfg[block].stmt.clone() {

            for v in instr.operands_mut() {
                if spill.contains(v) {
                    let id = cfg.fresh_var();
                    stmt.push(RInstr::LoadLocal{addr: slots[v], dest: id});
                    *v = id;
                }
            }

            if let Some(dest) = instr.destination_mut() && spill.contains(dest) {
                let id = cfg.fresh_var();
                let store =
                    RInstr::StoreLocal{val: id, addr: slots[dest]};
                *dest = id;
                stmt.push(instr);
                stmt.push(store);
            } else {
                stmt.push(instr);
            }
        }

        cfg.set_block_stmt(block, stmt);
    }
}

pub fn alloc_register<A:Arch>(cfg: &mut Rtl<A::Op, A::Cond>) -> Coloring {
    let color = prepare_coloring::<A>(cfg);

    loop {
        let mut copy = color.clone();
        let heuristic = coalessing::<A>(cfg);
        let spill_set = solve_coloring::<A>(cfg, &mut copy, heuristic);

        if spill_set.is_empty() {
            //println!("{cfg}");
            let saved = A::caller_saved().into_iter().collect();
            save_caller_saved::<A>(cfg, &copy, saved);
            return copy;
        }

        spill_vars::<A>(cfg, spill_set);

    }
}

pub fn save_caller_saved<A: Arch>(cfg: &mut Rtl<A::Op, A::Cond>, color: &Coloring, saved: BTreeSet<Phys>) {
    let mut liveness = Liveness::new(cfg);
    liveness.run(cfg);

    let mut slots: Vec<Slot> = vec![];

    for block in cfg.labels() {
        let mut stmt: Vec<RInstr<A::Op, A::Cond>> = vec![];
        let mut lives = liveness[block].outputs.clone();

        //println!("{block}:");
        //for x in lives.iter() { print!(" {x}"); }
        //println!();

        for instr in cfg[block].stmt.clone().into_iter().rev() {
            if let Some(dest) = instr.destination() {
                lives.remove(&dest);
            }

            let is_call = matches!(instr, RInstr::Call(..));

            if is_call {
                let mut i: usize = 0;
                for &v in lives.iter() {
                    if saved.contains(&Phys(color[v])) {
                        if slots.len() == i { slots.push(cfg.fresh_stack_var(4)); }
                        stmt.push( RInstr::LoadLocal{addr: slots[i], dest: v} );

                        i += 1;
                    }
                }
            }

            stmt.push(instr.clone());

            if is_call {
                let mut i: usize = 0;
                for &v in lives.iter() {
                    if saved.contains(&Phys(color[v])) {
                        stmt.push( RInstr::StoreLocal{addr: slots[i], val: v} );

                        i += 1;
                    }
                }
            }

            for x in instr.operands() {
                lives.insert(x);
            }
        }

        stmt.reverse();

        cfg.set_block_stmt(block, stmt);
    }

}
