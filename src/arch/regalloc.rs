use std::collections::{BTreeSet, HashMap, HashSet};

use crate::utils::union_find::*;
use crate::ssa::interference::*;
use crate::ssa::liveness::*;
use crate::ssa::*;
use crate::arch::*;

use slotmap::*;

pub type Coloring = SparseSecondaryMap<Var, usize>;

pub fn show_coloring(color: &Coloring) {
    for (v, c) in color {
        println!("\tcolor({v}) := {c}");
    }
}

pub fn aggressive_coalescing<A: Arch>(
    cfg: &mut Cfg<A::Op, A::Cond>,
    color: &mut Coloring,
) -> InterferenceGraph {
    let mut liveness = Liveness::new(cfg);
    liveness.run(cfg);

    let mut graph = InterferenceGraph::new(cfg);
    graph.run(cfg, &liveness);

    let mut uf: UnionFind<Var> = UnionFind::new();

    for (var, _) in cfg.iter_vars() {
        uf.insert(var);
    }

    for (_, block) in cfg.iter_blocks() {
        for instr in block.stmt.iter() {
            if let Instr::Move(new, Lit::Var(old)) = instr {
                let new = uf.find(*new);
                let old = uf.find(*old);

                // To combine two registers, they:
                // - Need to use the same physical register if they are already allocated
                // - if one is already allocated, we must not create a conflict with another
                // register of the same color
                let mut same_color =
                    !color.contains_key(old) ||
                    !color.contains_key(new) ||
                    (color[old] == color[new]);

                if color.contains_key(new) {
                    for &x in graph[old].iter() {
                        same_color &=
                            !color.contains_key(x) ||
                            (color[new] != color[x]);
                    }
                }

                if color.contains_key(old) {
                    for &x in graph[new].iter() {
                        same_color &=
                            !color.contains_key(x) ||
                            (color[old] != color[x]);
                    }
                }

                // In addition those registers need to have no interferences
                let no_interference =
                    !graph[new].contains(&old);

                if new != old && same_color && no_interference {
                    if color.contains_key(old) { color.insert(new, color[old]); }
                    graph.merge(new, old);
                    uf.merge(new, old);
                }
            }
        }
    }

    for label in cfg.labels() {
        let mut stmt: Vec<Instr<A::Op, A::Cond>> = vec![];

        for mut instr in cfg[label].stmt.iter().cloned() {
            if let Instr::Move(new, Lit::Var(old)) = instr
                && uf.find(new) == uf.find(old) {
                continue;
            }

            for x in instr.operands_mut() {
                *x = uf.find(*x);
            }

            if let Some(x) = instr.destination_mut() {
                *x = uf.find(*x);
            }

            stmt.push(instr);
        }

        cfg.set_block_stmt(label, stmt);
    }

    graph
}

pub fn search_caller_saved<A: Arch>(cfg: &Cfg<A::Op, A::Cond>) -> BTreeSet<Var> {
    let mut liveness = Liveness::new(cfg);
    liveness.run(cfg);

    let mut set: BTreeSet<Var> = BTreeSet::new();

    for block in cfg.labels() {
        let mut lives = liveness[block].outputs.clone();

        for instr in cfg[block].stmt.iter().rev() {
            if let Some(dest) = instr.destination() {
                lives.remove(&dest);
            }

            if matches!(instr, Instr::Call(..)) {
                for &v in lives.iter() {
                    set.insert(v);
                }
            }

            for x in instr.operands() {
                lives.insert(x);
            }
        }
    }

    set
}

/// Prepare coloring, in particular it introduce copies and precolor registers for
/// calls/return instructions
pub fn prepare_coloring<A: Arch>(cfg: &mut Cfg<A::Op, A::Cond>) -> Coloring {
    let mut color: Coloring = Coloring::new();
    let arg_regs = A::arg_regs();

    let args: HashMap<Var, Var> =
        cfg.args.clone().into_iter()
        .map(|v| (v, cfg.fresh_var()))
        .collect();

    let mut incoming: Vec<Instr<A::Op, A::Cond>> = vec![];

    for i in 0..cfg.args.len() {
        if i < arg_regs.len() {
            color.insert(cfg.args[i], arg_regs[i].0);
        } else {
            let slot = cfg.fresh_incoming_var(i - arg_regs.len());
            incoming.push(Instr::LoadLocal{dest: cfg.args[i], addr: slot, kind: MemopKind::Word});
        }
    }

    for block in cfg.labels() {
        let mut stmt: Vec<Instr<A::Op, A::Cond>> = vec![];

        if block == cfg.entry() {
            stmt.extend(incoming.iter().cloned());

            for (&old, &new) in args.iter() {
                stmt.push(Instr::Move(new, Lit::Var(old)));
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
                Instr::Call(dest, name, args) => {
                    let new_dest = cfg.fresh_var();

                    let mut new_args: Vec<Var> = vec![];

                    for i in 0..args.len() {
                        if i >= arg_regs.len() {
                            let slot = cfg.fresh_outgoing_var(i - arg_regs.len());
                            stmt.push(
                                Instr::StoreLocal{val: args[i], addr: slot, kind: MemopKind::Word});
                        } else {
                            let id = cfg.fresh_var();
                            stmt.push(Instr::Move(id, Lit::Var(args[i])));
                            color.insert(id, arg_regs[i].0);
                            new_args.push(id);
                        }
                    }

                    stmt.push(Instr::Call(new_dest, name, new_args));

                    color.insert(new_dest, A::ret_reg().0);
                    stmt.push(Instr::Move(dest, Lit::Var(new_dest)));

                }
                Instr::Return(var) => {
                    let id = cfg.fresh_var();
                    stmt.push(Instr::Move(id, Lit::Var(var)));
                    stmt.push(Instr::Return(id));
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
    cfg: &Cfg<A::Op, A::Cond>,
    coloring: &mut Coloring,
    graph: InterferenceGraph,
) -> BTreeSet<Var> {
    let avail: BTreeSet<usize> =
        A::callee_saved().into_iter()
        .chain(A::caller_saved().into_iter())
        .map(|x|x.0)
        .collect();

    let callee_saved: BTreeSet<usize> =
        A::callee_saved().into_iter().map(|x|x.0).collect();

    let caller_saved: BTreeSet<usize> =
        A::caller_saved().into_iter().map(|x|x.0).collect();

    let mut spill_set: BTreeSet<Var> = BTreeSet::new();

    let mut worklist: Vec<Var> = cfg.iter_vars().map(|(v,_)| v).collect();

    let must_be_saved = search_caller_saved::<A>(cfg);

    while let Some(var) = worklist.pop() {
        if coloring.contains_key(var) { continue; }

        let others: BTreeSet<usize> =
            graph.get(var)
            .unwrap_or(&HashSet::new())
            .iter()
            .filter_map(|v| {
                coloring.get(*v).cloned()
            }).collect();

        if must_be_saved.contains(&var) {
            if let Some(c) = callee_saved.difference(&others).into_iter().next() {
                coloring.insert(var, *c);
                continue;
            }
        } else {
            if let Some(c) = caller_saved.difference(&others).into_iter().next() {
                coloring.insert(var, *c);
                continue;
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

pub fn spill_vars<A: Arch>(cfg: &mut Cfg<A::Op, A::Cond>, spill: BTreeSet<Var>) {

    let slots: HashMap<Var, Slot> =
        spill.iter()
        .map(|v| (*v, cfg.fresh_stack_var(4)))
        .collect();

    for block in cfg.labels() {
        let mut stmt: Vec<Instr<A::Op, A::Cond>> = vec![];

        for mut instr in cfg[block].stmt.clone() {

            for v in instr.operands_mut() {
                if spill.contains(v) {
                    let id = cfg.fresh_var();
                    stmt.push(Instr::LoadLocal{addr: slots[v], dest: id, kind: MemopKind::Word});
                    *v = id;
                }
            }

            if let Some(dest) = instr.destination_mut() && spill.contains(dest) {
                let id = cfg.fresh_var();
                let store =
                    Instr::StoreLocal{val: id, addr: slots[dest], kind: MemopKind::Word};
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

pub fn alloc_register<A:Arch>(cfg: &mut Cfg<A::Op, A::Cond>) -> Coloring {
    let mut color =
        prepare_coloring::<A>(cfg);

    loop {

        let graph =
            aggressive_coalescing::<A>(cfg, &mut color);

        let mut copy = color.clone();

        let spill_set =
            solve_coloring::<A>(cfg, &mut copy, graph);

        if spill_set.is_empty() {
            let saved = A::caller_saved().into_iter().collect();
            save_caller_saved::<A>(cfg, &copy, saved);
            return copy;
        }

        spill_vars::<A>(cfg, spill_set);
    }
}

pub fn save_caller_saved<A: Arch>
    (cfg: &mut Cfg<A::Op, A::Cond>, color: &Coloring, saved: BTreeSet<Phys>) {
    let mut liveness = Liveness::new(cfg);
    liveness.run(cfg);

    let mut slots: Vec<Slot> = vec![];

    for block in cfg.labels() {
        let mut stmt: Vec<Instr<A::Op, A::Cond>> = vec![];
        let mut lives = liveness[block].outputs.clone();

        for instr in cfg[block].stmt.clone().into_iter().rev() {
            if let Some(dest) = instr.destination() {
                lives.remove(&dest);
            }

            let is_call = matches!(instr, Instr::Call(..));

            if is_call {
                let mut i: usize = 0;
                for &v in lives.iter() {
                    if saved.contains(&Phys(color[v])) {
                        if slots.len() == i { slots.push(cfg.fresh_stack_var(4)); }
                        stmt.push( Instr::LoadLocal{addr: slots[i], dest: v, kind: MemopKind::Word} );

                        i += 1;
                    }
                }
            }

            stmt.push(instr.clone());

            if is_call {
                let mut i: usize = 0;
                for &v in lives.iter() {
                    if saved.contains(&Phys(color[v])) {
                        stmt.push( Instr::StoreLocal{addr: slots[i], val: v, kind: MemopKind::Word} );

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
