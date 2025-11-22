//! This file define the register allocator of the compiler, it is based on a graph coloring
//! approach and use agressive coalescing coalescing.
//!
//! It works using multiple steps:
//! - First we solve the calling conventions of the internal function calls of the current
//!     procedure, allocate pre-allocated fresh variables (or local loads/stores) for the
//!     arguments of the function, and at each call/return instructions.
//! - Then we loop until we find a solution:
//!     - We remove all the moves without interference: if we have `move rd,rs` and `rs` and `rd`
//!         doen't interfer, then we remove the instruction from the CFG and we remove the
//!         instruction, this is agressive coalescing
//!     - We solve the coloring problem to allocate all the virtual registers in the CFG.
//!     - If we doesn't find a solution, we spill the unallocated variables: if `r` is not
//!         allocated we allocate a stack slot `s`, then each instructions of the form
//!         `x := op(..., r, ...)` is replaced by `r' := load.local 0(s); x := op(..., r', ...)`
//!         with r' a fresh variable, and each instructions of the form `r := op(...)` is replaced
//!         by `r' := op(...); store.local r', 0(s)`
//!
//! This module depends on the instruction type with the following interface:
//! - It need to known the register dependencies of the instructions (operands/destination)
//! - It need to pattern match on Move, Call and Return instructions.
//! - It need to create Move, Call, Return, Load, LocalLocal, StoreLocal (register spilling/calling
//!     conventions).

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

// Search the set of constant literals in a control flow graph
pub fn search_constants<A: Arch>(cfg: &Cfg<A::Op, A::Cond>) -> SparseSecondaryMap<Var, Lit> {
    let mut map: SparseSecondaryMap<Var, Lit> = SparseSecondaryMap::new();

    for (_, block) in cfg.iter_blocks() {
        for instr in block.stmt.iter() {
            if let Instr::Move(var, lit) = instr && !matches!(lit, Lit::Var(_)) {
                map.insert(*var, lit.clone());
            }
        }
    }

    for (_, block) in cfg.iter_blocks() {
        for instr in block.stmt.iter() {
            if let Some(dest) = instr.destination() && map.contains_key(dest) {
                if !matches!(instr, Instr::Move(..)) {
                    map.remove(dest);
                } else if let Instr::Move(_, lit) = instr && lit != &map[dest] {
                    map.remove(dest);
                }
            }
        }
    }

    return map;
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
    let avail: PhysSet =
        A::callee_saved().into_iter()
        .chain(A::caller_saved().into_iter())
        .collect();

    let callee_saved: PhysSet =
        A::callee_saved().into_iter().collect();

    let caller_saved: PhysSet =
        A::caller_saved().into_iter().collect();

    let mut spill_set: BTreeSet<Var> = BTreeSet::new();

    let mut worklist: Vec<Var> = cfg.iter_vars().map(|(v,_)| v).collect();

    let must_be_saved = search_caller_saved::<A>(cfg);

    while let Some(var) = worklist.pop() {
        if coloring.contains_key(var) { continue; }

        let others: PhysSet =
            graph.get(var)
            .unwrap_or(&HashSet::new())
            .iter()
            .filter_map(|v| {
                coloring.get(*v).cloned()
            }).map(|x| Phys(x)).collect();

        let mut set =
            if must_be_saved.contains(&var) { callee_saved.clone() }
            else { caller_saved.clone() };

        set.difference(others.clone());
        if let Some(c) = set.next() {
            coloring.insert(var, c.0);
            continue;
        }

        set = avail.clone();
        set.difference(others);
        // Allocation succede without spilling the variable
        if let Some(c) = set.next() {
            coloring.insert(var, c.0);
            continue;
        }

        // Allocation fail, we need to introduce a new register
        spill_set.insert(var);
    }

    spill_set
}

pub fn spill_vars<A: Arch>(cfg: &mut Cfg<A::Op, A::Cond>, spill: BTreeSet<Var>) {
    // We recompute the unallocated constants instead of spilling htose variables
    let constants = search_constants::<A>(cfg);

    let slots: HashMap<Var, Slot> =
        spill.iter()
        .filter(|v| !constants.contains_key(**v))
        .map(|v| (*v, cfg.fresh_stack_var(4, 2)))
        .collect();

    for block in cfg.labels() {
        let mut stmt: Vec<Instr<A::Op, A::Cond>> = vec![];

        for mut instr in cfg[block].stmt.clone() {

            for v in instr.operands_mut() {
                if spill.contains(v) {
                    if let Some(lit) = constants.get(*v) {
                        let id = cfg.fresh_var();
                        stmt.push(Instr::Move(id, lit.clone()));
                        *v = id;
                        continue;
                    }

                    let id = cfg.fresh_var();
                    stmt.push(Instr::LoadLocal{addr: slots[v], dest: id, kind: MemopKind::Word});
                    *v = id;
                }
            }

            if let Some(dest) = instr.destination_mut() && spill.contains(dest) {
                if constants.contains_key(*dest) {continue;}
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
                        if slots.len() == i { slots.push(cfg.fresh_stack_var(4, 2)); }
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
