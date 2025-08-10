use crate::ssa::*;
use slotmap::*;

pub fn tail_call_elim(name: &str, cfg: &mut Cfg<Instr>) {
    let mut tail_calls: Vec<InstrId> = vec![];

    for (_, block) in cfg.iter_blocks() {
        let mut candidate: Option<InstrId> = None;
        let mut found: Option<Var> = None;

        for &id in block.ids.iter() {
            if let Instr::Call(dest, func, args) = &cfg[id] {
                if args.len() == cfg.args.len() && func == name {
                    candidate = Some(id);
                    found = Some(*dest);
                    continue;
                }
            }

            if let Instr::Return(Lit::Var(x)) = &cfg[id] && Some(*x) == found {
                tail_calls.push(candidate.unwrap());
                continue;
            }

            found = None;
        }
    }

    if tail_calls.len() > 0 && cfg.stack.len() == 0 {
        do_tail_elim(tail_calls, cfg);
    }
}

fn do_tail_elim(calls: Vec<InstrId>, cfg: &mut Cfg<Instr>) {

    // First copy the entry block into a new one, and add phi symbols for the arguments
    let mut env: SparseSecondaryMap<Var, Var> = SparseSecondaryMap::new();

    let entry_copy = cfg.fresh_label();


    let mut phis: Vec<Vec<(Lit, Label)>> =
        cfg
        .args
        .iter()
        .map(|&arg|vec![(Lit::Var(arg), cfg.entry())])
        .collect();

    for &call in calls.iter() {
        let label = cfg.label_instr(call);
        let mut stmt: Vec<Instr> = vec![];

        let mut found: Option<Var> = None;

        let body = cfg[label].stmt.clone();
        let ids = cfg[label].ids.clone();
        for (instr, id) in body.into_iter().zip(ids.into_iter()) {
            if let Some(id) = found {
                assert!(instr == Instr::Return(Lit::Var(id)));
                continue;
            }


            if id == call {
                let Instr::Call(dest, _, args) = instr else { panic!() };
                found = Some(dest);

                for (i, lit) in args.iter().enumerate() {
                    let id = cfg.fresh_var();
                    phis[i].push((Lit::Var(id), cfg.label_instr(call)));
                    stmt.push(Instr::Move(id, lit.clone()));
                }

                stmt.push(Instr::Jump(entry_copy));
            } else {
                stmt.push(instr.clone());
            }
        }

        cfg.set_block_stmt(label, stmt);
    }

    let mut stmt: Vec<Instr> = vec![];

    for (i, args) in phis.into_iter().enumerate() {
        let id = cfg.fresh_var();
        stmt.push(Instr::Phi(id, args));
        env.insert(cfg.args[i], id);
    }

    for block in cfg.labels() {
        let mut stmt: Vec<Instr> = vec![];

        for mut instr in cfg[block].stmt.iter().cloned() {
            for x in instr.operands_mut() {
                if env.contains_key(*x) { *x = env[*x] }
            }

            if let Instr::Phi(_, args) = &mut instr {
                args.iter_mut()
                    .for_each(|(_,l)|{
                        if *l == cfg.entry() { *l = entry_copy }
                    });
            }

            stmt.push(instr);
        }

        cfg.set_block_stmt(block, stmt);
    }

    stmt.extend(cfg[cfg.entry()].stmt.iter().cloned());

    cfg.set_block_stmt(cfg.entry(), vec![Instr::Jump(entry_copy)]);
    cfg.set_block_stmt(entry_copy, stmt);
}
