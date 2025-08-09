/// This module is used to generate sequential copies of variables from a list of parallel copies
/// of those variables, by introducing new fresh variables in case of a cycle.

use crate::ssa::*;


pub fn copies_to_moves(
    mut fresh: impl FnMut() -> Var,
    mut copies: Vec<(Var, Lit)>
    ) -> Vec<(Var, Lit)> {

    let mut moves: Vec<(Var, Lit)> = vec![];

    while let Some((dst, src)) = copies.pop() {
        // If any other copy read from `dst`, we need to save the current value
        // of `dst` into another fresh variable
        let found = copies
            .iter()
            .any(|(_, l)| l == &Lit::Var(dst));

        if found {
            let tmp = fresh();
            moves.push((tmp, Lit::Var(dst)));
            for copy in copies.iter_mut() {
                if let (_, Lit::Var(src2)) = copy && src2 == &dst {
                    *src2 = tmp;
                }
            }
        }

        moves.push((dst, src));
    }

    moves
}
