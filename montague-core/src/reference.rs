//! Reference (naive DFS) parser — differential-test oracle for CKY.
//!
//! Only compiled in `#[cfg(test)]` (gated by `lib.rs`). Exists exclusively for
//! the `cky_matches_naive_dfs` proptest and is never part of the public API.
#![allow(dead_code)]
//!
//! This module preserves the original `reduce` / `get_all_parses` algorithm from
//! the Haskell port. It is NOT exposed in the public API; it exists solely so
//! the CKY chart parser can be verified against a brute-force ground truth.
//!
//! The algorithm: exhaustive DFS over `view_substrings` × `try_reduce_pair`.

use std::collections::HashSet;
use std::hash::Hash;

use crate::reduction::{ReductionCtx, ReductionEngine};
use crate::semantics::Semantics;
use crate::types::{AnnotatedTerm, LambekType};

fn view_substrings<T: Clone>(xs: &[T]) -> Vec<(Vec<T>, [T; 2], Vec<T>)> {
    let l = xs.len();
    if l < 2 {
        return vec![];
    }
    (0..l - 1)
        .map(|n| {
            let prefix = xs[..n].to_vec();
            let pair = [xs[n].clone(), xs[n + 1].clone()];
            let suffix = xs[n + 2..].to_vec();
            (prefix, pair, suffix)
        })
        .collect()
}

fn cartesian_product<T: Clone>(lists: Vec<Vec<T>>) -> Vec<Vec<T>> {
    lists.into_iter().fold(vec![vec![]], |acc, list| {
        acc.into_iter()
            .flat_map(|prefix| {
                list.iter().map(move |item| {
                    let mut row = prefix.clone();
                    row.push(item.clone());
                    row
                })
            })
            .collect()
    })
}

/// Exhaustive DFS reduction over all annotated term sequences.
fn reduce_dfs<A, T, X>(
    engine: &ReductionEngine<A, T>,
    ctx: &ReductionCtx<'_, T>,
    sem: &Semantics<A, T, X>,
    terms: Vec<AnnotatedTerm<A, T>>,
) -> Vec<X>
where
    A: Clone + Eq + Hash + Send + Sync + 'static,
    T: crate::types::TypeCheck + Send + Sync + 'static,
    X: Clone,
{
    if terms.len() == 1 {
        return vec![(sem.interp)(terms.into_iter().next().unwrap())];
    }

    let mut results = Vec::new();
    for (prefix, pair, suffix) in view_substrings(&terms) {
        for reduced in engine.try_reduce_pair(ctx, &pair[0], &pair[1]) {
            let mut new_terms = prefix.clone();
            new_terms.push(reduced);
            new_terms.extend(suffix.clone());
            results.extend(reduce_dfs(engine, ctx, sem, new_terms));
        }
    }
    results
}

/// Parse input with the lexicon and return all unique annotated terms with
/// the given goal type (deduplicated by structural equality).
pub fn parse<A, T>(
    engine: &ReductionEngine<A, T>,
    ctx: &ReductionCtx<'_, T>,
    sem: &Semantics<A, T, AnnotatedTerm<A, T>>,
    goal: &LambekType<T>,
    input: &str,
) -> HashSet<AnnotatedTerm<A, T>>
where
    A: Clone + Eq + Hash + Send + Sync + 'static,
    T: crate::types::TypeCheck + Send + Sync + 'static,
{
    use crate::{annotate, reduce};
    let mut results = HashSet::new();
    for seq in annotate(sem, input) {
        for r in reduce(engine, ctx, sem, seq) {
            if r.ty.leq(goal, ctx.lattice) {
                results.insert(r);
            }
        }
    }
    results
}
