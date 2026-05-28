//! Lowering from syntactic [`SemTermExpr`] to core [`Term<AtomType>`].
//!
//! Logical primitives (∧, ∨, ¬, →, ∀, ∃, =, ints, strings) become
//! reserved-name atoms that backends can pattern-match on.
//!
//! Lambda-bound variables become `Term::Var` (substitutable); free variables
//! become `Term::Atom` (opaque constants — needed so `type_of_atom` can find
//! their types).

use std::collections::HashSet;

use montague_core::types::{AtomType, Term};

use crate::ast::{SemTermExpr, Spanned};

/// Lower a parsed semantic term expression into a core `Term<AtomType>`.
pub fn lower_sem_term(expr: &Spanned<SemTermExpr>) -> Term<AtomType> {
    lower(&expr.item, &HashSet::new())
}

fn lower(expr: &SemTermExpr, bound: &HashSet<String>) -> Term<AtomType> {
    match expr {
        SemTermExpr::Var(x) => {
            if bound.contains(x) {
                Term::Var(x.clone())
            } else {
                Term::Atom(AtomType::new(x))
            }
        }
        SemTermExpr::IntLit(n) => Term::Atom(reserved_atom(&format!("int_{n}"))),
        SemTermExpr::StringLit(s) => Term::Atom(reserved_atom(&format!("str_{s}"))),
        SemTermExpr::App(fun, args) => {
            let f = lower(&fun.item, bound);
            let a: Vec<Term<AtomType>> = args.iter().map(|a| lower(&a.item, bound)).collect();
            Term::App(Box::new(f), a)
        }
        SemTermExpr::Lambda(binders, body) => {
            let mut inner_bound = bound.clone();
            for b in binders {
                inner_bound.insert(b.clone());
            }
            lower_with_binders(&body.item, binders, &inner_bound)
        }
        SemTermExpr::And(p, q) => {
            let and_atom = Term::Atom(reserved_atom("and"));
            Term::App(
                Box::new(and_atom),
                vec![lower(&p.item, bound), lower(&q.item, bound)],
            )
        }
        SemTermExpr::Or(p, q) => {
            let or_atom = Term::Atom(reserved_atom("or"));
            Term::App(
                Box::new(or_atom),
                vec![lower(&p.item, bound), lower(&q.item, bound)],
            )
        }
        SemTermExpr::Not(x) => {
            let not_atom = Term::Atom(reserved_atom("not"));
            Term::App(Box::new(not_atom), vec![lower(&x.item, bound)])
        }
        SemTermExpr::Implies(p, q) => {
            let implies_atom = Term::Atom(reserved_atom("implies"));
            Term::App(
                Box::new(implies_atom),
                vec![lower(&p.item, bound), lower(&q.item, bound)],
            )
        }
        SemTermExpr::Forall(var, body) => {
            let forall_atom = Term::Atom(reserved_atom("forall"));
            let mut inner_bound = bound.clone();
            inner_bound.insert(var.clone());
            let body_term = Term::Lambda(var.clone(), Box::new(lower(&body.item, &inner_bound)));
            Term::App(Box::new(forall_atom), vec![body_term])
        }
        SemTermExpr::Exists(var, body) => {
            let exists_atom = Term::Atom(reserved_atom("exists"));
            let mut inner_bound = bound.clone();
            inner_bound.insert(var.clone());
            let body_term = Term::Lambda(var.clone(), Box::new(lower(&body.item, &inner_bound)));
            Term::App(Box::new(exists_atom), vec![body_term])
        }
        SemTermExpr::Eq(p, q) => {
            let eq_atom = Term::Atom(reserved_atom("eq"));
            Term::App(
                Box::new(eq_atom),
                vec![lower(&p.item, bound), lower(&q.item, bound)],
            )
        }
    }
}

/// Lower a body under the given lambda binders, wrapping in nested `Term::Lambda`.
fn lower_with_binders(
    body: &SemTermExpr,
    binders: &[String],
    bound: &HashSet<String>,
) -> Term<AtomType> {
    let mut term = lower(body, bound);
    for var in binders.iter().rev() {
        term = Term::Lambda(var.clone(), Box::new(term));
    }
    term
}

/// Build a reserved-name `AtomType` (no sort args).
fn reserved_atom(name: &str) -> AtomType {
    AtomType {
        name: name.to_string(),
        args: vec![],
    }
}
