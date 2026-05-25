//! Core syntax algebra and term types for Montague.
//!
//! [`LambekType`] is the closed-enum syntax algebra with an interned [`CustomTag`]
//! escape hatch for research-grade connectives. [`Term`] and [`AnnotatedTerm`] are
//! the term / type-annotated-term pairing produced by parsing.
//!
//! Subtyping is consulted via a [`crate::subtyping::SubtypeLattice`] rather than
//! a compile-time `LatticeOrd` trait — this is what makes `.mont`-declared
//! `Person :< Noun.` first-class (see D0/D1).

use std::hash::Hash;

use crate::registry::CustomTag;
use crate::subtyping::SubtypeLattice;

// ---------------------------------------------------------------------------
// LambekType
// ---------------------------------------------------------------------------

/// Types in the Lambek grammar, parameterized over the set of basic types `T`.
///
/// Includes the standard slashes plus extraction (`↑`), scoping (`⇑`),
/// conjunction (`∧`), and disjunction (`∨`); and an interned `Custom`
/// trapdoor for research-grade syntactic connectives (pregroup adjoints,
/// multimodal slashes, Morrill's wrap, …).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LambekType<T> {
    Basic(T),
    /// `b \ a` — consumes argument of type `a` from the LEFT, produces `b`.
    /// Stored as `RightArrow(a, b)` for historical reasons (matches the
    /// original Haskell encoding).
    RightArrow(Box<LambekType<T>>, Box<LambekType<T>>),
    /// `a / b` — consumes argument of type `b` from the RIGHT, produces `a`.
    /// Stored as `LeftArrow(a, b)`.
    LeftArrow(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Extraction `a ↑ b` — gap-filler binding (wh-movement, relative clauses).
    Extract(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Scoping `b ⇑ a` — quantifier scope via continuation-passing.
    Scoped(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Conjunction `a ∧ b`.
    Conj(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Disjunction `a ∨ b`.
    Disj(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Trapdoor for research-grade syntactic connectives. The `tag` is
    /// registered in the [`crate::registry::Registry`]; subtyping defaults
    /// to invariant in v1 (per-tag variance lookup added when a plugin needs it).
    Custom {
        tag: CustomTag,
        args: Vec<LambekType<T>>,
    },
}

impl<T: Hash + Eq + Clone> LambekType<T> {
    /// Structural subtype check.
    ///
    /// Arrow variance: `LeftArrow` / `RightArrow` / `Extract` / `Scoped` are
    /// contravariant in their first argument and covariant in their second.
    /// `Conj` / `Disj` are covariant in both arguments (standard product /
    /// sum variance). `Custom` defaults to invariant; per-tag variance
    /// lookup is added in a later milestone.
    pub fn leq(&self, other: &Self, lat: &SubtypeLattice<T>) -> bool {
        use LambekType::*;
        match (self, other) {
            (Basic(a), Basic(b)) => lat.leq(a, b),
            (LeftArrow(x1, y1), LeftArrow(x2, y2)) => x2.leq(x1, lat) && y1.leq(y2, lat),
            (RightArrow(x1, y1), RightArrow(x2, y2)) => x2.leq(x1, lat) && y1.leq(y2, lat),
            (Extract(x1, y1), Extract(x2, y2)) => x2.leq(x1, lat) && y1.leq(y2, lat),
            (Scoped(x1, y1), Scoped(x2, y2)) => x2.leq(x1, lat) && y1.leq(y2, lat),
            (Conj(x1, y1), Conj(x2, y2)) => x1.leq(x2, lat) && y1.leq(y2, lat),
            (Disj(x1, y1), Disj(x2, y2)) => x1.leq(x2, lat) && y1.leq(y2, lat),
            (Custom { tag: t1, args: a1 }, Custom { tag: t2, args: a2 }) => {
                t1 == t2
                    && a1.len() == a2.len()
                    && a1
                        .iter()
                        .zip(a2)
                        .all(|(x, y)| x.leq(y, lat) && y.leq(x, lat))
            }
            _ => false,
        }
    }

    /// Convenience: `self.geq(other) == other.leq(self)`.
    pub fn geq(&self, other: &Self, lat: &SubtypeLattice<T>) -> bool {
        other.leq(self, lat)
    }

    /// Recursive depth of the type tree (atoms = 0; functions = 1 + max(child)).
    pub fn depth(&self) -> usize {
        use LambekType::*;
        match self {
            Basic(_) => 0,
            LeftArrow(a, b) | RightArrow(a, b) | Extract(a, b) | Scoped(a, b)
            | Conj(a, b) | Disj(a, b) => 1 + a.depth().max(b.depth()),
            Custom { args, .. } => 1 + args.iter().map(|a| a.depth()).max().unwrap_or(0),
        }
    }

    /// Apply a [`LambekFold`] visitor bottom-up. Each constructor maps to
    /// one trait method; new visitors implement the trait once and work
    /// across every variant.
    pub fn fold<F: LambekFold<T>>(&self, f: &mut F) -> F::Out {
        use LambekType::*;
        match self {
            Basic(t) => f.basic(t),
            LeftArrow(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.left_arrow(av, bv)
            }
            RightArrow(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.right_arrow(av, bv)
            }
            Extract(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.extract(av, bv)
            }
            Scoped(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.scoped(av, bv)
            }
            Conj(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.conj(av, bv)
            }
            Disj(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.disj(av, bv)
            }
            Custom { tag, args } => {
                let av: Vec<F::Out> = args.iter().map(|a| a.fold(f)).collect();
                f.custom(*tag, av)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// LambekFold
// ---------------------------------------------------------------------------

/// Visitor for `LambekType` — one method per variant.
///
/// Interpreters (display, IR lowering, autocomplete) implement this trait once
/// and work uniformly across every variant. New `Custom` connectives integrate
/// via the `custom` method using their interned `CustomTag`.
pub trait LambekFold<T> {
    type Out;
    fn basic(&mut self, t: &T) -> Self::Out;
    fn left_arrow(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn right_arrow(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn extract(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn scoped(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn conj(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn disj(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn custom(&mut self, tag: CustomTag, args: Vec<Self::Out>) -> Self::Out;
}

// ---------------------------------------------------------------------------
// Term
// ---------------------------------------------------------------------------

/// Terms in the Lambek calculus.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term<A> {
    Atom(A),
    Var(String),
    Lambda(String, Box<Term<A>>),
    App(Box<Term<A>>, Vec<Term<A>>),
}

impl<A: Clone> Term<A> {
    /// Returns true if this term is an `App` (i.e., partially applied).
    pub fn is_partial_pred(&self) -> bool {
        matches!(self, Term::App(_, _))
    }

    /// If `self` is `App(f, args)`, push `arg` onto args.
    /// Otherwise wrap in a fresh `App(self, [arg])`.
    pub fn apply_partial(self, arg: Term<A>) -> Term<A> {
        match self {
            Term::App(f, mut args) => {
                args.push(arg);
                Term::App(f, args)
            }
            other => Term::App(Box::new(other), vec![arg]),
        }
    }
}

impl<A: std::fmt::Display> std::fmt::Display for Term<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Atom(a) => write!(f, "{}", a),
            Term::Var(s) => write!(f, "{}", s),
            Term::Lambda(s, body) => write!(f, "λ{}.{}", s, body),
            Term::App(func, args) => {
                write!(f, "{}(", func)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", a)?;
                }
                write!(f, ")")
            }
        }
    }
}

// ---------------------------------------------------------------------------
// AnnotatedTerm
// ---------------------------------------------------------------------------

/// A term paired with its Lambek type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnotatedTerm<A, T> {
    pub term: Term<A>,
    pub ty: LambekType<T>,
}

/// Non-deterministic result: all possible values of a parse.
pub type NonDet<A> = Vec<A>;

// ---------------------------------------------------------------------------
// Property tests for LambekType subtyping
// ---------------------------------------------------------------------------

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    /// Recursive generator for `LambekType<u8>` with bounded depth.
    ///
    /// Doesn't generate `Custom` (would require a CustomTag which lives in
    /// the registry and we want to keep these props lattice-agnostic).
    fn arb_lambek_type() -> impl Strategy<Value = LambekType<u8>> {
        let leaf = any::<u8>().prop_map(LambekType::Basic);
        leaf.prop_recursive(
            4,  // depth
            32, // nodes total
            4,  // branches per recursion
            |inner| {
                let pair = (inner.clone(), inner);
                prop_oneof![
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::LeftArrow(Box::new(a), Box::new(b))),
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::RightArrow(Box::new(a), Box::new(b))),
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::Extract(Box::new(a), Box::new(b))),
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::Scoped(Box::new(a), Box::new(b))),
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::Conj(Box::new(a), Box::new(b))),
                    pair.prop_map(|(a, b)| LambekType::Disj(Box::new(a), Box::new(b))),
                ]
            },
        )
    }

    proptest! {
        // With an empty lattice, basic-type subtyping is pure equality.
        // Structural laws should still hold for the recursive variants.

        #[test]
        fn leq_reflexive(t in arb_lambek_type()) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            prop_assert!(t.leq(&t, &lat));
        }

        #[test]
        fn leq_transitive(a in arb_lambek_type(), b in arb_lambek_type(), c in arb_lambek_type()) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            if a.leq(&b, &lat) && b.leq(&c, &lat) {
                prop_assert!(a.leq(&c, &lat));
            }
        }

        #[test]
        fn leq_antisymmetric_on_empty_lattice(a in arb_lambek_type(), b in arb_lambek_type()) {
            // With an empty lattice, leq collapses to structural equality on
            // basic types — which forces full structural equality on the
            // whole type. So mutual `leq` ⇒ `a == b`.
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            if a.leq(&b, &lat) && b.leq(&a, &lat) {
                prop_assert_eq!(a, b);
            }
        }

        #[test]
        fn left_arrow_first_arg_contravariant(
            x1 in arb_lambek_type(),
            x2 in arb_lambek_type(),
            y in arb_lambek_type(),
        ) {
            // Build a lattice where x2 :< x1 by adding the structural-equality
            // edge through a fresh basic-type pair.
            //
            // We can't easily add subtyping between arbitrary LambekTypes —
            // the lattice is over basic types only. So we test the structural
            // recursion: if x2.leq(x1) holds (by reflexivity when x1 == x2),
            // then LeftArrow(x1, y).leq(LeftArrow(x2, y)) must hold.
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            if x2.leq(&x1, &lat) {
                let big = LambekType::LeftArrow(Box::new(x1.clone()), Box::new(y.clone()));
                let small = LambekType::LeftArrow(Box::new(x2.clone()), Box::new(y.clone()));
                prop_assert!(big.leq(&small, &lat));
            }
        }

        #[test]
        fn arrow_second_arg_covariant(
            x in arb_lambek_type(),
            y1 in arb_lambek_type(),
            y2 in arb_lambek_type(),
        ) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            if y1.leq(&y2, &lat) {
                let a = LambekType::LeftArrow(Box::new(x.clone()), Box::new(y1.clone()));
                let b = LambekType::LeftArrow(Box::new(x.clone()), Box::new(y2.clone()));
                prop_assert!(a.leq(&b, &lat));
            }
        }

        #[test]
        fn basic_subtyping_threads_through_arrows(
            sub in any::<u8>(), sup in any::<u8>(),
            y in arb_lambek_type(),
        ) {
            // If sub :< sup at the basic level, the arrow with sup in the
            // arg position should be ≤ the arrow with sub there (contra).
            prop_assume!(sub != sup);
            let mut lat: SubtypeLattice<u8> = SubtypeLattice::new();
            lat.add_subtype(sub, sup);
            let big = LambekType::LeftArrow(
                Box::new(LambekType::Basic(sup)),
                Box::new(y.clone()),
            );
            let small = LambekType::LeftArrow(
                Box::new(LambekType::Basic(sub)),
                Box::new(y.clone()),
            );
            // small.first = sub, big.first = sup; sub ≤ sup, so big.leq(small).
            prop_assert!(big.leq(&small, &lat));
        }
    }
}
