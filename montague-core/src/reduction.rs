//! Reduction rules and the reduction engine.
//!
//! Rules are pluggable through the [`ReductionRule`] trait; [`ReductionEngine`]
//! holds a `Vec<Box<dyn ReductionRule<_, _>>>` and dispatches them. Built-in
//! absorption rules ([`LeftAbsorption`], [`RightAbsorption`]) are bundled into
//! [`ReductionEngine::standard`].
//!
//! In M2 the engine drives a linear DFS reduction (matching the original
//! semantics). M4 will replace this with a CKY chart parser; the
//! `ReductionRule` trait stays stable across that change.

use std::hash::Hash;

use crate::registry::{CustomTag, Registry};
use crate::subtyping::SubtypeLattice;
use crate::types::{AnnotatedTerm, LambekType, Term};

// ---------------------------------------------------------------------------
// Reduction context
// ---------------------------------------------------------------------------

/// Per-reduction state threaded through every rule application.
///
/// Holds the subtyping lattice (always required) and optionally the
/// [`Registry`] (used for diagnostics and, in later milestones, per-tag
/// variance lookup for `LambekType::Custom`).
pub struct ReductionCtx<'a, T: Hash + Eq + Clone> {
    pub lattice: &'a SubtypeLattice<T>,
    pub registry: Option<&'a Registry>,
}

impl<'a, T: Hash + Eq + Clone> ReductionCtx<'a, T> {
    /// Construct a context backed only by a lattice. The registry is unset.
    pub fn new(lattice: &'a SubtypeLattice<T>) -> Self {
        Self {
            lattice,
            registry: None,
        }
    }

    /// Attach a registry (enables future diagnostics / variance lookups).
    pub fn with_registry(mut self, reg: &'a Registry) -> Self {
        self.registry = Some(reg);
        self
    }
}

// ---------------------------------------------------------------------------
// Type shape & applicability hints
// ---------------------------------------------------------------------------

/// Shape descriptor a rule may require of its left or right operand's type.
///
/// Used by [`RuleApplicability`] hints so the chart parser (M4) can skip
/// rules that cannot fire on a given (left-cell, right-cell) pair without
/// iterating over their contents.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TypeShape {
    AnyBasic,
    LeftArrow,
    RightArrow,
    Extract,
    Scoped,
    Conj,
    Disj,
    CustomTag(CustomTag),
}

impl TypeShape {
    /// True iff `ty`'s top-level constructor matches this shape.
    pub fn matches<T>(&self, ty: &LambekType<T>) -> bool {
        match (self, ty) {
            (TypeShape::AnyBasic, LambekType::Basic(_)) => true,
            (TypeShape::LeftArrow, LambekType::LeftArrow(..)) => true,
            (TypeShape::RightArrow, LambekType::RightArrow(..)) => true,
            (TypeShape::Extract, LambekType::Extract(..)) => true,
            (TypeShape::Scoped, LambekType::Scoped(..)) => true,
            (TypeShape::Conj, LambekType::Conj(..)) => true,
            (TypeShape::Disj, LambekType::Disj(..)) => true,
            (TypeShape::CustomTag(tag1), LambekType::Custom { tag, .. }) => tag1 == tag,
            _ => false,
        }
    }
}

/// Applicability hint declared by a rule.
///
/// The chart parser uses this to prune the rule set per cell pair without
/// invoking `try_apply`. The linear M2 reduction engine also uses it as a
/// cheap pre-filter, though it's optional for correctness.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum RuleApplicability {
    Always,
    LeftMustBe(TypeShape),
    RightMustBe(TypeShape),
    Both { left: TypeShape, right: TypeShape },
}

impl RuleApplicability {
    /// True iff both operands' shapes match the hint.
    pub fn matches<A, T>(&self, left: &AnnotatedTerm<A, T>, right: &AnnotatedTerm<A, T>) -> bool {
        match self {
            RuleApplicability::Always => true,
            RuleApplicability::LeftMustBe(s) => s.matches(&left.ty),
            RuleApplicability::RightMustBe(s) => s.matches(&right.ty),
            RuleApplicability::Both { left: l, right: r } => {
                l.matches(&left.ty) && r.matches(&right.ty)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// ReductionRule
// ---------------------------------------------------------------------------

/// A rewrite rule consuming an adjacent pair `(left, right)` and producing
/// zero or more reduced annotated terms.
///
/// Rules are typically zero-sized and side-effect-free; they read from `ctx`
/// (lattice, registry) but never mutate it. Multiple rules may fire on the
/// same pair — the engine collects all results.
///
/// `Send + Sync` is *not* required in v1 — single-threaded reduction only.
/// When parallel parsing lands, this trait will gain a `Send + Sync` bound
/// (or a parallel-only sub-trait) explicitly.
pub trait ReductionRule<A, T>
where
    A: Clone,
    T: Hash + Eq + Clone,
{
    /// Short identifier suitable for diagnostics.
    fn name(&self) -> &'static str;

    /// Static hint about which operand shapes this rule can fire on.
    /// The engine uses this to skip the rule entirely when neither operand
    /// could satisfy it.
    fn applicability(&self) -> RuleApplicability;

    /// Attempt to reduce `(left, right)`. Returns all possible reduced
    /// terms (often 0 or 1; rarely 2+).
    fn try_apply(
        &self,
        ctx: &ReductionCtx<'_, T>,
        left: &AnnotatedTerm<A, T>,
        right: &AnnotatedTerm<A, T>,
    ) -> Vec<AnnotatedTerm<A, T>>;
}

// ---------------------------------------------------------------------------
// ReductionEngine
// ---------------------------------------------------------------------------

/// Bundle of reduction rules applied during parsing.
///
/// `ReductionEngine::standard()` ships with `LeftAbsorption` and
/// `RightAbsorption` (the canonical Lambek-calculus reductions). Plugins
/// extend it via `add_rule` / `with_rule`.
pub struct ReductionEngine<A, T>
where
    A: Clone,
    T: Hash + Eq + Clone,
{
    rules: Vec<Box<dyn ReductionRule<A, T>>>,
}

impl<A, T> Default for ReductionEngine<A, T>
where
    A: Clone + 'static,
    T: Hash + Eq + Clone + 'static,
{
    fn default() -> Self {
        Self::standard()
    }
}

impl<A, T> ReductionEngine<A, T>
where
    A: Clone + 'static,
    T: Hash + Eq + Clone + 'static,
{
    /// Engine with no rules. Use `add_rule` / `with_rule` to populate.
    pub fn empty() -> Self {
        Self { rules: Vec::new() }
    }

    /// Engine pre-loaded with the standard Lambek absorption rules.
    pub fn standard() -> Self {
        Self::empty()
            .with_rule(LeftAbsorption)
            .with_rule(RightAbsorption)
    }

    /// Append a rule. Returns `self` for chaining.
    pub fn with_rule(mut self, rule: impl ReductionRule<A, T> + 'static) -> Self {
        self.rules.push(Box::new(rule));
        self
    }

    /// Append a rule (mutating variant).
    pub fn add_rule(&mut self, rule: Box<dyn ReductionRule<A, T>>) {
        self.rules.push(rule);
    }

    /// Number of registered rules.
    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }

    /// Iterate over rule names — useful for debugging which rules are loaded.
    pub fn rule_names(&self) -> impl Iterator<Item = &'static str> + '_ {
        self.rules.iter().map(|r| r.name())
    }

    /// Try every applicable rule on `(left, right)`. Returns all reductions
    /// from rules that fired.
    pub fn try_reduce_pair(
        &self,
        ctx: &ReductionCtx<'_, T>,
        left: &AnnotatedTerm<A, T>,
        right: &AnnotatedTerm<A, T>,
    ) -> Vec<AnnotatedTerm<A, T>> {
        let mut results = Vec::new();
        for rule in &self.rules {
            if !rule.applicability().matches(left, right) {
                continue;
            }
            results.extend(rule.try_apply(ctx, left, right));
        }
        results
    }
}

// ---------------------------------------------------------------------------
// Built-in absorption rules
// ---------------------------------------------------------------------------

/// `t1 : x` + `t2 : RightArrow(x', y)` where `x ≤ x'` → `App(t2, [t1]) : y`.
///
/// Right-hand term has a `RightArrow` type; it consumes the left as its argument.
pub struct LeftAbsorption;

impl<A, T> ReductionRule<A, T> for LeftAbsorption
where
    A: Clone,
    T: Hash + Eq + Clone,
{
    fn name(&self) -> &'static str {
        "left-absorption"
    }

    fn applicability(&self) -> RuleApplicability {
        RuleApplicability::RightMustBe(TypeShape::RightArrow)
    }

    fn try_apply(
        &self,
        ctx: &ReductionCtx<'_, T>,
        left: &AnnotatedTerm<A, T>,
        right: &AnnotatedTerm<A, T>,
    ) -> Vec<AnnotatedTerm<A, T>> {
        let LambekType::RightArrow(x_prime, y) = &right.ty else {
            return vec![];
        };
        if !left.ty.leq(x_prime, ctx.lattice) {
            return vec![];
        }
        let new_term = if right.term.is_partial_pred() {
            right.term.clone().apply_partial(left.term.clone())
        } else {
            Term::App(Box::new(right.term.clone()), vec![left.term.clone()])
        };
        vec![AnnotatedTerm {
            term: new_term,
            ty: (**y).clone(),
        }]
    }
}

/// `t1 : LeftArrow(x, y)` + `t2 : y'` where `y' ≤ y` → `App(t1, [t2]) : x`.
///
/// Left-hand term has a `LeftArrow` type; it consumes the right as its argument.
pub struct RightAbsorption;

impl<A, T> ReductionRule<A, T> for RightAbsorption
where
    A: Clone,
    T: Hash + Eq + Clone,
{
    fn name(&self) -> &'static str {
        "right-absorption"
    }

    fn applicability(&self) -> RuleApplicability {
        RuleApplicability::LeftMustBe(TypeShape::LeftArrow)
    }

    fn try_apply(
        &self,
        ctx: &ReductionCtx<'_, T>,
        left: &AnnotatedTerm<A, T>,
        right: &AnnotatedTerm<A, T>,
    ) -> Vec<AnnotatedTerm<A, T>> {
        let LambekType::LeftArrow(x, y) = &left.ty else {
            return vec![];
        };
        if !right.ty.leq(y, ctx.lattice) {
            return vec![];
        }
        let new_term = if left.term.is_partial_pred() {
            left.term.clone().apply_partial(right.term.clone())
        } else {
            Term::App(Box::new(left.term.clone()), vec![right.term.clone()])
        };
        vec![AnnotatedTerm {
            term: new_term,
            ty: (**x).clone(),
        }]
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    enum BT {
        S,
        N,
    }

    fn basic(t: BT) -> LambekType<BT> {
        LambekType::Basic(t)
    }
    fn left(a: LambekType<BT>, b: LambekType<BT>) -> LambekType<BT> {
        LambekType::LeftArrow(Box::new(a), Box::new(b))
    }
    fn right(a: LambekType<BT>, b: LambekType<BT>) -> LambekType<BT> {
        LambekType::RightArrow(Box::new(a), Box::new(b))
    }

    fn at(term: Term<i32>, ty: LambekType<BT>) -> AnnotatedTerm<i32, BT> {
        AnnotatedTerm { term, ty }
    }

    #[test]
    fn left_absorption_fires_on_matching_pair() {
        let lat = SubtypeLattice::new();
        let ctx = ReductionCtx::new(&lat);
        let l = at(Term::Atom(1), basic(BT::N));
        let r = at(Term::Atom(2), right(basic(BT::N), basic(BT::S)));
        let results = LeftAbsorption.try_apply(&ctx, &l, &r);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].ty, basic(BT::S));
    }

    #[test]
    fn left_absorption_skips_non_matching_shape() {
        let lat = SubtypeLattice::new();
        let ctx = ReductionCtx::new(&lat);
        let l = at(Term::Atom(1), basic(BT::N));
        let r = at(Term::Atom(2), basic(BT::N));
        let results = LeftAbsorption.try_apply(&ctx, &l, &r);
        assert!(results.is_empty());
    }

    #[test]
    fn right_absorption_fires_with_subtyping() {
        let mut lat = SubtypeLattice::new();
        lat.add_subtype(BT::N, BT::S); // pretend N :< S for the sake of the lookup
        let ctx = ReductionCtx::new(&lat);
        let l = at(Term::Atom(1), left(basic(BT::S), basic(BT::S)));
        let r = at(Term::Atom(2), basic(BT::N));
        let results = RightAbsorption.try_apply(&ctx, &l, &r);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].ty, basic(BT::S));
    }

    #[test]
    fn engine_standard_has_two_rules() {
        let eng: ReductionEngine<i32, BT> = ReductionEngine::standard();
        assert_eq!(eng.rule_count(), 2);
        let names: Vec<&str> = eng.rule_names().collect();
        assert!(names.contains(&"left-absorption"));
        assert!(names.contains(&"right-absorption"));
    }

    #[test]
    fn engine_dispatches_both_rules() {
        let lat = SubtypeLattice::new();
        let ctx = ReductionCtx::new(&lat);
        let eng: ReductionEngine<i32, BT> = ReductionEngine::standard();
        let l = at(Term::Atom(1), basic(BT::N));
        let r = at(Term::Atom(2), right(basic(BT::N), basic(BT::S)));
        let out = eng.try_reduce_pair(&ctx, &l, &r);
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].ty, basic(BT::S));
    }

    #[test]
    fn left_absorption_with_subtype_match() {
        // When left's type is a subtype of x' in RightArrow(x', y), the rule fires.
        let mut lat = SubtypeLattice::new();
        lat.add_subtype(BT::N, BT::S); // N :< S so N can fill an S slot
        let ctx = ReductionCtx::new(&lat);
        let l = at(Term::Atom(1), basic(BT::N));
        let r = at(Term::Atom(2), right(basic(BT::S), basic(BT::N)));
        let out = LeftAbsorption.try_apply(&ctx, &l, &r);
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].ty, basic(BT::N)); // result type = y of RightArrow
    }

    #[test]
    fn applicability_filters_skip_non_matching() {
        // A rule that says LeftMustBe(LeftArrow) should never fire when the left
        // is Basic — the engine skips it before calling try_apply.
        struct PanicOnFire;
        impl ReductionRule<i32, BT> for PanicOnFire {
            fn name(&self) -> &'static str {
                "panic"
            }
            fn applicability(&self) -> RuleApplicability {
                RuleApplicability::LeftMustBe(TypeShape::LeftArrow)
            }
            fn try_apply(
                &self,
                _ctx: &ReductionCtx<'_, BT>,
                _left: &AnnotatedTerm<i32, BT>,
                _right: &AnnotatedTerm<i32, BT>,
            ) -> Vec<AnnotatedTerm<i32, BT>> {
                panic!("applicability should have filtered this out");
            }
        }
        let lat = SubtypeLattice::new();
        let ctx = ReductionCtx::new(&lat);
        let eng = ReductionEngine::<i32, BT>::empty().with_rule(PanicOnFire);
        let l = at(Term::Atom(1), basic(BT::N));
        let r = at(Term::Atom(2), basic(BT::N));
        // Should not panic — applicability filter skips the rule.
        let _ = eng.try_reduce_pair(&ctx, &l, &r);
    }
}

// ---------------------------------------------------------------------------
// Property tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    fn arb_lambek_type() -> impl Strategy<Value = LambekType<u8>> {
        let leaf = any::<u8>().prop_map(LambekType::Basic);
        leaf.prop_recursive(3, 16, 4, |inner| {
            let pair = (inner.clone(), inner);
            prop_oneof![
                pair.clone()
                    .prop_map(|(a, b)| LambekType::LeftArrow(Box::new(a), Box::new(b))),
                pair.prop_map(|(a, b)| LambekType::RightArrow(Box::new(a), Box::new(b))),
            ]
        })
    }

    proptest! {
        /// When `LeftAbsorption` fires it must produce a term whose type is `y`,
        /// the second component of the right-hand `RightArrow(x', y)`.
        #[test]
        fn left_absorption_soundness(
            left_ty in arb_lambek_type(),
            right_first in arb_lambek_type(),
            right_second in arb_lambek_type(),
        ) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            let ctx = ReductionCtx::new(&lat);
            let left = AnnotatedTerm {
                term: Term::<u8>::Atom(0),
                ty: left_ty.clone(),
            };
            let right = AnnotatedTerm {
                term: Term::<u8>::Atom(1),
                ty: LambekType::RightArrow(
                    Box::new(right_first.clone()),
                    Box::new(right_second.clone()),
                ),
            };
            let results = LeftAbsorption.try_apply(&ctx, &left, &right);
            for r in results {
                prop_assert_eq!(&r.ty, &right_second);
            }
        }

        /// When `RightAbsorption` fires it must produce a term whose type is `x`,
        /// the first component of the left-hand `LeftArrow(x, y)`.
        #[test]
        fn right_absorption_soundness(
            left_first in arb_lambek_type(),
            left_second in arb_lambek_type(),
            right_ty in arb_lambek_type(),
        ) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            let ctx = ReductionCtx::new(&lat);
            let left = AnnotatedTerm {
                term: Term::<u8>::Atom(0),
                ty: LambekType::LeftArrow(
                    Box::new(left_first.clone()),
                    Box::new(left_second.clone()),
                ),
            };
            let right = AnnotatedTerm {
                term: Term::<u8>::Atom(1),
                ty: right_ty,
            };
            let results = RightAbsorption.try_apply(&ctx, &left, &right);
            for r in results {
                prop_assert_eq!(&r.ty, &left_first);
            }
        }

        /// Either rule should fire if and only if the relevant arrow shape is
        /// present AND the subtyping check passes. This tests the "fires iff"
        /// direction implicitly via empty lattice (only structural equality
        /// passes the leq check).
        #[test]
        fn rule_fires_only_when_types_align(
            t1 in any::<u8>(),
            t2 in any::<u8>(),
            t3 in any::<u8>(),
        ) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            let ctx = ReductionCtx::new(&lat);
            let left = AnnotatedTerm {
                term: Term::<u8>::Atom(0),
                ty: LambekType::Basic(t1),
            };
            let right = AnnotatedTerm {
                term: Term::<u8>::Atom(1),
                ty: LambekType::RightArrow(
                    Box::new(LambekType::Basic(t2)),
                    Box::new(LambekType::Basic(t3)),
                ),
            };
            let fired = !LeftAbsorption.try_apply(&ctx, &left, &right).is_empty();
            // With empty lattice, fires iff t1 == t2.
            prop_assert_eq!(fired, t1 == t2);
        }
    }
}
