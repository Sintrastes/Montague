//! Default engine — bundles registry, profiles, reduction, and backends.
//!
//! [`Engine::default()`] wires up `montague.fol` + `montague.standard` +
//! standard reduction rules, ready for parsing.

use std::hash::Hash;

use montague_core::{
    chart::Chart,
    reduction::{ReductionCtx, ReductionEngine},
    registry::Registry,
    subtyping::SubtypeLattice,
    types::AnnotatedTerm,
    Semantics,
};
use montague_fol::Fol;
use montague_standard::Standard;

/// An pre-configured Montague engine with FOL profile, standard lexicon,
/// and reduction rules.
pub struct Engine<A, T>
where
    A: Clone + Eq + Hash + 'static,
    T: Hash + Eq + Clone + 'static,
{
    pub registry: Registry,
    pub lattice: SubtypeLattice<T>,
    pub reduction: ReductionEngine<A, T>,
    pub fol: Fol,
    pub standard: Standard,
}

impl<A, T> Default for Engine<A, T>
where
    A: Clone + Eq + Hash + 'static,
    T: Hash + Eq + Clone + 'static,
{
    fn default() -> Self {
        let mut registry = Registry::empty();
        let lattice = SubtypeLattice::new();

        let fol = Fol::register(&mut registry).expect("Fol::register");
        let standard = montague_standard::register(&mut registry).expect("register standard");

        let reduction = ReductionEngine::standard();

        Engine {
            registry,
            lattice,
            reduction,
            fol,
            standard,
        }
    }
}

impl<A, T> Engine<A, T>
where
    A: Clone + Eq + Hash + 'static,
    T: Hash + Eq + Clone + 'static,
{
    /// Build an engine from existing components.
    pub fn new(
        registry: Registry,
        lattice: SubtypeLattice<T>,
        reduction: ReductionEngine<A, T>,
        fol: Fol,
        standard: Standard,
    ) -> Self {
        Engine {
            registry,
            lattice,
            reduction,
            fol,
            standard,
        }
    }

    /// Create a reduction context borrowing from this engine.
    pub fn ctx(&self) -> ReductionCtx<'_, T> {
        let mut ctx = ReductionCtx::new(&self.lattice);
        ctx.registry = Some(&self.registry);
        ctx
    }

    /// Parse an input string into a CKY chart using the given semantics.
    pub fn parse_chart(
        &self,
        sem: &Semantics<A, T, AnnotatedTerm<A, T>>,
        input: &str,
    ) -> Chart<A, T> {
        let ctx = self.ctx();
        let tokens = montague_core::annotate(sem, input);
        // Flatten the ND list: for each word we take all annotations.
        // `annotate` returns `Vec<Vec<AnnotatedTerm<A,T>>>` — that's
        // already per-word token vectors.
        let mut chart = Chart::from_tokens(tokens);
        chart.fill(&self.reduction, &ctx);
        chart
    }
}
