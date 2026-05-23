//! Runtime subtyping lattice and variance.
//!
//! Replaces the compile-time `LatticeOrd` trait. The lattice is constructed
//! at lexicon-load time (or programmatically in Rust-defined lexicons) and
//! consulted during reduction to check Lambek-type subtyping. This is what
//! makes `.mont` declarations like `Person :< Noun.` first-class.
//!
//! ### Example
//!
//! ```
//! use montague_core::subtyping::SubtypeLattice;
//!
//! let mut lat: SubtypeLattice<&'static str> = SubtypeLattice::new();
//! lat.add_subtype("Person", "Noun");
//! lat.add_subtype("Mammal", "Animal");
//! lat.add_subtype("Person", "Mammal");
//!
//! assert!(lat.leq(&"Person", &"Noun"));
//! assert!(lat.leq(&"Person", &"Animal"));   // transitive
//! assert!(lat.leq(&"Person", &"Person"));   // reflexive
//! assert!(!lat.leq(&"Animal", &"Person"));
//! ```

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

// ---------------------------------------------------------------------------
// Variance
// ---------------------------------------------------------------------------

/// Per-argument variance of a type constructor.
///
/// Used for both Lambek-type connectives and `SemTerm::Op` operators with
/// subtyping-sensitive arguments. Lambek arrows `LeftArrow(a, b)` /
/// `RightArrow(a, b)` are fixed-variance (contra in `a`, co in `b`); plugin
/// `Custom` connectives declare their variance vector at registration time.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Variance {
    /// Covariant: subtype on the supertype side.
    Co,
    /// Contravariant: supertype on the subtype side (function argument position).
    Contra,
    /// Invariant: both directions must hold.
    Inv,
}

impl Variance {
    /// Given the two directional `leq` outcomes between a corresponding
    /// argument pair (sub-side-to-sup-side and the reverse), report
    /// whether this variance is satisfied.
    pub fn check(self, sub_to_sup: bool, sup_to_sub: bool) -> bool {
        match self {
            Variance::Co => sub_to_sup,
            Variance::Contra => sup_to_sub,
            Variance::Inv => sub_to_sup && sup_to_sub,
        }
    }
}

// ---------------------------------------------------------------------------
// SubtypeLattice
// ---------------------------------------------------------------------------

/// Acyclic DAG of basic-type subtype relationships.
///
/// Reflexivity is implicit — `leq(t, t)` is always true. Transitivity is
/// computed lazily on lookup via DFS with visited-set cycle protection
/// (the DAG is acyclic by contract, but the protection covers buggy input
/// without panicking).
///
/// The lattice is append-only: subtype edges can be added at any time
/// during the registration phase but never removed.
#[derive(Debug, Clone)]
pub struct SubtypeLattice<T: Hash + Eq + Clone> {
    /// `direct_supertypes[t]` = the set of types `s` such that `t :< s` was declared.
    direct_supertypes: HashMap<T, HashSet<T>>,
}

impl<T: Hash + Eq + Clone> Default for SubtypeLattice<T> {
    fn default() -> Self {
        Self {
            direct_supertypes: HashMap::new(),
        }
    }
}

impl<T: Hash + Eq + Clone> SubtypeLattice<T> {
    /// An empty lattice — every type is only `leq` to itself.
    pub fn new() -> Self {
        Self::default()
    }

    /// Declare `sub :< sup`. Idempotent.
    pub fn add_subtype(&mut self, sub: T, sup: T) {
        self.direct_supertypes.entry(sub).or_default().insert(sup);
    }

    /// True iff `sub` is a (reflexive, transitive) subtype of `sup`.
    pub fn leq(&self, sub: &T, sup: &T) -> bool {
        if sub == sup {
            return true;
        }
        let mut visited: HashSet<T> = HashSet::new();
        self.proper_subtype(sub, sup, &mut visited)
    }

    fn proper_subtype(&self, sub: &T, sup: &T, visited: &mut HashSet<T>) -> bool {
        if !visited.insert(sub.clone()) {
            return false;
        }
        let Some(parents) = self.direct_supertypes.get(sub) else {
            return false;
        };
        for parent in parents {
            if parent == sup {
                return true;
            }
            if self.proper_subtype(parent, sup, visited) {
                return true;
            }
        }
        false
    }

    /// All declared direct supertypes of `t`. Useful for diagnostics.
    pub fn direct_supertypes_of(&self, t: &T) -> impl Iterator<Item = &T> {
        self.direct_supertypes
            .get(t)
            .into_iter()
            .flat_map(|s| s.iter())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_lattice_only_reflexive() {
        let lat: SubtypeLattice<&'static str> = SubtypeLattice::new();
        assert!(lat.leq(&"A", &"A"));
        assert!(!lat.leq(&"A", &"B"));
    }

    #[test]
    fn direct_subtype() {
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("Person", "Noun");
        assert!(lat.leq(&"Person", &"Noun"));
        assert!(!lat.leq(&"Noun", &"Person"));
        assert!(lat.leq(&"Person", &"Person"));
        assert!(lat.leq(&"Noun", &"Noun"));
    }

    #[test]
    fn transitive_subtype() {
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("Person", "Mammal");
        lat.add_subtype("Mammal", "Animal");
        lat.add_subtype("Animal", "Living");
        assert!(lat.leq(&"Person", &"Mammal"));
        assert!(lat.leq(&"Person", &"Animal"));
        assert!(lat.leq(&"Person", &"Living"));
        assert!(lat.leq(&"Mammal", &"Living"));
        assert!(!lat.leq(&"Animal", &"Person"));
    }

    #[test]
    fn diamond_subtype() {
        // Person :< Mammal, Person :< Pet, Mammal :< Animal, Pet :< Animal
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("Person", "Mammal");
        lat.add_subtype("Person", "Pet");
        lat.add_subtype("Mammal", "Animal");
        lat.add_subtype("Pet", "Animal");
        assert!(lat.leq(&"Person", &"Mammal"));
        assert!(lat.leq(&"Person", &"Pet"));
        assert!(lat.leq(&"Person", &"Animal"));
        assert!(lat.leq(&"Mammal", &"Animal"));
        assert!(lat.leq(&"Pet", &"Animal"));
        assert!(!lat.leq(&"Mammal", &"Pet"));
    }

    #[test]
    fn idempotent_add_subtype() {
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("A", "B");
        lat.add_subtype("A", "B");
        assert!(lat.leq(&"A", &"B"));
    }

    #[test]
    fn cycle_protection_does_not_loop() {
        // Cycles are *not* a contract violation we currently reject — protect
        // the lookup with a visited set so a buggy lexicon at most returns
        // a wrong answer instead of hanging.
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("A", "B");
        lat.add_subtype("B", "A");
        // The cycle means both A and B reach each other; we accept that.
        assert!(lat.leq(&"A", &"B"));
        assert!(lat.leq(&"B", &"A"));
        // And neither reaches a third type.
        assert!(!lat.leq(&"A", &"C"));
    }
}

// ---------------------------------------------------------------------------
// Property tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    /// Build an acyclic lattice from a Vec of edges interpreted as
    /// `(child, parent)`. Edges that would introduce a cycle are skipped.
    fn build_acyclic(edges: &[(u8, u8)]) -> SubtypeLattice<u8> {
        let mut lat = SubtypeLattice::new();
        for &(c, p) in edges {
            if c == p {
                continue;
            }
            // Skip edges that would create a cycle.
            if lat.leq(&p, &c) {
                continue;
            }
            lat.add_subtype(c, p);
        }
        lat
    }

    proptest! {
        #[test]
        fn leq_reflexive(t in any::<u8>(), edges in proptest::collection::vec((any::<u8>(), any::<u8>()), 0..30)) {
            let lat = build_acyclic(&edges);
            prop_assert!(lat.leq(&t, &t));
        }

        #[test]
        fn leq_transitive(
            edges in proptest::collection::vec((any::<u8>(), any::<u8>()), 0..30),
            a in any::<u8>(), b in any::<u8>(), c in any::<u8>(),
        ) {
            let lat = build_acyclic(&edges);
            if lat.leq(&a, &b) && lat.leq(&b, &c) {
                prop_assert!(lat.leq(&a, &c));
            }
        }

        #[test]
        fn leq_antisymmetric_on_acyclic(
            edges in proptest::collection::vec((any::<u8>(), any::<u8>()), 0..30),
            a in any::<u8>(), b in any::<u8>(),
        ) {
            // build_acyclic ensures the lattice is a DAG, so leq is antisymmetric.
            let lat = build_acyclic(&edges);
            if lat.leq(&a, &b) && lat.leq(&b, &a) {
                prop_assert_eq!(a, b);
            }
        }
    }
}
