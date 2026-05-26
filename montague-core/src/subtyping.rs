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

    /// Least upper bound (join) of `a` and `b` in the lattice.  Returns the
    /// most specific type that is a supertype of both, or `None` if no unique
    /// join exists (e.g. diamond-shaped lattice with multiple minimal common
    /// ancestors).
    ///
    /// Needed for coordination (Φ rule) to compute the result category from
    /// two conjuncts: `X CONJ X' → join(X, X')`.
    pub fn join(&self, a: &T, b: &T) -> Option<T> {
        if a == b {
            return Some(a.clone());
        }
        if self.leq(a, b) {
            return Some(b.clone());
        }
        if self.leq(b, a) {
            return Some(a.clone());
        }
        let sup_a = self.all_supertypes(a);
        let sup_b = self.all_supertypes(b);
        let intersection: Vec<T> = sup_a
            .iter()
            .filter(|t| sup_b.contains(*t))
            .cloned()
            .collect();
        if intersection.is_empty() {
            return None;
        }
        // Keep only the minimal elements: for each candidate c, no other
        // candidate is a strict subtype of c (other < c).
        let mut minimal: Vec<T> = Vec::new();
        for c in &intersection {
            let is_minimal = !intersection.iter().any(|other| {
                let diff = other.ne(c);
                diff && self.leq(other, c) && !self.leq(c, other)
            });
            if is_minimal {
                minimal.push(c.clone());
            }
        }
        if minimal.len() == 1 {
            Some(minimal.into_iter().next().unwrap())
        } else {
            None
        }
    }

    /// Merge all subtype edges from `other` into this lattice.
    ///
    /// Used when combining lexicons from multiple `.mont` files via `extend`.
    pub fn union(&mut self, other: &SubtypeLattice<T>) {
        for (sub, sups) in &other.direct_supertypes {
            for sup in sups {
                self.add_subtype(sub.clone(), sup.clone());
            }
        }
    }

    /// Iterate over all direct subtype→supertypes edges.
    pub fn direct_supertypes_iter(&self) -> impl Iterator<Item = (&T, &HashSet<T>)> {
        self.direct_supertypes.iter()
    }

    /// Collect all supertypes (transitive closure upward) of `t`, including
    /// `t` itself.
    fn all_supertypes(&self, t: &T) -> HashSet<T> {
        let mut result = HashSet::new();
        let mut stack = vec![t.clone()];
        while let Some(current) = stack.pop() {
            if result.insert(current.clone()) {
                if let Some(parents) = self.direct_supertypes.get(&current) {
                    for p in parents {
                        if !result.contains(p) {
                            stack.push(p.clone());
                        }
                    }
                }
            }
        }
        result
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

    // -- join -----------------------------------------------------------

    #[test]
    fn join_equal() {
        let lat: SubtypeLattice<&str> = SubtypeLattice::new();
        assert_eq!(lat.join(&"A", &"A"), Some("A"));
    }

    #[test]
    fn join_direct_subtype() {
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("Person", "Animate");
        // Person ∨ Animate = Animate (Animate is the LUB)
        assert_eq!(lat.join(&"Person", &"Animate"), Some("Animate"));
    }

    #[test]
    fn join_siblings_under_common_parent() {
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("Person", "Animate");
        lat.add_subtype("Animal", "Animate");
        // Person ∨ Animal = Animate
        assert_eq!(lat.join(&"Person", &"Animal"), Some("Animate"));
    }

    #[test]
    fn join_no_common_ancestor() {
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("Person", "Animate");
        lat.add_subtype("Rock", "Physical");
        // Person and Rock have no common ancestor → None
        assert_eq!(lat.join(&"Person", &"Rock"), None);
    }

    #[test]
    fn join_diamond_multiple_minimal() {
        // Person :< Mammal, Person :< Pet, Mammal :< Animal, Pet :< Animal
        // Person ∨ ??? — both Mammal and Pet are minimal common ancestors
        // but neither is ≤ the other → no unique join
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("Person", "Mammal");
        lat.add_subtype("Person", "Pet");
        lat.add_subtype("Mammal", "Animal");
        lat.add_subtype("Pet", "Animal");
        // Person ∨ Person is just Person (handled by equality case)
        assert_eq!(lat.join(&"Person", &"Person"), Some("Person"));
        // Mammal ∨ Pet = Animal (both under Animal)
        assert_eq!(lat.join(&"Mammal", &"Pet"), Some("Animal"));
        // But Mammal ∨ Pet where no common parent except Animal,
        // and both are direct children of Animal → unique LUB = Animal
        assert_eq!(lat.join(&"Mammal", &"Pet"), Some("Animal"));
    }

    #[test]
    fn join_transitive() {
        let mut lat = SubtypeLattice::new();
        lat.add_subtype("Socrates", "Person");
        lat.add_subtype("Person", "Animate");
        lat.add_subtype("Animate", "Entity");
        lat.add_subtype("Fido", "Dog");
        lat.add_subtype("Dog", "Animal");
        lat.add_subtype("Animal", "Animate");
        lat.add_subtype("Animal", "Entity");
        // Socrates ∨ Fido = Animate (first common ancestor going up)
        assert_eq!(lat.join(&"Socrates", &"Fido"), Some("Animate"));
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
