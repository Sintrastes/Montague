//! Lightweight sort registry for the core engine.
//!
//! Maps each sort name to its internal [`SubtypeLattice`] for member-to-member
//! subtyping (e.g. `Person :< Animate` within sort `entity`).  The reduction
//! engine consults this during sort-variable unification.
//!
//! The full-featured [`montague_mont::sort::SortRegistry`] (with member-to-sort
//! reverse indexing, cross-sort collision detection, etc.) lives in
//! `montague-mont`.  This version supplies only the query methods needed at
//! reduction time.

use std::collections::HashMap;

use crate::subtyping::SubtypeLattice;

/// Maps `sort_name → SubtypeLattice<member_name>` for querying sort-internal
/// subtyping at reduction time.
#[derive(Debug, Clone, Default)]
pub struct SortRegistry {
    pub sorts: HashMap<String, SubtypeLattice<String>>,
}

impl SortRegistry {
    pub fn new() -> Self {
        SortRegistry {
            sorts: HashMap::new(),
        }
    }

    /// Register a sort (empty lattice).
    pub fn add_sort(&mut self, name: String) {
        self.sorts.entry(name).or_default();
    }

    /// Add a member to a sort (registers the reflexive edge).
    pub fn add_member(&mut self, sort: &str, member: String) {
        let lat = self.sorts.entry(sort.to_string()).or_default();
        lat.add_subtype(member.clone(), member);
    }

    /// Add a subtype edge within a sort.
    pub fn add_subtype(&mut self, sort: &str, sub: &str, sup: &str) {
        if let Some(lat) = self.sorts.get_mut(sort) {
            lat.add_subtype(sub.to_string(), sup.to_string());
        }
    }

    /// Check `a ≤ b` within their sort.  Both must belong to the same sort.
    pub fn leq(&self, a: &str, b: &str) -> bool {
        // Same member → reflexive
        if a == b {
            return true;
        }
        let a_s: String = a.to_string();
        let b_s: String = b.to_string();
        for (_sort_name, lat) in &self.sorts {
            if lat.leq(&a_s, &b_s) {
                return true;
            }
        }
        false
    }

    /// Merge another [`SortRegistry`] into this one.
    pub fn extend(&mut self, other: &SortRegistry) {
        for (name, lat) in &other.sorts {
            self.sorts
                .entry(name.clone())
                .or_default()
                .union(lat);
        }
    }
}
