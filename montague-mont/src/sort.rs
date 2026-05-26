//! Sort registry for the `.mont` DSL.
//!
//! Sorts are orthogonal namespaces for semantic features (case, entity kind,
//! gender) that ride along with grammatical types via parametric polymorphism.
//! Each sort has its own internal [`SubtypeLattice`] for member-to-member
//! subtyping (e.g., `Person :< Animate` within sort `entity`).

use std::collections::{HashMap, HashSet};

use montague_core::subtyping::SubtypeLattice;

/// Information about a registered sort.
#[derive(Debug, Clone)]
pub struct SortInfo {
    pub members: HashSet<String>,
    pub lattice: SubtypeLattice<String>,
}

impl SortInfo {
    pub fn new(member: String) -> Self {
        let mut members = HashSet::new();
        members.insert(member.clone());
        let mut lattice = SubtypeLattice::new();
        lattice.add_subtype(member.clone(), member);
        SortInfo { members, lattice }
    }
}

/// Registry of all sorts declared across a lexicon closure.
#[derive(Debug, Clone, Default)]
pub struct SortRegistry {
    pub sorts: HashMap<String, SortInfo>,
}

impl SortRegistry {
    pub fn new() -> Self {
        SortRegistry {
            sorts: HashMap::new(),
        }
    }

    /// Declare a new sort. Errors if the sort already exists.
    pub fn add_sort(&mut self, name: String) -> Result<(), String> {
        if self.sorts.contains_key(&name) {
            return Err(format!("duplicate sort declaration: `{name}`"));
        }
        self.sorts.insert(name, SortInfo {
            members: HashSet::new(),
            lattice: SubtypeLattice::new(),
        });
        Ok(())
    }

    /// Add a member to a sort. Errors if the sort doesn't exist or the
    /// member already belongs to a different sort.
    pub fn add_member(&mut self, sort_name: &str, member: String) -> Result<(), String> {
        if !self.sorts.contains_key(sort_name) {
            return Err(format!("unknown sort `{sort_name}` for member `{member}`"));
        }
        // Check cross-sort collisions before borrowing mutably
        if let Some(other_sort) = self.member_of(&member) {
            if other_sort != sort_name {
                return Err(format!(
                    "`{member}` already declared as member of sort `{other_sort}`, cannot also be in `{sort_name}`"
                ));
            }
        }
        let info = self.sorts.get_mut(sort_name).unwrap();
        if info.members.contains(&member) {
            return Ok(()); // Idempotent — already registered.
        }
        info.members.insert(member.clone());
        // Each member is reflexively ≤ itself.
        info.lattice.add_subtype(member.clone(), member);
        Ok(())
    }

    /// Add a subtype edge within a sort: `sub :< sup`.
    /// Both must be members of the same sort.
    pub fn add_subtype(&mut self, sub: &str, sup: &str) -> Result<(), String> {
        let sort_a = self.member_of(sub).map(|s| s.to_string());
        let sort_b = self.member_of(sup).map(|s| s.to_string());
        match (&sort_a, &sort_b) {
            (Some(a), Some(b)) if a == b => {
                self.sorts.get_mut(a).unwrap().lattice.add_subtype(sub.to_string(), sup.to_string());
                Ok(())
            }
            (Some(a), Some(b)) => {
                Err(format!(
                    "`{sub}` is in sort `{a}`, `{sup}` is in sort `{b}` — cross-sort subtyping is not allowed"
                ))
            }
            (None, _) => Err(format!("`{sub}` is not a member of any sort")),
            (_, None) => Err(format!("`{sup}` is not a member of any sort")),
        }
    }

    /// Check whether `a ≤ b` within its sort's lattice.
    /// If either is not a sort member, returns `false`.
    pub fn leq(&self, a: &String, b: &String) -> bool {
        match (self.member_of(a), self.member_of(b)) {
            (Some(sa), Some(sb)) if sa == sb => {
                self.sorts[sa].lattice.leq(a, b)
            }
            _ => a == b,
        }
    }

    /// Returns the sort name that `member` belongs to, if any.
    pub fn member_of(&self, member: &str) -> Option<&str> {
        self.sorts
            .iter()
            .find(|(_, info)| info.members.contains(member))
            .map(|(name, _)| name.as_str())
    }

    /// Returns the sortinfo for a sort by name.
    pub fn get(&self, name: &str) -> Option<&SortInfo> {
        self.sorts.get(name)
    }

    /// Merge another `SortRegistry` into this one. Collisions on sort names,
    /// with overlapping member sets, are allowed (union); a member appearing
    /// in two sorts is an error.
    pub fn extend(&mut self, other: &SortRegistry) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        for (name, info) in &other.sorts {
            if !self.sorts.contains_key(name) {
                self.sorts.insert(name.clone(), info.clone());
                continue;
            }
            // Check members for cross-sort conflicts first (immutable borrow)
            let existing = self.sorts.get(name).unwrap();
            for m in &info.members {
                if existing.members.contains(m) {
                    continue;
                }
                if let Some(other_sort) = self.member_of(m) {
                    errors.push(format!(
                        "`{m}` is already in sort `{other_sort}`, cannot merge from `{name}`"
                    ));
                }
            }
            if !errors.is_empty() {
                return Err(errors);
            }
            // Now mutable merge
            let existing = self.sorts.get_mut(name).unwrap();
            for m in &info.members {
                if !existing.members.contains(m) {
                    existing.members.insert(m.clone());
                    existing.lattice.add_subtype(m.clone(), m.clone());
                }
            }
            existing.lattice.union(&info.lattice);
        }
        Ok(())
    }

    /// Convert to the lightweight core [`montague_core::sort::SortRegistry`]
    /// used at reduction time.
    pub fn to_core(&self) -> montague_core::sort::SortRegistry {
        let mut core = montague_core::sort::SortRegistry::new();
        for (name, info) in &self.sorts {
            core.sorts.insert(name.clone(), info.lattice.clone());
        }
        core
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declare_sort_and_add_members() {
        let mut reg = SortRegistry::new();
        reg.add_sort("entity".into()).unwrap();
        reg.add_member("entity", "Person".into()).unwrap();
        reg.add_member("entity", "Animate".into()).unwrap();
        reg.add_member("entity", "Abstract".into()).unwrap();

        assert_eq!(reg.sorts["entity"].members.len(), 3);
        assert_eq!(reg.member_of("Person"), Some("entity"));
        assert_eq!(reg.member_of("Animate"), Some("entity"));
        assert_eq!(reg.member_of("Abstract"), Some("entity"));
        assert!(reg.member_of("Unknown").is_none());
    }

    #[test]
    fn sort_internal_subtyping() {
        let mut reg = SortRegistry::new();
        reg.add_sort("entity".into()).unwrap();
        reg.add_member("entity", "Person".into()).unwrap();
        reg.add_member("entity", "Animate".into()).unwrap();
        reg.add_member("entity", "Abstract".into()).unwrap();
        reg.add_subtype("Person", "Animate").unwrap();
        reg.add_subtype("Animate", "Abstract").unwrap();

        assert!(reg.leq(&"Person".to_string(), &"Animate".to_string()));
        assert!(reg.leq(&"Person".to_string(), &"Abstract".to_string()));
        assert!(!reg.leq(&"Abstract".to_string(), &"Person".to_string()));
        assert!(reg.leq(&"Person".to_string(), &"Person".to_string()));
    }

    #[test]
    fn cross_sort_subtype_rejected() {
        let mut reg = SortRegistry::new();
        reg.add_sort("entity".into()).unwrap();
        reg.add_sort("case".into()).unwrap();
        reg.add_member("entity", "Animate".into()).unwrap();
        reg.add_member("case", "Nominative".into()).unwrap();
        let err = reg.add_subtype("Animate", "Nominative").unwrap_err();
        assert!(err.contains("cross-sort"));
    }

    #[test]
    fn member_cannot_be_in_two_sorts() {
        let mut reg = SortRegistry::new();
        reg.add_sort("entity".into()).unwrap();
        reg.add_sort("case".into()).unwrap();
        reg.add_member("entity", "Person".into()).unwrap();
        let err = reg.add_member("case", "Person".into()).unwrap_err();
        assert!(err.contains("already declared"));
    }

    #[test]
    fn duplicate_member_is_idempotent() {
        let mut reg = SortRegistry::new();
        reg.add_sort("entity".into()).unwrap();
        assert!(reg.add_member("entity", "Person".into()).is_ok());
        assert!(reg.add_member("entity", "Person".into()).is_ok()); // idempotent
        assert_eq!(reg.sorts["entity"].members.len(), 1);
    }

    #[test]
    fn leq_with_non_members() {
        let mut reg = SortRegistry::new();
        reg.add_sort("entity".into()).unwrap();
        reg.add_member("entity", "Person".into()).unwrap();
        // Non-members only equal themselves
        assert!(reg.leq(&"SomeType".to_string(), &"SomeType".to_string()));
        assert!(!reg.leq(&"SomeType".to_string(), &"OtherType".to_string()));
    }

    #[test]
    fn extend_merge_sorts() {
        let mut a = SortRegistry::new();
        a.add_sort("entity".into()).unwrap();
        a.add_member("entity", "Person".into()).unwrap();
        a.add_member("entity", "Animate".into()).unwrap();

        let mut b = SortRegistry::new();
        b.add_sort("entity".into()).unwrap();
        b.add_member("entity", "Animal".into()).unwrap();
        b.add_member("entity", "Animate".into()).unwrap(); // overlap, ok
        b.add_sort("case".into()).unwrap();
        b.add_member("case", "Nominative".into()).unwrap();

        a.extend(&b).unwrap();
        assert!(a.member_of("Person").is_some());
        assert!(a.member_of("Animal").is_some());
        assert!(a.member_of("Nominative").is_some());
    }

    #[test]
    fn duplicate_sort_declaration_errors() {
        let mut reg = SortRegistry::new();
        reg.add_sort("entity".into()).unwrap();
        let err = reg.add_sort("entity".into()).unwrap_err();
        assert!(err.contains("duplicate"));
    }
}
