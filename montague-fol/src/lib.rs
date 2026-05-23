//! First-order logic profile for Montague.
//!
//! Registers the `montague.fol` namespace with operators `and`, `or`, `not`,
//! `implies`, `forall`, `exists`, `iota`, `eq`.
//!
//! ## Usage
//!
//! ```ignore
//! let fol = Fol::register(&mut reg)?;
//! let term = SemTerm::bin_op(fol.and, a, b);
//! ```

use montague_core::registry::OpId;
use montague_derive::MontagueProfile;

/// The FOL profile — typed handles into the `montague.fol` namespace.
///
/// The derive adds a `register()` method. The `ns` field is populated by it.
#[derive(Debug, MontagueProfile)]
#[montague(namespace = "montague.fol", alias = "fol")]
pub struct Fol {
    /// The namespace id — populated by `register()`.
    pub ns: montague_core::registry::NamespaceId,
    #[op(name = "and", arity = 2)]
    pub and: OpId,
    #[op(name = "or", arity = 2)]
    pub or: OpId,
    #[op(name = "not", arity = 1)]
    pub not: OpId,
    #[op(name = "implies", arity = 2)]
    pub implies: OpId,
    #[binder(name = "forall", restrictor)]
    pub forall: OpId,
    #[binder(name = "exists", restrictor)]
    pub exists: OpId,
    #[binder(name = "iota", restrictor)]
    pub iota: OpId,
    #[op(name = "eq", arity = 2)]
    pub eq: OpId,
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use montague_core::registry::{IdKind, Registry};

    use super::*;

    #[test]
    fn registers_all_fol_operators() {
        let mut reg = Registry::empty();
        let fol = Fol::register(&mut reg).unwrap();

        assert_eq!(reg.namespace_count(), 1);
        assert_eq!(reg.qname_count(), 8);

        assert_eq!(reg.lookup_op(fol.ns, "and"), Some(fol.and));
        assert_eq!(reg.lookup_op(fol.ns, "or"), Some(fol.or));
        assert_eq!(reg.lookup_op(fol.ns, "not"), Some(fol.not));
        assert_eq!(reg.lookup_op(fol.ns, "implies"), Some(fol.implies));
        assert_eq!(reg.lookup_op(fol.ns, "forall"), Some(fol.forall));
        assert_eq!(reg.lookup_op(fol.ns, "exists"), Some(fol.exists));
        assert_eq!(reg.lookup_op(fol.ns, "iota"), Some(fol.iota));
        assert_eq!(reg.lookup_op(fol.ns, "eq"), Some(fol.eq));

        let res = reg.resolve_namespace("fol");
        assert!(matches!(
            res,
            montague_core::registry::NamespaceResolution::Found(_)
        ));
    }

    #[test]
    fn duplicate_registration_errors() {
        let mut reg = Registry::empty();
        Fol::register(&mut reg).unwrap();
        let err = Fol::register(&mut reg).unwrap_err();
        assert!(matches!(
            err,
            montague_core::registry::RegisterError::NamespaceExists { .. }
        ));
    }

    #[test]
    fn operators_have_correct_kinds() {
        let mut reg = Registry::empty();
        let fol = Fol::register(&mut reg).unwrap();

        assert_eq!(reg.kind_of(fol.and.qname()), IdKind::Op);
        assert_eq!(reg.kind_of(fol.or.qname()), IdKind::Op);
        assert_eq!(reg.kind_of(fol.implies.qname()), IdKind::Op);
        assert_eq!(reg.kind_of(fol.forall.qname()), IdKind::Binder);
        assert_eq!(reg.kind_of(fol.exists.qname()), IdKind::Binder);
        assert_eq!(reg.kind_of(fol.iota.qname()), IdKind::Binder);
    }
}
