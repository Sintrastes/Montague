//! Standard (starter) lexicon for Montague.
//!
//! Registers the `montague.standard` namespace with common predicate symbols
//! and individual constants used in introductory examples (syllogism, simple
//! predication).

use montague_core::registry::{owner_info, ConstId, PredId, RegisterError, Registry};

/// The standard lexicon profile — typed handles into `montague.standard`.
#[derive(Debug)]
pub struct Standard {
    pub ns: montague_core::registry::NamespaceId,
    /// Predicates
    pub man: PredId,
    pub mortal: PredId,
    pub like: PredId,
    /// Individual constants
    pub socrates: ConstId,
}

/// Register the `montague.standard` namespace and common symbols.
pub fn register(reg: &mut Registry) -> Result<Standard, RegisterError> {
    let ns = reg
        .namespace()
        .canonical("montague.standard")
        .alias("std")
        .owner(owner_info!())
        .create()?;
    Ok(Standard {
        ns,
        man: reg.pred(ns, "man").arity_fixed(1).register()?,
        mortal: reg.pred(ns, "mortal").arity_fixed(1).register()?,
        like: reg.pred(ns, "like").arity_fixed(2).register()?,
        socrates: reg.const_(ns, "socrates").register()?,
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registers_standard_symbols() {
        let mut reg = Registry::empty();
        let std = register(&mut reg).unwrap();

        assert_eq!(reg.namespace_count(), 1);
        // 3 preds + 1 const = 4 entries
        assert_eq!(reg.qname_count(), 4);

        assert_eq!(reg.lookup_pred(std.ns, "man"), Some(std.man));
        assert_eq!(reg.lookup_pred(std.ns, "mortal"), Some(std.mortal));
        assert_eq!(reg.lookup_pred(std.ns, "like"), Some(std.like));
        assert_eq!(reg.lookup_const(std.ns, "socrates"), Some(std.socrates));

        // Kinds.
        use montague_core::registry::{IdKind, Registry};
        assert_eq!(reg.kind_of(std.man.qname()), IdKind::Pred);
        assert_eq!(reg.kind_of(std.socrates.qname()), IdKind::Const);
    }

    #[test]
    fn alias_resolves() {
        let mut reg = Registry::empty();
        let std = register(&mut reg).unwrap();
        let res = reg.resolve_namespace("std");
        assert!(matches!(
            res,
            montague_core::registry::NamespaceResolution::Found(id) if id == std.ns
        ));
    }
}
