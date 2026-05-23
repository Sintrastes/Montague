//! Name resolver for `.mont` files.
//!
//! Takes a [`MontFile`] AST plus a [`Registry`] and produces a typed lexicon
//! with populated subtype lattice. Two-phase: parsing produces syntax, resolution
//! produces typed information.
//!
//! [`Registry`]: montague_core::registry::Registry
//!
//! TODO(M6): implement resolve() — currently a stub. Will be completed
//! alongside snapshot and property tests.

use montague_core::registry::Registry;
use montague_core::subtyping::SubtypeLattice;

use crate::ast::{AtomEntry, MontFile, ProductionEntry};
use crate::error::ResolveError;

/// The output of name resolution — a typed lexicon ready for parsing.
#[derive(Debug, Clone)]
pub struct ResolvedLexicon<T: std::hash::Hash + std::cmp::Eq + Clone> {
    pub atoms: Vec<AtomEntry<T>>,
    pub productions: Vec<ProductionEntry>,
    pub lattice: SubtypeLattice<T>,
}

/// Resolve a [`MontFile`] AST against a [`Registry`], producing a typed lexicon
/// and subtype lattice.
///
/// TODO(M6): full implementation.
pub fn resolve<T>(
    _file: &MontFile,
    _reg: &Registry,
) -> Result<ResolvedLexicon<T>, Vec<ResolveError>>
where
    T: std::hash::Hash + std::cmp::Eq + Clone + std::fmt::Debug,
{
    Ok(ResolvedLexicon {
        atoms: Vec::new(),
        productions: Vec::new(),
        lattice: SubtypeLattice::new(),
    })
}
