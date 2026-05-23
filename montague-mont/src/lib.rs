//! `.mont` DSL parser and name resolver.
//!
//! Two-phase pipeline: [`parse`] produces a purely-syntactic AST,
//! [`resolve`] takes that AST plus a `Registry` and produces a typed
//! `Lexicon` with populated `SubtypeLattice`.
//!
//! Populated in milestone M6 (chumsky-based).
