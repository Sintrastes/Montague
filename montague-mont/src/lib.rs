//! `.mont` DSL parser and name resolver.
//!
//! Two-phase pipeline:
//! - [`parser::parse`] produces a purely-syntactic AST ([`ast::MontFile`]).
//! - [`resolver::resolve`] takes the AST plus a [`Registry`] and produces a
//!   typed lexicon with populated subtype lattice.
//!
//! [`Registry`]: montague_core::registry::Registry

pub mod ast;
pub mod error;
pub mod parser;
pub mod resolver;
pub mod sort;
