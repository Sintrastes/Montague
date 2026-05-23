//! Structured errors for `.mont` parsing and name resolution.
//!
//! Both [`MontParseError`] and [`ResolveError`] carry [`Span`] information for
//! downstream `ariadne` rendering (in `montague-cli`). Library crates depend on
//! `thiserror`; only the CLI pulls in `ariadne`.

use thiserror::Error;

use crate::ast::Span;

/// Errors from the chumsky parser.
///
/// Produced during [`crate::parser::parse`]; each error carries the byte range
/// where the problem occurred.
#[derive(Debug, Clone, Error)]
pub enum MontParseError {
    #[error("expected a declaration or directive")]
    ExpectedDeclaration { span: Span },

    #[error("invalid type name: must start with an uppercase letter")]
    InvalidTypeName { found: String, span: Span },

    #[error("invalid entity name: must start with a lowercase letter")]
    InvalidEntityName { found: String, span: Span },

    #[error("unexpected token `{found}`")]
    UnexpectedToken {
        expected: String,
        found: String,
        span: Span,
    },
}

/// Errors from name resolution.
///
/// Produced during [`crate::resolver::resolve`]; these reference the registry
/// and check for unknown names, ambiguous references, and subtype cycles.
#[derive(Debug, Clone, Error)]
pub enum ResolveError {
    #[error("unknown namespace `{name}`")]
    UnknownNamespace { name: String, span: Span },

    #[error("ambiguous reference `{local}` — could be {candidates:?}")]
    AmbiguousReference {
        local: String,
        candidates: Vec<String>,
        span: Span,
    },

    #[error("unknown type `{name}`")]
    UnknownType { name: String, span: Span },

    #[error("duplicate entity declaration `{entity}`")]
    DuplicateEntity {
        entity: String,
        original_span: Span,
        span: Span,
    },

    #[error("subtype declaration references an unknown type `{name}`")]
    SubtypeUnknownType { name: String, span: Span },

    #[error("subtype cycle detected: {chain}")]
    SubtypeCycle { chain: String, spans: Vec<Span> },

    #[error("production references unknown entity `{entity}`")]
    UnknownEntity { entity: String, span: Span },

    #[error("could not parse qualified connective `{name}`")]
    UnresolvedConnective { name: String, span: Span },
}
