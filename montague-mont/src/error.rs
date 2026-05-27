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
/// where the problem occurred. The `contexts` field on `UnexpectedToken`
/// contains "while parsing this X" breadcrumbs from chumsky's `Rich::contexts()`.
#[derive(Debug, Clone, Error)]
pub enum MontParseError {
    #[error("expected a declaration or directive")]
    ExpectedDeclaration { span: Span },

    #[error("invalid type name `{found}` — must start with an uppercase letter")]
    InvalidTypeName { found: String, span: Span },

    #[error("invalid entity name `{found}` — must start with a lowercase letter")]
    InvalidEntityName { found: String, span: Span },

    #[error("unexpected token")]
    UnexpectedToken {
        /// What the parser expected (chumsky `reason()`). Empty if unknown.
        expected: String,
        /// What was found at the span. None ⇒ end of input.
        found: String,
        /// "while parsing this X" breadcrumbs from `Rich::contexts()`.
        #[allow(dead_code)]
        contexts: Vec<(String, Span)>,
        span: Span,
    },

    #[error("invalid character `{found}` in source")]
    InvalidChar { found: char, span: Span },

    #[error("unterminated string literal")]
    UnterminatedString { span: Span },

    #[error("{message}")]
    Custom { message: String, span: Span },
}

/// Errors from name resolution.
///
/// Produced during [`crate::resolver::resolve`]; these reference the registry
/// and check for unknown names, ambiguous references, and subtype cycles.
#[derive(Debug, Clone, Error)]
pub enum ResolveError {
    #[error("unknown namespace `{name}`")]
    UnknownNamespace {
        name: String,
        span: Span,
        /// Source file path for cross-file diagnostics.
        file_path: Option<String>,
    },

    #[error("ambiguous reference `{local}` — could be {candidates:?}")]
    AmbiguousReference {
        local: String,
        candidates: Vec<String>,
        span: Span,
        file_path: Option<String>,
    },

    #[error("unknown type `{name}`")]
    UnknownType {
        name: String,
        span: Span,
        file_path: Option<String>,
    },

    #[error("duplicate entity declaration `{entity}`")]
    DuplicateEntity {
        entity: String,
        original_span: Span,
        span: Span,
        file_path: Option<String>,
    },

    #[error("subtype declaration references an unknown type `{name}`")]
    SubtypeUnknownType {
        name: String,
        span: Span,
        file_path: Option<String>,
    },

    #[error("subtype cycle detected: {chain}")]
    SubtypeCycle {
        chain: String,
        spans: Vec<Span>,
        file_path: Option<String>,
    },

    #[error("production references unknown entity `{entity}`")]
    UnknownEntity {
        entity: String,
        span: Span,
        file_path: Option<String>,
    },

    #[error("could not parse qualified connective `{name}`")]
    UnresolvedConnective {
        name: String,
        span: Span,
        file_path: Option<String>,
    },
}

impl ResolveError {
    /// Set the source file path for cross-file diagnostics.
    pub fn with_file(mut self, path: String) -> Self {
        match &mut self {
            ResolveError::UnknownNamespace { file_path, .. }
            | ResolveError::AmbiguousReference { file_path, .. }
            | ResolveError::UnknownType { file_path, .. }
            | ResolveError::DuplicateEntity { file_path, .. }
            | ResolveError::SubtypeUnknownType { file_path, .. }
            | ResolveError::SubtypeCycle { file_path, .. }
            | ResolveError::UnknownEntity { file_path, .. }
            | ResolveError::UnresolvedConnective { file_path, .. } => *file_path = Some(path),
        }
        self
    }
}
