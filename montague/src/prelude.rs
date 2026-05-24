//! Convenience re-exports for the default Montague setup.
//!
//! `use montague::prelude::*` brings in the most commonly-used types.

pub use montague_core::{
    annotate,
    chart::{Chart, Derivation},
    get_all_parses, get_parse,
    reduction::{ReductionCtx, ReductionEngine, RuleApplicability, TypeShape},
    registry::{ConstId, OpId, PredId, Registry},
    sem::{
        alpha_eq, alpha_rename, beta_normalize, free_vars, Backend, BackendCtx, BetaNormalize,
        OpSet, Pass, PassCtx, SemTerm, VarEnv, VarId,
    },
    subtyping::SubtypeLattice,
    types::{AnnotatedTerm, LambekFold, LambekType, Term},
    Semantics,
};

pub use montague_fol::Fol;
pub use montague_standard::Standard;

pub use montague_pretty::{
    display_lambek_as_sexp, display_semterm_as_prolog, display_semterm_as_sexp, SExpBackend,
};
pub use montague_prolog::{
    lower_to_clause, lower_to_clauses, PrologBackend, PrologClause, PrologTerm,
};

pub use crate::engine::Engine;
