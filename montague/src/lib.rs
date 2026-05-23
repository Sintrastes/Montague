//! Montague — Lambek-calculus categorial-grammar parser with subtyping.
//!
//! Umbrella crate. Re-exports the curated default bundle: `montague-core`,
//! `montague-fol`, `montague-mont`, `montague-pretty`, `montague-prolog`.
//!
//! Populated alongside `Engine::default()` in milestone M8.
//!
//! For ad-hoc usage, prefer depending on the individual crates directly.

pub use montague_core as core;
pub use montague_fol as fol;
pub use montague_mont as mont;
pub use montague_pretty as pretty;
pub use montague_prolog as prolog;
