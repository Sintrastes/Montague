//! Montague — Lambek-calculus categorial-grammar parser with subtyping.
//!
//! Umbrella crate re-exporting the default bundle. For ad-hoc usage, prefer
//! depending on the individual crates directly.
//!
//! ## Quick start
//!
//! ```ignore
//! use montague::prelude::*;
//!
//! let engine = Engine::default();
//! let chart = engine.parse("every cat is happy")?;
//! let prolog = engine.lower::<PrologBackend>(&chart)?;
//! ```

pub mod engine;
pub mod prelude;

pub use montague_core as core;
pub use montague_fol as fol;
pub use montague_mont as mont;
pub use montague_pretty as pretty;
pub use montague_prolog as prolog;
pub use montague_standard as standard;
