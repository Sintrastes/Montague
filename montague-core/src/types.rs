//! Core syntax algebra and term types for Montague.
//!
//! [`LambekType`] is the closed-enum syntax algebra with an interned [`CustomTag`]
//! escape hatch for research-grade connectives. [`Term`] and [`AnnotatedTerm`] are
//! the term / type-annotated-term pairing produced by parsing.
//!
//! Subtyping is consulted via a [`crate::subtyping::SubtypeLattice`] rather than
//! a compile-time `LatticeOrd` trait — this is what makes `.mont`-declared
//! `Person :< Noun.` first-class (see D0/D1).

use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

use crate::registry::CustomTag;
use crate::sort::SortRegistry;
use crate::subtyping::SubtypeLattice;

// ---------------------------------------------------------------------------
// Unification error types (for chart-failure diagnostics)
// ---------------------------------------------------------------------------

/// Reason a sort unification or type-check failed.
#[derive(Debug, Clone)]
pub enum UnifyError {
    /// Sort mismatch: expected sort `A`, got sort `B`.
    SortMismatch {
        expected_sort: String,
        actual_sort: String,
        position: TypePath,
    },
    /// Sort-internal subtyping failure: `expected` is not above `actual`.
    SortMemberLeq {
        sort: String,
        expected: String,
        actual: String,
        position: TypePath,
    },
    /// Different atom names (e.g. `NP` vs `S`).
    NameMismatch {
        expected: String,
        actual: String,
        position: TypePath,
    },
    /// Wrong arity for an atom.
    ArityMismatch {
        name: String,
        expected: usize,
        actual: usize,
        position: TypePath,
    },
    /// Structural mismatch (e.g. `LeftArrow` vs `Basic`).
    StructureMismatch {
        expected_shape: String,
        actual_shape: String,
        position: TypePath,
    },
    /// Occurs-check failure when binding a variable.
    OccursCheck {
        var: SortVarId,
        position: TypePath,
    },
}

impl UnifyError {
    /// Update the `position` field by applying a function to the current path.
    pub fn with_position(mut self, f: impl FnOnce(TypePath) -> TypePath) -> Self {
        let new_pos = f(TypePath::default());
        match &mut self {
            UnifyError::SortMismatch { position, .. }
            | UnifyError::SortMemberLeq { position, .. }
            | UnifyError::NameMismatch { position, .. }
            | UnifyError::ArityMismatch { position, .. }
            | UnifyError::StructureMismatch { position, .. }
            | UnifyError::OccursCheck { position, .. } => *position = new_pos,
        }
        self
    }
}

impl fmt::Display for UnifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SortMismatch { expected_sort, actual_sort, .. } => {
                write!(f, "sort mismatch: expected sort `{expected_sort}`, got sort `{actual_sort}`")
            }
            Self::SortMemberLeq { sort, expected, actual, .. } => {
                write!(f, "expected `{expected}`, got `{actual}` (both in sort `{sort}`)")
            }
            Self::NameMismatch { expected, actual, .. } => {
                write!(f, "expected type `{expected}`, got `{actual}`")
            }
            Self::ArityMismatch { name, expected, actual, .. } => {
                write!(f, "`{name}` expects {expected} parameter(s), got {actual}")
            }
            Self::StructureMismatch { expected_shape, actual_shape, .. } => {
                write!(f, "expected {expected_shape} but found {actual_shape}")
            }
            Self::OccursCheck { var, .. } => {
                write!(f, "occurs check failure for variable {var:?}")
            }
        }
    }
}

/// Position in a type tree, e.g. "left arg's right arg's first sort param".
#[derive(Debug, Clone, Default)]
pub struct TypePath(pub Vec<TypePathStep>);

#[derive(Debug, Clone)]
pub enum TypePathStep {
    LeftArrowArg,
    LeftArrowResult,
    RightArrowArg,
    RightArrowResult,
    ExtractArg,
    ExtractResult,
    ScopedArg,
    ScopedResult,
    ConjLeft,
    ConjRight,
    DisjLeft,
    DisjRight,
    AtomArg(usize),
}

impl TypePath {
    pub fn push(&self, step: TypePathStep) -> Self {
        let mut new = self.0.clone();
        new.push(step);
        TypePath(new)
    }
}

impl fmt::Display for TypePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }
        for (i, step) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " → ")?;
            }
            match step {
                TypePathStep::LeftArrowArg => write!(f, "left arg")?,
                TypePathStep::LeftArrowResult => write!(f, "left result")?,
                TypePathStep::RightArrowArg => write!(f, "right arg")?,
                TypePathStep::RightArrowResult => write!(f, "right result")?,
                TypePathStep::AtomArg(n) => write!(f, "param {}", n + 1)?,
                TypePathStep::ExtractArg => write!(f, "extract arg")?,
                TypePathStep::ExtractResult => write!(f, "extract result")?,
                TypePathStep::ScopedArg => write!(f, "scoped arg")?,
                TypePathStep::ScopedResult => write!(f, "scoped result")?,
                TypePathStep::ConjLeft => write!(f, "left conjunct")?,
                TypePathStep::ConjRight => write!(f, "right conjunct")?,
                TypePathStep::DisjLeft => write!(f, "left disjunct")?,
                TypePathStep::DisjRight => write!(f, "right disjunct")?,
            }
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// AtomType — leaf types with sort parameters
// ---------------------------------------------------------------------------

/// A single sort variable identifier. Fresh IDs are assigned at chart-lexical-
/// fill time so different occurrences of the same polymorphic variable in a
/// lexical entry get distinct handles (standard HM freshen-on-use).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SortVarId(pub u32);

/// An argument in a parametric atom type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SortArg {
    /// A concrete sort-member reference, e.g. `{entity, Person}`.
    Concrete { sort: String, member: String },
    /// A polymorphic variable with a fresh handle.
    Var(SortVarId),
}

impl SortArg {
    /// Try to unify `self ≤ other` (subtyping direction), recording variable
    /// bindings in `var_table`.  Returns `Ok(())` on success, `Err(UnifyError)` with
    /// diagnostic details on failure.
    pub fn unify_with(
        &self,
        other: &SortArg,
        sort_registry: Option<&SortRegistry>,
        var_table: &mut SortVarTable,
    ) -> Result<(), UnifyError> {
        let a = var_table.resolve(self);
        let b = var_table.resolve(other);
        match (&a, &b) {
            (
                SortArg::Concrete {
                    sort: sa,
                    member: ma,
                },
                SortArg::Concrete {
                    sort: sb,
                    member: mb,
                },
            ) => {
                if sa != sb {
                    return Err(UnifyError::SortMismatch {
                        expected_sort: sa.clone(),
                        actual_sort: sb.clone(),
                        position: TypePath::default(),
                    });
                }
                if let Some(sr) = sort_registry {
                    if sr.leq(ma, mb) {
                        Ok(())
                    } else {
                        Err(UnifyError::SortMemberLeq {
                            sort: sa.clone(),
                            expected: ma.clone(),
                            actual: mb.clone(),
                            position: TypePath::default(),
                        })
                    }
                } else if ma == mb {
                    Ok(())
                } else {
                    Err(UnifyError::SortMemberLeq {
                        sort: sa.clone(),
                        expected: ma.clone(),
                        actual: mb.clone(),
                        position: TypePath::default(),
                    })
                }
            }
            (SortArg::Var(id), sup) => var_table
                .bind(*id, sup.clone())
                .map_err(|_| UnifyError::OccursCheck {
                    var: *id,
                    position: TypePath::default(),
                }),
            (sub, SortArg::Var(id)) => var_table
                .bind(*id, sub.clone())
                .map_err(|_| UnifyError::OccursCheck {
                    var: *id,
                    position: TypePath::default(),
                }),
        }
    }

    /// Substitute any bound variables with their concrete values.
    pub fn substitute(&self, var_table: &SortVarTable) -> SortArg {
        match var_table.resolve(self) {
            SortArg::Var(_) => self.clone(), // unbound — keep as-is
            concrete => concrete,
        }
    }
}

// ---------------------------------------------------------------------------
// Sort variable table
// ---------------------------------------------------------------------------

/// Tracks variable bindings during unification. Each `SortVarId` starts
/// unbound; when a variable is unified with a concrete value, the binding
/// is recorded here. Chart cells carry the substituted results.
#[derive(Debug, Clone, Default)]
pub struct SortVarTable {
    /// Mapping from variable ID to its binding (may be concrete or another var).
    bindings: HashMap<SortVarId, SortArg>,
    /// Last sort-level unification error for diagnostics.
    pub last_error: Option<UnifyError>,
}

impl SortVarTable {
    pub fn new() -> Self {
        SortVarTable {
            bindings: HashMap::new(),
            last_error: None,
        }
    }

    /// Follow any chain of var-to-var bindings to find the actual value.
    pub fn resolve(&self, arg: &SortArg) -> SortArg {
        match arg {
            SortArg::Var(id) => match self.bindings.get(id) {
                Some(bound) => {
                    let resolved = self.resolve(bound);
                    if resolved == *bound {
                        resolved
                    } else {
                        resolved
                    }
                }
                None => arg.clone(),
            },
            _ => arg.clone(),
        }
    }

    /// Bind a variable to a value. Returns `Err` on conflict with an existing
    /// binding.
    pub fn bind(&mut self, id: SortVarId, val: SortArg) -> Result<(), String> {
        let resolved = self.resolve(&val);
        // Prevent binding a var directly to itself
        if let SortArg::Var(vid) = &resolved {
            if *vid == id {
                return Ok(());
            }
        }
        match self.bindings.get(&id) {
            Some(existing) if self.resolve(existing) != resolved => Err(format!(
                "sort variable conflict: cannot bind {:?} to {:?}",
                existing, resolved
            )),
            _ => {
                self.bindings.insert(id, resolved);
                Ok(())
            }
        }
    }

    /// Look up what a variable is currently bound to (without resolution).
    pub fn lookup_raw(&self, id: SortVarId) -> Option<&SortArg> {
        self.bindings.get(&id)
    }
}

/// A basic (leaf) type in the Lambek grammar, possibly parameterized by sort
/// arguments: `NP`, `N[Animal]`, `NP[Person, Nominative]`.
///
/// For backward compatibility, `From<String>` produces a 0-arity atom
/// (`args: vec![]`), which means all existing code that constructs
/// `LambekType<String>` can mechanically migrate to `LambekType<AtomType>`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AtomType {
    pub name: String,
    pub args: Vec<SortArg>,
}

impl AtomType {
    pub fn new(name: &str) -> Self {
        AtomType {
            name: name.to_string(),
            args: vec![],
        }
    }

    pub fn with_args(name: &str, args: Vec<SortArg>) -> Self {
        AtomType {
            name: name.to_string(),
            args,
        }
    }

    /// True if this atom and `other` have the same name and arity.
    pub fn structural_eq(&self, other: &AtomType) -> bool {
        self.name == other.name && self.args.len() == other.args.len()
    }
}

impl From<String> for AtomType {
    fn from(s: String) -> Self {
        AtomType {
            name: s,
            args: vec![],
        }
    }
}

impl From<&str> for AtomType {
    fn from(s: &str) -> Self {
        AtomType {
            name: s.to_string(),
            args: vec![],
        }
    }
}

impl std::fmt::Display for AtomType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.args.is_empty() {
            write!(f, "[")?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                match arg {
                    SortArg::Concrete { member, .. } => write!(f, "{member}")?,
                    SortArg::Var(_) => write!(f, "?")?,
                }
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// TypeCheck — sort-aware atom comparisons
// ---------------------------------------------------------------------------

/// Trait for basic types that may carry sort information.
///
/// [`AtomType`] implements the full sort-aware logic (variable unification,
/// per-sort lattice lookup).  Other types (e.g. `u8`, `&str`, test enums)
/// return `None` from `check_atoms`, which causes the fallback to ordinary
/// lattice-based `leq`.
pub trait TypeCheck: Hash + Eq + Clone {
    /// Sort-aware subtype check for two atoms.  Returns `Some(true)` if the
    /// check passes (possibly after binding variables), `Some(false)` if it
    /// definitively fails, or `None` to fall back to `SubtypeLattice::leq`.
    fn check_atoms(
        &self,
        other: &Self,
        sort_registry: Option<&SortRegistry>,
        var_table: &mut SortVarTable,
    ) -> Option<bool>;

    /// Substitute sort variables with their concrete bindings.
    fn substitute_atom(&self, var_table: &SortVarTable) -> Self;

    /// Head name for chart indexing — when `Some`, the chart indexes by this
    /// string rather than the full `T` value, which lets `NP[Person, Nom]`
    /// and `NP[Animate, Nom]` share the same index bucket.
    fn head_name(&self) -> Option<String> {
        None
    }

    /// Human-readable display of this basic type including sort parameters.
    fn basic_display(&self) -> String {
        self.head_name().unwrap_or_else(|| "?".into())
    }
}

impl TypeCheck for AtomType {
    fn check_atoms(
        &self,
        other: &AtomType,
        sort_registry: Option<&SortRegistry>,
        var_table: &mut SortVarTable,
    ) -> Option<bool> {
        // 0-arity atoms: defer to the grammatical lattice (e.g. Person :< Noun).
        if self.args.is_empty() && other.args.is_empty() {
            return None;
        }
        // Parameterized atoms: names must match, then unify args.
        if self.name != other.name {
            return Some(false);
        }
        if self.args.len() != other.args.len() {
            return Some(false);
        }
        for (a, b) in self.args.iter().zip(other.args.iter()) {
            if let Err(e) = a.unify_with(b, sort_registry, var_table) {
                var_table.last_error = Some(e);
                return Some(false);
            }
        }
        Some(true)
    }

// (SortArg errors are captured via SortVarTable::last_error for diagnostics)

    fn substitute_atom(&self, var_table: &SortVarTable) -> Self {
        AtomType {
            name: self.name.clone(),
            args: self.args.iter().map(|a| a.substitute(var_table)).collect(),
        }
    }

    fn head_name(&self) -> Option<String> {
        Some(self.name.clone())
    }

    fn basic_display(&self) -> String {
        // Use the Display impl which shows sort args: "NP[Animate]"
        format!("{self}")
    }
}

/// Macro to generate a trivial `TypeCheck` impl (always defers to lattice).
#[macro_export]
macro_rules! impl_type_check_trivial {
    ($t:ty) => {
        impl $crate::types::TypeCheck for $t {
            fn check_atoms(
                &self,
                _other: &Self,
                _sort_registry: Option<&$crate::sort::SortRegistry>,
                _var_table: &mut $crate::types::SortVarTable,
            ) -> Option<bool> {
                None
            }
            fn substitute_atom(
                &self,
                _var_table: &$crate::types::SortVarTable,
            ) -> Self {
                self.clone()
            }
        }
    };
    ($t:ty, display) => {
        impl $crate::types::TypeCheck for $t {
            fn check_atoms(
                &self,
                _other: &Self,
                _sort_registry: Option<&$crate::sort::SortRegistry>,
                _var_table: &mut $crate::types::SortVarTable,
            ) -> Option<bool> {
                None
            }
            fn substitute_atom(
                &self,
                _var_table: &$crate::types::SortVarTable,
            ) -> Self {
                self.clone()
            }
            fn basic_display(&self) -> String {
                format!("{self}")
            }
        }
    };
}

// Trivial impls for test types used in property tests.
#[cfg(test)]
impl_type_check_trivial!(u8);
#[cfg(test)]
impl_type_check_trivial!(&str);

// ---------------------------------------------------------------------------
// LambekType
// ---------------------------------------------------------------------------

/// Types in the Lambek grammar, parameterized over the set of basic types `T`.
///
/// Includes the standard slashes plus extraction (`↑`), scoping (`⇑`),
/// conjunction (`∧`), and disjunction (`∨`); and an interned `Custom`
/// trapdoor for research-grade syntactic connectives (pregroup adjoints,
/// multimodal slashes, Morrill's wrap, …).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LambekType<T> {
    Basic(T),
    /// `b \ a` — consumes argument of type `a` from the LEFT, produces `b`.
    /// Stored as `RightArrow(a, b)` for historical reasons (matches the
    /// original Haskell encoding).
    RightArrow(Box<LambekType<T>>, Box<LambekType<T>>),
    /// `a / b` — consumes argument of type `b` from the RIGHT, produces `a`.
    /// Stored as `LeftArrow(a, b)`.
    LeftArrow(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Extraction `a ↑ b` — gap-filler binding (wh-movement, relative clauses).
    Extract(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Scoping `b ⇑ a` — quantifier scope via continuation-passing.
    Scoped(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Conjunction `a ∧ b`.
    Conj(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Disjunction `a ∨ b`.
    Disj(Box<LambekType<T>>, Box<LambekType<T>>),
    /// Trapdoor for research-grade syntactic connectives. The `tag` is
    /// registered in the [`crate::registry::Registry`]; subtyping defaults
    /// to invariant in v1 (per-tag variance lookup added when a plugin needs it).
    Custom {
        tag: CustomTag,
        args: Vec<LambekType<T>>,
    },
}

impl<T: Hash + Eq + Clone> LambekType<T> {
    /// Structural subtype check.
    ///
    /// Arrow variance: `LeftArrow` / `RightArrow` / `Extract` / `Scoped` are
    /// contravariant in their first argument and covariant in their second.
    /// `Conj` / `Disj` are covariant in both arguments (standard product /
    /// sum variance). `Custom` defaults to invariant; per-tag variance
    /// lookup is added in a later milestone.
    pub fn leq(&self, other: &Self, lat: &SubtypeLattice<T>) -> bool {
        use LambekType::*;
        match (self, other) {
            (Basic(a), Basic(b)) => lat.leq(a, b),
            (LeftArrow(x1, y1), LeftArrow(x2, y2)) => x2.leq(x1, lat) && y1.leq(y2, lat),
            (RightArrow(x1, y1), RightArrow(x2, y2)) => x2.leq(x1, lat) && y1.leq(y2, lat),
            (Extract(x1, y1), Extract(x2, y2)) => x2.leq(x1, lat) && y1.leq(y2, lat),
            (Scoped(x1, y1), Scoped(x2, y2)) => x2.leq(x1, lat) && y1.leq(y2, lat),
            (Conj(x1, y1), Conj(x2, y2)) => x1.leq(x2, lat) && y1.leq(y2, lat),
            (Disj(x1, y1), Disj(x2, y2)) => x1.leq(x2, lat) && y1.leq(y2, lat),
            (Custom { tag: t1, args: a1 }, Custom { tag: t2, args: a2 }) => {
                t1 == t2
                    && a1.len() == a2.len()
                    && a1
                        .iter()
                        .zip(a2)
                        .all(|(x, y)| x.leq(y, lat) && y.leq(x, lat))
            }
            _ => false,
        }
    }

    /// Convenience: `self.geq(other) == other.leq(self)`.
    pub fn geq(&self, other: &Self, lat: &SubtypeLattice<T>) -> bool {
        other.leq(self, lat)
    }

    /// Recursive depth of the type tree (atoms = 0; functions = 1 + max(child)).
    pub fn depth(&self) -> usize {
        use LambekType::*;
        match self {
            Basic(_) => 0,
            LeftArrow(a, b) | RightArrow(a, b) | Extract(a, b) | Scoped(a, b)
            | Conj(a, b) | Disj(a, b) => 1 + a.depth().max(b.depth()),
            Custom { args, .. } => 1 + args.iter().map(|a| a.depth()).max().unwrap_or(0),
        }
    }

    /// Apply a [`LambekFold`] visitor bottom-up. Each constructor maps to
    /// one trait method; new visitors implement the trait once and work
    /// across every variant.
    pub fn fold<F: LambekFold<T>>(&self, f: &mut F) -> F::Out {
        use LambekType::*;
        match self {
            Basic(t) => f.basic(t),
            LeftArrow(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.left_arrow(av, bv)
            }
            RightArrow(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.right_arrow(av, bv)
            }
            Extract(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.extract(av, bv)
            }
            Scoped(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.scoped(av, bv)
            }
            Conj(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.conj(av, bv)
            }
            Disj(a, b) => {
                let av = a.fold(f);
                let bv = b.fold(f);
                f.disj(av, bv)
            }
            Custom { tag, args } => {
                let av: Vec<F::Out> = args.iter().map(|a| a.fold(f)).collect();
                f.custom(*tag, av)
            }
        }
    }
}

impl<T: TypeCheck> LambekType<T> {
    /// Shape name for error messages.
    fn shape_name(&self) -> &'static str {
        use LambekType::*;
        match self {
            Basic(_) => "a type",
            LeftArrow(_, _) => "function `/`",
            RightArrow(_, _) => "function `\\`",
            Extract(_, _) => "extraction",
            Scoped(_, _) => "scope",
            Conj(_, _) => "conjunction",
            Disj(_, _) => "disjunction",
            Custom { .. } => "custom connective",
        }
    }

    /// Sort-aware subtyping check that unifies variables and returns diagnostic
    /// errors on failure.  Variance: first arg contravariant, second covariant.
    pub fn unify_with(
        &self,
        other: &Self,
        lattice: &SubtypeLattice<T>,
        sort_registry: Option<&SortRegistry>,
        var_table: &mut SortVarTable,
    ) -> Result<(), UnifyError> {
        use LambekType::*;
        match (self, other) {
            (Basic(a), Basic(b)) => match a.check_atoms(b, sort_registry, var_table) {
                Some(true) => Ok(()),
                Some(false) => {
                    // Prefer the sort-level error if available; fall back to name mismatch.
                    if let Some(e) = var_table.last_error.take() {
                        Err(e)
                    } else {
                        let a_name = a.head_name().unwrap_or_else(|| "?".into());
                        let b_name = b.head_name().unwrap_or_else(|| "?".into());
                        Err(UnifyError::NameMismatch {
                            expected: format!("compatible with `{a_name}`"),
                            actual: format!("`{b_name}`"),
                            position: TypePath::default(),
                        })
                    }
                }
                None => {
                    if lattice.leq(a, b) {
                        Ok(())
                    } else {
                        let a_name = a.head_name().unwrap_or_else(|| "?".into());
                        let b_name = b.head_name().unwrap_or_else(|| "?".into());
                        Err(UnifyError::NameMismatch {
                            expected: format!("subtype of `{a_name}`"),
                            actual: format!("`{b_name}`"),
                            position: TypePath::default(),
                        })
                    }
                }
            },
            (LeftArrow(x1, y1), LeftArrow(x2, y2)) => {
                x2.unify_with(x1, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::LeftArrowArg)))?;
                y1.unify_with(y2, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::LeftArrowResult)))
            }
            (RightArrow(x1, y1), RightArrow(x2, y2)) => {
                x2.unify_with(x1, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::RightArrowArg)))?;
                y1.unify_with(y2, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::RightArrowResult)))
            }
            (Extract(x1, y1), Extract(x2, y2)) => {
                x2.unify_with(x1, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::ExtractArg)))?;
                y1.unify_with(y2, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::ExtractResult)))
            }
            (Scoped(x1, y1), Scoped(x2, y2)) => {
                x2.unify_with(x1, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::ScopedArg)))?;
                y1.unify_with(y2, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::ScopedResult)))
            }
            (Conj(x1, y1), Conj(x2, y2)) => {
                x1.unify_with(x2, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::ConjLeft)))?;
                y1.unify_with(y2, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::ConjRight)))
            }
            (Disj(x1, y1), Disj(x2, y2)) => {
                x1.unify_with(x2, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::DisjLeft)))?;
                y1.unify_with(y2, lattice, sort_registry, var_table)
                    .map_err(|e| e.with_position(|p| p.push(TypePathStep::DisjRight)))
            }
            (Custom { tag: t1, args: a1 }, Custom { tag: t2, args: a2 })
                if t1 == t2 && a1.len() == a2.len() =>
            {
                for (i, (x, y)) in a1.iter().zip(a2).enumerate() {
                    x.unify_with(y, lattice, sort_registry, var_table)
                        .map_err(|e| e.with_position(|p| p.push(TypePathStep::AtomArg(i))))?;
                }
                Ok(())
            }
            (a, b) => Err(UnifyError::StructureMismatch {
                expected_shape: a.shape_name().to_string(),
                actual_shape: b.shape_name().to_string(),
                position: TypePath::default(),
            }),
        }
    }

    /// Substitute sort-variable bindings through this type tree.
    pub fn substitute_vars(&self, var_table: &SortVarTable) -> Self {
        use LambekType::*;
        match self {
            Basic(a) => Basic(a.substitute_atom(var_table)),
            LeftArrow(a, b) => LeftArrow(
                Box::new(a.substitute_vars(var_table)),
                Box::new(b.substitute_vars(var_table)),
            ),
            RightArrow(a, b) => RightArrow(
                Box::new(a.substitute_vars(var_table)),
                Box::new(b.substitute_vars(var_table)),
            ),
            Extract(a, b) => Extract(
                Box::new(a.substitute_vars(var_table)),
                Box::new(b.substitute_vars(var_table)),
            ),
            Scoped(a, b) => Scoped(
                Box::new(a.substitute_vars(var_table)),
                Box::new(b.substitute_vars(var_table)),
            ),
            Conj(a, b) => Conj(
                Box::new(a.substitute_vars(var_table)),
                Box::new(b.substitute_vars(var_table)),
            ),
            Disj(a, b) => Disj(
                Box::new(a.substitute_vars(var_table)),
                Box::new(b.substitute_vars(var_table)),
            ),
            Custom { tag, args } => Custom {
                tag: *tag,
                args: args.iter().map(|a| a.substitute_vars(var_table)).collect(),
            },
        }
    }
}

impl LambekType<AtomType> {
    /// Assign fresh [`SortVarId`]s to every sort variable in this type tree.
    ///
    /// Within a single type, the same old ID maps to the same new ID (so
    /// co-occurrences like `N[a]` and `NP[a]` in one entry stay linked).
    /// The caller passes a `next_id` counter shared across the entire chart
    /// fill so that different lexical tokens get disjoint ID ranges.
    pub fn freshen_vars(&self, next_id: &mut u32) -> Self {
        let mut mapping: HashMap<SortVarId, SortVarId> = HashMap::new();
        self.freshen_with(&mut mapping, next_id)
    }

    fn freshen_with(
        &self,
        mapping: &mut HashMap<SortVarId, SortVarId>,
        next_id: &mut u32,
    ) -> Self {
        use LambekType::*;
        match self {
            Basic(a) => Basic(AtomType {
                name: a.name.clone(),
                args: a
                    .args
                    .iter()
                    .map(|arg| match arg {
                        SortArg::Var(id) => {
                            let fresh = *mapping.entry(*id).or_insert_with(|| {
                                let new_id = SortVarId(*next_id);
                                *next_id += 1;
                                new_id
                            });
                            SortArg::Var(fresh)
                        }
                        other => other.clone(),
                    })
                    .collect(),
            }),
            LeftArrow(x, y) => LeftArrow(
                Box::new(x.freshen_with(mapping, next_id)),
                Box::new(y.freshen_with(mapping, next_id)),
            ),
            RightArrow(x, y) => RightArrow(
                Box::new(x.freshen_with(mapping, next_id)),
                Box::new(y.freshen_with(mapping, next_id)),
            ),
            Extract(x, y) => Extract(
                Box::new(x.freshen_with(mapping, next_id)),
                Box::new(y.freshen_with(mapping, next_id)),
            ),
            Scoped(x, y) => Scoped(
                Box::new(x.freshen_with(mapping, next_id)),
                Box::new(y.freshen_with(mapping, next_id)),
            ),
            Conj(x, y) => Conj(
                Box::new(x.freshen_with(mapping, next_id)),
                Box::new(y.freshen_with(mapping, next_id)),
            ),
            Disj(x, y) => Disj(
                Box::new(x.freshen_with(mapping, next_id)),
                Box::new(y.freshen_with(mapping, next_id)),
            ),
            Custom { tag, args } => Custom {
                tag: *tag,
                args: args
                    .iter()
                    .map(|a| a.freshen_with(mapping, next_id))
                    .collect(),
            },
        }
    }
}

// ---------------------------------------------------------------------------
// LambekFold
// ---------------------------------------------------------------------------

/// Visitor for `LambekType` — one method per variant.
///
/// Interpreters (display, IR lowering, autocomplete) implement this trait once
/// and work uniformly across every variant. New `Custom` connectives integrate
/// via the `custom` method using their interned `CustomTag`.
pub trait LambekFold<T> {
    type Out;
    fn basic(&mut self, t: &T) -> Self::Out;
    fn left_arrow(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn right_arrow(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn extract(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn scoped(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn conj(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn disj(&mut self, a: Self::Out, b: Self::Out) -> Self::Out;
    fn custom(&mut self, tag: CustomTag, args: Vec<Self::Out>) -> Self::Out;
}

// ---------------------------------------------------------------------------
// Term
// ---------------------------------------------------------------------------

/// Terms in the Lambek calculus.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term<A> {
    Atom(A),
    Var(String),
    Lambda(String, Box<Term<A>>),
    App(Box<Term<A>>, Vec<Term<A>>),
}

impl<A: Clone> Term<A> {
    /// Returns true if this term is an `App` (i.e., partially applied).
    pub fn is_partial_pred(&self) -> bool {
        matches!(self, Term::App(_, _))
    }

    /// If `self` is `App(f, args)`, push `arg` onto args.
    /// Otherwise wrap in a fresh `App(self, [arg])`.
    pub fn apply_partial(self, arg: Term<A>) -> Term<A> {
        match self {
            Term::App(f, mut args) => {
                args.push(arg);
                Term::App(f, args)
            }
            other => Term::App(Box::new(other), vec![arg]),
        }
    }
}

impl<A: std::fmt::Display> std::fmt::Display for Term<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Atom(a) => write!(f, "{}", a),
            Term::Var(s) => write!(f, "{}", s),
            Term::Lambda(s, body) => write!(f, "λ{}.{}", s, body),
            Term::App(func, args) => {
                write!(f, "{}(", func)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", a)?;
                }
                write!(f, ")")
            }
        }
    }
}

// ---------------------------------------------------------------------------
// AnnotatedTerm
// ---------------------------------------------------------------------------

/// A term paired with its Lambek type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnotatedTerm<A, T> {
    pub term: Term<A>,
    pub ty: LambekType<T>,
}

/// Non-deterministic result: all possible values of a parse.
pub type NonDet<A> = Vec<A>;

// ---------------------------------------------------------------------------
// Property tests for LambekType subtyping
// ---------------------------------------------------------------------------

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    /// Recursive generator for `LambekType<u8>` with bounded depth.
    ///
    /// Doesn't generate `Custom` (would require a CustomTag which lives in
    /// the registry and we want to keep these props lattice-agnostic).
    fn arb_lambek_type() -> impl Strategy<Value = LambekType<u8>> {
        let leaf = any::<u8>().prop_map(LambekType::Basic);
        leaf.prop_recursive(
            4,  // depth
            32, // nodes total
            4,  // branches per recursion
            |inner| {
                let pair = (inner.clone(), inner);
                prop_oneof![
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::LeftArrow(Box::new(a), Box::new(b))),
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::RightArrow(Box::new(a), Box::new(b))),
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::Extract(Box::new(a), Box::new(b))),
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::Scoped(Box::new(a), Box::new(b))),
                    pair.clone()
                        .prop_map(|(a, b)| LambekType::Conj(Box::new(a), Box::new(b))),
                    pair.prop_map(|(a, b)| LambekType::Disj(Box::new(a), Box::new(b))),
                ]
            },
        )
    }

    proptest! {
        // With an empty lattice, basic-type subtyping is pure equality.
        // Structural laws should still hold for the recursive variants.

        #[test]
        fn leq_reflexive(t in arb_lambek_type()) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            prop_assert!(t.leq(&t, &lat));
        }

        #[test]
        fn leq_transitive(a in arb_lambek_type(), b in arb_lambek_type(), c in arb_lambek_type()) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            if a.leq(&b, &lat) && b.leq(&c, &lat) {
                prop_assert!(a.leq(&c, &lat));
            }
        }

        #[test]
        fn leq_antisymmetric_on_empty_lattice(a in arb_lambek_type(), b in arb_lambek_type()) {
            // With an empty lattice, leq collapses to structural equality on
            // basic types — which forces full structural equality on the
            // whole type. So mutual `leq` ⇒ `a == b`.
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            if a.leq(&b, &lat) && b.leq(&a, &lat) {
                prop_assert_eq!(a, b);
            }
        }

        #[test]
        fn left_arrow_first_arg_contravariant(
            x1 in arb_lambek_type(),
            x2 in arb_lambek_type(),
            y in arb_lambek_type(),
        ) {
            // Build a lattice where x2 :< x1 by adding the structural-equality
            // edge through a fresh basic-type pair.
            //
            // We can't easily add subtyping between arbitrary LambekTypes —
            // the lattice is over basic types only. So we test the structural
            // recursion: if x2.leq(x1) holds (by reflexivity when x1 == x2),
            // then LeftArrow(x1, y).leq(LeftArrow(x2, y)) must hold.
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            if x2.leq(&x1, &lat) {
                let big = LambekType::LeftArrow(Box::new(x1.clone()), Box::new(y.clone()));
                let small = LambekType::LeftArrow(Box::new(x2.clone()), Box::new(y.clone()));
                prop_assert!(big.leq(&small, &lat));
            }
        }

        #[test]
        fn arrow_second_arg_covariant(
            x in arb_lambek_type(),
            y1 in arb_lambek_type(),
            y2 in arb_lambek_type(),
        ) {
            let lat: SubtypeLattice<u8> = SubtypeLattice::new();
            if y1.leq(&y2, &lat) {
                let a = LambekType::LeftArrow(Box::new(x.clone()), Box::new(y1.clone()));
                let b = LambekType::LeftArrow(Box::new(x.clone()), Box::new(y2.clone()));
                prop_assert!(a.leq(&b, &lat));
            }
        }

        #[test]
        fn basic_subtyping_threads_through_arrows(
            sub in any::<u8>(), sup in any::<u8>(),
            y in arb_lambek_type(),
        ) {
            // If sub :< sup at the basic level, the arrow with sup in the
            // arg position should be ≤ the arrow with sub there (contra).
            prop_assume!(sub != sup);
            let mut lat: SubtypeLattice<u8> = SubtypeLattice::new();
            lat.add_subtype(sub, sup);
            let big = LambekType::LeftArrow(
                Box::new(LambekType::Basic(sup)),
                Box::new(y.clone()),
            );
            let small = LambekType::LeftArrow(
                Box::new(LambekType::Basic(sub)),
                Box::new(y.clone()),
            );
            // small.first = sub, big.first = sup; sub ≤ sup, so big.leq(small).
            prop_assert!(big.leq(&small, &lat));
        }
    }
}
