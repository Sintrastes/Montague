//! Canonical semantic IR (`SemTerm`) and canonical passes.
//!
//! `SemTerm` is the logic-agnostic intermediate representation between a
//! completed syntactic parse (annotated with `LambekType`) and a target
//! backend (Prolog, miniKanren, Cozo, S-expression, ...). The enum has six
//! structural primitives; infinitely many operators via `Op { op: OpId }`
//! and `Binder { op: OpId }` carrying logic-specific `OpId`s (see D0/D2).
//!
//! Canonical passes (α-rename, β-reduce) live here because every downstream
//! consumer benefits from them. Logic-specific passes (prenex, pragmatic
//! enrichment) live in their respective profile crates.

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

use crate::registry::{ConstId, OpId, PredId};

// ---------------------------------------------------------------------------
// VarId
// ---------------------------------------------------------------------------

/// Interned variable identifier.
///
/// Backed by a u32 into a per-pass `VarEnv` that tracks α-equivalence and
/// freshness. Two `VarId`s are equal iff they refer to the same environment
/// slot; α-equivalence checks equate terms even when `VarId`s differ
/// (see [`alpha_eq`]).
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct VarId(pub u32);

/// Per-pass variable environment — maps VarId → name, tracks freshness.
#[derive(Default, Debug, Clone)]
pub struct VarEnv {
    names: Vec<String>,
}

impl VarEnv {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a named variable, returning its id. Returns the existing id if
    /// the name is already bound.
    pub fn named(&mut self, name: &str) -> VarId {
        if let Some(pos) = self.names.iter().position(|n| n == name) {
            return VarId(pos as u32);
        }
        let id = VarId(self.names.len() as u32);
        self.names.push(name.to_string());
        id
    }

    /// Create a fresh variable guaranteed not to collide with any existing
    /// name in the environment.
    pub fn fresh(&mut self, prefix: &str) -> VarId {
        let mut i = 0u32;
        loop {
            let cand = format!("{prefix}{i}");
            if !self.names.contains(&cand) {
                return self.named(&cand);
            }
            i += 1;
        }
    }

    /// Look up the display name of a variable.
    pub fn name_of(&self, id: VarId) -> &str {
        &self.names[id.0 as usize]
    }

    /// Number of variables allocated so far (used as a freshness bound).
    pub fn len(&self) -> usize {
        self.names.len()
    }

    /// True if no variables have been allocated.
    pub fn is_empty(&self) -> bool {
        self.names.is_empty()
    }
}

// ---------------------------------------------------------------------------
// SemTerm
// ---------------------------------------------------------------------------

/// Canonical semantic IR — the "what did this sentence mean" representation.
///
/// Logic-agnostic core: six structural primitives (variable, constant,
/// predicate application, λ-abstraction, function application, equation) plus
/// two operator slots (`Op`/`Binder`) parameterized by logic-specific `OpId`s.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemTerm {
    /// Variable reference.
    Var(VarId),
    /// Individual constant (`socrates`, `berlin`, ...).
    Const(ConstId),
    /// Predicate application: `pred(arg1, arg2, ...)`.
    Pred { head: PredId, args: Vec<SemTerm> },
    /// λ-abstraction.
    Lambda { binder: VarId, body: Box<SemTerm> },
    /// Function application `fun(arg1, arg2, ...)`.
    App {
        fun: Box<SemTerm>,
        args: Vec<SemTerm>,
    },
    /// Equality `lhs = rhs`.
    Eq(Box<SemTerm>, Box<SemTerm>),

    /// Non-binding operator (`∧`, `∨`, `¬`, `→`, `G`, `F`, `⊗`, ...).
    Op { op: OpId, args: Vec<SemTerm> },

    /// Variable-binding operator (`∀`, `∃`, `ι`, ...) with optional restrictor
    /// (generalized-quantifier style: `∀x: φ. ψ`).
    Binder {
        op: OpId,
        binder: VarId,
        restr: Option<Box<SemTerm>>,
        body: Box<SemTerm>,
    },
}

impl SemTerm {
    /// Convenience constructor for binary `Op` (e.g. `and`, `implies`).
    pub fn bin_op(op: OpId, a: SemTerm, b: SemTerm) -> Self {
        SemTerm::Op {
            op,
            args: vec![a, b],
        }
    }

    /// Convenience constructor for unary `Op` (e.g. `not`, `G`).
    pub fn un_op(op: OpId, a: SemTerm) -> Self {
        SemTerm::Op { op, args: vec![a] }
    }

    /// Convenience constructor for a `Binder` without restrictor.
    pub fn forall_like(op: OpId, binder: VarId, body: SemTerm) -> Self {
        SemTerm::Binder {
            op,
            binder,
            restr: None,
            body: Box::new(body),
        }
    }

    /// Convenience constructor for a generalized-quantifier `Binder`.
    pub fn quant(op: OpId, binder: VarId, restr: SemTerm, body: SemTerm) -> Self {
        SemTerm::Binder {
            op,
            binder,
            restr: Some(Box::new(restr)),
            body: Box::new(body),
        }
    }
}

impl fmt::Display for SemTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemTerm::Var(v) => write!(f, "v{}", v.0),
            SemTerm::Const(c) => write!(f, "c{}", c.qname().0),
            SemTerm::Pred { head, args } => {
                write!(f, "p{}(", head.qname().0)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{a}")?;
                }
                write!(f, ")")
            }
            SemTerm::Lambda { binder, body } => write!(f, "λv{}.({body})", binder.0),
            SemTerm::App { fun, args } => {
                write!(f, "({fun}")?;
                for a in args {
                    write!(f, " {a}")?;
                }
                write!(f, ")")
            }
            SemTerm::Eq(a, b) => write!(f, "({a} = {b})"),
            SemTerm::Op { op, args } => {
                write!(f, "op{}(", op.qname().0)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{a}")?;
                }
                write!(f, ")")
            }
            SemTerm::Binder {
                op,
                binder,
                restr,
                body,
            } => {
                write!(f, "b{} v{}.", op.qname().0, binder.0)?;
                if let Some(r) = restr {
                    write!(f, " ({r})")?;
                }
                write!(f, " ({body})")
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Canonical passes
// ---------------------------------------------------------------------------

/// Compute the free variables of a term.
pub fn free_vars(term: &SemTerm) -> HashSet<VarId> {
    let mut fv = HashSet::new();
    free_vars_into(term, &mut fv);
    fv
}

fn free_vars_into(term: &SemTerm, fv: &mut HashSet<VarId>) {
    match term {
        SemTerm::Var(v) => {
            fv.insert(*v);
        }
        SemTerm::Const(_) => {}
        SemTerm::Pred { args, .. } => {
            for a in args {
                free_vars_into(a, fv);
            }
        }
        SemTerm::Lambda { binder, body } => {
            free_vars_into(body, fv);
            fv.remove(binder);
        }
        SemTerm::App { fun, args } => {
            free_vars_into(fun, fv);
            for a in args {
                free_vars_into(a, fv);
            }
        }
        SemTerm::Eq(a, b) => {
            free_vars_into(a, fv);
            free_vars_into(b, fv);
        }
        SemTerm::Op { args, .. } => {
            for a in args {
                free_vars_into(a, fv);
            }
        }
        SemTerm::Binder {
            binder,
            restr,
            body,
            ..
        } => {
            if let Some(r) = restr {
                free_vars_into(r, fv);
            }
            free_vars_into(body, fv);
            fv.remove(binder);
        }
    }
}

/// α-rename the variable `from` to `to` throughout `term`.
pub fn alpha_rename(term: &SemTerm, from: VarId, to: VarId) -> SemTerm {
    if from == to {
        return term.clone();
    }
    alpha_rename_impl(term, from, to, &mut HashSet::new())
}

fn alpha_rename_impl(
    term: &SemTerm,
    from: VarId,
    to: VarId,
    shadowed: &mut HashSet<VarId>,
) -> SemTerm {
    match term {
        SemTerm::Var(v) if *v == from && !shadowed.contains(v) => SemTerm::Var(to),
        SemTerm::Var(_) | SemTerm::Const(_) => term.clone(),
        SemTerm::Pred { head, args } => SemTerm::Pred {
            head: *head,
            args: args
                .iter()
                .map(|a| alpha_rename_impl(a, from, to, shadowed))
                .collect(),
        },
        SemTerm::Lambda { binder, body } => {
            let new_body: Box<SemTerm> = if *binder == from || *binder == to {
                body.clone()
            } else {
                Box::new(alpha_rename_impl(body, from, to, shadowed))
            };
            SemTerm::Lambda {
                binder: *binder,
                body: new_body,
            }
        }
        SemTerm::App { fun, args } => SemTerm::App {
            fun: Box::new(alpha_rename_impl(fun, from, to, shadowed)),
            args: args
                .iter()
                .map(|a| alpha_rename_impl(a, from, to, shadowed))
                .collect(),
        },
        SemTerm::Eq(a, b) => SemTerm::Eq(
            Box::new(alpha_rename_impl(a, from, to, shadowed)),
            Box::new(alpha_rename_impl(b, from, to, shadowed)),
        ),
        SemTerm::Op { op, args } => SemTerm::Op {
            op: *op,
            args: args
                .iter()
                .map(|a| alpha_rename_impl(a, from, to, shadowed))
                .collect(),
        },
        SemTerm::Binder {
            op,
            binder,
            restr,
            body,
        } => {
            let new_body: Box<SemTerm> = if *binder == from || *binder == to {
                body.clone()
            } else {
                Box::new(alpha_rename_impl(body, from, to, shadowed))
            };
            let new_restr: Option<Box<SemTerm>> = restr.as_ref().map(|r| {
                if *binder == from || *binder == to {
                    r.clone()
                } else {
                    Box::new(alpha_rename_impl(r, from, to, shadowed))
                }
            });
            SemTerm::Binder {
                op: *op,
                binder: *binder,
                restr: new_restr,
                body: new_body,
            }
        }
    }
}

/// α-equivalence check (terms are equal up to bound-variable renaming).
pub fn alpha_eq(t1: &SemTerm, t2: &SemTerm) -> bool {
    let mut renaming = HashMap::new();
    let mut reverse = HashMap::new();
    alpha_eq_impl(t1, t2, &mut renaming, &mut reverse)
}

fn alpha_eq_impl(
    t1: &SemTerm,
    t2: &SemTerm,
    renaming: &mut HashMap<VarId, VarId>,
    reverse: &mut HashMap<VarId, VarId>,
) -> bool {
    match (t1, t2) {
        (SemTerm::Var(v1), SemTerm::Var(v2)) => {
            // If there's a mapping for v1, it must be v2; if not, add one.
            match (renaming.get(v1), reverse.get(v2)) {
                (Some(&mapped), _) => mapped == *v2,
                (None, Some(_)) => false,
                (None, None) => {
                    renaming.insert(*v1, *v2);
                    reverse.insert(*v2, *v1);
                    true
                }
            }
        }
        (SemTerm::Const(c1), SemTerm::Const(c2)) => c1 == c2,
        (SemTerm::Pred { head: h1, args: a1 }, SemTerm::Pred { head: h2, args: a2 }) => {
            h1 == h2
                && a1.len() == a2.len()
                && a1
                    .iter()
                    .zip(a2)
                    .all(|(x, y)| alpha_eq_impl(x, y, renaming, reverse))
        }
        (
            SemTerm::Lambda {
                binder: b1,
                body: t1,
            },
            SemTerm::Lambda {
                binder: b2,
                body: t2,
            },
        ) => {
            let saved_renaming = renaming.clone();
            let saved_reverse = reverse.clone();
            renaming.insert(*b1, *b2);
            reverse.insert(*b2, *b1);
            let ok = alpha_eq_impl(t1, t2, renaming, reverse);
            *renaming = saved_renaming;
            *reverse = saved_reverse;
            ok
        }
        (SemTerm::App { fun: f1, args: a1 }, SemTerm::App { fun: f2, args: a2 }) => {
            alpha_eq_impl(f1, f2, renaming, reverse)
                && a1.len() == a2.len()
                && a1
                    .iter()
                    .zip(a2)
                    .all(|(x, y)| alpha_eq_impl(x, y, renaming, reverse))
        }
        (SemTerm::Eq(a1, b1), SemTerm::Eq(a2, b2)) => {
            alpha_eq_impl(a1, a2, renaming, reverse) && alpha_eq_impl(b1, b2, renaming, reverse)
        }
        (SemTerm::Op { op: o1, args: a1 }, SemTerm::Op { op: o2, args: a2 }) => {
            o1 == o2
                && a1.len() == a2.len()
                && a1
                    .iter()
                    .zip(a2)
                    .all(|(x, y)| alpha_eq_impl(x, y, renaming, reverse))
        }
        (
            SemTerm::Binder {
                op: o1,
                binder: b1,
                restr: r1,
                body: t1,
            },
            SemTerm::Binder {
                op: o2,
                binder: b2,
                restr: r2,
                body: t2,
            },
        ) => {
            if o1 != o2 {
                return false;
            }
            let saved_renaming = renaming.clone();
            let saved_reverse = reverse.clone();
            renaming.insert(*b1, *b2);
            reverse.insert(*b2, *b1);
            let ok = match (r1, r2) {
                (None, None) => true,
                (Some(r1), Some(r2)) => alpha_eq_impl(r1, r2, renaming, reverse),
                _ => false,
            };
            let body_ok = ok && alpha_eq_impl(t1, t2, renaming, reverse);
            *renaming = saved_renaming;
            *reverse = saved_reverse;
            body_ok
        }
        _ => false,
    }
}

/// Perform a single β-reduction step (capture-avoiding).
///
/// If the term is an `App` whose `fun` is a `Lambda`, substitute the first
/// argument for the binder throughout the body. Return the reduced term,
/// or `None` if no β-redex was found at the top level.
pub fn beta_step(term: &SemTerm) -> Option<SemTerm> {
    match term {
        SemTerm::App { fun, args } => match fun.as_ref() {
            SemTerm::Lambda { binder, body } if !args.is_empty() => {
                Some(substitute(body, *binder, &args[0]))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Normalize to β-normal form (recursively reduce all β-redexes).
pub fn beta_normalize(term: &SemTerm) -> SemTerm {
    let mut current = term.clone();
    while let Some(next) = beta_step_normalize(&current) {
        current = next;
    }
    current
}

/// Like `beta_step`, but recurses into the term tree to find any redex.
fn beta_step_normalize(term: &SemTerm) -> Option<SemTerm> {
    if let Some(reduced) = beta_step(term) {
        return Some(beta_normalize(&reduced));
    }

    match term {
        SemTerm::App { fun, args } => {
            if let Some(f) = beta_step_normalize(fun) {
                return Some(SemTerm::App {
                    fun: Box::new(f),
                    args: args.clone(),
                });
            }
            for (i, a) in args.iter().enumerate() {
                if let Some(a2) = beta_step_normalize(a) {
                    let mut args2 = args.clone();
                    args2[i] = a2;
                    return Some(SemTerm::App {
                        fun: fun.clone(),
                        args: args2,
                    });
                }
            }
            None
        }
        SemTerm::Lambda { binder, body } => beta_step_normalize(body).map(|b| SemTerm::Lambda {
            binder: *binder,
            body: Box::new(b),
        }),
        SemTerm::Pred { head, args } => {
            for (i, a) in args.iter().enumerate() {
                if let Some(a2) = beta_step_normalize(a) {
                    let mut args2 = args.clone();
                    args2[i] = a2;
                    return Some(SemTerm::Pred {
                        head: *head,
                        args: args2,
                    });
                }
            }
            None
        }
        SemTerm::Eq(a, b) => {
            if let Some(a2) = beta_step_normalize(a) {
                return Some(SemTerm::Eq(Box::new(a2), b.clone()));
            }
            beta_step_normalize(b).map(|b2| SemTerm::Eq(a.clone(), Box::new(b2)))
        }
        SemTerm::Op { op, args } => {
            for (i, a) in args.iter().enumerate() {
                if let Some(a2) = beta_step_normalize(a) {
                    let mut args2 = args.clone();
                    args2[i] = a2;
                    return Some(SemTerm::Op {
                        op: *op,
                        args: args2,
                    });
                }
            }
            None
        }
        SemTerm::Binder {
            op,
            binder,
            restr,
            body,
        } => {
            if let Some(r) = restr {
                if let Some(r2) = beta_step_normalize(r) {
                    return Some(SemTerm::Binder {
                        op: *op,
                        binder: *binder,
                        restr: Some(Box::new(r2)),
                        body: body.clone(),
                    });
                }
            }
            beta_step_normalize(body).map(|b| SemTerm::Binder {
                op: *op,
                binder: *binder,
                restr: restr.clone(),
                body: Box::new(b),
            })
        }
        SemTerm::Var(_) | SemTerm::Const(_) => None,
    }
}

/// Capture-avoiding substitution: replace all free occurrences of `var` in
/// `term` with `value`.
pub fn substitute(term: &SemTerm, var: VarId, value: &SemTerm) -> SemTerm {
    match term {
        SemTerm::Var(v) if *v == var => value.clone(),
        SemTerm::Var(_) | SemTerm::Const(_) => term.clone(),
        SemTerm::Pred { head, args } => SemTerm::Pred {
            head: *head,
            args: args.iter().map(|a| substitute(a, var, value)).collect(),
        },
        SemTerm::Lambda { binder, body } => {
            if *binder == var {
                // Shadowed — no substitution into body.
                return term.clone();
            }
            // If value has a free var that would be captured, α-rename the binder.
            let value_fv = free_vars(value);
            if value_fv.contains(binder) {
                let fresh = VarId(binder.0 + 1);
                let body2 = alpha_rename(body, *binder, fresh);
                return SemTerm::Lambda {
                    binder: fresh,
                    body: Box::new(substitute(&body2, var, value)),
                };
            }
            SemTerm::Lambda {
                binder: *binder,
                body: Box::new(substitute(body, var, value)),
            }
        }
        SemTerm::App { fun, args } => SemTerm::App {
            fun: Box::new(substitute(fun, var, value)),
            args: args.iter().map(|a| substitute(a, var, value)).collect(),
        },
        SemTerm::Eq(a, b) => SemTerm::Eq(
            Box::new(substitute(a, var, value)),
            Box::new(substitute(b, var, value)),
        ),
        SemTerm::Op { op, args } => SemTerm::Op {
            op: *op,
            args: args.iter().map(|a| substitute(a, var, value)).collect(),
        },
        SemTerm::Binder {
            op,
            binder,
            restr,
            body,
        } => {
            if *binder == var {
                return term.clone();
            }
            let value_fv = free_vars(value);
            let (binder2, body2) = if value_fv.contains(binder) {
                let fresh = VarId(binder.0 + 1);
                (fresh, alpha_rename(body, *binder, fresh))
            } else {
                (*binder, body.as_ref().clone())
            };
            let restr2 = restr.as_ref().map(|r| {
                if value_fv.contains(binder) {
                    let fresh = VarId(binder.0 + 1);
                    alpha_rename(r, *binder, fresh)
                } else {
                    substitute(r, var, value)
                }
            });
            SemTerm::Binder {
                op: *op,
                binder: binder2,
                restr: restr2.map(Box::new),
                body: Box::new(substitute(&body2, var, value)),
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Passes
// ---------------------------------------------------------------------------

/// Context threaded through a [`Pass`] execution.
///
/// The registry is borrowed rather than owned to avoid `Clone` requirements.
pub struct PassCtx<'a> {
    /// Registry for operator/constant/predicate lookups.
    pub registry: Option<&'a crate::registry::Registry>,
}

impl<'a> PassCtx<'a> {
    pub fn empty() -> Self {
        Self { registry: None }
    }
}

/// A semantic pass transforming `SemTerm → Vec<SemTerm>`.
///
/// Zero results = the pass rejects this term (e.g., type mismatch).
/// More than one = the pass introduces ambiguity (e.g., pragmatic readings).
pub trait Pass<'a> {
    fn name(&self) -> &str;
    fn run(&self, term: SemTerm, ctx: &PassCtx<'a>) -> Vec<SemTerm>;
}

/// Built-in β-normalization pass.
pub struct BetaNormalize;

impl<'a> Pass<'a> for BetaNormalize {
    fn name(&self) -> &str {
        "beta-normalize"
    }
    fn run(&self, term: SemTerm, _ctx: &PassCtx<'a>) -> Vec<SemTerm> {
        vec![beta_normalize(&term)]
    }
}

// ---------------------------------------------------------------------------
// Backend
// ---------------------------------------------------------------------------

/// Context threaded to a [`Backend::lower`] call.
///
/// The registry is borrowed rather than owned to avoid `Clone` requirements.
pub struct BackendCtx<'a> {
    /// Registry for resolving operator/constant/predicate display names.
    pub registry: Option<&'a crate::registry::Registry>,
}

impl<'a> BackendCtx<'a> {
    pub fn empty() -> Self {
        Self { registry: None }
    }
}

/// Set of supported operators.
#[derive(Debug, Clone, Default)]
pub struct OpSet {
    ops: HashSet<OpId>,
}

impl OpSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with(mut self, op: OpId) -> Self {
        self.ops.insert(op);
        self
    }

    pub fn contains(&self, op: &OpId) -> bool {
        self.ops.contains(op)
    }

    pub fn extend(&mut self, ops: impl IntoIterator<Item = OpId>) {
        self.ops.extend(ops);
    }
}

/// A target backend: lowers a [`SemTerm`] to a target-specific representation.
pub trait Backend {
    type Output;
    type Error;

    /// Declared operator coverage. Backends that encounter an `Op`/`Binder`
    /// whose `OpId` is not in this set produce a structured error.
    fn supported_ops(&self) -> &OpSet;

    /// Lower a `SemTerm` to this backend's output.
    fn lower(&self, term: &SemTerm, ctx: &BackendCtx) -> Result<Self::Output, Self::Error>;
}

// ---------------------------------------------------------------------------
// DirectInterp
// ---------------------------------------------------------------------------

/// Zero-IR escape hatch: interpret a typed annotated term directly without
/// going through `SemTerm`. For debug printers, experimental fused
/// parse-and-interp, and backends that don't benefit from the IR.
pub trait DirectInterp<A, T> {
    type Out;
    fn interp(&self, atree: &crate::types::AnnotatedTerm<A, T>) -> Self::Out;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::Registry;

    fn make_reg() -> Registry {
        let mut reg = Registry::empty();
        let _ns = reg
            .namespace()
            .canonical("test")
            .owner(crate::owner_info!())
            .create()
            .unwrap();
        reg
    }

    fn get_test_ns(reg: &Registry) -> NamespaceId {
        match reg.resolve_namespace("test") {
            NamespaceResolution::Found(id) => id,
            _ => panic!("expected Found for \"test\""),
        }
    }

    fn test_op(reg: &mut Registry, name: &'static str) -> OpId {
        let ns = get_test_ns(reg);
        reg.op(ns, name).arity_fixed(2).register().unwrap()
    }

    fn test_const(reg: &mut Registry, name: &'static str) -> ConstId {
        let ns = get_test_ns(reg);
        reg.const_(ns, name).register().unwrap()
    }

    fn test_pred(reg: &mut Registry, name: &'static str) -> PredId {
        let ns = get_test_ns(reg);
        reg.pred(ns, name).arity_fixed(2).register().unwrap()
    }

    use crate::registry::{NamespaceId, NamespaceResolution};

    #[test]
    fn free_vars_basic() {
        let _env = VarEnv::new();
        let x = VarId(0);
        let y = VarId(1);
        // λx. (x = y)  →  fv = {y}
        let term = SemTerm::Lambda {
            binder: x,
            body: Box::new(SemTerm::Eq(
                Box::new(SemTerm::Var(x)),
                Box::new(SemTerm::Var(y)),
            )),
        };
        let fv = free_vars(&term);
        assert!(!fv.contains(&x));
        assert!(fv.contains(&y));
    }

    #[test]
    fn alpha_rename_simple() {
        let mut reg = make_reg();
        let c = test_const(&mut reg, "c");
        let x = VarId(0);
        let y = VarId(1);
        // λx. Const(c) applied to x
        let term = SemTerm::App {
            fun: Box::new(SemTerm::Lambda {
                binder: x,
                body: Box::new(SemTerm::Const(c)),
            }),
            args: vec![SemTerm::Var(x)],
        };
        let renamed = alpha_rename(&term, x, y);
        // Binder x should still be x (bound); the argument x should become y.
        match &renamed {
            SemTerm::App { fun, args } => {
                match fun.as_ref() {
                    SemTerm::Lambda { binder, .. } => assert_eq!(*binder, x),
                    _ => panic!("expected Lambda"),
                }
                assert_eq!(args[0], SemTerm::Var(y));
            }
            _ => panic!("expected App"),
        }
    }

    #[test]
    fn beta_step_reduces() {
        let x = VarId(0);
        let y = VarId(1);
        // (λx. x) y  →  y
        let term = SemTerm::App {
            fun: Box::new(SemTerm::Lambda {
                binder: x,
                body: Box::new(SemTerm::Var(x)),
            }),
            args: vec![SemTerm::Var(y)],
        };
        let reduced = beta_step(&term);
        assert_eq!(reduced, Some(SemTerm::Var(y)));
    }

    #[test]
    fn beta_normalize_deep() {
        let x = VarId(0);
        let y = VarId(1);
        // (λx. (λy. x) y) y
        let term = SemTerm::App {
            fun: Box::new(SemTerm::Lambda {
                binder: x,
                body: Box::new(SemTerm::App {
                    fun: Box::new(SemTerm::Lambda {
                        binder: y,
                        body: Box::new(SemTerm::Var(x)),
                    }),
                    args: vec![SemTerm::Var(y)],
                }),
            }),
            args: vec![SemTerm::Var(y)],
        };
        let norm = beta_normalize(&term);
        assert_eq!(norm, SemTerm::Var(y));
    }

    #[test]
    fn alpha_eq_basic() {
        let x = VarId(0);
        let y = VarId(1);
        // λx. x  vs  λy. y
        let t1 = SemTerm::Lambda {
            binder: x,
            body: Box::new(SemTerm::Var(x)),
        };
        let t2 = SemTerm::Lambda {
            binder: y,
            body: Box::new(SemTerm::Var(y)),
        };
        assert!(alpha_eq(&t1, &t2));
    }

    #[test]
    fn test_op_and_const_helpers_work() {
        let mut reg = make_reg();
        let and = test_op(&mut reg, "and");
        let socrates = test_const(&mut reg, "socrates");
        assert_eq!(reg.kind_of(and.qname()), crate::registry::IdKind::Op);
        assert_eq!(
            reg.kind_of(socrates.qname()),
            crate::registry::IdKind::Const
        );
    }

    #[test]
    fn test_pred_helper_works() {
        let mut reg = make_reg();
        let man = test_pred(&mut reg, "man");
        assert_eq!(reg.kind_of(man.qname()), crate::registry::IdKind::Pred);
    }

    #[test]
    fn alpha_eq_distinguishes_free_vars() {
        let x = VarId(0);
        let y = VarId(1);
        // λx. y  vs  λx. x — not equivalent (y is free in first, bound in second pattern)
        let t1 = SemTerm::Lambda {
            binder: x,
            body: Box::new(SemTerm::Var(y)),
        };
        let t2 = SemTerm::Lambda {
            binder: x,
            body: Box::new(SemTerm::Var(x)),
        };
        assert!(!alpha_eq(&t1, &t2));
    }
}

// ---------------------------------------------------------------------------
// Property tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod proptests {
    use super::*;
    use crate::registry::{NamespaceId, NamespaceResolution, Registry};
    use proptest::prelude::*;

    fn test_reg() -> Registry {
        let mut reg = Registry::empty();
        reg.namespace()
            .canonical("test")
            .owner(crate::owner_info!())
            .create()
            .unwrap();
        reg
    }

    fn get_ns(reg: &Registry) -> NamespaceId {
        match reg.resolve_namespace("test") {
            NamespaceResolution::Found(id) => id,
            _ => panic!(),
        }
    }

    fn arb_sem_term(reg: &mut Registry, depth: u32) -> BoxedStrategy<SemTerm> {
        let ns = get_ns(reg);
        let and = reg.op(ns, "and").arity_fixed(2).register().unwrap();
        let c = reg.const_(ns, "c").register().unwrap();
        let p = reg.pred(ns, "p").arity_fixed(1).register().unwrap();

        let leaf = prop_oneof![
            (0u32..10u32).prop_map(VarId).prop_map(SemTerm::Var),
            Just(SemTerm::Const(c)),
        ];

        leaf.prop_recursive(depth, 64, 8, move |inner| {
            let i2 = inner.clone();
            let i3 = inner.clone();
            prop_oneof![
                i2.clone().prop_map(move |a| SemTerm::un_op(and, a)),
                (i2.clone(), i3.clone()).prop_map(move |(a, b)| SemTerm::bin_op(and, a, b)),
                (any::<u32>().prop_map(VarId), i3.clone()).prop_map(move |(v, body)| {
                    SemTerm::Lambda {
                        binder: v,
                        body: Box::new(body),
                    }
                }),
                (i2.clone(), i3.clone()).prop_map(move |(fun, arg)| SemTerm::App {
                    fun: Box::new(fun),
                    args: vec![arg],
                }),
                i2.prop_map(move |a| SemTerm::Pred {
                    head: p,
                    args: vec![a],
                }),
            ]
        })
        .boxed()
    }

    proptest! {
        #[test]
        fn alpha_eq_reflexive(t in arb_sem_term(&mut test_reg(), 4)) {
            prop_assert!(alpha_eq(&t, &t));
        }

        #[test]
        fn alpha_rename_preserves_alpha_eq(
            (v, t) in (any::<u32>().prop_map(VarId))
                .prop_flat_map(|v| {
                    let mut reg = test_reg();
                    (Just(v), arb_sem_term(&mut reg, 4))
                }),
            w in any::<u32>().prop_map(VarId),
        ) {
            prop_assume!(v != w);
            let renamed = alpha_rename(&t, v, w);
            prop_assert!(alpha_eq(&t, &renamed));
        }

        #[test]
        fn beta_terminates(t in arb_sem_term(&mut test_reg(), 6)) {
            let _ = beta_normalize(&t);
        }

        #[test]
        fn beta_preserves_alpha_eq(
            t in arb_sem_term(&mut test_reg(), 4),
            v in any::<u32>().prop_map(VarId),
            w in any::<u32>().prop_map(VarId),
        ) {
            prop_assume!(v != w);
            let renamed = alpha_rename(&t, v, w);
            let n1 = beta_normalize(&t);
            let n2 = beta_normalize(&renamed);
            prop_assert!(alpha_eq(&n1, &n2));
        }
    }
}
