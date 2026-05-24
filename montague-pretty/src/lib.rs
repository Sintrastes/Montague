//! Pretty-printing backends for Montague.
//!
//! Provides S-expression and Prolog-term renderers for both [`SemTerm`] and
//! [`LambekType`], plus a [`Backend`] impl for direct lowering.

pub mod tree;

use std::fmt::Write;

use montague_core::{
    registry::{CustomTag, Registry},
    sem::{Backend, BackendCtx, OpSet, SemTerm},
    types::{LambekFold, LambekType},
};

// ---------------------------------------------------------------------------
// SemTerm → S-expression
// ---------------------------------------------------------------------------

/// Render a `SemTerm` as an S-expression, using registry names when available.
pub fn display_semterm_as_sexp(term: &SemTerm, reg: Option<&Registry>) -> String {
    let mut buf = String::new();
    write_sexp(term, reg, &mut buf, 0).unwrap();
    buf
}

fn write_sexp(
    term: &SemTerm,
    reg: Option<&Registry>,
    buf: &mut String,
    _depth: usize,
) -> std::fmt::Result {
    match term {
        SemTerm::Var(v) => write!(buf, "v{}", v.0),
        SemTerm::Const(c) => {
            if let Some(r) = reg {
                write!(buf, "{}", r.qname_entry(c.qname()).local)
            } else {
                write!(buf, "c{}", c.qname().0)
            }
        }
        SemTerm::Pred { head, args } => {
            let name = reg
                .map(|r| r.qname_entry(head.qname()).local.as_ref())
                .unwrap_or("?");
            write!(buf, "({name}")?;
            for a in args {
                write!(buf, " ")?;
                write_sexp(a, reg, buf, 0)?;
            }
            write!(buf, ")")
        }
        SemTerm::Lambda { binder, body } => {
            write!(buf, "(λ v{} ", binder.0)?;
            write_sexp(body, reg, buf, 0)?;
            write!(buf, ")")
        }
        SemTerm::App { fun, args } => {
            write!(buf, "(")?;
            write_sexp(fun, reg, buf, 0)?;
            for a in args {
                write!(buf, " ")?;
                write_sexp(a, reg, buf, 0)?;
            }
            write!(buf, ")")
        }
        SemTerm::Eq(a, b) => {
            write!(buf, "(= ")?;
            write_sexp(a, reg, buf, 0)?;
            write!(buf, " ")?;
            write_sexp(b, reg, buf, 0)?;
            write!(buf, ")")
        }
        SemTerm::Op { op, args } => {
            let name = reg
                .map(|r| r.qname_entry(op.qname()).local.as_ref())
                .unwrap_or("?");
            write!(buf, "({name}")?;
            for a in args {
                write!(buf, " ")?;
                write_sexp(a, reg, buf, 0)?;
            }
            write!(buf, ")")
        }
        SemTerm::Binder {
            op,
            binder,
            restr,
            body,
        } => {
            let name = reg
                .map(|r| r.qname_entry(op.qname()).local.as_ref())
                .unwrap_or("?");
            write!(buf, "({name} v{}", binder.0)?;
            if let Some(r) = restr {
                write!(buf, " ")?;
                write_sexp(r, reg, buf, 0)?;
            }
            write!(buf, " ")?;
            write_sexp(body, reg, buf, 0)?;
            write!(buf, ")")
        }
    }
}

// ---------------------------------------------------------------------------
// Term → S-expression (sentence mode)
// ---------------------------------------------------------------------------

use montague_core::types::Term;

/// Display a `Term<String>` as an S-expression. App arguments are reversed
/// for sentence order: last-absorbed (subject) appears first.
pub fn display_term_as_sexp(term: &Term<String>) -> String {
    match term {
        Term::Atom(name) => name.clone(),
        Term::Var(s) => s.clone(),
        Term::Lambda(s, body) => format!("(λ {s} {})", display_term_as_sexp(body)),
        Term::App(f, args) => {
            let mut parts = vec![display_term_as_sexp(f)];
            parts.extend(args.iter().rev().map(display_term_as_sexp));
            format!("({})", parts.join(" "))
        }
    }
}

// ---------------------------------------------------------------------------
// SemTerm → Prolog term
// ---------------------------------------------------------------------------

/// Render a `SemTerm` as a Prolog term, using registry names when available.
pub fn display_semterm_as_prolog(term: &SemTerm, reg: Option<&Registry>) -> String {
    let mut buf = String::new();
    write_prolog(term, reg, &mut buf).unwrap();
    buf
}

fn write_prolog(term: &SemTerm, reg: Option<&Registry>, buf: &mut String) -> std::fmt::Result {
    match term {
        SemTerm::Var(v) => write!(buf, "V{}", v.0),
        SemTerm::Const(c) => {
            if let Some(r) = reg {
                write!(buf, "{}", r.qname_entry(c.qname()).local)
            } else {
                write!(buf, "c{}", c.qname().0)
            }
        }
        SemTerm::Pred { head, args } => {
            let name = reg
                .map(|r| r.qname_entry(head.qname()).local.as_ref())
                .unwrap_or("?");
            write!(buf, "{name}(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(buf, ", ")?;
                }
                write_prolog(a, reg, buf)?;
            }
            write!(buf, ")")
        }
        SemTerm::Lambda { binder, body } => {
            write!(buf, "lambda(v{}, ", binder.0)?;
            write_prolog(body, reg, buf)?;
            write!(buf, ")")
        }
        SemTerm::App { fun, args } => {
            write_prolog(fun, reg, buf)?;
            write!(buf, "(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(buf, ", ")?;
                }
                write_prolog(a, reg, buf)?;
            }
            write!(buf, ")")
        }
        SemTerm::Eq(a, b) => {
            write_prolog(a, reg, buf)?;
            write!(buf, " = ")?;
            write_prolog(b, reg, buf)
        }
        SemTerm::Op { op, args } => {
            let name = reg
                .map(|r| r.qname_entry(op.qname()).local.as_ref())
                .unwrap_or("?");
            write!(buf, "{name}(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    write!(buf, ", ")?;
                }
                write_prolog(a, reg, buf)?;
            }
            write!(buf, ")")
        }
        SemTerm::Binder {
            op,
            binder,
            restr,
            body,
        } => {
            let name = reg
                .map(|r| r.qname_entry(op.qname()).local.as_ref())
                .unwrap_or("?");
            write!(buf, "{name}(V{}", binder.0)?;
            if let Some(r) = restr {
                write!(buf, ", ")?;
                write_prolog(r, reg, buf)?;
            }
            write!(buf, ", ")?;
            write_prolog(body, reg, buf)?;
            write!(buf, ")")
        }
    }
}

// ---------------------------------------------------------------------------
// LambekType → S-expression (via LambekFold)
// ---------------------------------------------------------------------------

struct SExpLambekFold<'a, T> {
    reg: Option<&'a Registry>,
    _phantom: std::marker::PhantomData<T>,
}

impl<'a, T> SExpLambekFold<'a, T> {
    fn new(reg: Option<&'a Registry>) -> Self {
        SExpLambekFold {
            reg,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, T: Clone + std::hash::Hash + std::cmp::Eq + std::fmt::Debug> LambekFold<T>
    for SExpLambekFold<'a, T>
{
    type Out = String;

    fn basic(&mut self, t: &T) -> String {
        format!("{:?}", t)
    }

    fn left_arrow(&mut self, a: String, b: String) -> String {
        format!("(/ {a} {b})")
    }

    fn right_arrow(&mut self, a: String, b: String) -> String {
        format!("(\\ {a} {b})")
    }

    fn extract(&mut self, a: String, b: String) -> String {
        format!("(extract {a} {b})")
    }

    fn scoped(&mut self, a: String, b: String) -> String {
        format!("(scoped {a} {b})")
    }

    fn conj(&mut self, a: String, b: String) -> String {
        format!("(and {a} {b})")
    }

    fn disj(&mut self, a: String, b: String) -> String {
        format!("(or {a} {b})")
    }

    fn custom(&mut self, tag: CustomTag, args: Vec<String>) -> String {
        let name = self
            .reg
            .map(|r| r.qname_entry(tag.qname()).local.as_ref())
            .unwrap_or("?");
        let args_str = args.join(" ");
        format!("({name} {args_str})")
    }
}

/// Render a `LambekType` as an S-expression.
pub fn display_lambek_as_sexp<T: Clone + std::hash::Hash + std::cmp::Eq + std::fmt::Debug>(
    ty: &LambekType<T>,
    reg: Option<&Registry>,
) -> String {
    ty.fold(&mut SExpLambekFold::new(reg))
}

// ---------------------------------------------------------------------------
// Backend impl
// ---------------------------------------------------------------------------

/// S-expression backend for Montague.
pub struct SExpBackend;

impl Backend for SExpBackend {
    type Output = String;
    type Error = std::convert::Infallible;

    fn supported_ops(&self) -> &OpSet {
        // S-exp backend accepts any operator — it just prints whatever it gets.
        static OPS: std::sync::LazyLock<OpSet> = std::sync::LazyLock::new(OpSet::new);
        &OPS
    }

    fn lower(&self, term: &SemTerm, ctx: &BackendCtx) -> Result<Self::Output, Self::Error> {
        Ok(display_semterm_as_sexp(term, ctx.registry))
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use montague_core::{registry::Registry, VarId};

    fn test_reg() -> Registry {
        let mut reg = Registry::empty();
        let ns = reg
            .namespace()
            .canonical("test")
            .owner(montague_core::registry::owner_info!())
            .create()
            .unwrap();
        reg.op(ns, "and").arity_fixed(2).register().unwrap();
        reg
    }

    #[test]
    fn sexp_var() {
        let term = SemTerm::Var(VarId(0));
        assert_eq!(display_semterm_as_sexp(&term, None), "v0");
    }

    #[test]
    fn sexp_pred_without_registry() {
        let mut reg = Registry::empty();
        let ns = reg
            .namespace()
            .canonical("test")
            .owner(montague_core::registry::owner_info!())
            .create()
            .unwrap();
        let p = reg.pred(ns, "man").arity_fixed(1).register().unwrap();
        let term = SemTerm::Pred {
            head: p,
            args: vec![SemTerm::Const(
                reg.const_(ns, "socrates").register().unwrap(),
            )],
        };
        // Without registry, uses fallback `?` for the predicate name
        let s = display_semterm_as_sexp(&term, None);
        assert!(s.starts_with("(? "), "expected fallback pred: {s}");
    }

    #[test]
    fn sexp_pred_with_registry() {
        let mut reg = Registry::empty();
        let ns = reg
            .namespace()
            .canonical("test")
            .owner(montague_core::registry::owner_info!())
            .create()
            .unwrap();
        let p = reg.pred(ns, "man").arity_fixed(1).register().unwrap();
        let socrates = reg.const_(ns, "socrates").register().unwrap();
        let term = SemTerm::Pred {
            head: p,
            args: vec![SemTerm::Const(socrates)],
        };
        let s = display_semterm_as_sexp(&term, Some(&reg));
        assert_eq!(s, "(man socrates)");
    }

    #[test]
    fn sexp_op_with_registry() {
        let reg = test_reg();
        let ns = match reg.resolve_namespace("test") {
            montague_core::registry::NamespaceResolution::Found(id) => id,
            _ => panic!(),
        };
        let and = reg.lookup_op(ns, "and").unwrap();
        let term = SemTerm::Op {
            op: and,
            args: vec![SemTerm::Var(VarId(0)), SemTerm::Var(VarId(1))],
        };
        let s = display_semterm_as_sexp(&term, Some(&reg));
        assert_eq!(s, "(and v0 v1)");
    }

    #[test]
    fn prolog_pred_with_registry() {
        let mut reg = Registry::empty();
        let ns = reg
            .namespace()
            .canonical("test")
            .owner(montague_core::registry::owner_info!())
            .create()
            .unwrap();
        let p = reg.pred(ns, "man").arity_fixed(1).register().unwrap();
        let socrates = reg.const_(ns, "socrates").register().unwrap();
        let term = SemTerm::Pred {
            head: p,
            args: vec![SemTerm::Const(socrates)],
        };
        let s = display_semterm_as_prolog(&term, Some(&reg));
        assert_eq!(s, "man(socrates)");
    }

    #[test]
    fn lambek_fold_basic() {
        let ty = LambekType::RightArrow(
            Box::new(LambekType::Basic("Noun")),
            Box::new(LambekType::Basic("Sentence")),
        );
        let s = display_lambek_as_sexp(&ty, None);
        assert_eq!(s, "(\\ \"Noun\" \"Sentence\")");
    }

    #[test]
    fn backend_lower() {
        let mut reg = Registry::empty();
        let ns = reg
            .namespace()
            .canonical("test")
            .owner(montague_core::registry::owner_info!())
            .create()
            .unwrap();
        let p = reg.pred(ns, "man").arity_fixed(1).register().unwrap();
        let socrates = reg.const_(ns, "socrates").register().unwrap();
        let term = SemTerm::Pred {
            head: p,
            args: vec![SemTerm::Const(socrates)],
        };

        let backend = SExpBackend;
        let ctx = BackendCtx {
            registry: Some(&reg),
        };
        let output = backend.lower(&term, &ctx).unwrap();
        assert_eq!(output, "(man socrates)");
    }
}
