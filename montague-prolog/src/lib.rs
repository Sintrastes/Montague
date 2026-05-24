//! Prolog backend for Montague.
//!
//! Lowers [`SemTerm`] to Prolog clauses. Supports the Horn-clause fragment
//! of first-order logic: facts, rules, and conjunctions of facts.

use std::fmt::Write;

use montague_core::{
    registry::Registry,
    sem::{Backend, BackendCtx, OpSet, SemTerm},
};

// ---------------------------------------------------------------------------
// Prolog clause
// ---------------------------------------------------------------------------

/// A single Prolog clause (fact or rule).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrologClause {
    /// `head(arg1, ..., argN).`
    Fact { head: String, args: Vec<PrologTerm> },
    /// `head(args) :- body1, body2, ... .`
    Rule {
        head: String,
        head_args: Vec<PrologTerm>,
        body: Vec<PrologClause>,
    },
}

/// A Prolog term within a clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrologTerm {
    Var(String),
    Const(String),
}

impl PrologClause {
    /// Render this clause as a Prolog source string.
    pub fn to_prolog(&self) -> String {
        let mut s = String::new();
        self.write(&mut s, true).unwrap();
        s
    }

    fn write(&self, s: &mut String, top_level: bool) -> std::fmt::Result {
        match self {
            PrologClause::Fact { head, args } => {
                write!(s, "{head}")?;
                if !args.is_empty() {
                    write!(s, "(")?;
                    for (i, a) in args.iter().enumerate() {
                        if i > 0 {
                            write!(s, ", ")?;
                        }
                        a.write(s)?;
                    }
                    write!(s, ")")?;
                }
                if top_level {
                    write!(s, ".")
                } else {
                    Ok(())
                }
            }
            PrologClause::Rule {
                head,
                head_args,
                body,
            } => {
                write!(s, "{head}")?;
                if !head_args.is_empty() {
                    write!(s, "(")?;
                    for (i, a) in head_args.iter().enumerate() {
                        if i > 0 {
                            write!(s, ", ")?;
                        }
                        a.write(s)?;
                    }
                    write!(s, ")")?;
                }
                write!(s, " :- ")?;
                for (i, b) in body.iter().enumerate() {
                    if i > 0 {
                        write!(s, ", ")?;
                    }
                    b.write(s, false)?;
                }
                if top_level {
                    write!(s, ".")
                } else {
                    Ok(())
                }
            }
        }
    }
}

impl PrologTerm {
    fn write(&self, s: &mut String) -> std::fmt::Result {
        match self {
            PrologTerm::Var(v) => write!(s, "{v}"),
            PrologTerm::Const(c) => write!(s, "{c}"),
        }
    }
}

// ---------------------------------------------------------------------------
// Lowering: SemTerm → PrologClause
// ---------------------------------------------------------------------------

/// Try to lower a SemTerm into a Prolog clause.
///
/// Supports:
/// - `pred(const1, const2, ...)` → fact
/// - `Op(implies, [Pred([Var]), body_pred([Var])])` → rule
/// - `Binder(forall, v, Pred([v]), Pred([v]))` → rule (universal Horn)
pub fn lower_to_clause(term: &SemTerm, reg: Option<&Registry>) -> Option<PrologClause> {
    match term {
        SemTerm::Pred { head, args } => {
            let name = reg
                .map(|r| r.qname_entry(head.qname()).local.as_ref())
                .unwrap_or("?")
                .to_string();
            let args: Vec<PrologTerm> = args.iter().filter_map(lower_term).collect();
            Some(PrologClause::Fact { head: name, args })
        }
        SemTerm::Op { op, args } if args.len() == 2 => {
            // Check if op is "implies" — match by name if registry available
            let is_implies = reg
                .map(|r| r.qname_entry(op.qname()).local.as_ref() == "implies")
                .unwrap_or(false);
            if !is_implies {
                return None;
            }
            // (implies, body, head)
            let head_clause = lower_to_clause(&args[1], reg)?;
            let body_clause = lower_to_clause(&args[0], reg)?;
            match head_clause {
                PrologClause::Fact { head, args: ha } => Some(PrologClause::Rule {
                    head,
                    head_args: ha,
                    body: vec![body_clause],
                }),
                _ => None,
            }
        }
        SemTerm::Binder {
            op,
            binder,
            restr: Some(restr),
            body,
        } => {
            // Universal quantifier: ∀x. restr(x) → body(x)
            let is_forall = reg
                .map(|r| r.qname_entry(op.qname()).local.as_ref() == "forall")
                .unwrap_or(false);
            if !is_forall {
                return None;
            }
            let var_name = format!("X{}", binder.0);
            let head = lower_to_clause_with_var(body, &var_name, reg)?;
            let body = lower_to_clause_with_var(restr, &var_name, reg)?;
            match head {
                PrologClause::Fact { head: h, args: ha } => Some(PrologClause::Rule {
                    head: h,
                    head_args: ha,
                    body: vec![body],
                }),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Lower a SemTerm to a PrologTerm (variable or constant).
fn lower_term(term: &SemTerm) -> Option<PrologTerm> {
    match term {
        SemTerm::Var(v) => Some(PrologTerm::Var(format!("X{}", v.0))),
        SemTerm::Const(c) => Some(PrologTerm::Const(format!("c{}", c.qname().0))),
        _ => None,
    }
}

/// Like `lower_to_clause`, but substitutes the given variable name wherever
/// the SemTerm Var with the given index appears.
fn lower_to_clause_with_var(
    term: &SemTerm,
    var_name: &str,
    reg: Option<&Registry>,
) -> Option<PrologClause> {
    match term {
        SemTerm::Pred { head, args } => {
            let name = reg
                .map(|r| r.qname_entry(head.qname()).local.as_ref())
                .unwrap_or("?")
                .to_string();
            let args: Vec<PrologTerm> = args
                .iter()
                .map(|a| lower_term_with_var(a, var_name))
                .collect();
            Some(PrologClause::Fact { head: name, args })
        }
        _ => None,
    }
}

fn lower_term_with_var(term: &SemTerm, var_name: &str) -> PrologTerm {
    match term {
        SemTerm::Var(_) => PrologTerm::Var(var_name.to_string()),
        SemTerm::Const(c) => PrologTerm::Const(format!("c{}", c.qname().0)),
        _ => PrologTerm::Var(var_name.to_string()),
    }
}

/// Lower a SemTerm to a vector of clauses. For `And`-operator compounds,
/// each conjunct becomes a separate clause.
pub fn lower_to_clauses(term: &SemTerm, reg: Option<&Registry>) -> Vec<PrologClause> {
    match term {
        SemTerm::Op { op, args } => {
            let is_and = reg
                .map(|r| r.qname_entry(op.qname()).local.as_ref() == "and")
                .unwrap_or(false);
            if is_and && args.len() == 2 {
                let mut clauses = lower_to_clauses(&args[0], reg);
                clauses.extend(lower_to_clauses(&args[1], reg));
                return clauses;
            }
            // Try the simple lowering path
            lower_to_clause(term, reg).into_iter().collect()
        }
        _ => lower_to_clause(term, reg).into_iter().collect(),
    }
}

// ---------------------------------------------------------------------------
// Term-level lowering (sentence mode)
// ---------------------------------------------------------------------------

use montague_core::types::Term;

/// Lower a `Term<String>` to a Prolog clause string.
///
/// Arguments are reversed for sentence order: the last-absorbed argument
/// (subject via RightArrow) appears first, earlier arguments (objects via
/// LeftArrow) appear after.
pub fn lower_term_to_prolog(term: &Term<String>) -> Option<String> {
    match term {
        Term::App(f, args) => {
            let fun_name = match f.as_ref() {
                Term::Atom(name) => name.clone(),
                _ => return None,
            };
            let arg_strs: Vec<String> = args.iter().rev().filter_map(term_to_prolog_arg).collect();
            if arg_strs.len() != args.len() {
                return None;
            }
            Some(format!("{}({}).", fun_name, arg_strs.join(", ")))
        }
        Term::Atom(name) => Some(name.clone()),
        _ => None,
    }
}

/// Convert a sub-term to a Prolog argument string.
pub fn term_to_prolog_arg(term: &Term<String>) -> Option<String> {
    match term {
        Term::Atom(name) => Some(name.clone()),
        Term::App(_, _) => lower_term_to_prolog(term).map(|s| s.trim_end_matches('.').to_string()),
        Term::Var(_) => Some("_".to_string()),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Backend impl
// ---------------------------------------------------------------------------

/// Prolog backend: lowers SemTerm to a Prolog program (Vec of clause strings).
pub struct PrologBackend;

impl Backend for PrologBackend {
    type Output = Vec<String>;
    type Error = String;

    fn supported_ops(&self) -> &OpSet {
        // Prolog backend handles and, implies, forall, and basic predication.
        // Unknown operators produce errors during lowering.
        static OPS: std::sync::LazyLock<OpSet> = std::sync::LazyLock::new(OpSet::new);
        &OPS
    }

    fn lower(&self, term: &SemTerm, ctx: &BackendCtx) -> Result<Self::Output, Self::Error> {
        let clauses = lower_to_clauses(term, ctx.registry);
        if clauses.is_empty() {
            return Err("no clauses produced".to_string());
        }
        Ok(clauses.iter().map(|c| c.to_prolog()).collect())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use montague_core::registry::{ConstId, PredId, Registry};
    use montague_core::sem::VarId;

    fn make_reg() -> (Registry, PredId, PredId, ConstId) {
        let mut reg = Registry::empty();
        let ns = reg
            .namespace()
            .canonical("test")
            .owner(montague_core::registry::owner_info!())
            .create()
            .unwrap();
        let man = reg.pred(ns, "man").arity_fixed(1).register().unwrap();
        let mortal = reg.pred(ns, "mortal").arity_fixed(1).register().unwrap();
        let socrates = reg.const_(ns, "socrates").register().unwrap();
        (reg, man, mortal, socrates)
    }

    #[test]
    fn lower_fact() {
        let (reg, man, _, socrates) = make_reg();
        let term = SemTerm::Pred {
            head: man,
            args: vec![SemTerm::Const(socrates)],
        };
        let clause = lower_to_clause(&term, Some(&reg)).unwrap();
        // Without registry name lookup, consts use numeric `c{N}`.
        let prolog = clause.to_prolog();
        assert!(prolog.starts_with("man(c"), "got: {prolog}");
        assert!(prolog.ends_with(")."), "got: {prolog}");
    }

    #[test]
    fn lower_rule() {
        let mut reg = Registry::empty();
        let ns = reg
            .namespace()
            .canonical("test")
            .owner(montague_core::registry::owner_info!())
            .create()
            .unwrap();
        let implies = reg.op(ns, "implies").arity_fixed(2).register().unwrap();
        let man = reg.pred(ns, "man").arity_fixed(1).register().unwrap();
        let mortal = reg.pred(ns, "mortal").arity_fixed(1).register().unwrap();

        // implies(man(X), mortal(X))
        let term = SemTerm::Op {
            op: implies,
            args: vec![
                SemTerm::Pred {
                    head: man,
                    args: vec![SemTerm::Var(VarId(0))],
                },
                SemTerm::Pred {
                    head: mortal,
                    args: vec![SemTerm::Var(VarId(0))],
                },
            ],
        };

        let clause = lower_to_clause(&term, Some(&reg)).unwrap();
        assert_eq!(clause.to_prolog(), "mortal(X0) :- man(X0).");
    }

    #[test]
    fn backend_produces_prolog() {
        let (reg, man, _, socrates) = make_reg();
        let term = SemTerm::Pred {
            head: man,
            args: vec![SemTerm::Const(socrates)],
        };
        let backend = PrologBackend;
        let ctx = BackendCtx {
            registry: Some(&reg),
        };
        let result = backend.lower(&term, &ctx).unwrap();
        assert_eq!(result.len(), 1);
        assert!(result[0].starts_with("man(c"), "got: {:?}", result);
        assert!(result[0].ends_with(")."));
    }
}
