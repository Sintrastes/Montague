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

/// Lower a `Term<String>` to one or more Prolog clause strings.
///
/// Returns a vector where each element is a Prolog clause (fact, rule, or
/// query goal).  For simple predication this produces a single clause; for
/// conjunction (`and`) it produces one clause per conjunct.
///
/// Arguments are reversed for sentence order: the last-absorbed argument
/// (subject via RightArrow) appears first, earlier arguments (objects via
/// LeftArrow) appear after.
///
/// Copula stripping: `App(is_cop, [pred, subj])` → `pred(subj).` and
/// `App(is_cop, [App(a_art, [noun]), subj])` → `noun(subj).`
///
/// Conjunction: `App(is_cop, [App(and, [p1, p2]), subj])` →
///   `[p1(subj)., p2(subj).]`
///
/// Quantifier: `App(all_q, [noun, vp])` → `pred(X) :- noun(X).`
pub fn lower_term_to_prolog(term: &Term<String>) -> Option<Vec<String>> {
    // Copula detection: is_cop(subject, predicate...) → predicate(subject).
    if let Term::App(f, args) = term {
        let fname = match f.as_ref() {
            Term::Atom(n) => n.clone(),
            _ => String::new(),
        };

        // Copula stripping for assertions and queries.
        if (fname == "is_cop" || fname == "are_cop") && args.len() >= 2 {
            let pred = strip_article(&args[0]);
            let subj = term_to_prolog_arg(&args[1])?;

            // Conjunction: and(pred1, pred2) → two clauses sharing the subject.
            if let Some(conjuncts) = extract_conjuncts(&pred) {
                let clauses: Vec<String> = conjuncts
                    .iter()
                    .filter_map(|c| {
                        let stripped = strip_article(c);
                        let name = term_to_prolog_arg(&stripped)?;
                        Some(format!("{name}({subj})."))
                    })
                    .collect();
                if !clauses.is_empty() {
                    return Some(clauses);
                }
            }

            let pred_name = term_to_prolog_arg(&pred)?;
            return Some(vec![format!("{pred_name}({subj}).")]);
        }

        // Inverted copula (polar question): is_q(subj, pred) → pred(subj).
        // `is_q : (Q/Adj)/N` consumes subj first (right-app), then pred (right-app).
        if fname == "is_q" && args.len() == 2 {
            let subj = term_to_prolog_arg(&args[0])?;
            let pred = strip_article(&args[1]);

            // Conjunction in polar question: "Is Socrates a man and mortal?"
            if let Some(conjuncts) = extract_conjuncts(&pred) {
                let clauses: Vec<String> = conjuncts
                    .iter()
                    .filter_map(|c| {
                        let stripped = strip_article(c);
                        let name = term_to_prolog_arg(&stripped)?;
                        Some(format!("{name}({subj})."))
                    })
                    .collect();
                if !clauses.is_empty() {
                    return Some(clauses);
                }
            }

            let pred_name = term_to_prolog_arg(&pred)?;
            return Some(vec![format!("{pred_name}({subj}).")]);
        }

        // Wh-pronoun: who_q(vp) → lower the VP with a free variable X.
        // The VP is typically a copula compound like is_cop(mortal) or a verb.
        if fname == "who_q" && args.len() == 1 {
            return lower_vp_to_query(&args[0]).map(|s| vec![s]);
        }

        // Quantifier: all_q / every_q
        if (fname == "all_q" || fname == "every_q") && args.len() == 2 {
            let noun = term_to_prolog_arg(&args[0])?;
            let pred = extract_predicate(&args[1])?;
            return Some(vec![format!("{pred}(X) :- {noun}(X).")]);
        }

        // Default: generic App → function(args).
        let arg_strs: Vec<String> = args.iter().rev().filter_map(term_to_prolog_arg).collect();
        if arg_strs.len() != args.len() {
            return None;
        }
        return Some(vec![format!("{fname}({}).", arg_strs.join(", "))]);
    }

    if let Term::Atom(name) = term {
        return Some(vec![name.clone()]);
    }
    None
}

/// If `term` is an `and`-conjunction `App(and, [c1, c2])`, return the two
/// conjuncts.  The first conjunct in the predicate position (closest to the
/// copula) is `args[0]`; the second (article-wrapped noun) is `args[1]`.
fn extract_conjuncts(term: &Term<String>) -> Option<Vec<Term<String>>> {
    match term {
        Term::App(f, args) => {
            let fname = match f.as_ref() {
                Term::Atom(n) => n,
                _ => return None,
            };
            if fname == "and" && args.len() >= 2 {
                Some(args.iter().cloned().collect())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Lower a VP (Verb Phrase, type `Noun \ Sentence`) to a Prolog query with
/// a free variable `X` for the missing subject. Used for wh-questions like
/// "Who is mortal?" where the VP `is mortal` becomes `mortal(X).`
///
/// Handles:
/// - `is_cop(pred)`  → `pred(X).`
/// - `is_cop(App(a_art, [noun]))` → `noun(X).`
/// - Conjunction: `is_cop(App(and, [p1, p2]))` → `p1(X), p2(X).`
fn lower_vp_to_query(term: &Term<String>) -> Option<String> {
    match term {
        Term::App(f, args) => {
            let fname = match f.as_ref() {
                Term::Atom(n) => n.clone(),
                _ => return None,
            };
            // Copula VP: is_cop(predicate) → predicate(X).
            if (fname == "is_cop" || fname == "are_cop") && !args.is_empty() {
                let pred = strip_article(&args[0]);

                // Conjunction inside VP: "who is a man and mortal?" → mortal(X), man_noun(X).
                if let Some(conjuncts) = extract_conjuncts(&pred) {
                    let goals: Vec<String> = conjuncts
                        .iter()
                        .filter_map(|c| {
                            let stripped = strip_article(c);
                            let name = term_to_prolog_arg(&stripped)?;
                            Some(format!("{name}(X)"))
                        })
                        .collect();
                    if !goals.is_empty() {
                        return Some(format!("{}.", goals.join(", ")));
                    }
                }

                let pred_name = term_to_prolog_arg(&pred)?;
                return Some(format!("{pred_name}(X)."));
            }
            // Generic VP: verb(...args) → verb(X, ...args).
            let arg_strs: Vec<String> = args.iter().filter_map(term_to_prolog_arg).collect();
            if arg_strs.len() != args.len() {
                return None;
            }
            Some(format!("{fname}(X, {}).", arg_strs.join(", ")))
        }
        Term::Atom(name) => {
            // Intransitive verb: run → run(X).
            Some(format!("{name}(X)."))
        }
        _ => None,
    }
}

/// Strip the article `a_art` and `both` wrappers:
/// - `App(a_art, [noun])` → just the `noun`.
/// - `App(both, [a_art, noun])` → just the `noun` (skip both + article).
fn strip_article(term: &Term<String>) -> Term<String> {
    match term {
        Term::App(f, args) => {
            let fname = match f.as_ref() {
                Term::Atom(n) => n,
                _ => return term.clone(),
            };
            if fname == "a_art" && !args.is_empty() {
                args[0].clone()
            } else if fname == "both" && !args.is_empty() {
                // both(a_art, noun) → skip both, try stripping article from the last arg
                strip_article(&args[args.len() - 1])
            } else {
                term.clone()
            }
        }
        _ => term.clone(),
    }
}

/// Extract the semantic predicate from a VP compound like `App(is_cop, [mortal])` →
/// `"mortal"`. Skips copulas and returns the innermost non-copula atom.
fn extract_predicate(term: &Term<String>) -> Option<String> {
    match term {
        Term::Atom(name) => {
            if name == "is_cop" || name == "are_cop" {
                None // copula alone has no predicate
            } else {
                Some(name.clone())
            }
        }
        Term::App(f, args) => {
            let f_name = match f.as_ref() {
                Term::Atom(n) => n.clone(),
                _ => return None,
            };
            // If the function is a copula, the predicate is in the args.
            if f_name == "is_cop" || f_name == "are_cop" {
                args.last().and_then(extract_predicate)
            } else {
                // Otherwise, the predicate is the function itself.
                Some(f_name)
            }
        }
        _ => None,
    }
}

/// Convert a sub-term to a Prolog argument string.
pub fn term_to_prolog_arg(term: &Term<String>) -> Option<String> {
    match term {
        Term::Atom(name) => Some(name.clone()),
        Term::App(_, _) => lower_term_to_prolog(term)
            .and_then(|clauses| clauses.into_iter().next())
            .map(|s| s.trim_end_matches('.').to_string()),
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

    // -----------------------------------------------------------------------
    // Term-level lowering tests (sentence → Prolog string)
    // -----------------------------------------------------------------------

    use montague_core::types::Term as CTerm;

    /// `App(is_cop, [mortal, socrates])` → `["mortal(socrates)."]`
    #[test]
    fn lower_simple_copula() {
        let term = CTerm::App(
            Box::new(CTerm::Atom("is_cop".into())),
            vec![CTerm::Atom("mortal".into()), CTerm::Atom("socrates".into())],
        );
        let clauses = lower_term_to_prolog(&term).unwrap();
        assert_eq!(clauses, vec!["mortal(socrates)."]);
    }

    /// `App(is_cop, [App(and, [mortal, App(a_art, [man_noun])]), socrates])` →
    /// `["mortal(socrates).", "man_noun(socrates)."]`
    #[test]
    fn lower_conjoined_copula() {
        let a_man = CTerm::App(
            Box::new(CTerm::Atom("a_art".into())),
            vec![CTerm::Atom("man_noun".into())],
        );
        let and_pred = CTerm::App(
            Box::new(CTerm::Atom("and".into())),
            vec![CTerm::Atom("mortal".into()), a_man],
        );
        let term = CTerm::App(
            Box::new(CTerm::Atom("is_cop".into())),
            vec![and_pred, CTerm::Atom("socrates".into())],
        );
        let clauses = lower_term_to_prolog(&term).unwrap();
        assert_eq!(clauses.len(), 2);
        assert!(clauses.contains(&"mortal(socrates).".into()));
        assert!(clauses.contains(&"man_noun(socrates).".into()));
    }

    /// `App(is_q, [socrates, App(and, [mortal, App(a_art, [man_noun])])])` →
    /// `["mortal(socrates).", "man_noun(socrates)."]`
    #[test]
    fn lower_conjoined_polar_question() {
        let a_man = CTerm::App(
            Box::new(CTerm::Atom("a_art".into())),
            vec![CTerm::Atom("man_noun".into())],
        );
        let and_pred = CTerm::App(
            Box::new(CTerm::Atom("and".into())),
            vec![CTerm::Atom("mortal".into()), a_man],
        );
        let term = CTerm::App(
            Box::new(CTerm::Atom("is_q".into())),
            vec![CTerm::Atom("socrates".into()), and_pred],
        );
        let clauses = lower_term_to_prolog(&term).unwrap();
        assert_eq!(clauses.len(), 2);
        assert!(clauses.contains(&"mortal(socrates).".into()));
        assert!(clauses.contains(&"man_noun(socrates).".into()));
    }

    /// `App(who_q, [App(is_cop, [App(and, [mortal, App(a_art, [man_noun])])])])` →
    /// `["mortal(X), man_noun(X)."]`
    #[test]
    fn lower_conjoined_wh_question() {
        let a_man = CTerm::App(
            Box::new(CTerm::Atom("a_art".into())),
            vec![CTerm::Atom("man_noun".into())],
        );
        let and_pred = CTerm::App(
            Box::new(CTerm::Atom("and".into())),
            vec![CTerm::Atom("mortal".into()), a_man],
        );
        let copula = CTerm::App(
            Box::new(CTerm::Atom("is_cop".into())),
            vec![and_pred],
        );
        let term = CTerm::App(
            Box::new(CTerm::Atom("who_q".into())),
            vec![copula],
        );
        let clauses = lower_term_to_prolog(&term).unwrap();
        assert_eq!(clauses.len(), 1);
        // The two goals are comma-joined: mortal(X), man_noun(X).
        let clause = &clauses[0];
        assert!(clause.contains("mortal(X)"), "got: {clause}");
        assert!(clause.contains("man_noun(X)"), "got: {clause}");
        assert!(clause.contains(", "), "goals should be comma-separated: {clause}");
    }

    /// `App(is_cop, [App(both, [a_art, man_noun]), socrates])` (without and) →
    /// just the noun after stripping both + article.
    #[test]
    fn lower_both_without_conjunction() {
        let both_man = CTerm::App(
            Box::new(CTerm::Atom("both".into())),
            vec![
                CTerm::Atom("a_art".into()),
                CTerm::Atom("man_noun".into()),
            ],
        );
        let term = CTerm::App(
            Box::new(CTerm::Atom("is_cop".into())),
            vec![both_man, CTerm::Atom("socrates".into())],
        );
        let clauses = lower_term_to_prolog(&term).unwrap();
        assert_eq!(clauses, vec!["man_noun(socrates)."]);
    }
}
