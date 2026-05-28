//! Beta-reduction for [`Term`] values.
//!
//! Reduces `App(Lambda(x, body), [arg])` → `body[arg/x]` and recurses
//! through the term structure.  Backends call `beta_normalize` before
//! lowering so they see clean terms.

use crate::types::Term;

/// Substitute `arg` for variable `var` throughout `body`.
fn substitute<A: Clone>(body: &Term<A>, var: &str, arg: &Term<A>) -> Term<A> {
    match body {
        Term::Var(v) if v == var => arg.clone(),
        Term::Var(_) => body.clone(),
        Term::Atom(_) => body.clone(),
        Term::Lambda(v, b) => {
            if v == var {
                // Shadowed — leave unchanged.
                body.clone()
            } else {
                Term::Lambda(v.clone(), Box::new(substitute(b, var, arg)))
            }
        }
        Term::App(f, args) => Term::App(
            Box::new(substitute(f, var, arg)),
            args.iter().map(|a| substitute(a, var, arg)).collect(),
        ),
    }
}

/// Reduce top-level redexes and recurse. Does NOT reduce under lambdas
/// (weak head normal form is sufficient for our use case).
fn reduce<A: Clone>(term: &Term<A>) -> Term<A> {
    match term {
        // The key case: App(Lambda(x, body), [arg, ...])
        Term::App(f, args) => {
            let reduced_f = Box::new(reduce(f));
            let reduced_args: Vec<Term<A>> = args.iter().map(reduce).collect();

            match reduced_f.as_ref() {
                Term::Lambda(var, body) => {
                    if reduced_args.is_empty() {
                        // No args to apply — leave as App(Lambda(x,body), [])
                        Term::App(reduced_f, reduced_args)
                    } else {
                        // β-reduce: substitute first arg, continue with rest
                        let after_one = substitute(body, var, &reduced_args[0]);
                        if reduced_args.len() > 1 {
                            reduce(&Term::App(Box::new(after_one), reduced_args[1..].to_vec()))
                        } else {
                            reduce(&after_one)
                        }
                    }
                }
                _ => {
                    // Not a lambda — just recurse into subterms
                    Term::App(reduced_f, reduced_args)
                }
            }
        }
        Term::Lambda(v, body) => Term::Lambda(v.clone(), Box::new(reduce(body))),
        other => other.clone(),
    }
}

/// Beta-normalize a term by reducing all redexes.
/// Idempotent: `beta_normalize(beta_normalize(t)) == beta_normalize(t)`.
pub fn beta_normalize<A: Clone>(term: &Term<A>) -> Term<A> {
    reduce(term)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_beta() {
        // (λx. x)(a) → a
        let term = Term::App(
            Box::new(Term::Lambda("x".into(), Box::new(Term::Var("x".into())))),
            vec![Term::Atom("a")],
        );
        assert_eq!(beta_normalize(&term), Term::Atom("a"));
    }

    #[test]
    fn multi_arg() {
        // (λx. λy. f(x, y))(a)(b) → f(a, b)
        let term = Term::App(
            Box::new(Term::Lambda(
                "x".into(),
                Box::new(Term::Lambda(
                    "y".into(),
                    Box::new(Term::App(
                        Box::new(Term::Atom("f")),
                        vec![Term::Var("x".into()), Term::Var("y".into())],
                    )),
                )),
            )),
            vec![Term::Atom("a"), Term::Atom("b")],
        );
        let expected = Term::App(
            Box::new(Term::Atom("f")),
            vec![Term::Atom("a"), Term::Atom("b")],
        );
        assert_eq!(beta_normalize(&term), expected);
    }

    #[test]
    fn no_redex() {
        let term = Term::App(Box::new(Term::Atom("f")), vec![Term::Atom("a")]);
        assert_eq!(beta_normalize(&term), term);
    }

    #[test]
    fn nested_lambda() {
        // (λP. λQ. P ∧ Q)(happy)(cat) → happy ∧ cat
        let term = Term::App(
            Box::new(Term::Lambda(
                "P".into(),
                Box::new(Term::Lambda(
                    "Q".into(),
                    Box::new(Term::App(
                        Box::new(Term::Atom("and")),
                        vec![Term::Var("P".into()), Term::Var("Q".into())],
                    )),
                )),
            )),
            vec![Term::Atom("happy"), Term::Atom("cat")],
        );
        let expected = Term::App(
            Box::new(Term::Atom("and")),
            vec![Term::Atom("happy"), Term::Atom("cat")],
        );
        assert_eq!(beta_normalize(&term), expected);
    }
}
