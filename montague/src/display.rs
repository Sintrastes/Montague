use crate::types::Term;

/// Format a term as a Prolog term: `f(x,y)`.
pub fn display_as_prolog_term<A: std::fmt::Display>(term: &Term<A>) -> String {
    term.to_string()
}

/// Format a term as an S-expression: `(f x y)`.
pub fn display_as_sexp<A: std::fmt::Display>(term: &Term<A>) -> String {
    match term {
        Term::Atom(a) => a.to_string(),
        Term::Var(s) => s.clone(),
        Term::Lambda(s, body) => format!("(λ {} {})", s, display_as_sexp(body)),
        Term::App(f, args) => {
            let mut parts = vec![display_as_sexp(f)];
            parts.extend(args.iter().map(display_as_sexp));
            format!("({})", parts.join(" "))
        }
    }
}
