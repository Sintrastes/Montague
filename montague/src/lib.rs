pub mod autocomplete;
pub mod display;
pub mod semantics;
pub mod types;

pub use semantics::Semantics;
pub use types::{AnnotatedTerm, LambekType, LatticeOrd, NonDet, Term};

use types::AnnotatedTerm as AT;

/// Split `input` into lowercase words, resolve each through the lexicon, and
/// return every combination of per-word annotated terms.
///
/// Mirrors `Montague.annotate`.
pub fn annotate<A, T, X>(sem: &Semantics<A, T, X>, input: &str) -> NonDet<Vec<AT<A, T>>>
where
    A: Clone,
    T: Clone,
{
    let words: Vec<String> = input
        .chars()
        .filter(|c| !matches!(c, ',' | ';'))
        .collect::<String>()
        .split_whitespace()
        .map(|w| w.to_lowercase())
        .collect();

    // For each word, collect all possible AnnotatedTerms.
    let per_word: Vec<NonDet<AT<A, T>>> = words
        .iter()
        .map(|w| {
            let terms = (sem.parse_term)(w);
            terms
                .into_iter()
                .flat_map(|term| {
                    // Only Atom terms have a type from typeOfAtom; others are skipped.
                    let types = match &term {
                        Term::Atom(a) => (sem.type_of_atom)(a),
                        _ => vec![],
                    };
                    types.into_iter().map(move |ty| AT {
                        term: term.clone(),
                        ty,
                    })
                })
                .collect()
        })
        .collect();

    // Cartesian product of all words' possibilities.
    cartesian_product(per_word)
}

/// Generate all 3-way splits `(prefix, [i, i+1], suffix)` of `xs` for every
/// adjacent pair. Mirrors `Montague.viewSubstrings`.
fn view_substrings<T: Clone>(xs: &[T]) -> Vec<(Vec<T>, [T; 2], Vec<T>)> {
    let l = xs.len();
    if l < 2 {
        return vec![];
    }
    (0..l - 1)
        .map(|n| {
            let prefix = xs[..n].to_vec();
            let pair = [xs[n].clone(), xs[n + 1].clone()];
            let suffix = xs[n + 2..].to_vec();
            (prefix, pair, suffix)
        })
        .collect()
}

/// Try to apply Lambek reduction rules to an adjacent pair `(left, right)`.
///
/// Rule 1 (left absorption): `t1 : x` and `t2 : RightArrow(x', y)` where `x <= x'`
///   → `App(t2, [t1]) : y`
///
/// Rule 2 (right absorption): `t1 : LeftArrow(x, y)` and `t2 : y'` where `y' <= y`
///   → `App(t1, [t2]) : x`
fn try_reduce<A, T>(left: &AT<A, T>, right: &AT<A, T>) -> Vec<AT<A, T>>
where
    A: Clone,
    T: LatticeOrd + Clone,
{
    let mut results = Vec::new();

    // Rule 1: right-hand term has a RightArrow type, absorbs left argument.
    if let LambekType::RightArrow(x_prime, y) = &right.ty {
        if left.ty.leq(x_prime) {
            let new_term = if right.term.is_partial_pred() {
                right.term.clone().apply_partial(left.term.clone())
            } else {
                Term::App(Box::new(right.term.clone()), vec![left.term.clone()])
            };
            results.push(AT {
                term: new_term,
                ty: *y.clone(),
            });
        }
    }

    // Rule 2: left-hand term has a LeftArrow type, absorbs right argument.
    if let LambekType::LeftArrow(x, y) = &left.ty {
        if right.ty.leq(y) {
            let new_term = if left.term.is_partial_pred() {
                left.term.clone().apply_partial(right.term.clone())
            } else {
                Term::App(Box::new(left.term.clone()), vec![right.term.clone()])
            };
            results.push(AT {
                term: new_term,
                ty: *x.clone(),
            });
        }
    }

    results
}

/// Recursively apply Lambek reductions until a single term remains, then
/// interpret it. Returns all possible interpretations (DFS over reductions).
///
/// Mirrors `Montague.reduce`.
pub fn reduce<A, T, X>(sem: &Semantics<A, T, X>, terms: Vec<AT<A, T>>) -> NonDet<X>
where
    A: Clone,
    T: LatticeOrd + Clone,
    X: Clone,
{
    if terms.len() == 1 {
        return vec![(sem.interp)(terms.into_iter().next().unwrap())];
    }

    let mut results = Vec::new();
    for (prefix, pair, suffix) in view_substrings(&terms) {
        for reduced in try_reduce(&pair[0], &pair[1]) {
            let mut new_terms = prefix.clone();
            new_terms.push(reduced);
            new_terms.extend(suffix.clone());
            results.extend(reduce(sem, new_terms));
        }
    }
    results
}

/// Parse `input` and return all unique interpretations.
///
/// Mirrors `Montague.getAllParses`.
pub fn get_all_parses<A, T, X>(sem: &Semantics<A, T, X>, input: &str) -> Vec<X>
where
    A: Clone,
    T: LatticeOrd + Clone,
    X: Clone + PartialEq,
{
    let mut results = Vec::new();
    for word_sequence in annotate(sem, input) {
        for parse in reduce(sem, word_sequence) {
            if !results.contains(&parse) {
                results.push(parse);
            }
        }
    }
    results
}

/// Parse `input` and return the first interpretation, if any.
///
/// Mirrors `Montague.getParse`.
pub fn get_parse<A, T, X>(sem: &Semantics<A, T, X>, input: &str) -> Option<X>
where
    A: Clone,
    T: LatticeOrd + Clone,
    X: Clone + PartialEq,
{
    get_all_parses(sem, input).into_iter().next()
}

/// Compute the cartesian product of a list of option-lists.
fn cartesian_product<T: Clone>(lists: Vec<Vec<T>>) -> Vec<Vec<T>> {
    lists.into_iter().fold(vec![vec![]], |acc, list| {
        acc.into_iter()
            .flat_map(|prefix| {
                list.iter().map(move |item| {
                    let mut row = prefix.clone();
                    row.push(item.clone());
                    row
                })
            })
            .collect()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use display::{display_as_prolog_term, display_as_sexp};

    // ---- Types ----

    #[derive(Debug, Clone, PartialEq, Eq)]
    #[allow(dead_code)]
    enum BT {
        Sentence,
        Noun,
        Adjective,
        Person,
        Thing,
    }

    impl LatticeOrd for BT {
        fn leq(&self, other: &Self) -> bool {
            match (self, other) {
                (BT::Person, BT::Noun) | (BT::Thing, BT::Noun) => true,
                (a, b) => a == b,
            }
        }
    }

    // ---- Atoms ----

    #[derive(Debug, Clone, PartialEq)]
    enum BA {
        // Proper nouns
        Nate,
        Rick,
        Socrates,
        // Common nouns / things
        Cat,
        Man,
        // Copula
        Is,
        // Indefinite article
        A,
        // Predicates / adjectives
        Happy,
        Mortal,
        // Used inside lambda terms only:
        All(String, Box<BA>),
        Implies(Box<BA>, Box<BA>),
        AVar(String),
    }

    impl std::fmt::Display for BA {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                BA::Nate => write!(f, "nate"),
                BA::Rick => write!(f, "rick"),
                BA::Socrates => write!(f, "socrates"),
                BA::Cat => write!(f, "cat"),
                BA::Man => write!(f, "man"),
                BA::Is => write!(f, "is"),
                BA::A => write!(f, "a"),
                BA::Happy => write!(f, "happy"),
                BA::Mortal => write!(f, "mortal"),
                BA::All(v, body) => write!(f, "∀{}.{}", v, body),
                BA::Implies(a, b) => write!(f, "({} → {})", a, b),
                BA::AVar(s) => write!(f, "{}", s),
            }
        }
    }

    type LT = LambekType<BT>;

    fn basic(t: BT) -> LT {
        LT::Basic(t)
    }
    fn right(x: LT, y: LT) -> LT {
        LT::RightArrow(Box::new(x), Box::new(y))
    }
    fn left(x: LT, y: LT) -> LT {
        LT::LeftArrow(Box::new(x), Box::new(y))
    }

    /// Type of `Is`/`Are`: `(Noun → Sentence) ← Adjective`
    fn is_type() -> LT {
        left(right(basic(BT::Noun), basic(BT::Sentence)), basic(BT::Adjective))
    }

    /// Type of `A` (indefinite article): `Adjective ← Noun`
    ///
    /// "a man" absorbs the common noun on the right and produces an adjectival
    /// predicate that can fill the argument slot of "is".
    fn a_type() -> LT {
        left(basic(BT::Adjective), basic(BT::Noun))
    }

    /// Type of `every`/`all`: `(Sentence ← (Noun → Sentence)) ← Noun`
    ///
    /// Step 1: absorbs a common noun (Noun) on the right → generalized quantifier GQ
    /// Step 2: GQ absorbs a verb-phrase predicate (Noun → Sentence) on the right → Sentence
    fn every_type() -> LT {
        left(
            left(basic(BT::Sentence), right(basic(BT::Noun), basic(BT::Sentence))),
            basic(BT::Noun),
        )
    }

    /// The standard Montague lambda for "every"/"all":
    /// `λp. λq. ∀x. p(x) → q(x)`
    fn every_lambda() -> Term<BA> {
        Term::Lambda(
            "p".into(),
            Box::new(Term::Lambda(
                "q".into(),
                Box::new(Term::Atom(BA::All(
                    "x".into(),
                    Box::new(BA::Implies(
                        Box::new(BA::AVar("p(x)".into())),
                        Box::new(BA::AVar("q(x)".into())),
                    )),
                ))),
            )),
        )
    }

    /// Semantics record for all atom-only sentences.
    ///
    /// Lambda terms ("every", "all") are not handled here because `annotate`
    /// can only type `Atom` terms via `type_of_atom`. Use `annotate_test` for
    /// sentences that contain quantifiers.
    fn make_sem() -> Semantics<BA, BT, AnnotatedTerm<BA, BT>> {
        Semantics::new(
            |atom: &BA| match atom {
                BA::Nate | BA::Rick | BA::Socrates | BA::Cat => vec![basic(BT::Person)],
                BA::Man => vec![basic(BT::Noun)],
                BA::Is => vec![is_type()],
                BA::A => vec![a_type()],
                BA::Happy | BA::Mortal => vec![basic(BT::Adjective)],
                _ => vec![],
            },
            |word: &str| match word {
                "nate" | "nathan" => vec![Term::Atom(BA::Nate)],
                "rick" => vec![Term::Atom(BA::Rick)],
                "socrates" => vec![Term::Atom(BA::Socrates)],
                "cat" => vec![Term::Atom(BA::Cat)],
                "man" | "men" => vec![Term::Atom(BA::Man)],
                "is" | "are" => vec![Term::Atom(BA::Is)],
                "a" => vec![Term::Atom(BA::A)],
                "happy" => vec![Term::Atom(BA::Happy)],
                "mortal" => vec![Term::Atom(BA::Mortal)],
                _ => vec![],
            },
            |at| at,
        )
    }

    /// Full-lexicon annotate that types every word — including lambda terms like
    /// "every"/"all" whose types cannot be recovered from `type_of_atom` alone.
    ///
    /// Returns every combination of per-word annotated terms (cartesian product).
    fn annotate_test(input: &str) -> Vec<Vec<AnnotatedTerm<BA, BT>>> {
        let words: Vec<String> = input
            .split_whitespace()
            .map(|w| w.to_lowercase())
            .collect();

        let per_word: Vec<Vec<AnnotatedTerm<BA, BT>>> = words
            .iter()
            .map(|w| {
                let pairs: Vec<(Term<BA>, LT)> = match w.as_str() {
                    "nate" | "nathan" => vec![(Term::Atom(BA::Nate), basic(BT::Person))],
                    "rick" => vec![(Term::Atom(BA::Rick), basic(BT::Person))],
                    "socrates" => vec![(Term::Atom(BA::Socrates), basic(BT::Person))],
                    "cat" => vec![(Term::Atom(BA::Cat), basic(BT::Person))],
                    "man" | "men" => vec![(Term::Atom(BA::Man), basic(BT::Noun))],
                    "is" | "are" => vec![(Term::Atom(BA::Is), is_type())],
                    "a" => vec![(Term::Atom(BA::A), a_type())],
                    "happy" => vec![(Term::Atom(BA::Happy), basic(BT::Adjective))],
                    "mortal" => vec![(Term::Atom(BA::Mortal), basic(BT::Adjective))],
                    "every" | "all" => vec![(every_lambda(), every_type())],
                    _ => vec![],
                };
                pairs
                    .into_iter()
                    .map(|(term, ty)| AnnotatedTerm { term, ty })
                    .collect()
            })
            .collect();

        cartesian_product(per_word)
    }

    /// Run `reduce` over all annotated sequences, dedup by term, return unique results.
    fn parse_all(sem: &Semantics<BA, BT, AnnotatedTerm<BA, BT>>, input: &str)
        -> Vec<AnnotatedTerm<BA, BT>>
    {
        let mut results: Vec<AnnotatedTerm<BA, BT>> = Vec::new();
        for seq in annotate_test(input) {
            for r in reduce(sem, seq) {
                if !results.iter().any(|x| x.term == r.term) {
                    results.push(r);
                }
            }
        }
        results
    }

    // ---- Tests ----

    #[test]
    fn test_nate_is_happy() {
        let sem = make_sem();
        let result = get_parse(&sem, "nate is happy");
        let at = result.expect("should parse");
        assert_eq!(at.ty, basic(BT::Sentence));
        assert_eq!(
            at.term,
            Term::App(
                Box::new(Term::Atom(BA::Is)),
                vec![Term::Atom(BA::Happy), Term::Atom(BA::Nate)]
            )
        );
    }

    #[test]
    fn test_nathan_is_happy_same_as_nate() {
        let sem = make_sem();
        let r1 = get_parse(&sem, "nate is happy");
        let r2 = get_parse(&sem, "nathan is happy");
        assert_eq!(r1.map(|a| a.term), r2.map(|a| a.term));
    }

    #[test]
    fn test_uniqueness() {
        let sem = make_sem();
        let parses = get_all_parses(&sem, "nate is happy");
        assert_eq!(parses.len(), 1, "should have exactly one parse");
    }

    #[test]
    fn test_parse_failure() {
        let sem = make_sem();
        let result = get_parse(&sem, "happy nate");
        assert!(result.is_none(), "adjective before noun should not parse");
    }

    #[test]
    fn test_quantifier_every_cat_is_happy() {
        let sem = make_sem();
        let results = parse_all(&sem, "every cat is happy");
        assert!(!results.is_empty(), "should produce at least one parse");
        let at = &results[0];
        assert_eq!(at.ty, basic(BT::Sentence));
        // Outermost term is App(every_lambda, [Cat, predicate])
        match &at.term {
            Term::App(f, args) => {
                assert!(
                    matches!(f.as_ref(), Term::Lambda(_, _)),
                    "head should be the every lambda"
                );
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], Term::Atom(BA::Cat));
            }
            _ => panic!("expected App at top level, got {:?}", at.term),
        }
    }

    // ---- Syllogism tests ----

    /// "Socrates is mortal" — simple predication, same structure as "nate is happy".
    #[test]
    fn test_socrates_is_mortal() {
        let sem = make_sem();
        let result = get_parse(&sem, "socrates is mortal");
        let at = result.expect("should parse");
        assert_eq!(at.ty, basic(BT::Sentence));
        assert_eq!(
            at.term,
            Term::App(
                Box::new(Term::Atom(BA::Is)),
                vec![Term::Atom(BA::Mortal), Term::Atom(BA::Socrates)]
            )
        );
    }

    /// "Socrates is a man" — article "a" bridges the common-noun slot of "is".
    ///
    /// Reduction:
    ///   a (Adj←N) + man (N) → App(A,[Man]) : Adj
    ///   is ((N→S)←Adj) + App(A,[Man]) → App(Is,[App(A,[Man])]) : N→S
    ///   Socrates (Person≤N) + App(Is,[App(A,[Man])]) → App(Is,[App(A,[Man]),Socrates]) : S
    #[test]
    fn test_socrates_is_a_man() {
        let sem = make_sem();
        let result = get_parse(&sem, "socrates is a man");
        let at = result.expect("should parse");
        assert_eq!(at.ty, basic(BT::Sentence));
        assert_eq!(
            at.term,
            Term::App(
                Box::new(Term::Atom(BA::Is)),
                vec![
                    Term::App(Box::new(Term::Atom(BA::A)), vec![Term::Atom(BA::Man)]),
                    Term::Atom(BA::Socrates),
                ]
            )
        );
    }

    /// "All men are mortal" — universal quantifier, the core of the Aristotelian syllogism.
    ///
    /// Reduction:
    ///   are + mortal → App(Is,[Mortal]) : N→S
    ///   all + men → App(every_lambda,[Man]) : GQ  (GQ = S←(N→S))
    ///   GQ + App(Is,[Mortal]) → App(every_lambda,[Man,App(Is,[Mortal])]) : S
    ///
    /// The resulting term encodes `∀x. man(x) → mortal(x)`.
    #[test]
    fn test_all_men_are_mortal() {
        let sem = make_sem();
        let results = parse_all(&sem, "all men are mortal");
        assert!(!results.is_empty(), "should produce at least one parse");

        let at = &results[0];
        assert_eq!(at.ty, basic(BT::Sentence));

        // Top level: App(every_lambda, [Man, App(Is,[Mortal])])
        match &at.term {
            Term::App(f, args) => {
                assert!(
                    matches!(f.as_ref(), Term::Lambda(_, _)),
                    "head should be the universal quantifier lambda (λp.λq.∀x. p(x)→q(x))"
                );
                assert_eq!(args.len(), 2, "should have two arguments: the noun and the predicate");
                // First argument: the noun restriction "man"
                assert_eq!(args[0], Term::Atom(BA::Man));
                // Second argument: the nuclear scope "are mortal" = App(Is,[Mortal])
                assert_eq!(
                    args[1],
                    Term::App(
                        Box::new(Term::Atom(BA::Is)),
                        vec![Term::Atom(BA::Mortal)]
                    )
                );
            }
            _ => panic!("expected App at top level, got {:?}", at.term),
        }
    }

    // ---- Display tests ----

    #[test]
    fn test_display_prolog() {
        let term = Term::App(
            Box::new(Term::Atom(BA::Is)),
            vec![Term::Atom(BA::Happy), Term::Atom(BA::Nate)],
        );
        assert_eq!(display_as_prolog_term(&term), "is(happy,nate)");
    }

    #[test]
    fn test_display_sexp() {
        let term = Term::App(
            Box::new(Term::Atom(BA::Is)),
            vec![Term::Atom(BA::Happy), Term::Atom(BA::Nate)],
        );
        assert_eq!(display_as_sexp(&term), "(is happy nate)");
    }
}
