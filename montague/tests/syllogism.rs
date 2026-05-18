//! Integration test: parse English premises with Montague semantics, convert the
//! resulting logical forms to Prolog clauses, load them into Scryer Prolog, and
//! verify that the classical syllogism inference holds.
//!
//! Pipeline:
//!   English sentence
//!     → Montague reduction → AnnotatedTerm<BA, BT>
//!       → Prolog clause string
//!         → Scryer Prolog machine
//!           → query mortal(socrates) → LeafAnswer::True

use montague::{reduce, AnnotatedTerm, LambekType, LatticeOrd, Term};
use scryer_prolog::{LeafAnswer, MachineBuilder};

// Disambiguate from scryer_prolog::Term
type MTerm<A> = Term<A>;
type MLT<T> = LambekType<T>;
type MAT<A, T> = AnnotatedTerm<A, T>;

// ---------------------------------------------------------------------------
// Base types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
enum BT {
    Sentence,
    Noun,
    Adjective,
    Person,
}

impl LatticeOrd for BT {
    fn leq(&self, other: &Self) -> bool {
        match (self, other) {
            (BT::Person, BT::Noun) => true,
            (a, b) => a == b,
        }
    }
}

// ---------------------------------------------------------------------------
// Atoms
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum BA {
    Socrates,
    Man,
    Is,
    A,
    Mortal,
    // Used only inside the quantifier lambda term:
    All(String, Box<BA>),
    Implies(Box<BA>, Box<BA>),
    AVar(String),
}

// ---------------------------------------------------------------------------
// Type helpers
// ---------------------------------------------------------------------------

fn basic(t: BT) -> MLT<BT> {
    MLT::Basic(t)
}
fn right(x: MLT<BT>, y: MLT<BT>) -> MLT<BT> {
    MLT::RightArrow(Box::new(x), Box::new(y))
}
fn left(x: MLT<BT>, y: MLT<BT>) -> MLT<BT> {
    MLT::LeftArrow(Box::new(x), Box::new(y))
}

/// `(Noun → Sentence) ← Adjective`
fn is_type() -> MLT<BT> {
    left(right(basic(BT::Noun), basic(BT::Sentence)), basic(BT::Adjective))
}

/// `Adjective ← Noun`  ("a man" → adjectival predicate)
fn a_type() -> MLT<BT> {
    left(basic(BT::Adjective), basic(BT::Noun))
}

/// `(Sentence ← (Noun → Sentence)) ← Noun`
fn every_type() -> MLT<BT> {
    left(
        left(basic(BT::Sentence), right(basic(BT::Noun), basic(BT::Sentence))),
        basic(BT::Noun),
    )
}

/// `λp. λq. ∀x. p(x) → q(x)` — the standard Montague analysis of "every"/"all".
fn every_lambda() -> MTerm<BA> {
    MTerm::Lambda(
        "p".into(),
        Box::new(MTerm::Lambda(
            "q".into(),
            Box::new(MTerm::Atom(BA::All(
                "x".into(),
                Box::new(BA::Implies(
                    Box::new(BA::AVar("p(x)".into())),
                    Box::new(BA::AVar("q(x)".into())),
                )),
            ))),
        )),
    )
}

// ---------------------------------------------------------------------------
// Lexicon — maps surface words to (term, type) pairs
// ---------------------------------------------------------------------------

fn lex(word: &str) -> Vec<MAT<BA, BT>> {
    let pairs: Vec<(MTerm<BA>, MLT<BT>)> = match word {
        "socrates" => vec![(MTerm::Atom(BA::Socrates), basic(BT::Person))],
        "man" | "men" => vec![(MTerm::Atom(BA::Man), basic(BT::Noun))],
        "is" | "are" => vec![(MTerm::Atom(BA::Is), is_type())],
        "a" => vec![(MTerm::Atom(BA::A), a_type())],
        "mortal" => vec![(MTerm::Atom(BA::Mortal), basic(BT::Adjective))],
        "all" | "every" => vec![(every_lambda(), every_type())],
        _ => vec![],
    };
    pairs
        .into_iter()
        .map(|(term, ty)| MAT { term, ty })
        .collect()
}

// ---------------------------------------------------------------------------
// Annotation — builds every combination of per-word annotated terms
// ---------------------------------------------------------------------------

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

fn annotate(input: &str) -> Vec<Vec<MAT<BA, BT>>> {
    let per_word: Vec<Vec<MAT<BA, BT>>> = input
        .split_whitespace()
        .map(|w| lex(&w.to_lowercase()))
        .collect();
    cartesian_product(per_word)
}

// ---------------------------------------------------------------------------
// Parsing — reduce all annotated sequences, collect unique sentence-type results
// ---------------------------------------------------------------------------

fn parse(input: &str) -> Vec<MAT<BA, BT>> {
    // A stub Semantics whose only job is to call `interp = identity`.
    let sem = montague::Semantics::new(
        |_: &BA| vec![],
        |_: &str| vec![],
        |at: MAT<BA, BT>| at,
    );

    let mut results: Vec<MAT<BA, BT>> = Vec::new();
    for seq in annotate(input) {
        for r in reduce(&sem, seq) {
            if r.ty == basic(BT::Sentence) && !results.iter().any(|x| x.term == r.term) {
                results.push(r);
            }
        }
    }
    results
}

// ---------------------------------------------------------------------------
// Montague term → Prolog clause string
// ---------------------------------------------------------------------------

/// Map a leaf atom to its Prolog-safe name.
fn atom_name(a: &BA) -> Option<&'static str> {
    match a {
        BA::Socrates => Some("socrates"),
        BA::Man => Some("man"),
        BA::Mortal => Some("mortal"),
        _ => None,
    }
}

/// `App(Is, [pred])` with one argument is a verb-phrase predicate.
/// Extract the Prolog predicate name.
fn vp_pred_name(t: &MTerm<BA>) -> Option<&'static str> {
    match t {
        MTerm::App(f, args)
            if matches!(f.as_ref(), MTerm::Atom(BA::Is)) && args.len() == 1 =>
        {
            match &args[0] {
                MTerm::Atom(a) => atom_name(a),
                _ => None,
            }
        }
        _ => None,
    }
}

fn is_quantifier_lambda(t: &MTerm<BA>) -> bool {
    matches!(t, MTerm::Lambda(p, _) if p == "p")
}

/// Convert an annotated Montague sentence to a Prolog clause string.
///
/// - `App(Is, [adj, subj])` → `adj(subj).`
/// - `App(Is, [App(A, [noun]), subj])` → `noun(subj).`
/// - `App(∀-lambda, [noun, vp])` → `vp_pred(X) :- noun(X).`
fn to_prolog_clause(at: &MAT<BA, BT>) -> Option<String> {
    match &at.term {
        // Predication: App(Is, [predicate_or_a_noun, subject])
        MTerm::App(f, args)
            if matches!(f.as_ref(), MTerm::Atom(BA::Is)) && args.len() == 2 =>
        {
            let subj = match &args[1] {
                MTerm::Atom(a) => atom_name(a)?,
                _ => return None,
            };
            let pred: String = match &args[0] {
                // is(adj, subj)  →  adj(subj).
                MTerm::Atom(a) => atom_name(a)?.into(),
                // is(a(noun), subj)  →  noun(subj).
                MTerm::App(f2, a_args)
                    if matches!(f2.as_ref(), MTerm::Atom(BA::A)) && a_args.len() == 1 =>
                {
                    match &a_args[0] {
                        MTerm::Atom(a) => atom_name(a)?.into(),
                        _ => return None,
                    }
                }
                _ => return None,
            };
            Some(format!("{}({}).", pred, subj))
        }
        // Universal quantification: App(∀-lambda, [noun_restriction, verb_phrase])
        MTerm::App(f, args) if is_quantifier_lambda(f) && args.len() == 2 => {
            let noun = match &args[0] {
                MTerm::Atom(a) => atom_name(a)?,
                _ => return None,
            };
            let pred = vp_pred_name(&args[1])?;
            Some(format!("{}(X) :- {}(X).", pred, noun))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// Parse a sentence and expect exactly one Prolog clause back.
fn parse_to_clause(sentence: &str) -> String {
    let results = parse(sentence);
    assert!(!results.is_empty(), "no parse for: {sentence:?}");
    to_prolog_clause(&results[0])
        .unwrap_or_else(|| panic!("could not convert to Prolog: {sentence:?}"))
}

#[test]
fn test_syllogism_inference() {
    // --- Step 1: parse premises into Prolog clauses ---

    // "Socrates is a man"  →  man(socrates).
    let fact_man = parse_to_clause("socrates is a man");
    assert_eq!(fact_man, "man(socrates).");

    // "All men are mortal"  →  mortal(X) :- man(X).
    let rule_mortal = parse_to_clause("all men are mortal");
    assert_eq!(rule_mortal, "mortal(X) :- man(X).");

    // --- Step 2: load into Scryer Prolog ---

    let program = format!("{}\n{}", fact_man, rule_mortal);
    let mut machine = MachineBuilder::default().build();
    machine.load_module_string("syllogism", program);

    // --- Step 3: verify inference ---

    // mortal(socrates) was never stated directly; it must be *inferred*.
    let answers: Vec<LeafAnswer> = machine
        .run_query("mortal(socrates).")
        .collect::<Result<_, _>>()
        .expect("query error");

    assert_eq!(
        answers,
        vec![LeafAnswer::True],
        "Prolog should infer mortal(socrates) from man(socrates) and mortal(X):-man(X)"
    );
}

/// Verify that a fact about Socrates being mortal is also directly assertable.
#[test]
fn test_direct_predication_clause() {
    let clause = parse_to_clause("socrates is mortal");
    assert_eq!(clause, "mortal(socrates).");

    let mut machine = MachineBuilder::default().build();
    machine.load_module_string("direct", clause);

    let answers: Vec<LeafAnswer> = machine
        .run_query("mortal(socrates).")
        .collect::<Result<_, _>>()
        .expect("query error");

    assert_eq!(answers, vec![LeafAnswer::True]);
}
