//! Compare DFS vs CKY chart on a small ambiguous sentence.
//!
//! Tracks:
//! 1. CKY fill time — guards chart-fill regressions.
//! 2. CKY vs DFS on the canonical syllogism-sized input.

use criterion::{criterion_group, criterion_main, Criterion};
use montague_core::{
    annotate,
    chart::Chart,
    reduce,
    reduction::{ReductionCtx, ReductionEngine},
    subtyping::SubtypeLattice,
    types::{AnnotatedTerm, AtomType, LambekType, Term},
    Semantics,
};

// Shorthand for the annotate-term type.
type AT = AnnotatedTerm<&'static str, AtomType>;
type Sem = Semantics<&'static str, AtomType, AT>;

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

/// Build a tiny lexicon: N and N\S.
fn fixtures() -> (
    Sem,
    ReductionEngine<&'static str, AtomType>,
    SubtypeLattice<AtomType>,
    LambekType<AtomType>,
) {
    let sem = Semantics::new(
        |a: &&str| match *a {
            "n" => vec![LambekType::Basic(AtomType::new("N"))],
            "fun" => vec![LambekType::RightArrow(
                Box::new(LambekType::Basic(AtomType::new("N"))),
                Box::new(LambekType::Basic(AtomType::new("S"))),
            )],
            _ => vec![],
        },
        |w: &str| match w {
            "n" => vec![Term::Atom("n")],
            "fun" => vec![Term::Atom("fun")],
            _ => vec![],
        },
        |at| at,
    );
    let engine = ReductionEngine::standard();
    let lat = SubtypeLattice::new();
    let goal = LambekType::Basic(AtomType::new("S"));
    (sem, engine, lat, goal)
}

/// Lexically annotate an input string into per-word token vectors.
fn tokenize(sem: &Sem, input: &str) -> Vec<Vec<AT>> {
    input
        .split_whitespace()
        .map(|w| {
            let terms = (sem.parse_term)(w);
            terms
                .into_iter()
                .flat_map(|t| {
                    let types = match &t {
                        Term::Atom(a) => (sem.type_of_atom)(a),
                        _ => vec![],
                    };
                    types
                        .into_iter()
                        .map(move |ty| AnnotatedTerm {
                            term: t.clone(),
                            ty,
                        })
                        .collect::<Vec<_>>()
                })
                .collect()
        })
        .collect()
}

// ---------------------------------------------------------------------------
// DFS bench
// ---------------------------------------------------------------------------

fn bench_dfs(c: &mut Criterion) {
    let (sem, engine, lat, goal) = fixtures();
    let ctx = ReductionCtx::new(&lat);
    let input = "n fun n fun";
    let expected = {
        let mut results = Vec::new();
        for seq in annotate(&sem, input) {
            for r in reduce(&engine, &ctx, &sem, seq) {
                if r.ty.leq(&goal, &lat) {
                    results.push(r);
                }
            }
        }
        results.len()
    };

    c.bench_function("dfs_4_words", |b| {
        b.iter(|| {
            let mut results = Vec::new();
            for seq in annotate(&sem, input) {
                for r in reduce(&engine, &ctx, &sem, seq) {
                    let keep =
                        r.ty.leq(std::hint::black_box(&goal), std::hint::black_box(&lat));
                    if keep {
                        results.push(r);
                    }
                }
            }
            assert_eq!(results.len(), expected, "DFS parse count changed");
            results
        })
    });
}

// ---------------------------------------------------------------------------
// CKY bench
// ---------------------------------------------------------------------------

fn bench_cky(c: &mut Criterion) {
    let (sem, engine, lat, goal) = fixtures();
    let ctx = ReductionCtx::new(&lat);
    let input = "n fun n fun";
    let tokens = tokenize(&sem, input);
    let expected = {
        let mut chart = Chart::from_tokens(tokens.clone());
        chart.fill(&engine, &ctx);
        chart.complete_parses(&goal, &lat).len()
    };

    c.bench_function("cky_4_words", |b| {
        b.iter(|| {
            let mut chart = Chart::from_tokens(tokens.clone());
            chart.fill(std::hint::black_box(&engine), std::hint::black_box(&ctx));
            let results: Vec<_> = chart
                .complete_parses(std::hint::black_box(&goal), std::hint::black_box(&lat))
                .into_iter()
                .map(|d| (d.term.clone(), d.ty.clone()))
                .collect();
            assert_eq!(results.len(), expected, "CKY parse count changed");
            results
        })
    });
}

// ---------------------------------------------------------------------------
// Subtype lattice benchmark (hot path inside every cell merge)
// ---------------------------------------------------------------------------

fn bench_lattice_lookup(c: &mut Criterion) {
    // Build a moderate lattice.
    let mut lat: SubtypeLattice<u32> = SubtypeLattice::new();
    for i in 0..20 {
        for j in (i + 1)..20 {
            if j - i <= 3 {
                lat.add_subtype(i, j);
            }
        }
    }

    c.bench_function("lattice_leq_hit", |b| {
        b.iter(|| lat.leq(std::hint::black_box(&0), std::hint::black_box(&19)))
    });

    c.bench_function("lattice_leq_miss", |b| {
        b.iter(|| lat.leq(std::hint::black_box(&19), std::hint::black_box(&0)))
    });
}

criterion_group!(benches, bench_dfs, bench_cky, bench_lattice_lookup);
criterion_main!(benches);
