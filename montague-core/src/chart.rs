//! CKY chart parser with packed forest and subtype-aware indexing.
//!
//! Bottom-up: fills a triangular table of cells where cell `(i, j)` holds every
//! derivation derivable from span `i..j`, packed by type. The chart dispatches
//! [`ReductionRule`]s using their [`RuleApplicability`] hints to skip rules that
//! cannot match a given cell pair.
//!
//! Long-distance rules ([`ChartPostpass`]) run over the completed chart.
//!
//! [`ReductionRule`]: crate::reduction::ReductionRule
//! [`RuleApplicability`]: crate::reduction::RuleApplicability

use std::collections::HashMap;
use std::hash::Hash;

use crate::reduction::{ReductionCtx, ReductionEngine, RuleApplicability, TypeShape};
use crate::subtyping::SubtypeLattice;
use crate::types::{AnnotatedTerm, LambekType, Term, TypeCheck};

/// Maximum category depth allowed in the chart.  Composition rules can build
/// unboundedly deep categories; this guard keeps the chart finite.
const MAX_DEPTH: usize = 8;

// ---------------------------------------------------------------------------
// Derivation store
// ---------------------------------------------------------------------------

/// Opaque identifier for a derivation within a [`Chart`].
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DerivationId(pub(crate) u32);

/// A single derivation — the result of applying a rule or a leaf (lexical) entry.
#[derive(Debug, Clone)]
pub struct Derivation<A, T> {
    /// The syntactic term.
    pub term: Term<A>,
    /// The Lambek type.
    pub ty: LambekType<T>,
    /// `None` for leaf entries, `Some(i,j)` span pair for binary rule applications.
    pub left_span: Option<(usize, usize)>,
    pub right_span: Option<(usize, usize)>,
}

// ---------------------------------------------------------------------------
// Cell
// ---------------------------------------------------------------------------

/// All derivations for a single span `(i, j)`.
pub struct Cell<A, T> {
    /// Flat store of all derivations in this cell.
    all: Vec<Derivation<A, T>>,
    /// `by_type[ty]` = list of derivation indices into `all`.
    by_type: HashMap<LambekType<T>, Vec<DerivationId>>,
    /// `by_basic_head[name]` = list of `LambekType<T>` values in this cell
    /// whose top-level constructor is `Basic` with that head name.
    /// Keyed by [`TypeCheck::head_name`] so that `NP[Person]` and `NP[Animate]`
    /// share the same bucket.
    by_basic_head: HashMap<String, Vec<LambekType<T>>>,
}

impl<A, T> Cell<A, T>
where
    A: Clone + Eq + Hash,
    T: TypeCheck,
{
    fn new() -> Self {
        Cell {
            all: Vec::new(),
            by_type: HashMap::new(),
            by_basic_head: HashMap::new(),
        }
    }

    /// Insert a derivation, returning its `DerivationId`.
    fn insert(&mut self, d: Derivation<A, T>) -> DerivationId {
        let id = DerivationId(self.all.len() as u32);
        let ty = d.ty.clone();
        self.by_type.entry(ty.clone()).or_default().push(id);
        if let LambekType::Basic(ref b) = &ty {
            if let Some(name) = b.head_name() {
                self.by_basic_head.entry(name).or_default().push(ty);
            }
        }
        self.all.push(d);
        id
    }

    /// Derivation by id.
    pub fn get(&self, id: DerivationId) -> &Derivation<A, T> {
        &self.all[id.0 as usize]
    }

    /// Number of derivation entries.
    pub fn len(&self) -> usize {
        self.all.len()
    }

    /// True if no derivations are stored in this cell.
    pub fn is_empty(&self) -> bool {
        self.all.is_empty()
    }

    /// All types in this cell.
    pub fn types(&self) -> impl Iterator<Item = &LambekType<T>> {
        self.by_type.keys()
    }

    /// All derivation ids for a given type in this cell.
    pub fn derivations_of(&self, ty: &LambekType<T>) -> Option<&[DerivationId]> {
        self.by_type.get(ty).map(|v| v.as_slice())
    }
}

// ---------------------------------------------------------------------------
// Chart
// ---------------------------------------------------------------------------

/// Packed parse forest over an input sequence of length `n`.
pub struct Chart<A, T> {
    /// `cells[i][j]` = derivations for span `(i, j)`. `i` and `j` are indices
    /// between 0 and n, inclusive; `cells[i][j]` is `None` for spans that were
    /// never filled.
    cells: Vec<Vec<Option<Cell<A, T>>>>,
    /// Input tokens — cached from the annotate step for reference.
    tokens: Vec<Vec<AnnotatedTerm<A, T>>>,
    n: usize,
}

impl<A, T> Chart<A, T>
where
    A: Clone + Eq + Hash + Send + Sync + 'static,
    T: TypeCheck + Send + Sync + 'static,
{
    /// Build a chart of size `n` from per-word lexical entries.
    ///
    /// `token_sequences[word_index]` = all annotated terms for that word.
    pub fn from_tokens(tokens: Vec<Vec<AnnotatedTerm<A, T>>>) -> Self {
        let n = tokens.len();
        let cells: Vec<Vec<Option<Cell<A, T>>>> =
            (0..=n).map(|_| (0..=n).map(|_| None).collect()).collect();
        Chart { cells, tokens, n }
    }

    /// Number of input tokens.
    pub fn len(&self) -> usize {
        self.n
    }

    pub fn is_empty(&self) -> bool {
        self.n == 0
    }

    /// Fill the chart bottom-up using the given engine.
    pub fn fill(&mut self, engine: &ReductionEngine<A, T>, ctx: &ReductionCtx<'_, T>) {
        // Step 1: seed lexical entries for each span (i, i+1).
        for i in 0..self.n {
            let token_clones: Vec<_> = self.tokens[i].to_vec();
            let cell = self.cell_mut(i, i + 1);
            for at in &token_clones {
                cell.insert(Derivation {
                    term: at.term.clone(),
                    ty: at.ty.clone(),
                    left_span: None,
                    right_span: None,
                });
            }
        }

        // Step 2: bottom-up fill for spans of length 2..n.
        for span_len in 2..=self.n {
            for i in 0..=self.n - span_len {
                let j = i + span_len;
                let mut local_results: Vec<AnnotatedTerm<A, T>> = Vec::new();

                for k in (i + 1)..j {
                    // Snapshot failure count so we can annotate new failures
                    // with this cell's coordinates.
                    let fail_count_before = ctx.failures.borrow().len();

                    // Collect type-derivation pairs into locals so the immutable
                    // borrows from cell_ref drop before the mutable cell_mut below.
                    let left_pairs: Vec<_> = {
                        let cell = self.cell_ref(i, k);
                        cell.by_type
                            .iter()
                            .filter(|(_, ids)| !ids.is_empty())
                            .map(|(ty, ids)| {
                                let d = cell.get(ids[0]);
                                (ty.clone(), d.term.clone())
                            })
                            .collect()
                    };
                    let right_pairs: Vec<_> = {
                        let cell = self.cell_ref(k, j);
                        cell.by_type
                            .iter()
                            .filter(|(_, ids)| !ids.is_empty())
                            .map(|(ty, ids)| {
                                let d = cell.get(ids[0]);
                                (ty.clone(), d.term.clone())
                            })
                            .collect()
                    };

                    for rule in engine.rules_ref() {
                        let hint = rule.applicability();
                        for (lt, lterm) in &left_pairs {
                            if !left_matches_hint(&hint, lt) {
                                continue;
                            }
                            for (rt, rterm) in &right_pairs {
                                if !right_matches_hint(&hint, rt) {
                                    continue;
                                }
                                let left_at = AnnotatedTerm {
                                    term: lterm.clone(),
                                    ty: lt.clone(),
                                };
                                let right_at = AnnotatedTerm {
                                    term: rterm.clone(),
                                    ty: rt.clone(),
                                };
                                for reduced in rule.try_apply(ctx, &left_at, &right_at) {
                                    local_results.push(reduced);
                                }
                            }
                        }
                    }

                    // Annotate newly-collected failures with this split's coordinates.
                    let mut fails = ctx.failures.borrow_mut();
                    for t in &mut fails[fail_count_before..] {
                        t.left_span = (i, k);
                        t.right_span = (k, j);
                    }
                    drop(fails);
                }

                // Deduplicate by (term, type) before insertion.
                let cell = self.cell_mut(i, j);
                for at in &local_results {
                    let already = cell
                        .by_type
                        .get(&at.ty)
                        .map(|ids| ids.iter().any(|id| cell.get(*id).term == at.term))
                        .unwrap_or(false);
                    if !already && at.ty.depth() <= MAX_DEPTH {
                        cell.insert(Derivation {
                            term: at.term.clone(),
                            ty: at.ty.clone(),
                            left_span: Some((i, j)), // simplified — records span range only
                            right_span: None,
                        });
                    }
                }
            }
        }
    }

    /// Run postpasses on the completed chart.
    pub fn apply_postpasses(
        &mut self,
        postpasses: &[Box<dyn ChartPostpass<A, T>>],
        ctx: &ReductionCtx<'_, T>,
    ) {
        for pp in postpasses {
            pp.apply(self, ctx);
        }
    }

    /// Iterate over all complete parses of the full span `(0, n)` whose type
    /// is ≤ `goal` (by the lattice).
    pub fn complete_parses(
        &self,
        goal: &LambekType<T>,
        lat: &SubtypeLattice<T>,
    ) -> Vec<&Derivation<A, T>> {
        let cell = match self.cell_try(0, self.n) {
            Some(c) => c,
            None => return vec![],
        };
        let mut out = Vec::new();
        for (ty, ids) in &cell.by_type {
            if ty.leq(goal, lat) {
                for id in ids {
                    out.push(cell.get(*id));
                }
            }
        }
        out
    }

    /// Return all derivations that span the full input `(0, n)`, regardless of
    /// type.  Used when the goal type isn't known (e.g. Sentence vs Question).
    pub fn all_parses(&self) -> Vec<&Derivation<A, T>> {
        match self.cell_try(0, self.n) {
            Some(c) => c
                .by_type
                .values()
                .flat_map(|ids| ids.iter().map(|id| c.get(*id)))
                .collect(),
            None => vec![],
        }
    }

    // -- internal helpers ------------------------------------------------

    fn cell_ref(&self, i: usize, j: usize) -> &Cell<A, T> {
        self.cells[i][j].as_ref().unwrap()
    }

    fn cell_mut(&mut self, i: usize, j: usize) -> &mut Cell<A, T> {
        self.cells[i][j].get_or_insert_with(Cell::new)
    }

    fn cell_try(&self, i: usize, j: usize) -> Option<&Cell<A, T>> {
        self.cells[i][j].as_ref()
    }
}

fn left_matches_hint<T>(hint: &RuleApplicability, ty: &LambekType<T>) -> bool {
    match hint {
        RuleApplicability::Always => true,
        RuleApplicability::LeftMustBe(s) | RuleApplicability::Both { left: s, .. } => s.matches(ty),
        RuleApplicability::RightMustBe(_) => true,
    }
}

fn right_matches_hint<T>(hint: &RuleApplicability, ty: &LambekType<T>) -> bool {
    match hint {
        RuleApplicability::Always => true,
        RuleApplicability::RightMustBe(s) | RuleApplicability::Both { right: s, .. } => {
            s.matches(ty)
        }
        RuleApplicability::LeftMustBe(_) => true,
    }
}

// ---------------------------------------------------------------------------
// ChartPostpass
// ---------------------------------------------------------------------------

/// Set of connective shapes a postpass applies to.
#[derive(Debug, Clone)]
pub struct ConnectiveSet {
    shapes: Vec<TypeShape>,
}

impl ConnectiveSet {
    pub fn new() -> Self {
        ConnectiveSet { shapes: vec![] }
    }
}

impl Default for ConnectiveSet {
    fn default() -> Self {
        Self::new()
    }
}

impl ConnectiveSet {
    pub fn with(mut self, s: TypeShape) -> Self {
        self.shapes.push(s);
        self
    }
}

/// A rule that runs over the completed chart, adding new derivations.
///
/// Used for long-distance phenomena (extraction, scoping) that cannot be
/// handled by the bottom-up sweep alone.
pub trait ChartPostpass<A, T>: Send + Sync
where
    T: TypeCheck,
{
    fn name(&self) -> &'static str;

    /// Which connective shapes trigger this postpass.
    fn applies_to(&self) -> &ConnectiveSet;

    /// Apply the postpass to the chart, potentially adding derivations.
    fn apply(&self, chart: &mut Chart<A, T>, ctx: &ReductionCtx<'_, T>);
}

// ---------------------------------------------------------------------------
// Coordination post-pass (Step 4 — Φ rule)
// ---------------------------------------------------------------------------

/// Returns `true` if `ty` has the shape of a coordinator: `(X\X)/X` or
/// `(X/X)\X` — i.e., it takes two arguments of the same (joinable) category
/// and returns that category.
fn is_coordinator_type<T: TypeCheck>(ty: &LambekType<T>) -> bool {
    // (X\X)/X = LeftArrow(RightArrow(x1, x2), x3) where x1 ≈ x2 ≈ x3
    if let LambekType::LeftArrow(inner, arg) = ty {
        if let LambekType::RightArrow(x1, x2) = inner.as_ref() {
            return x1.as_ref() == x2.as_ref() && x2.as_ref() == arg.as_ref();
        }
    }
    // (X/X)\X = RightArrow(LeftArrow(x1, x2), x3) where x1 ≈ x2 ≈ x3
    if let LambekType::RightArrow(inner, arg) = ty {
        if let LambekType::LeftArrow(x1, x2) = inner.as_ref() {
            return x1.as_ref() == x2.as_ref() && x2.as_ref() == arg.as_ref();
        }
    }
    false
}

/// Join two Lambek types using the lattice.  Equal types trivially join.
/// Both `Basic` → delegates to `SubtypeLattice::join`.  Function types with
/// matching structure join component-wise.  Returns `None` otherwise.
fn join_types<T: TypeCheck>(
    lat: &SubtypeLattice<T>,
    a: &LambekType<T>,
    b: &LambekType<T>,
) -> Option<LambekType<T>> {
    if a == b {
        return Some(a.clone());
    }
    if let (LambekType::Basic(ba), LambekType::Basic(bb)) = (a, b) {
        return lat.join(ba, bb).map(LambekType::Basic);
    }
    match (a, b) {
        (LambekType::LeftArrow(x1, y1), LambekType::LeftArrow(x2, y2))
        | (LambekType::RightArrow(x1, y1), LambekType::RightArrow(x2, y2)) => {
            let x = join_types(lat, x1, x2)?;
            let y = join_types(lat, y1, y2)?;
            if let LambekType::LeftArrow(..) = a {
                Some(LambekType::LeftArrow(Box::new(x), Box::new(y)))
            } else {
                Some(LambekType::RightArrow(Box::new(x), Box::new(y)))
            }
        }
        _ => None,
    }
}

/// Coordination post-pass: implements the Φ rule `X CONJ X → X`.
///
/// After the binary bottom-up fill, scans the chart for coordinator tokens
/// flanked by two constituents of joinable categories.  The result category
/// is the least upper bound computed by `SubtypeLattice::join`.
pub struct CoordinationPostpass;

impl<A, T> ChartPostpass<A, T> for CoordinationPostpass
where
    A: Clone + Eq + Hash + Send + Sync + 'static,
    T: TypeCheck + Send + Sync + 'static,
{
    fn name(&self) -> &'static str {
        "coordination"
    }
    fn applies_to(&self) -> &ConnectiveSet {
        static SET: std::sync::OnceLock<ConnectiveSet> = std::sync::OnceLock::new();
        SET.get_or_init(|| {
            ConnectiveSet::new()
                .with(TypeShape::LeftArrow)
                .with(TypeShape::RightArrow)
        })
    }
    fn apply(&self, chart: &mut Chart<A, T>, ctx: &ReductionCtx<'_, T>) {
        let n = chart.len();
        // Need spans of at least 3 tokens: left + coord + right.
        for span_len in 3..=n {
            for i in 0..=n - span_len {
                let j = i + span_len;
                for k in (i + 1)..j {
                    // The coordinator must occupy exactly one token.
                    if k + 1 >= j {
                        continue;
                    }
                    // k is the middle split — check cell[k][k+1] for coordinators.
                    let coord_cell = match chart.cell_try(k, k + 1) {
                        Some(c) => c,
                        None => continue,
                    };
                    // Find a coordinator term to use for semantics.
                    let coord_term = coord_cell.by_type.iter().find_map(|(ty, ids)| {
                        if is_coordinator_type(ty) {
                            Some(coord_cell.get(ids[0]).term.clone())
                        } else {
                            None
                        }
                    });
                    let Some(conj) = coord_term else { continue };

                    // Collect left and right derivations.
                    let left_cell = match chart.cell_try(i, k) {
                        Some(c) => c,
                        None => continue,
                    };
                    let right_cell = match chart.cell_try(k + 1, j) {
                        Some(c) => c,
                        None => continue,
                    };

                    let mut new_derivs: Vec<Derivation<A, T>> = Vec::new();
                    for (lty, lids) in &left_cell.by_type {
                        for lid in lids {
                            let ld = left_cell.get(*lid);
                            for (rty, rids) in &right_cell.by_type {
                                if let Some(joined) = join_types(ctx.lattice, lty, rty) {
                                    for rid in rids {
                                        let rd = right_cell.get(*rid);
                                        let sem = Term::App(
                                            Box::new(conj.clone()),
                                            vec![ld.term.clone(), rd.term.clone()],
                                        );
                                        new_derivs.push(Derivation {
                                            term: sem,
                                            ty: joined.clone(),
                                            left_span: Some((i, j)),
                                            right_span: None,
                                        });
                                    }
                                }
                            }
                        }
                    }

                    // Insert new derivations into cell[i][j], deduplicating.
                    let target = chart.cell_mut(i, j);
                    for d in &new_derivs {
                        let already = target
                            .by_type
                            .get(&d.ty)
                            .map(|ids| ids.iter().any(|id| target.get(*id).term == d.term))
                            .unwrap_or(false);
                        if !already && d.ty.depth() <= MAX_DEPTH {
                            target.insert(Derivation {
                                term: d.term.clone(),
                                ty: d.ty.clone(),
                                left_span: Some((i, j)),
                                right_span: None,
                            });
                        }
                    }
                }
            }
        }
    }
}

// Stub ExtractionPostpass — real logic in M9.
pub struct ExtractionPostpass;

impl<A, T> ChartPostpass<A, T> for ExtractionPostpass
where
    A: Clone + Send + Sync + 'static,
    T: TypeCheck + Send + Sync + 'static,
{
    fn name(&self) -> &'static str {
        "extraction"
    }
    fn applies_to(&self) -> &ConnectiveSet {
        static SET: std::sync::OnceLock<ConnectiveSet> = std::sync::OnceLock::new();
        SET.get_or_init(|| ConnectiveSet::new().with(TypeShape::Extract))
    }
    fn apply(&self, _chart: &mut Chart<A, T>, _ctx: &ReductionCtx<'_, T>) {
        // Stub — real logic in M9.
    }
}

// Stub ScopingPostpass — real logic in M9.
pub struct ScopingPostpass;

impl<A, T> ChartPostpass<A, T> for ScopingPostpass
where
    A: Clone + Send + Sync + 'static,
    T: TypeCheck + Send + Sync + 'static,
{
    fn name(&self) -> &'static str {
        "scoping"
    }
    fn applies_to(&self) -> &ConnectiveSet {
        static SET: std::sync::OnceLock<ConnectiveSet> = std::sync::OnceLock::new();
        SET.get_or_init(|| ConnectiveSet::new().with(TypeShape::Scoped))
    }
    fn apply(&self, _chart: &mut Chart<A, T>, _ctx: &ReductionCtx<'_, T>) {
        // Stub — real logic in M9.
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use crate::SubtypeLattice;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    enum BT {
        S,
        N,
    }

    crate::impl_type_check_trivial!(BT);

    fn basic(t: BT) -> LambekType<BT> {
        LambekType::Basic(t)
    }
    fn lex_engine() -> ReductionEngine<i32, BT> {
        ReductionEngine::standard()
    }

    #[test]
    fn chart_empty_has_no_complete_parses() {
        let chart: Chart<i32, BT> = Chart::from_tokens(vec![]);
        let lat = SubtypeLattice::new();
        let parses = chart.complete_parses(&basic(BT::S), &lat);
        assert!(parses.is_empty());
    }

    #[test]
    fn single_lexical_entry_seeds_cell() {
        let at = AnnotatedTerm {
            term: Term::Atom(1),
            ty: basic(BT::N),
        };
        let mut chart = Chart::from_tokens(vec![vec![at.clone()]]);
        let lat = SubtypeLattice::new();
        let ctx = ReductionCtx::new(&lat);
        let engine = lex_engine();
        chart.fill(&engine, &ctx);
        let parses = chart.complete_parses(&basic(BT::N), &lat);
        assert_eq!(parses.len(), 1);
        assert_eq!(parses[0].term, Term::Atom(1));
        assert_eq!(parses[0].ty, basic(BT::N));
    }

    #[test]
    fn two_word_reduction_via_chart() {
        // Build a longer engine with the two absorption rules.
        let engine: ReductionEngine<i32, BT> = ReductionEngine::standard();
        // Token 0: type N, Token 1: type RightArrow(N, S) = N\S
        let at0 = AnnotatedTerm {
            term: Term::Atom(1),
            ty: basic(BT::N),
        };
        let at1 = AnnotatedTerm {
            term: Term::Atom(2),
            ty: LambekType::RightArrow(Box::new(basic(BT::N)), Box::new(basic(BT::S))),
        };
        let mut chart = Chart::from_tokens(vec![vec![at0], vec![at1]]);
        let lat = SubtypeLattice::new();
        let ctx = ReductionCtx::new(&lat);
        chart.fill(&engine, &ctx);
        let parses = chart.complete_parses(&basic(BT::S), &lat);
        assert!(!parses.is_empty(), "should parse N + N\\S → S");
        assert_eq!(parses[0].ty, basic(BT::S));
    }

    // Diff test: chart must agree with naive DFS on the same input.
    #[test]
    fn chart_matches_naive_dfs() {
        // Tiny lexicon: N and N\S.
        let sem = crate::Semantics::new(
            |a: &&str| match *a {
                "n" => vec![LambekType::Basic("N")],
                "fun" => vec![LambekType::RightArrow(
                    Box::new(LambekType::Basic("N")),
                    Box::new(LambekType::Basic("S")),
                )],
                _ => vec![],
            },
            |w: &str| match w {
                "n" => vec![Term::Atom("n")],
                "fun" => vec![Term::Atom("fun")],
                _ => vec![],
            },
            |at: AnnotatedTerm<&str, &str>| at,
        );
        let engine: ReductionEngine<&str, &str> = ReductionEngine::standard();
        let lat = SubtypeLattice::new();
        let ctx = ReductionCtx::new(&lat);
        let goal = LambekType::Basic("S");
        let input = "n fun";

        // DFS path (via crate::reduce, which is still DFS in lib.rs).
        let dfs: HashSet<_> = {
            let mut results = Vec::new();
            for seq in crate::annotate(&sem, input) {
                for r in crate::reduce(&engine, &ctx, &sem, seq) {
                    if r.ty.leq(&goal, &lat) {
                        results.push(r);
                    }
                }
            }
            results.into_iter().collect::<HashSet<_>>()
        };

        // CKY path.
        let tokens: Vec<Vec<_>> = input
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
            .collect();

        let mut chart = Chart::from_tokens(tokens);
        chart.fill(&engine, &ctx);
        let cky: HashSet<_> = {
            let pars = chart.complete_parses(&goal, &lat);
            pars.into_iter()
                .map(|d| AnnotatedTerm {
                    term: d.term.clone(),
                    ty: d.ty.clone(),
                })
                .collect()
        };

        assert_eq!(dfs, cky, "CKY chart and naive DFS must agree");
    }

    #[test]
    fn chart_fill_preserves_span_info() {
        let engine: ReductionEngine<i32, BT> = ReductionEngine::standard();
        let at0 = AnnotatedTerm {
            term: Term::Atom(1),
            ty: basic(BT::N),
        };
        let at1 = AnnotatedTerm {
            term: Term::Atom(2),
            ty: LambekType::RightArrow(Box::new(basic(BT::N)), Box::new(basic(BT::S))),
        };
        let mut chart = Chart::from_tokens(vec![vec![at0], vec![at1]]);
        let lat = SubtypeLattice::new();
        let ctx = ReductionCtx::new(&lat);
        chart.fill(&engine, &ctx);
        let parses = chart.complete_parses(&basic(BT::S), &lat);
        assert_eq!(parses.len(), 1);
        // The derivation should have a non-None span (recorded as (0, 2)).
        let d = parses[0];
        assert_eq!(d.left_span, Some((0, 2)));
    }
}
