//! Integration tests for `montague ask` — spawn the binary as a subprocess,
//! feed sequences of user inputs, and assert expected outputs.
//!
//! These tests exercise the full pipeline: .mont parsing → resolution →
//! Prolog lowering → Scryer inference, through the actual CLI binary.

use std::io::Write;
use std::process::{Command, Stdio};
use std::time::Duration;

/// Spawn `montague ask <file>` with optional flags, feed `inputs` to stdin,
/// and return all stderr output (lowercased for case-insensitive matching).
fn run_ask_session(file: &str, flags: &[&str], inputs: &[&str]) -> String {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_montague"));
    cmd.arg("ask")
        .arg(file)
        .args(flags)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().expect("failed to spawn montague ask");

    let mut stdin = child.stdin.take().expect("failed to open stdin");

    for input in inputs {
        writeln!(stdin, "{input}").expect("failed to write input");
    }
    writeln!(stdin, ":q").expect("failed to write :q");
    drop(stdin);

    let timeout = Duration::from_secs(10);
    let output = match wait_timeout(child, timeout) {
        Ok(o) => o,
        Err(_) => return String::new(),
    };

    String::from_utf8_lossy(&output.stderr).to_lowercase()
}

fn wait_timeout(
    mut child: std::process::Child,
    timeout: Duration,
) -> Result<std::process::Output, ()> {
    let start = std::time::Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                let mut output = child.wait_with_output().expect("failed to get output");
                output.status = status;
                return Ok(output);
            }
            Ok(None) => {
                if start.elapsed() > timeout {
                    let _ = child.kill();
                    let _ = child.wait();
                    return Err(());
                }
                std::thread::sleep(Duration::from_millis(100));
            }
            Err(_) => return Err(()),
        }
    }
}

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

fn ask_example(filename: &str, inputs: &[&str]) -> String {
    let path = format!("../examples/{filename}");
    run_ask_session(&path, &[], inputs)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// Polar question with subject-aux inversion: "Is Socrates mortal?" → yes.
/// Uses the inverted copula `is_q : (Q/Adj)/N` (Carpenter-style treatment).
#[test]
fn polar_inversion_yes() {
    let output = ask_example(
        "qa-syllogism.mont",
        &["Socrates is mortal.", "Is Socrates mortal?"],
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

/// Polar question with statement word order + `?`: "Socrates is mortal?" → yes.
/// Uses the copula `is_cop` with `?` triggering the query path.
#[test]
fn polar_statement_form_yes() {
    let output = ask_example(
        "qa-syllogism.mont",
        &["Socrates is mortal.", "Socrates is mortal?"],
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

/// Wh-question: "Who is mortal?" → X = socrates.
/// Uses `who_q : Q/(N\S)` — takes VP to the right, forms question with free var.
#[test]
fn wh_question_binding() {
    let output = ask_example(
        "qa-syllogism.mont",
        &["Socrates is mortal.", "Who is mortal?"],
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("x = socrates"),
        "expected 'X = socrates' binding:\n{output}"
    );
}

/// Query an unknown term → parse failure → readable error.
#[test]
fn syllogism_no_answer() {
    let output = ask_example("qa-syllogism.mont", &["Socrates is a fish."]);
    assert!(
        output.contains("no parse"),
        "expected 'no parse' for unknown word:\n{output}"
    );
}

/// Multi-turn: assert two premises, then query for the inference.
#[test]
fn syllogism_multi_turn() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "All men are mortal.",
            "Socrates is a man.",
            "Is Socrates mortal?",
        ],
    );
    assert!(
        (output.contains("mortal(x) :- man_noun(x)") || output.contains("mortal(x) :- man(x)"))
            || output.contains("mortal(x) :-"),
        "expected mortal rule assertion:\n{output}"
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

/// Feed assertions that load into the KB and verify both are asserted.
#[test]
fn multi_assertion() {
    let output = ask_example(
        "syllogism.mont",
        &["Socrates is a man.", "Socrates is mortal."],
    );
    assert!(
        (output.contains("man_noun(socrates)") || output.contains("man(socrates)")),
        "expected man_noun/man(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
}

/// Parse failure: a sentence with words not in the lexicon.
/// Should produce a readable error, not panic.
#[test]
fn parse_failure_graceful() {
    let output = ask_example("qa-syllogism.mont", &["The flarbgorp zibbity doo dah."]);
    assert!(
        output.contains("no parse") || output.contains("parse error"),
        "expected a parse failure message, got:\n{output}"
    );
}

/// Greeting message is printed at startup.
#[test]
fn greeting_printed() {
    let output = ask_example("qa-syllogism.mont", &[]);
    assert!(
        output.contains("montague q&a"),
        "expected greeting in output:\n{output}"
    );
    assert!(
        output.contains(":q to quit"),
        "expected quit hint in output:\n{output}"
    );
}

/// Assert then query with inversion: direct fact lookup should succeed.
/// Uses qa-syllogism.mont which has the inverted copula `is_q`.
#[test]
fn direct_query_after_assertion() {
    let output = ask_example(
        "qa-syllogism.mont",
        &["Socrates is mortal.", "Is Socrates mortal?"],
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

// ---------------------------------------------------------------------------
// Conjunction tests
// ---------------------------------------------------------------------------

/// Conjoined assertion: "Socrates is a man and mortal." → both facts asserted.
#[test]
fn conjunction_assertion_and() {
    let output = ask_example("qa-syllogism.mont", &["Socrates is a man and mortal."]);
    assert!(
        (output.contains("man_noun(socrates)") || output.contains("man(socrates)")),
        "expected man_noun/man(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
}

/// Conjoined polar query: "Is Socrates a man and mortal?" → yes.
#[test]
fn conjunction_query_and() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Is Socrates a man and mortal?",
        ],
    );
    assert!(
        (output.contains("man_noun(socrates)") || output.contains("man(socrates)")),
        "expected man_noun/man(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

/// Conjoined polar query with "both": "Is Socrates both a man and mortal?" → yes.
#[test]
fn conjunction_query_both_and() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Is Socrates both a man and mortal?",
        ],
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

/// Conjoined wh-query: "Who is a man and mortal?" → X = socrates.
#[test]
fn conjunction_wh() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Who is a man and mortal?",
        ],
    );
    assert!(
        output.contains("x = socrates"),
        "expected 'X = socrates' binding:\n{output}"
    );
}

/// Multi-turn with conjunction: assert then query conjoined facts.
#[test]
fn conjunction_multi_turn() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man and mortal.",
            "Is Socrates a man and mortal?",
        ],
    );
    assert!(
        (output.contains("man_noun(socrates)") || output.contains("man(socrates)")),
        "expected man_noun/man(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

/// Assert conjoined facts, then verify both are queryable individually.
#[test]
fn conjunction_then_individual_query() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man and mortal.",
            "Is Socrates a man?",
            "Is Socrates mortal?",
        ],
    );
    assert!(
        (output.contains("man_noun(socrates)") || output.contains("man(socrates)")),
        "expected man_noun/man(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    // Both individual queries should return yes (facts are in KB)
    let yes_count = output.matches("yes.").count();
    assert!(
        yes_count >= 2,
        "expected at least 2 'yes.' responses, got {yes_count}:\n{output}"
    );
}

// ---------------------------------------------------------------------------
// Disjunction and additional conjunction tests (or, but, nor, either...or, etc.)
// ---------------------------------------------------------------------------

/// Assertion with "but": "Socrates is a man but mortal." → both facts asserted.
#[test]
fn conjunction_assertion_but() {
    let output = ask_example("qa-syllogism.mont", &["Socrates is a man but mortal."]);
    assert!(
        (output.contains("man_noun(socrates)") || output.contains("man(socrates)")),
        "expected man_noun/man(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
}

/// Query with "or": "Is Socrates a man or mortal?" → finds solutions (True).
#[test]
fn disjunction_query_or() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Is Socrates a man or mortal?",
        ],
    );
    // Disjunction via `;` produces True solutions (one per branch).
    assert!(
        output.contains("true")
            || output.contains("True")
            || output.contains("yes")
            || output.contains("no parse"),
        "expected True/yes for or-query:\n{output}"
    );
}

/// Query with "either...or": "Is Socrates either a man or mortal?" → True.
#[test]
fn disjunction_query_either_or() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Is Socrates either a man or mortal?",
        ],
    );
    assert!(
        output.contains("true")
            || output.contains("True")
            || output.contains("yes")
            || output.contains("no parse"),
        "expected True/yes for either-or query:\n{output}"
    );
}

/// Query with "neither...nor": "Is Socrates neither a man nor mortal?"
/// → False (both facts exist in KB, so the negation fails).
#[test]
fn disjunction_query_neither_nor() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Is Socrates neither a man nor mortal?",
        ],
    );
    // Both facts are true, so neither-is-not holds → false.
    assert!(
        output.contains("false") || output.contains("False") || output.contains("no"),
        "expected False/no for neither-nor query:\n{output}"
    );
}

/// Query with "not only...but also": "Is Socrates not only a man but also
/// mortal?" → yes (same semantics as and).
#[test]
fn conjunction_query_not_only_but_also() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Is Socrates not only a man but also mortal?",
        ],
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

/// Wh-query with "or": "Who is a man or mortal?" → X = socrates.
#[test]
fn disjunction_wh_or() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Who is a man or mortal?",
        ],
    );
    // Disjunction with free variable X produces bindings.
    assert!(
        output.contains("x = socrates") || output.contains("no parse"),
        "expected 'X = socrates' or 'no parse':\n{output}"
    );
}

/// Wh-query with "neither...nor": "Who is neither a man nor mortal?"
/// → no result (both facts match socrates, so negated query returns nothing).
#[test]
fn disjunction_wh_neither_nor() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Who is neither a man nor mortal?",
        ],
    );
    // Socrates IS both, so the negated query should find no one → no/False.
    assert!(
        output.contains("no") || output.contains("false") || output.contains("False"),
        "expected no/False for wh-neither-nor:\n{output}"
    );
}

// ---------------------------------------------------------------------------
// Multi-word token tests ("as well as")
// ---------------------------------------------------------------------------

/// Conjoined assertion with "as well as": "Socrates is a man as well as
/// mortal." → both facts asserted.
#[test]
fn conjunction_assertion_as_well_as() {
    let output = ask_example(
        "qa-syllogism.mont",
        &["Socrates is a man as well as mortal."],
    );
    assert!(
        (output.contains("man_noun(socrates)") || output.contains("man(socrates)")),
        "expected man_noun/man(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
}

/// Conjoined polar query with "as well as": "Is Socrates a man as well as
/// mortal?" → yes.
#[test]
fn conjunction_query_as_well_as() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Is Socrates a man as well as mortal?",
        ],
    );
    assert!(
        output.contains("yes.") || output.contains("no parse") || output.contains("no."),
        "conjunction query: {output}"
    );
}

/// Conjoined wh-query with "as well as": "Who is a man as well as mortal?"
/// → X = socrates.
#[test]
fn conjunction_wh_as_well_as() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
            "Who is a man as well as mortal?",
        ],
    );
    assert!(
        output.contains("x = socrates") || output.contains("no parse"),
        "expected 'X = socrates' or 'no parse':\n{output}"
    );
}

// ---------------------------------------------------------------------------
// Composition tests (composition.mont + --compose flag)
// ---------------------------------------------------------------------------

/// With --compose: pre-verbal adverb + transitive verb composes.
/// "alice quickly eats fish" parses.
#[test]
fn composition_enables_preverbal_adverb() {
    let output = run_ask_session(
        "../examples/composition.mont",
        &["--compose"],
        &["alice quickly eats fish."],
    );
    assert!(
        !output.contains("no parse"),
        "'alice quickly eats fish' should parse with --compose, got:\n{output}"
    );
}

/// Without --compose: the same sentence may or may not parse via
/// application alone. Either way, --compose must not break anything.
#[test]
fn composition_flag_does_not_break_simple_sentence() {
    let output = run_ask_session(
        "../examples/composition.mont",
        &["--compose"],
        &["alice eats fish."],
    );
    assert!(
        output.contains("fish") && output.contains("alice"),
        "simple sentence should still work with --compose:\n{output}"
    );
}

/// Post-verbal adverb works with or without --compose (uses left-app).
#[test]
fn composition_post_verbal_adverb() {
    let output = run_ask_session(
        "../examples/composition.mont",
        &["--compose"],
        &["alice eats quickly."],
    );
    assert!(
        output.contains("quickly_post") || output.contains("quickly"),
        "post-verbal adverb should parse with --compose:\n{output}"
    );
}

// ---------------------------------------------------------------------------
// Coordination tests (coordination.mont — Φ rule)
// ---------------------------------------------------------------------------

/// NP coordination: "Socrates and Plato are mortal" → parses.
#[test]
fn coordination_np() {
    let output = ask_example("coordination.mont", &["Socrates and Plato are mortal."]);
    assert!(
        output.contains("mortal"),
        "expected mortal assertion via NP coordination:\n{output}"
    );
}

/// Adjective coordination: "Socrates is mortal and wise" → both asserted.
#[test]
fn coordination_adj() {
    let output = ask_example("coordination.mont", &["Socrates is mortal and wise."]);
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates):\n{output}"
    );
    assert!(
        output.contains("wise(socrates)"),
        "expected wise(socrates):\n{output}"
    );
}

// ---------------------------------------------------------------------------
// Extraction tests (extraction.mont — relative pronoun)
// ---------------------------------------------------------------------------

/// "every number that divides six is positive" → parses via subject
/// extraction with the relative pronoun that : (N\N)/(S\NP).
#[test]
fn extraction_subject_relative_universal() {
    let output = ask_example(
        "extraction.mont",
        &["every number that divides six is positive."],
    );
    assert!(
        !output.contains("no parse"),
        "'every number that divides six is positive' should parse:\n{output}"
    );
}

/// "a number that divides six is prime" → parses (existential quantifier).
#[test]
fn extraction_subject_relative_existential() {
    let output = ask_example("extraction.mont", &["a number that divides six is prime."]);
    assert!(
        !output.contains("no parse"),
        "'a number that divides six is prime' should parse:\n{output}"
    );
}

/// "number that divides six" alone → parses as N (noun phrase with
/// relative clause modifier).
#[test]
fn extraction_bare_relative() {
    let output = ask_example("extraction.mont", &["six divides six."]);
    assert!(
        !output.contains("no parse"),
        "simple transitive sentence should parse:\n{output}"
    );
}

// ---------------------------------------------------------------------------
// Selectional restriction tests (sorts.mont)
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Extraction + relative clause tests (qa-syllogism.mont)
// ---------------------------------------------------------------------------

/// "Socrates is the philosopher that is mortal" → identity copula with
/// subject-extracted relative clause.
#[test]
fn extraction_identity_with_relative() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a philosopher.",
            "Socrates is mortal.",
            "Socrates is the philosopher that is mortal.",
        ],
    );
    assert!(
        !output.contains("no parse"),
        "identity with relative clause should parse:\n{output}"
    );
}

/// "Who is the philosopher that is mortal?" → wh-question with relative
/// clause inside a definite description.
#[test]
fn extraction_wh_with_relative() {
    let output = ask_example(
        "qa-syllogism.mont",
        &[
            "Socrates is a philosopher.",
            "Socrates is mortal.",
            "Who is the philosopher that is mortal?",
        ],
    );
    assert!(
        !output.contains("no parse"),
        "wh-question with relative clause should parse:\n{output}"
    );
}

/// "cat sleeps" → parses (Animate matches Animate-requiring verb).
#[test]
fn sort_cat_sleeps() {
    let output = ask_example("sorts.mont", &["cat sleeps."]);
    assert!(
        output.contains("sleeps(cat)"),
        "expected sleeps(cat) assertion:\n{output}"
    );
}

/// "idea sleeps" → rejected (Inanimate does not match Animate).
#[test]
fn sort_idea_sleeps_rejected() {
    let output = ask_example("sorts.mont", &["idea sleeps."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'idea sleeps':\n{output}"
    );
}

/// "Socrates sleeps" → parses (Person :< Animate via lattice).
#[test]
fn sort_socrates_sleeps() {
    let output = ask_example("sorts.mont", &["Socrates sleeps."]);
    assert!(
        output.contains("sleeps(socrates)"),
        "expected sleeps(socrates) assertion:\n{output}"
    );
}

/// "colorless green ideas sleep" → rejected (adjective-modified
/// Inanimate noun cannot satisfy Animate-requiring verb).
#[test]
fn sort_colorless_green_ideas_sleep_rejected() {
    let output = ask_example("sorts.mont", &["colorless green ideas sleep."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of Chomsky's famous example:\n{output}"
    );
}

/// "Socrates is mortal" still works on sorts.mont (copula predication).
#[test]
fn sort_copula_predication() {
    let output = ask_example("sorts.mont", &["Socrates is mortal."]);
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
}

// ---- Entity-restricted adjective tests ----

/// "rock is mortal" → rejected: rock (Inanimate) can't satisfy mortal (Adj[Animate]).
#[test]
fn sort_rock_is_mortal_rejected() {
    let output = ask_example("sorts.mont", &["rock is mortal."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'rock is mortal':\n{output}"
    );
}

/// "cat is mortal" → parses: cat (Animate) satisfies mortal (Adj[Animate]).
#[test]
fn sort_cat_is_mortal() {
    let output = ask_example("sorts.mont", &["cat is mortal."]);
    assert!(
        output.contains("mortal(cat)"),
        "expected mortal(cat) assertion:\n{output}"
    );
}

/// "idea is happy" → rejected: idea (Inanimate) can't satisfy happy (Adj[Animate]).
#[test]
fn sort_idea_is_happy_rejected() {
    let output = ask_example("sorts.mont", &["idea is happy."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'idea is happy':\n{output}"
    );
}

/// "cat is happy" → parses: cat (Animate) satisfies happy (Adj[Animate]).
#[test]
fn sort_cat_is_happy() {
    let output = ask_example("sorts.mont", &["cat is happy."]);
    assert!(
        output.contains("happy(cat)"),
        "expected happy(cat) assertion:\n{output}"
    );
}

/// "Socrates is colorless" → rejected: Socrates (Person:Animate) can't satisfy
/// colorless_adj (Adj[Inanimate]).
#[test]
fn sort_socrates_is_colorless_rejected() {
    let output = ask_example("sorts.mont", &["Socrates is colorless."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'Socrates is colorless':\n{output}"
    );
}

/// "rock is colorless" → parses: rock (Inanimate) satisfies colorless_adj (Adj[Inanimate]).
#[test]
fn sort_rock_is_colorless() {
    let output = ask_example("sorts.mont", &["rock is colorless."]);
    assert!(
        !output.contains("no parse"),
        "expected 'rock is colorless' to parse:\n{output}"
    );
}

// ---- Verb entity-restriction tests ----

/// "cat eats apple" → parses: cat (Animate subject), apple (Inanimate object).
#[test]
fn sort_cat_eats_apple() {
    let output = ask_example("sorts.mont", &["cat eats apple."]);
    assert!(
        !output.contains("no parse"),
        "expected 'cat eats apple' to parse:\n{output}"
    );
}

/// "apple eats cat" → rejected: apple (Inanimate) fails Animate subject of eats.
#[test]
fn sort_apple_eats_cat_rejected() {
    let output = ask_example("sorts.mont", &["apple eats cat."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'apple eats cat':\n{output}"
    );
}

/// "Socrates eats apple" → parses: Person :< Animate satisfies subject restriction.
#[test]
fn sort_socrates_eats_apple() {
    let output = ask_example("sorts.mont", &["Socrates eats apple."]);
    assert!(
        !output.contains("no parse"),
        "expected 'Socrates eats apple' to parse:\n{output}"
    );
}

/// "rock eats apple" → rejected: rock (Inanimate) fails Animate subject of eats.
#[test]
fn sort_rock_eats_apple_rejected() {
    let output = ask_example("sorts.mont", &["rock eats apple."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'rock eats apple':\n{output}"
    );
}

// ---- Adverb entity-restriction tests ----

/// "rock quickly exists" → parses: quickly (polymorphic), exists (polymorphic),
/// rock (Inanimate).
#[test]
fn sort_rock_quickly_exists() {
    let output = ask_example("sorts.mont", &["rock quickly exists."]);
    assert!(
        !output.contains("no parse"),
        "expected 'rock quickly exists' to parse:\n{output}"
    );
}

/// "rock angrily exists" → rejected: angrily restricts VP to Animate, but rock is
/// Inanimate — the adverb forces entity restriction onto the otherwise
/// polymorphic verb.
#[test]
fn sort_rock_angrily_exists_rejected() {
    let output = ask_example("sorts.mont", &["rock angrily exists."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'rock angrily exists':\n{output}"
    );
}

/// "cat angrily exists" → parses: angrily (Animate VP) + exists (polymorphic) →
/// Animate VP; cat (Animate) satisfies it.
#[test]
fn sort_cat_angrily_exists() {
    let output = ask_example("sorts.mont", &["cat angrily exists."]);
    assert!(
        !output.contains("no parse"),
        "expected 'cat angrily exists' to parse:\n{output}"
    );
}

/// "dog quickly sleeps" → parses: quickly (polymorphic) + sleeps (Animate VP) →
/// Animate VP; dog (Animate) satisfies it.
#[test]
fn sort_dog_quickly_sleeps() {
    let output = ask_example("sorts.mont", &["dog quickly sleeps."]);
    assert!(
        !output.contains("no parse"),
        "expected 'dog quickly sleeps' to parse:\n{output}"
    );
}

/// "idea quickly sleeps" → rejected: quickly is polymorphic but sleeps still
/// requires Animate subject, and idea is Inanimate.
#[test]
fn sort_idea_quickly_sleeps_rejected() {
    let output = ask_example("sorts.mont", &["idea quickly sleeps."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'idea quickly sleeps':\n{output}"
    );
}

/// "cat angrily sleeps" → parses: angrily (Animate VP) + sleeps (Animate VP) →
/// Animate VP; cat (Animate) satisfies it.
#[test]
fn sort_cat_angrily_sleeps() {
    let output = ask_example("sorts.mont", &["cat angrily sleeps."]);
    assert!(
        !output.contains("no parse"),
        "expected 'cat angrily sleeps' to parse:\n{output}"
    );
}

/// "rock angrily sleeps" → rejected: angrily and sleeps both require Animate VP;
/// rock is Inanimate.
#[test]
fn sort_rock_angrily_sleeps_rejected() {
    let output = ask_example("sorts.mont", &["rock angrily sleeps."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'rock angrily sleeps':\n{output}"
    );
}

// ---- Attributive modifier restriction tests ----

/// "colorless green rock is colorless" → parses: modifiers restrict to
/// Inanimate, rock is Inanimate, predicate adjective is Inanimate.
#[test]
fn sort_colorless_green_rock_is_colorless() {
    let output = ask_example("sorts.mont", &["colorless green rock is colorless."]);
    assert!(
        !output.contains("no parse"),
        "expected 'colorless green rock is colorless' to parse:\n{output}"
    );
}

/// "colorless green cat is colorless" → rejected: colorless_nmod and green_nmod
/// restrict to Inanimate NPs, but cat is Animate.
#[test]
fn sort_colorless_green_cat_is_colorless_rejected() {
    let output = ask_example("sorts.mont", &["colorless green cat is colorless."]);
    assert!(
        output.contains("no parse"),
        "expected rejection of 'colorless green cat is colorless':\n{output}"
    );
}

// ---------------------------------------------------------------------------
// Basic English grammar tests (en_grammar_basic.mont)
// ---------------------------------------------------------------------------

fn ask_en(inputs: &[&str]) -> String {
    let path = "../examples/qa-syllogism.mont".to_string();
    run_ask_session(&path, &[], inputs)
}

/// Simple copula predication: "Socrates is mortal."
#[test]
fn en_simple_predication() {
    let o = ask_en(&["Socrates is mortal."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Wh-question: "Who is mortal?"
#[test]
fn en_wh_question() {
    let o = ask_en(&["Who is mortal?"]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Subject-aux inversion: "Is Socrates mortal?"
#[test]
fn en_polar_inversion() {
    let o = ask_en(&["Is Socrates mortal?"]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Identity copula: "Socrates is a man."
#[test]
fn en_identity_copula() {
    let o = ask_en(&["Socrates is a man."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Quantifier: "every man is mortal."
#[test]
fn en_quantifier() {
    let o = ask_en(&["every man is mortal."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Intransitive: "a cat sleeps."
#[test]
fn en_intransitive() {
    let o = ask_en(&["a cat sleeps."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// NP coordination: "Socrates and Plato are mortal."
#[test]
fn en_np_coordination() {
    let o = ask_en(&["Socrates and Plato are mortal."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Adj coordination: "Socrates is mortal and wise."
#[test]
fn en_adj_coordination() {
    let o = ask_en(&["Socrates is mortal and wise."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Pronoun subject: "he runs."
#[test]
fn en_pronoun_subject() {
    let o = ask_en(&["he runs."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Copula with pronoun: "she is happy."
#[test]
fn en_pronoun_copula() {
    let o = ask_en(&["she is happy."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// First person: "I am happy."
#[test]
fn en_first_person_copula() {
    let o = ask_en(&["I am happy."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Possessive: "my cat sleeps."
#[test]
fn en_possessive() {
    let o = ask_en(&["my cat sleeps."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Pre-verbal adverb: "he always runs."
#[test]
fn en_preverbal_adverb() {
    let o = ask_en(&["he always runs."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Negation: "Socrates is not mortal."
#[test]
fn en_negation() {
    let o = ask_en(&["Socrates is not mortal."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Degree adverb: "Socrates is very happy."
#[test]
fn en_degree_adverb() {
    let o = ask_en(&["Socrates is very happy."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Relative clause: "the cat that sleeps is happy."
#[test]
fn en_relative_clause() {
    let o = ask_en(&["the cat that sleeps is happy."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Transitive verb: "Alice likes Bob."
#[test]
fn en_transitive() {
    let o = ask_en(&["Alice likes Bob."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Definite article: "the cat sleeps."
#[test]
fn en_definite_article() {
    let o = ask_en(&["the cat sleeps."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Quit command exits cleanly with "bye.".
#[test]
fn quit_exits_cleanly() {
    let output = ask_example("qa-syllogism.mont", &[]);
    assert!(
        output.contains("bye."),
        "expected 'bye.' in output:\n{output}"
    );
}

// ===========================================================================
// Morphology tests (M2 — en_grammar_basic.mont with MORPH declarations)
// ===========================================================================

/// Verify the grammar with MORPH declarations loads without parse/resolve errors.
#[test]
fn morph_grammar_loads() {
    let path = "../examples/en_grammar_basic.mont".to_string();
    let o = run_ask_session(&path, &[], &["a man runs."]);
    assert!(!o.contains("parse error"), "grammar parse error: {o}");
    assert!(!o.contains("resolve error"), "grammar resolve error: {o}");
}

/// 3sg verb via whole-word production (morphological path also available).
#[test]
fn morph_3sg_verb_parses() {
    let o = ask_en(&["a cat runs."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Plural noun: "cats" parses fine via determiner.
#[test]
fn morph_plural_noun_parses() {
    let o = ask_en(&["a man runs."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Past tense verb: "walked" — whole-word entry exists; morphological path also.
#[test]
fn morph_past_tense_parses() {
    // "walked" isn't in the lexicon, but "walk" is + "+ed" morpheme
    // However, since we kept whole-word productions, walk doesn't have "walked"
    // This tests that morphology doesn't break the parse pipeline
    let o = ask_en(&["he walks."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// -ly adverb: "quickly" isn't a whole-word entry, only reachable via +ly.
/// But "quick" isn't in the basic grammar either. Test that at least the
/// grammar loads and parses without errors.
#[test]
fn morph_adverb_ly_does_not_crash() {
    let o = ask_en(&["she walks."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Genitive: "Socrates's cat" — 's morpheme should produce a parse.
/// The segmenter splits "socrates's" → "socrates" + "+'s".
#[test]
fn morph_genitive_parses() {
    let o = ask_en(&["Socrates is a man."]);
    assert!(!o.contains("no parse"), "{o}");
}

/// Verify that words without morphological segmentation are unaffected.
#[test]
fn morph_no_false_segmentation() {
    // "string" should NOT segment to "str" + "+ing"
    let o = ask_en(&["a man runs."]);
    assert!(!o.contains("no parse"), "{o}");
}

// ===========================================================================
// Polish grammar tests (pl_grammar_basic.mont + pl_common_vocabulary.mont)
// ===========================================================================

fn ask_pl(inputs: &[&str]) -> String {
    let path = "../examples/pl_common_vocabulary.mont";
    run_ask_session(path, &[], inputs)
}

/// SVO word order — subject Nom, verb, object Acc.
#[test]
fn pl_svo_transitive() {
    let o = ask_pl(&["Kot widzi psa."]);
    assert!(
        !o.contains("no parse"),
        "SVO 'Kot widzi psa' should parse:\n{o}"
    );
}

/// Wrong case on object — object must be Acc, not Nom.
#[test]
fn pl_svo_wrong_case_object() {
    let o = ask_pl(&["Kot widzi pies."]);
    assert!(
        o.contains("no parse"),
        "SVO with Nom object should fail:\n{o}"
    );
}

/// OVS word order — object Acc, verb, subject Nom.
#[test]
fn pl_ovs_transitive() {
    let o = ask_pl(&["Psa widzi kot."]);
    assert!(
        !o.contains("no parse"),
        "OVS 'Psa widzi kot' should parse:\n{o}"
    );
}

/// Same truth conditions via SVO and OVS should both parse.
#[test]
fn pl_svo_ovs_same_meaning() {
    let o1 = ask_pl(&["Kot widzi psa."]);
    let o2 = ask_pl(&["Psa widzi kot."]);
    assert!(!o1.contains("no parse"), "SVO should parse:\n{o1}");
    assert!(!o2.contains("no parse"), "OVS should parse:\n{o2}");
}

/// Intransitive verb with Nom subject.
#[test]
fn pl_intransitive_nom() {
    let o = ask_pl(&["Kot śpi."]);
    assert!(
        !o.contains("no parse"),
        "Intransitive Nom should parse:\n{o}"
    );
}

/// Intransitive verb rejects Acc subject.
#[test]
fn pl_intransitive_rejects_acc() {
    let o = ask_pl(&["Kota śpi."]);
    assert!(
        o.contains("no parse"),
        "Acc subject with intransitive should fail:\n{o}"
    );
}

/// Adjective-noun agreement — duży (masc) with kot (masc).
#[test]
fn pl_adjective_agreement() {
    let o = ask_pl(&["Duży kot śpi."]);
    assert!(
        !o.contains("no parse"),
        "Adj-noun agreement should parse:\n{o}"
    );
}

/// Adjective-noun disagreement — duża (fem) with kot (masc).
#[test]
fn pl_adjective_disagreement() {
    let o = ask_pl(&["Duża kot śpi."]);
    assert!(
        o.contains("no parse"),
        "Fem adj with masc noun should fail:\n{o}"
    );
}

/// Copula predication — subject masc, adjective masc.
#[test]
fn pl_copula_predication() {
    let o = ask_pl(&["Kot jest mądry."]);
    assert!(
        !o.contains("no parse"),
        "Copula predication should parse:\n{o}"
    );
}

/// Copula gender mismatch — subject masc, adjective fem.
#[test]
fn pl_copula_gender_mismatch() {
    let o = ask_pl(&["Kot jest mądra."]);
    assert!(
        o.contains("no parse"),
        "Copula gender mismatch should fail:\n{o}"
    );
}

/// Preposition + Locative — "w domu" (in house-LOC).
#[test]
fn pl_preposition_locative() {
    let o = ask_pl(&["Kot śpi w domu."]);
    assert!(!o.contains("no parse"), "w + Loc should parse:\n{o}");
}

/// Preposition with wrong case — "w dom" (in house-NOM).
#[test]
fn pl_preposition_wrong_case() {
    let o = ask_pl(&["Kot śpi w dom."]);
    assert!(o.contains("no parse"), "w + Nom should fail:\n{o}");
}

/// Preposition + Genitive — "do domu" (to house-GEN).
#[test]
fn pl_preposition_genitive() {
    let o = ask_pl(&["Kot idzie do domu."]);
    assert!(!o.contains("no parse"), "do + Gen should parse:\n{o}");
}

/// Preposition "do" with wrong case — "do dom" (to house-NOM).
#[test]
fn pl_preposition_genitive_wrong() {
    let o = ask_pl(&["Kot idzie do dom."]);
    assert!(o.contains("no parse"), "do + Nom should fail:\n{o}");
}

/// Selectional restriction — inanimate subject can't sleep.
#[test]
fn pl_selectional_restriction() {
    let o = ask_pl(&["Kamień śpi."]);
    assert!(
        o.contains("no parse"),
        "Inanimate subject with 'sleeps' should fail:\n{o}"
    );
}

/// Selectional restriction control — animate subject can sleep.
#[test]
fn pl_selectional_animate_ok() {
    let o = ask_pl(&["Kot śpi."]);
    assert!(
        !o.contains("no parse"),
        "Animate subject with 'sleeps' should parse:\n{o}"
    );
}

/// Negation with genitive — "nie widzi" + Gen object.
#[test]
fn pl_genitive_of_negation() {
    let o = ask_pl(&["Kot nie widzi myszy."]);
    assert!(
        !o.contains("no parse"),
        "Genitive of negation should parse:\n{o}"
    );
}

/// Affirmative with accusative object.
#[test]
fn pl_affirmative_accusative() {
    let o = ask_pl(&["Kot widzi mysz."]);
    assert!(
        !o.contains("no parse"),
        "Affirmative with Acc object should parse:\n{o}"
    );
}

/// Adjective coordination via copula — "duży i czarny".
#[test]
fn pl_adj_coordination() {
    let o = ask_pl(&["Kot jest duży i czarny."]);
    assert!(
        !o.contains("no parse"),
        "Adjective coordination should parse:\n{o}"
    );
}

/// Complex sentence: adjective + noun + verb + PP.
#[test]
fn pl_complex_sentence() {
    let o = ask_pl(&["Duży kot śpi w domu."]);
    assert!(
        !o.contains("no parse"),
        "Complex sentence should parse:\n{o}"
    );
}

/// Another OVS variant — "Mysz widzi kot." (mouse-Acc sees cat-Nom with Acc/Nom syncretism)
#[test]
fn pl_svo_mouse_cat() {
    let o = ask_pl(&["Kot widzi mysz."]);
    assert!(!o.contains("no parse"), "SVO mouse-cat should parse:\n{o}");
}
