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
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is mortal.",
        "Is Socrates mortal?",
    ]);
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

/// Polar question with statement word order + `?`: "Socrates is mortal?" → yes.
/// Uses the copula `is_cop` with `?` triggering the query path.
#[test]
fn polar_statement_form_yes() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is mortal.",
        "Socrates is mortal?",
    ]);
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

/// Wh-question: "Who is mortal?" → X = socrates.
/// Uses `who_q : Q/(N\S)` — takes VP to the right, forms question with free var.
#[test]
fn wh_question_binding() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is mortal.",
        "Who is mortal?",
    ]);
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
        output.contains("mortal(x) :- man_noun(x)")
            || output.contains("mortal(x) :-"),
        "expected mortal rule assertion:\n{output}"
    );
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

/// Feed assertions that load into the KB and verify both are asserted.
#[test]
fn multi_assertion() {
    let output = ask_example(
        "syllogism.mont",
        &[
            "Socrates is a man.",
            "Socrates is mortal.",
        ],
    );
    assert!(
        output.contains("man_noun(socrates)"),
        "expected man_noun(socrates) assertion:\n{output}"
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
    let output = ask_example(
        "qa-syllogism.mont",
        &["The flarbgorp zibbity doo dah."],
    );
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
        &[
            "Socrates is mortal.",
            "Is Socrates mortal?",
        ],
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

// ---------------------------------------------------------------------------
// Conjunction tests
// ---------------------------------------------------------------------------

/// Conjoined assertion: "Socrates is a man and mortal." → both facts asserted.
#[test]
fn conjunction_assertion_and() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man and mortal.",
    ]);
    assert!(
        output.contains("man_noun(socrates)"),
        "expected man_noun(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
}

/// Conjoined polar query: "Is Socrates a man and mortal?" → yes.
#[test]
fn conjunction_query_and() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Is Socrates a man and mortal?",
    ]);
    assert!(
        output.contains("man_noun(socrates)"),
        "expected man_noun(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

/// Conjoined polar query with "both": "Is Socrates both a man and mortal?" → yes.
#[test]
fn conjunction_query_both_and() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Is Socrates both a man and mortal?",
    ]);
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

/// Conjoined wh-query: "Who is a man and mortal?" → X = socrates.
#[test]
fn conjunction_wh() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Who is a man and mortal?",
    ]);
    assert!(
        output.contains("x = socrates"),
        "expected 'X = socrates' binding:\n{output}"
    );
}

/// Multi-turn with conjunction: assert then query conjoined facts.
#[test]
fn conjunction_multi_turn() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man and mortal.",
        "Is Socrates a man and mortal?",
    ]);
    assert!(
        output.contains("man_noun(socrates)"),
        "expected man_noun(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

/// Assert conjoined facts, then verify both are queryable individually.
#[test]
fn conjunction_then_individual_query() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man and mortal.",
        "Is Socrates a man?",
        "Is Socrates mortal?",
    ]);
    assert!(
        output.contains("man_noun(socrates)"),
        "expected man_noun(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
    // Both individual queries should return yes (facts are in KB)
    let yes_count = output.matches("yes.").count();
    assert!(yes_count >= 2, "expected at least 2 'yes.' responses, got {yes_count}:\n{output}");
}

// ---------------------------------------------------------------------------
// Disjunction and additional conjunction tests (or, but, nor, either...or, etc.)
// ---------------------------------------------------------------------------

/// Assertion with "but": "Socrates is a man but mortal." → both facts asserted.
#[test]
fn conjunction_assertion_but() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man but mortal.",
    ]);
    assert!(
        output.contains("man_noun(socrates)"),
        "expected man_noun(socrates) assertion:\n{output}"
    );
    assert!(
        output.contains("mortal(socrates)"),
        "expected mortal(socrates) assertion:\n{output}"
    );
}

/// Query with "or": "Is Socrates a man or mortal?" → finds solutions (True).
#[test]
fn disjunction_query_or() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Is Socrates a man or mortal?",
    ]);
    // Disjunction via `;` produces True solutions (one per branch).
    assert!(
        output.contains("true") || output.contains("True") || output.contains("yes"),
        "expected True/yes for or-query:\n{output}"
    );
}

/// Query with "either...or": "Is Socrates either a man or mortal?" → True.
#[test]
fn disjunction_query_either_or() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Is Socrates either a man or mortal?",
    ]);
    assert!(
        output.contains("true") || output.contains("True") || output.contains("yes"),
        "expected True/yes for either-or query:\n{output}"
    );
}

/// Query with "neither...nor": "Is Socrates neither a man nor mortal?"
/// → False (both facts exist in KB, so the negation fails).
#[test]
fn disjunction_query_neither_nor() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Is Socrates neither a man nor mortal?",
    ]);
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
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Is Socrates not only a man but also mortal?",
    ]);
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

/// Wh-query with "or": "Who is a man or mortal?" → X = socrates.
#[test]
fn disjunction_wh_or() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Who is a man or mortal?",
    ]);
    // Disjunction with free variable X produces bindings.
    assert!(
        output.contains("x = socrates"),
        "expected 'X = socrates' binding:\n{output}"
    );
}

/// Wh-query with "neither...nor": "Who is neither a man nor mortal?"
/// → no result (both facts match socrates, so negated query returns nothing).
#[test]
fn disjunction_wh_neither_nor() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Who is neither a man nor mortal?",
    ]);
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
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man as well as mortal.",
    ]);
    assert!(
        output.contains("man_noun(socrates)"),
        "expected man_noun(socrates) assertion:\n{output}"
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
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Is Socrates a man as well as mortal?",
    ]);
    assert!(output.contains("yes."), "expected 'yes.' in output:\n{output}");
}

/// Conjoined wh-query with "as well as": "Who is a man as well as mortal?"
/// → X = socrates.
#[test]
fn conjunction_wh_as_well_as() {
    let output = ask_example("qa-syllogism.mont", &[
        "Socrates is a man.",
        "Socrates is mortal.",
        "Who is a man as well as mortal?",
    ]);
    assert!(
        output.contains("x = socrates"),
        "expected 'X = socrates' binding:\n{output}"
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
// Selectional restriction tests (sorts.mont)
// ---------------------------------------------------------------------------

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

/// Quit command exits cleanly with "bye.".
#[test]
fn quit_exits_cleanly() {
    let output = ask_example("qa-syllogism.mont", &[]);
    assert!(
        output.contains("bye."),
        "expected 'bye.' in output:\n{output}"
    );
}
