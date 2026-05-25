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

/// Quit command exits cleanly with "bye.".
#[test]
fn quit_exits_cleanly() {
    let output = ask_example("qa-syllogism.mont", &[]);
    assert!(
        output.contains("bye."),
        "expected 'bye.' in output:\n{output}"
    );
}
