//! Ariadne diagnostic rendering for parse and resolve errors.

use ariadne::{Color, Config, IndexType, Label, Report, ReportKind, Source};
use montague::core::reduction::FailureTrace;
use montague::mont::ast::{MontFile, Span};
use montague::mont::error::{MontParseError, ResolveError};

// ---------------------------------------------------------------------------
// Parser errors
// ---------------------------------------------------------------------------

/// Render a list of [`MontParseError`]s as coloured source-context reports.
pub fn render_parse_errors(errs: &[MontParseError], path: &str, src: &str) {
    for e in errs {
        render_parse_error(e, path, src);
    }
}

fn render_parse_error(e: &MontParseError, path: &str, src: &str) {
    match e {
        MontParseError::UnexpectedToken {
            expected,
            found,
            contexts,
            span,
        } => {
            let primary_msg = if expected.is_empty() {
                format!("unexpected `{found}`")
            } else if found.is_empty() || found == "end of input" {
                format!("unexpected end of input, expected {expected}")
            } else {
                format!("found `{found}`, expected {expected}")
            };

            let mut report = Report::build(
                ReportKind::Error,
                (path, span.start..span.end),
            )
            .with_config(Config::new().with_index_type(IndexType::Byte))
            .with_message(primary_msg.clone())
            .with_label(
                Label::new((path, span.start..span.end))
                    .with_message(primary_msg)
                    .with_color(Color::Red),
            );

            for (ctx, ctx_span) in contexts {
                report = report.with_label(
                    Label::new((path, ctx_span.start..ctx_span.end))
                        .with_message(format!("while parsing this {ctx}"))
                        .with_color(Color::Yellow),
                );
            }

            report
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        MontParseError::ExpectedDeclaration { span } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message("expected a declaration or directive")
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message("expected a declaration or directive here")
                        .with_color(Color::Red),
                )
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        MontParseError::InvalidTypeName { found, span } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("invalid type name `{found}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message("type names must start with an uppercase letter")
                        .with_color(Color::Red),
                )
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        MontParseError::InvalidEntityName { found, span } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("invalid entity name `{found}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message("entity names must start with a lowercase letter")
                        .with_color(Color::Red),
                )
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        MontParseError::InvalidChar { found, span } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("invalid character `{found}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message(format!("this character is not valid in a .mont file"))
                        .with_color(Color::Red),
                )
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        MontParseError::UnterminatedString { span } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message("unterminated string literal")
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message("this string is missing a closing `\"`")
                        .with_color(Color::Red),
                )
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        MontParseError::Custom { message, span } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(message)
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message(message)
                        .with_color(Color::Red),
                )
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }
    }
}

// ---------------------------------------------------------------------------
// Resolve errors
// ---------------------------------------------------------------------------

/// Render resolve errors with source context and "did you mean?" suggestions.
///
/// `type_candidates` and `entity_candidates` are names extracted from the AST
/// before resolution, used for Levenshtein-based suggestions.
pub fn render_resolve_errors(
    errs: &[ResolveError],
    path: &str,
    src: &str,
    type_candidates: &[String],
    entity_candidates: &[String],
) {
    for e in errs {
        render_resolve_error(e, path, src, type_candidates, entity_candidates);
    }
}

fn render_resolve_error(
    e: &ResolveError,
    path: &str,
    src: &str,
    type_candidates: &[String],
    entity_candidates: &[String],
) {
    match e {
        ResolveError::UnknownType { name, span, .. } => {
            let suggestion = did_you_mean(name, type_candidates);
            let mut report = Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("unknown type `{name}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message(format!("no type named `{name}` in scope"))
                        .with_color(Color::Red),
                );
            if let Some(s) = suggestion {
                report = report.with_note(format!("did you mean `{s}`?"));
            }
            report.finish().print((path, Source::from(src))).unwrap();
        }

        ResolveError::UnknownEntity { entity, span, .. } => {
            let suggestion = did_you_mean(entity, entity_candidates);
            let mut report = Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("unknown entity `{entity}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message(format!("no entity named `{entity}` declared"))
                        .with_color(Color::Red),
                );
            if let Some(s) = suggestion {
                report = report.with_note(format!("did you mean `{s}`?"));
            }
            report.finish().print((path, Source::from(src))).unwrap();
        }

        ResolveError::DuplicateEntity {
            entity,
            original_span,
            span, ..
        } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("duplicate entity `{entity}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message("redeclared here")
                        .with_color(Color::Red),
                )
                .with_label(
                    Label::new((path, original_span.start..original_span.end))
                        .with_message("first declared here")
                        .with_color(Color::Blue),
                )
                .with_note("each entity name may appear only once across the lexicon")
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        ResolveError::SubtypeCycle { chain, spans, .. } => {
            let primary = spans.first().copied().unwrap_or(Span::new(0, 0));
            let mut report = Report::build(
                ReportKind::Error,
                (path, primary.start..primary.end),
            )
            .with_config(Config::new().with_index_type(IndexType::Byte))
            .with_message(format!("subtype cycle detected: {chain}"));
            for (i, s) in spans.iter().enumerate() {
                report = report.with_label(
                    Label::new((path, s.start..s.end))
                        .with_message(format!("link {} in the cycle", i + 1))
                        .with_color(Color::Red),
                );
            }
            report
                .with_note("subtyping must be a DAG (no cycles)")
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        ResolveError::SubtypeUnknownType { name, span, .. } => {
            let suggestion = did_you_mean(name, type_candidates);
            let mut report = Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("unknown type `{name}` in subtype declaration"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message(format!("`{name}` is not a declared type"))
                        .with_color(Color::Red),
                );
            if let Some(s) = suggestion {
                report = report.with_note(format!("did you mean `{s}`?"));
            }
            report.finish().print((path, Source::from(src))).unwrap();
        }

        ResolveError::UnknownNamespace { name, span, .. } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("unknown namespace `{name}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message("namespace not found")
                        .with_color(Color::Red),
                )
                .with_note(format!(
                    "looked for `{name}.mont` in the extending file's directory"
                ))
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        ResolveError::AmbiguousReference {
            local,
            candidates,
            span, ..
        } => {
            let list = candidates
                .iter()
                .map(|c| format!("`{c}`"))
                .collect::<Vec<_>>()
                .join(", ");
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("ambiguous reference `{local}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message(format!("could refer to {list}"))
                        .with_color(Color::Red),
                )
                .with_note("qualify the name with its namespace to disambiguate")
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }

        ResolveError::UnresolvedConnective { name, span, .. } => {
            Report::build(ReportKind::Error, (path, span.start..span.end))
                .with_config(Config::new().with_index_type(IndexType::Byte))
                .with_message(format!("unresolved connective `{name}`"))
                .with_label(
                    Label::new((path, span.start..span.end))
                        .with_message("not registered in the connective registry")
                        .with_color(Color::Red),
                )
                .finish()
                .print((path, Source::from(src)))
                .unwrap();
        }
    }
}

/// Extract type name and entity name candidates from a parsed AST.
/// These are used for "did you mean?" suggestions in resolver errors.
pub fn extract_candidates(ast: &MontFile) -> (Vec<String>, Vec<String>) {
    let mut types = Vec::new();
    let mut entities = Vec::new();
    for d in &ast.declarations {
        match &d.item {
            montague::mont::ast::Declaration::TypeDecl(names) => types.extend(names.clone()),
            montague::mont::ast::Declaration::SingleTypeDecl { name, .. } => types.push(name.clone()),
            montague::mont::ast::Declaration::AtomDecl { entity, .. } => entities.push(entity.clone()),
            _ => {}
        }
    }
    types.sort();
    types.dedup();
    entities.sort();
    entities.dedup();
    (types, entities)
}

/// Levenshtein-based "did you mean?" suggestion.
fn did_you_mean(target: &str, candidates: &[String]) -> Option<String> {
    let max_dist = (target.len() / 3 + 1).max(1);
    candidates
        .iter()
        .filter_map(|c| {
            let d = strsim::levenshtein(target, c);
            if d <= max_dist { Some((d, c.clone())) } else { None }
        })
        .min_by_key(|(d, _)| *d)
        .map(|(_, c)| c)
}

// ---------------------------------------------------------------------------
// Chart-parse failures
// ---------------------------------------------------------------------------

/// Render a "no parse" diagnostic using failure traces from the chart parser.
///
/// Shows the sentence, highlights the span where unification failed, and
/// describes the sort/type error.
pub fn render_no_parse(
    sentence: &str,
    failures: &[FailureTrace],
    path: &str,
) {
    // Find the most useful failure: prefer SortMemberLeq over other kinds.
    let best = failures
        .iter()
        .max_by_key(|t| match &t.error {
            montague::core::types::UnifyError::SortMemberLeq { .. } => 3,
            montague::core::types::UnifyError::SortMismatch { .. } => 2,
            montague::core::types::UnifyError::ArityMismatch { .. } => 1,
            _ => 0,
        });

    let Some(trace) = best else {
        eprintln!("  No parse found for: {sentence:?}");
        return;
    };

    // Compute the byte span in the source from cell token indices.
    let words: Vec<&str> = sentence.split_whitespace().collect();
    let n = words.len();

    let left = trace.left_span.0.min(n);
    let right = trace.right_span.1.min(n);
    let span_start = if left < n {
        sentence.find(words[left]).unwrap_or(0)
    } else {
        0
    };
    let span_end = if right > 0 && right <= n {
        let pos = sentence.find(words[right - 1]).unwrap_or(sentence.len());
        pos + words[right - 1].len()
    } else {
        sentence.len()
    };
    let fail_span = if span_start <= span_end {
        span_start..span_end
    } else {
        0..sentence.len()
    };

    // Build the primary error message: what rule, what types, why it failed.
    let primary = format!(
        "{}: {} ⊀ {}",
        trace.rule_name,
        trace.left_ty,
        trace.right_ty,
    );

    let detail = match &trace.error {
        montague::core::types::UnifyError::SortMemberLeq { sort, expected, actual, .. } => {
            format!(
                "cannot combine `{} : {}` with `{} : {}`\n  required `{expected}` in sort `{sort}`, but got `{actual}` (`{actual}` ⊀ `{expected}`)",
                left_words(&words, &trace.left_span),
                trace.left_ty,
                right_words(&words, &trace.right_span),
                trace.right_ty,
            )
        }
        montague::core::types::UnifyError::SortMismatch { expected_sort, actual_sort, .. } => {
            format!(
                "cannot combine `{} : {}` with `{} : {}`\n  sort mismatch: expected `{expected_sort}`, got `{actual_sort}`",
                left_words(&words, &trace.left_span),
                trace.left_ty,
                right_words(&words, &trace.right_span),
                trace.right_ty,
            )
        }
        montague::core::types::UnifyError::StructureMismatch { expected_shape, actual_shape, .. } => {
            format!(
                "cannot combine `{} : {}` with `{} : {}`\n  expected {expected_shape} but found {actual_shape}",
                left_words(&words, &trace.left_span),
                trace.left_ty,
                right_words(&words, &trace.right_span),
                trace.right_ty,
            )
        }
        montague::core::types::UnifyError::NameMismatch { expected, actual, .. } => {
            format!(
                "cannot combine `{} : {}` with `{} : {}`\n  expected {expected}, found {actual}",
                left_words(&words, &trace.left_span),
                trace.left_ty,
                right_words(&words, &trace.right_span),
                trace.right_ty,
            )
        }
        other => format!(
            "cannot combine `{} : {}` with `{} : {}`\n  {other}",
            left_words(&words, &trace.left_span),
            trace.left_ty,
            right_words(&words, &trace.right_span),
            trace.right_ty,
        ),
    };

    Report::build(ReportKind::Error, (path, fail_span.clone()))
        .with_config(Config::new().with_index_type(IndexType::Byte))
        .with_message(format!("no parse for `{sentence}`"))
        .with_label(
            Label::new((path, fail_span))
                .with_message(primary)
                .with_color(Color::Red),
        )
        .with_note(detail)
        .finish()
        .eprint((path, Source::from(sentence)))
        .unwrap();
}

/// Extract the words covered by a span for display.
fn left_words<'a>(words: &[&'a str], span: &(usize, usize)) -> String {
    let start = span.0.min(words.len());
    let end = span.1.min(words.len());
    if start < end && start < words.len() {
        words[start..end].join(" ")
    } else {
        words.join(" ")
    }
}

fn right_words<'a>(words: &[&'a str], span: &(usize, usize)) -> String {
    let start = span.0.min(words.len());
    let end = span.1.min(words.len());
    if start < end && start < words.len() {
        words[start..end].join(" ")
    } else {
        words.join(" ")
    }
}
