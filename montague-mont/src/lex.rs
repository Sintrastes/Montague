//! Chumsky lexer for the `.mont` DSL.
//!
//! Two-stage pipeline (this is stage 1): `&'src str` → `Vec<Spanned<Token<'src>>>`.
//! The parser stage (`parse.rs`) consumes these tokens.

use chumsky::{error::Rich, prelude::*};

use crate::token::Token;

/// A spanned token: `(token, byte_range)`.
pub type Spanned<'src, T> = (T, SimpleSpan);

/// Produces a vector of spanned tokens from source text.
///
/// The return type MUST spell out all four type parameters; chumsky 0.13
/// cannot infer the `'src` lifetime or the `extra::Err` type.
pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<'src, Token<'src>>>, extra::Err<Rich<'src, char>>> {
    // ── Whitespace padding between tokens ────────────────────────────────
    // `one_of` is not Repeated internally, so wrapping in `.repeated()` is safe.
    let ws = one_of(" \t\n\r").repeated().ignored().boxed();

    // ── Doc comment: `-- | text` ─────────────────────────────────────────
    let doc_comment = just("-- |")
        .ignore_then(none_of('\n').repeated().to_slice())
        .map(|s: &str| Token::DocString(s.trim()))
        .boxed();

    // ── Multi-char operators (longest-prefix FIRST) ───────────────────────
    let op_multi = choice((
        just("-->").to(Token::Arrow),
        just("->").to(Token::RArrow),
        just(":<").to(Token::Subtype),
    ));

    // ── Single-char punctuation ──────────────────────────────────────────
    let op_single = choice((
        just('=').to(Token::ColonEq),
        just('|').to(Token::Pipe),
        just(':').to(Token::Colon),
        just('/').to(Token::Slash),
        just('\\').to(Token::Backslash),
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),
        just(',').to(Token::Comma),
    ));

    // ── `.` disambiguation: Dot vs End ───────────────────────────────────
    let dot_sep = just('.')
        .then(
            any()
                .filter(|c: &char| c.is_alphabetic() || *c == '_')
                .rewind(),
        )
        .to(Token::Dot);
    let end_terminator = just('.').to(Token::End);

    // ── `+suffix` morpheme surfaces ──────────────────────────────────────
    // `.to_slice()` captures the full "++suffix" span including the `+`.
    let plus_ident = just('+')
        .ignore_then(
            any()
                .filter(|c: &char| c.is_alphanumeric() || *c == '\'' || *c == '_')
                .repeated()
                .at_least(1),
        )
        .to_slice()
        .map(Token::PlusIdent)
        .boxed();

    // ── Quoted strings ───────────────────────────────────────────────────
    let quoted = none_of('"')
        .repeated()
        .to_slice()
        .delimited_by(just('"'), just('"'))
        .map(Token::QuotedString)
        .boxed();

    // ── Identifier / keyword dispatch ────────────────────────────────────
    let ident = text::ident().map(|s: &str| match s {
        "Type" => Token::Type,
        "type" => Token::TypeKw,
        "sort" => Token::SortKw,
        "MORPH" => Token::Morph,
        "STRIPS" => Token::Strips,
        "namespace" => Token::Namespace,
        "extend" => Token::Extend,
        "import" => Token::Import,
        "as" => Token::As,
        s => Token::Ident(s),
    });

    // ── Line comment (not a token, but part of the choice for priority) ──
    //
    // Because `doc_comment` and `op_multi` (with `-->`, `->`) appear
    // BEFORE this in the choice, `line_comment` only fires on genuine
    // `--` comments. No guard needed — chumsky `choice` ordering handles
    // the disambiguation.
    let line_comment = just("--")
        .ignore_then(none_of('\n').repeated())
        .ignored()
        .boxed();

    // ── Token choice ─────────────────────────────────────────────────────
    //
    // Longest-prefix alternatives first: `-->` before `->`, `-- |` before
    // `--` (comment). `dot_sep` before `end_terminator`. `plus_ident` before
    // `ident` (prevents `+s` splitting). `line_comment` is LAST among the
    // `--`-prefixed alternatives (after doc_comment and op_multi).
    //
    // Wrapped in `LexItem` because `choice` requires uniform types, and
    // `line_comment` returns `()` while tokens return `Token`.
    enum LexItem<'src> {
        Tok(Spanned<'src, Token<'src>>),
        Skip,
    }

    let token = choice((
        doc_comment.map_with(|t, e| LexItem::Tok((t, e.span()))),
        op_multi.map_with(|t, e| LexItem::Tok((t, e.span()))),
        plus_ident.map_with(|t, e| LexItem::Tok((t, e.span()))),
        quoted.map_with(|t, e| LexItem::Tok((t, e.span()))),
        dot_sep.map_with(|t, e| LexItem::Tok((t, e.span()))),
        end_terminator.map_with(|t, e| LexItem::Tok((t, e.span()))),
        op_single.map_with(|t, e| LexItem::Tok((t, e.span()))),
        ident.map_with(|t, e| LexItem::Tok((t, e.span()))),
        line_comment.map(|()| LexItem::Skip),
    ))
    .boxed();

    // ── Skip whitespace+comments, collect, filter ────────────────────────
    token
        .padded_by(ws)
        .repeated()
        .collect::<Vec<LexItem>>()
        .map(|items: Vec<LexItem>| {
            items
                .into_iter()
                .filter_map(|item| match item {
                    LexItem::Tok(t) => Some(t),
                    LexItem::Skip => None,
                })
                .collect()
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(src: &str) -> (Option<Vec<Spanned<'_, Token<'_>>>>, Vec<Rich<'_, char>>) {
        lexer().parse(src).into_output_errors()
    }

    fn token_names(tokens: &[Spanned<'_, Token<'_>>]) -> Vec<String> {
        tokens.iter().map(|(t, _)| t.to_string()).collect()
    }

    /// Convenience: lex + unwrap the option.
    fn lex_ok(src: &str) -> Vec<Spanned<'_, Token<'_>>> {
        let (tokens, errs) = lex(src);
        assert!(errs.is_empty(), "unexpected lex errors: {errs:?}");
        tokens.unwrap_or_default()
    }

    #[test]
    fn bare_ident() {
        let tokens = lex_ok("socrates");
        assert_eq!(token_names(&tokens), vec!["socrates"]);
    }

    #[test]
    fn keyword_type() {
        let tokens = lex_ok("type");
        assert!(matches!(tokens[0].0, Token::TypeKw));
    }

    #[test]
    fn operators() {
        let tokens = lex_ok("--> -> :< / \\ |");
        assert!(matches!(tokens[0].0, Token::Arrow));
        assert!(matches!(tokens[1].0, Token::RArrow));
        assert!(matches!(tokens[2].0, Token::Subtype));
        assert!(matches!(tokens[3].0, Token::Slash));
        assert!(matches!(tokens[4].0, Token::Backslash));
        assert!(matches!(tokens[5].0, Token::Pipe));
    }

    #[test]
    fn brackets_and_comma() {
        let tokens = lex_ok("( NP ) [ a , b ]");
        // Tokens: ( NP ) [ a , b ]
        assert!(matches!(tokens[0].0, Token::LParen));
        assert!(
            matches!(tokens[3].0, Token::LBracket),
            "tokens[3] = {:?}",
            tokens[3]
        );
        assert!(matches!(tokens[5].0, Token::Comma));
        assert!(matches!(tokens[7].0, Token::RBracket));
    }

    #[test]
    fn dot_vs_end() {
        let tokens = lex_ok("namespace foo.bar.");
        let dot_count = tokens
            .iter()
            .filter(|(t, _)| matches!(t, Token::Dot))
            .count();
        let end_count = tokens
            .iter()
            .filter(|(t, _)| matches!(t, Token::End))
            .count();
        assert_eq!(dot_count, 1, "expected one Dot (between foo and bar)");
        assert_eq!(end_count, 1, "expected one End (trailing)");
    }

    #[test]
    fn line_comment_skipped() {
        let tokens = lex_ok("-- this is a comment\nsocrates");
        assert_eq!(token_names(&tokens), vec!["socrates"]);
    }

    #[test]
    fn arrow_not_comment() {
        let tokens = lex_ok("kot --> cat.");
        assert!(
            matches!(tokens[1].0, Token::Arrow),
            "expected Arrow, got {:?}",
            tokens[1].0
        );
    }

    #[test]
    fn rarrow_not_comment() {
        let tokens = lex_ok("NP -> S.");
        assert!(matches!(tokens[1].0, Token::RArrow));
    }

    #[test]
    fn doc_comment() {
        let tokens = lex_ok("-- | a cat\nsocrates");
        assert!(matches!(&tokens[0].0, Token::DocString(s) if *s == "a cat"));
    }

    #[test]
    fn quoted_string() {
        let tokens = lex_ok(r#""as well as""#);
        assert!(matches!(&tokens[0].0, Token::QuotedString(s) if *s == "as well as"));
    }

    #[test]
    fn plus_ident() {
        let tokens = lex_ok("+s +ing +'s");
        assert!(
            matches!(tokens[0].0, Token::PlusIdent("+s")),
            "got {:?}",
            tokens[0].0
        );
        assert!(matches!(tokens[1].0, Token::PlusIdent("+ing")));
        assert!(matches!(tokens[2].0, Token::PlusIdent("+'s")));
    }

    #[test]
    fn type_decl_legacy() {
        let tokens = lex_ok("Type = S | NP | N.");
        assert!(matches!(tokens[0].0, Token::Type));
        assert!(matches!(tokens[1].0, Token::ColonEq));
        assert!(matches!(tokens[3].0, Token::Pipe));
        assert!(matches!(tokens[5].0, Token::Pipe));
        assert!(matches!(tokens[7].0, Token::End));
    }

    #[test]
    fn atom_decl() {
        let tokens = lex_ok("socrates: NP[Person, Nom].");
        assert!(matches!(tokens[0].0, Token::Ident("socrates")));
        assert!(matches!(tokens[1].0, Token::Colon));
        assert!(matches!(tokens[3].0, Token::LBracket));
        assert!(matches!(tokens[5].0, Token::Comma));
        assert!(matches!(tokens[8].0, Token::End));
    }

    #[test]
    fn morph_decl() {
        let tokens = lex_ok("MORPH +s : NP \\ S STRIPS e.");
        // Tokens: MORPH +s : NP \ S STRIPS e .
        assert!(matches!(tokens[0].0, Token::Morph));
        assert!(
            matches!(tokens[1].0, Token::PlusIdent("+s")),
            "got {:?}",
            tokens[1].0
        );
        assert!(matches!(tokens[2].0, Token::Colon));
        assert!(
            matches!(tokens[4].0, Token::Backslash),
            "tokens[4] = {:?}",
            tokens[4]
        );
        assert!(
            matches!(tokens[6].0, Token::Strips),
            "tokens[6] = {:?}",
            tokens[6]
        );
        assert!(matches!(tokens[8].0, Token::End));
    }

    #[test]
    fn subtype_decl() {
        let tokens = lex_ok("Person :< Animate.");
        assert!(matches!(tokens[1].0, Token::Subtype));
    }

    #[test]
    fn lex_import_stmt() {
        let tokens = lex_ok("import foo.bar as baz.");
        assert_eq!(
            token_names(&tokens),
            vec!["import", "foo", ".", "bar", "as", "baz", "."]
        );
    }
}
