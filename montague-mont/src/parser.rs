//! Parser for the `.mont` DSL.
//!
//! Uses chumsky 0.13 two-stage (lex + parse) pipeline. The lexer and parser
//! are in `lex.rs` and `parse.rs`; this module provides the `parse()` entry
//! point and bridge functions for error conversion.

use chumsky::Parser;
use chumsky::input::Input;

use crate::ast::*;
use crate::error::MontParseError;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

pub fn parse(src: &str) -> (MontFile, Vec<MontParseError>) {
    // Stage 1: lex with chumsky
    let (tokens, lex_errs) = crate::lex::lexer().parse(src).into_output_errors();

    let mut errors: Vec<MontParseError> = lex_errs
        .into_iter()
        .map(|e| convert_lex_error(&e))
        .collect();

    let Some(tokens) = tokens else {
        return (MontFile::empty(), errors);
    };

    if tokens.is_empty() {
        return (MontFile::empty(), errors);
    }

    // Stage 2: parse with chumsky.
    // Convert lexer's (Token, SimpleSpan) tuples to chumsky Spanned.
    let spanned_tokens: Vec<chumsky::span::Spanned<crate::token::Token<'_>>> = tokens
        .into_iter()
        .map(|(tok, span)| chumsky::span::Spanned { inner: tok, span })
        .collect();
    let eoi_span = (src.len()..src.len()).into();
    let token_input = spanned_tokens[..].split_spanned(eoi_span);
    let (ast, parse_errs) =
        crate::parse::parser().parse(token_input).into_output_errors();

    errors.extend(
        parse_errs
            .into_iter()
            .map(|e| convert_parse_error(&e)),
    );

    (ast.unwrap_or_else(MontFile::empty), errors)
}

fn convert_lex_error(e: &chumsky::error::Rich<'_, char>) -> MontParseError {
    MontParseError::UnexpectedToken {
        expected: format!("{}", e.reason()),
        found: e
            .found()
            .map(|c| c.to_string())
            .unwrap_or_else(|| "end of input".to_string()),
        span: Span::new(e.span().start, e.span().end),
    }
}

fn convert_parse_error(
    e: &chumsky::error::Rich<'_, crate::token::Token<'_>>,
) -> MontParseError {
    MontParseError::UnexpectedToken {
        expected: format!("{}", e.reason()),
        found: e
            .found()
            .map(|t| t.to_string())
            .unwrap_or_else(|| "end of input".to_string()),
        span: Span::new(e.span().start, e.span().end),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn check(src: &str) -> MontFile {
        let (f, errs) = parse(src);
        assert!(errs.is_empty(), "errors: {errs:?}");
        f
    }

    #[test]
    fn parse_empty() {
        let (f, e) = parse("");
        assert!(e.is_empty());
        assert!(f.declarations.is_empty());
    }
    #[test]
    fn parse_type_decl() {
        let f = check("Type = Noun | Sentence | Person.");
        match &f.declarations[0].item {
            Declaration::TypeDecl(ts) => assert_eq!(ts, &["Noun", "Sentence", "Person"]),
            _ => panic!(),
        }
    }
    #[test]
    fn parse_subtype_decl() {
        let f = check("Person :< Noun.");
        match &f.declarations[0].item {
            Declaration::SubtypeDecl { sub, sup } => {
                assert_eq!(sub, "Person");
                assert_eq!(sup, "Noun");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_atom_decl() {
        let f = check("nate: Person.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { entity, ty, .. } => {
                assert_eq!(entity, "nate");
                assert!(matches!(ty.item,TypeExpr::TypeIdent(ref s) if s=="Person"));
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_atom_with_doc() {
        let f = check("-- | The philosopher\nsocrates: Person.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { doc, entity, .. } => {
                assert_eq!(doc.as_deref(), Some("The philosopher"));
                assert_eq!(entity, "socrates");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_production_decl() {
        let f = check("like, love --> Like.");
        match &f.declarations[0].item {
            Declaration::ProductionDecl { words, entity } => {
                assert_eq!(words, &["like", "love"]);
                assert_eq!(entity, "Like");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_quoted_multiword_production() {
        let f = check(r#""as well as" --> as_well_as."#);
        match &f.declarations[0].item {
            Declaration::ProductionDecl { words, entity } => {
                assert_eq!(words, &["as well as"]);
                assert_eq!(entity, "as_well_as");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_mixed_production_with_quoted() {
        let f = check(r#"man, "as well as" --> man_noun."#);
        match &f.declarations[0].item {
            Declaration::ProductionDecl { words, entity } => {
                assert_eq!(words, &["man", "as well as"]);
                assert_eq!(entity, "man_noun");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_type_expr_arrow() {
        let f = check("walk: N \\ S.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { ty, .. } => {
                assert!(matches!(ty.item, TypeExpr::RightArrow(..)))
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_type_expr_slash() {
        let f = check("the: NP / N.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { ty, .. } => assert!(matches!(ty.item, TypeExpr::LeftArrow(..))),
            _ => panic!(),
        }
    }
    #[test]
    fn parse_type_expr_union() {
        let f = check("dog: Noun | Animal.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { ty, .. } => assert!(matches!(ty.item, TypeExpr::Union(..))),
            _ => panic!(),
        }
    }
    #[test]
    fn parse_full_example() {
        let f = check("Type = Noun | Sentence | Person | Adjective.\nPerson :< Noun.\nsocrates: Person.\nman, men --> Man.\nmortal: Adjective.");
        assert_eq!(f.declarations.len(), 5);
    }

    #[test]
    fn parse_morph_decl_basic() {
        let f = check("MORPH +s : (NP \\ S) \\ (NP \\ S).");
        match &f.declarations[0].item {
            Declaration::MorphemeDecl { surface, strips, .. } => {
                assert_eq!(surface, "+s");
                assert!(strips.is_empty());
            }
            _ => panic!("expected MorphemeDecl"),
        }
    }

    #[test]
    fn parse_morph_decl_with_strips() {
        let f = check("MORPH +ed : (NP \\ S) \\ (NP \\ S) STRIPS e, CC.");
        match &f.declarations[0].item {
            Declaration::MorphemeDecl { surface, strips, .. } => {
                assert_eq!(surface, "+ed");
                assert_eq!(strips.len(), 2);
                assert_eq!(strips[0], SpellingClass::EDeletion);
                assert_eq!(strips[1], SpellingClass::ConsonantDoubling);
            }
            _ => panic!("expected MorphemeDecl"),
        }
    }

    #[test]
    fn parse_morph_decl_y_i() {
        let f = check("MORPH +ly : Adv \\ Adj STRIPS y_i.");
        match &f.declarations[0].item {
            Declaration::MorphemeDecl { strips, .. } => {
                assert_eq!(strips.len(), 1);
                assert_eq!(strips[0], SpellingClass::YToI);
            }
            _ => panic!("expected MorphemeDecl"),
        }
    }

    #[test]
    fn parse_morph_decl_possessive() {
        let f = check("MORPH +'s : (NP / N) \\ NP STRIPS poss.");
        match &f.declarations[0].item {
            Declaration::MorphemeDecl { surface, strips, .. } => {
                assert_eq!(surface, "+'s");
                assert_eq!(strips.len(), 1);
                assert_eq!(strips[0], SpellingClass::Poss);
            }
            _ => panic!("expected MorphemeDecl"),
        }
    }
    #[test]
    fn parse_single_type_decl() {
        let f = check("type Noun.");
        match &f.declarations[0].item {
            Declaration::SingleTypeDecl { name, params } => {
                assert_eq!(name, "Noun");
                assert!(params.is_empty());
            }
            _ => panic!("expected SingleTypeDecl"),
        }
    }

    #[test]
    fn parse_single_type_decl_with_params() {
        let f = check("type NP[entity, case].");
        match &f.declarations[0].item {
            Declaration::SingleTypeDecl { name, params } => {
                assert_eq!(name, "NP");
                assert_eq!(params, &["entity", "case"]);
            }
            _ => panic!("expected SingleTypeDecl with params"),
        }
    }

    #[test]
    fn parse_sort_decl() {
        let f = check("sort entity.");
        match &f.declarations[0].item {
            Declaration::SortDecl(name) => assert_eq!(name, "entity"),
            _ => panic!("expected SortDecl"),
        }
    }

    #[test]
    fn parse_sort_member_decl() {
        let f = check("entity Person.");
        match &f.declarations[0].item {
            Declaration::SortMemberDecl { sort, members } => {
                assert_eq!(sort, "entity");
                assert_eq!(members, &vec!["Person".to_string()]);
            }
            _ => panic!("expected SortMemberDecl"),
        }
    }

    #[test]
    fn parse_sort_member_decl_multi() {
        let f = check("entity Person, Animate, Inanimate.");
        match &f.declarations[0].item {
            Declaration::SortMemberDecl { sort, members } => {
                assert_eq!(sort, "entity");
                assert_eq!(members, &vec!["Person".to_string(), "Animate".to_string(), "Inanimate".to_string()]);
            }
            _ => panic!("expected SortMemberDecl with multiple members"),
        }
    }

    #[test]
    fn parse_sort_and_member() {
        let src = "sort entity.\nentity Person.\nentity Animate.\nentity Abstract.";
        let f = check(src);
        assert_eq!(f.declarations.len(), 4);
        assert!(matches!(f.declarations[0].item, Declaration::SortDecl(_)));
        assert!(matches!(f.declarations[1].item, Declaration::SortMemberDecl { .. }));
        assert!(matches!(f.declarations[2].item, Declaration::SortMemberDecl { .. }));
        assert!(matches!(f.declarations[3].item, Declaration::SortMemberDecl { .. }));
    }

    #[test]
    fn parse_param_app_type() {
        let f = check("kot: N[Animal].");
        match &f.declarations[0].item {
            Declaration::AtomDecl { entity, ty, .. } => {
                assert_eq!(entity, "kot");
                match &ty.item {
                    TypeExpr::ParamApp { name, args } => {
                        assert_eq!(name, "N");
                        assert_eq!(args.len(), 1);
                        assert!(matches!(args[0].item, TypeArg::Concrete(ref c) if c == "Animal"));
                    }
                    _ => panic!("expected ParamApp, got {:?}", ty.item),
                }
            }
            _ => panic!("expected AtomDecl"),
        }
    }

    #[test]
    fn parse_param_app_type_with_var() {
        let f = check("the: NP[a] / N[a].");
        match &f.declarations[0].item {
            Declaration::AtomDecl { ty, .. } => {
                match &ty.item {
                    TypeExpr::LeftArrow(a, b) => {
                        match &a.item {
                            TypeExpr::ParamApp { name, args } => {
                                assert_eq!(name, "NP");
                                assert!(matches!(args[0].item, TypeArg::Var(ref v) if v == "a"));
                            }
                            _ => panic!("expected ParamApp in left, got {:?}", a.item),
                        }
                        match &b.item {
                            TypeExpr::ParamApp { name, args } => {
                                assert_eq!(name, "N");
                                assert!(matches!(args[0].item, TypeArg::Var(ref v) if v == "a"));
                            }
                            _ => panic!("expected ParamApp in right, got {:?}", b.item),
                        }
                    }
                    _ => panic!("expected LeftArrow, got {:?}", ty.item),
                }
            }
            _ => panic!("expected AtomDecl"),
        }
    }

    #[test]
    fn parse_namespace_decl() {
        let f = check("namespace montague.en_grammar_basic.");
        match &f.declarations[0].item {
            Declaration::NamespaceDecl(parts) => {
                assert_eq!(parts, &["montague", "en_grammar_basic"]);
            }
            _ => panic!("expected NamespaceDecl"),
        }
    }

    #[test]
    fn parse_extend_decl() {
        let f = check("extend montague.en_grammar_basic.");
        match &f.directives[0].item {
            Directive::Extend { ref namespace } => {
                assert_eq!(namespace, &["montague", "en_grammar_basic"]);
            }
            _ => panic!("expected Extend directive"),
        }
    }

    #[test]
    fn parse_extend_by_uri() {
        let f = check(r#"extend by "file:///path/to/grammar.mont"."#);
        match &f.directives[0].item {
            Directive::ExtendBy { uri } => {
                assert_eq!(uri, "file:///path/to/grammar.mont");
            }
            _ => panic!("expected ExtendBy directive"),
        }
    }

    #[test]
    fn parse_dot_disambiguation() {
        let src = "type S.\ntype NP.\n";
        let f = check(src);
        assert_eq!(f.declarations.len(), 2);
    }

    #[test]
    fn parse_mixed_old_new_type_syntax() {
        let src = "Type = A | B.\ntype C.";
        let f = check(src);
        assert_eq!(f.declarations.len(), 2);
        assert!(matches!(f.declarations[0].item, Declaration::TypeDecl(_)));
        assert!(matches!(f.declarations[1].item, Declaration::SingleTypeDecl { .. }));
    }
}
