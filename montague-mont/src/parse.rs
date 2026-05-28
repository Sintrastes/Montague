//! Chumsky parser for the `.mont` DSL (stage 2).
//!
//! Consumes `Vec<Spanned<Token<'src>>>` from the lexer, produces `MontFile`.

use chumsky::{error::Rich, input::MappedInput, pratt::*, prelude::*};

use crate::ast::Spanned as AstSpanned;
use crate::ast::{Declaration, Directive, MontFile, Span, SpellingClass, TypeArg, TypeExpr};
use crate::token::Token;

type ChSpan = SimpleSpan;

pub type ParserInput<'tokens, 'src> =
    MappedInput<'tokens, Token<'src>, ChSpan, &'tokens [chumsky::span::Spanned<Token<'src>>]>;

pub fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, MontFile, extra::Err<Rich<'tokens, Token<'src>>>>
{
    // ── Helpers (cloneable) ──────────────────────────────────────────────
    let ident = select_ref! { Token::Ident(s)        => *s }.boxed();
    let plus_ident = select_ref! { Token::PlusIdent(s)    => *s }.boxed();
    let doc_str_p = select_ref! { Token::DocString(s)    => *s }.boxed();
    let quoted = select_ref! { Token::QuotedString(s) => *s }.boxed();

    fn span_of(s: ChSpan) -> Span {
        Span::new(s.start, s.end)
    }

    // ── Type expressions (Pratt) ─────────────────────────────────────────
    let type_expr = recursive(|tyx| {
        let type_arg = ident
            .clone()
            .map(|s: &str| {
                if s.chars().next().is_some_and(char::is_uppercase) {
                    TypeArg::Concrete(s.to_string())
                } else {
                    TypeArg::Var(s.to_string())
                }
            })
            .map_with(|a, e| AstSpanned::new(a, span_of(e.span())))
            .boxed();

        let param_app = ident
            .clone()
            .then(
                type_arg
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<AstSpanned<TypeArg>>>()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket))
                    .or_not(),
            )
            .map_with(|(name, args), e| {
                let expr = match args {
                    Some(args) => TypeExpr::ParamApp {
                        name: name.to_string(),
                        args,
                    },
                    None => TypeExpr::TypeIdent(name.to_string()),
                };
                AstSpanned::new(expr, span_of(e.span()))
            })
            .labelled("type")
            .as_context()
            .boxed();

        let parens = tyx
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .boxed();

        let atom = param_app.or(parens).boxed();

        atom.pratt((
            infix(left(20), just(Token::Slash), |l, _, r, e| {
                AstSpanned::new(
                    TypeExpr::LeftArrow(Box::new(l), Box::new(r)),
                    span_of(e.span()),
                )
            }),
            infix(right(15), just(Token::Backslash), |l, _, r, e| {
                AstSpanned::new(
                    TypeExpr::RightArrow(Box::new(l), Box::new(r)),
                    span_of(e.span()),
                )
            }),
            infix(right(15), just(Token::RArrow), |l, _, r, e| {
                AstSpanned::new(
                    TypeExpr::RightArrow(Box::new(l), Box::new(r)),
                    span_of(e.span()),
                )
            }),
            infix(left(5), just(Token::Pipe), |l, _, r, e| {
                AstSpanned::new(TypeExpr::Union(Box::new(l), Box::new(r)), span_of(e.span()))
            }),
        ))
        .labelled("type expression")
        .as_context()
        .boxed()
    });

    // ── Directives ───────────────────────────────────────────────────────

    // `import ident [. ident] [as alias].`
    let import_directive = just(Token::Import)
        .ignore_then(ident.clone().labelled("module name"))
        .then(just(Token::Dot).ignore_then(ident.clone()).or_not())
        .then(just(Token::As).ignore_then(ident.clone()).or_not())
        .then_ignore(just(Token::End))
        .map_with(|((first, second), alias), e| {
            let mut path = vec![first.to_string()];
            if let Some(s) = second {
                path.push(s.to_string());
            }
            AstSpanned::new(
                Directive::Import {
                    path,
                    alias: alias.map(str::to_string),
                },
                span_of(e.span()),
            )
        })
        .labelled("import directive")
        .as_context()
        .boxed();

    // `extend by "uri".` — must be tried BEFORE namespace extend
    let extend_by_uri = ident
        .clone()
        .filter(|s: &&str| *s == "by")
        .ignore_then(quoted.clone())
        .then_ignore(just(Token::End))
        .map_with(|uri, e| {
            AstSpanned::new(
                Directive::ExtendBy {
                    uri: uri.to_string(),
                },
                span_of(e.span()),
            )
        })
        .labelled("extend by")
        .as_context()
        .boxed();

    // `extend foo.bar.`
    let extend_ns = ident
        .clone()
        .separated_by(just(Token::Dot))
        .at_least(1)
        .collect::<Vec<&str>>()
        .then_ignore(just(Token::End))
        .map_with(|parts, e| {
            AstSpanned::new(
                Directive::Extend {
                    namespace: parts.into_iter().map(str::to_string).collect(),
                },
                span_of(e.span()),
            )
        })
        .labelled("extend namespace")
        .as_context()
        .boxed();

    let extend_directive = just(Token::Extend)
        .ignore_then(choice((extend_ns, extend_by_uri)))
        .labelled("extend directive")
        .as_context()
        .boxed();

    // ── Declarations ─────────────────────────────────────────────────────

    // `type A[p1, p2].` or `type A.` — and comma-separated sugar:
    // `type A[B], C, D[E].`  →  three SingleTypeDecl.
    let single_type_entry = ident
        .clone()
        .then(
            ident
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<&str>>()
                .delimited_by(just(Token::LBracket), just(Token::RBracket))
                .or_not(),
        )
        .map(|(name, params)| Declaration::SingleTypeDecl {
            name: name.to_string(),
            params: params
                .unwrap_or_default()
                .into_iter()
                .map(str::to_string)
                .collect(),
        });

    let single_type_decl = just(Token::TypeKw)
        .ignore_then(
            single_type_entry
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<Declaration>>(),
        )
        .then_ignore(just(Token::End))
        .map_with(|decls, e| {
            let span = span_of(e.span());
            decls
                .into_iter()
                .map(move |d| AstSpanned::new(d, span))
                .collect::<Vec<_>>()
        })
        .labelled("single type declaration")
        .as_context()
        .boxed();

    // Legacy `Type = A | B | C.`
    let type_decl = just(Token::Type)
        .ignore_then(just(Token::Eq))
        .ignore_then(
            ident
                .clone()
                .separated_by(just(Token::Pipe))
                .collect::<Vec<&str>>(),
        )
        .then_ignore(just(Token::End))
        .map_with(|types, e| {
            AstSpanned::new(
                Declaration::TypeDecl(types.into_iter().map(str::to_string).collect()),
                span_of(e.span()),
            )
        })
        .labelled("type declaration (legacy)")
        .as_context()
        .boxed();

    // `sort entity.`
    let sort_decl = just(Token::SortKw)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::End))
        .map_with(|name, e| {
            AstSpanned::new(Declaration::SortDecl(name.to_string()), span_of(e.span()))
        })
        .labelled("sort declaration")
        .as_context()
        .boxed();

    // `A :< B.`
    let subtype_decl = ident
        .clone()
        .then_ignore(just(Token::Subtype))
        .then(ident.clone())
        .then_ignore(just(Token::End))
        .map_with(|(sub, sup), e| {
            AstSpanned::new(
                Declaration::SubtypeDecl {
                    sub: sub.to_string(),
                    sup: sup.to_string(),
                },
                span_of(e.span()),
            )
        })
        .labelled("subtype declaration")
        .as_context()
        .boxed();

    // `entity Person, Animate.`
    let sort_member_decl = ident
        .clone()
        .then(
            ident
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<&str>>(),
        )
        .then_ignore(just(Token::End))
        .map_with(|(sort_name, members), e| {
            AstSpanned::new(
                Declaration::SortMemberDecl {
                    sort: sort_name.to_string(),
                    members: members.into_iter().map(str::to_string).collect(),
                },
                span_of(e.span()),
            )
        })
        .labelled("sort member declaration")
        .as_context()
        .boxed();

    // `namespace foo.bar.`
    let namespace_decl = just(Token::Namespace)
        .ignore_then(
            ident
                .clone()
                .separated_by(just(Token::Dot))
                .at_least(1)
                .collect::<Vec<&str>>(),
        )
        .then_ignore(just(Token::End))
        .map_with(|parts, e| {
            AstSpanned::new(
                Declaration::NamespaceDecl(parts.into_iter().map(str::to_string).collect()),
                span_of(e.span()),
            )
        })
        .labelled("namespace declaration")
        .as_context()
        .boxed();

    // `word1, word2 --> entity.` or `"multi word" --> entity.`
    // Quoted strings have spaces replaced with underscores to match the
    // multi-word tokenizer convention (tokenize_multiword in montague-cli).
    let production_word_quoted = quoted.clone().map(|s: &str| vec![s.replace(' ', "_")]);
    let production_word_ident = ident.clone().map(|s: &str| vec![s.to_string()]);
    let production_decl = choice((production_word_quoted, production_word_ident))
        .separated_by(just(Token::Comma))
        .collect::<Vec<Vec<String>>>()
        .map(|nested: Vec<Vec<String>>| nested.into_iter().flatten().collect::<Vec<String>>())
        .then_ignore(just(Token::Arrow))
        .then(ident.clone())
        .then_ignore(just(Token::End))
        .map_with(|(words, entity), e| {
            AstSpanned::new(
                Declaration::ProductionDecl {
                    words,
                    entity: entity.to_string(),
                },
                span_of(e.span()),
            )
        })
        .labelled("production declaration")
        .as_context()
        .boxed();

    // `entity: type_expr.` with optional `-- | doc`
    let atom_decl = doc_str_p
        .or_not()
        .then(ident.clone().labelled("entity name"))
        .then_ignore(just(Token::Colon))
        .then(type_expr.clone().labelled("atom type"))
        .then_ignore(just(Token::End))
        .map_with(|((doc_str, entity), ty), e| {
            AstSpanned::new(
                Declaration::AtomDecl {
                    doc: doc_str.map(str::to_string),
                    entity: entity.to_string(),
                    ty,
                },
                span_of(e.span()),
            )
        })
        .labelled("atom declaration")
        .as_context()
        .boxed();

    // `MORPH +s : type_expr [STRIPS class,...].`
    let morpheme_decl = just(Token::Morph)
        .ignore_then(plus_ident.labelled("morpheme surface"))
        .then_ignore(just(Token::Colon))
        .then(type_expr.clone())
        .then(
            just(Token::Strips)
                .ignore_then(
                    ident
                        .clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<&str>>(),
                )
                .or_not(),
        )
        .then_ignore(just(Token::End))
        .map_with(|((surface, ty), strips_strs), e| {
            AstSpanned::new(
                Declaration::MorphemeDecl {
                    surface: surface.to_string(),
                    ty,
                    strips: strips_strs
                        .unwrap_or_default()
                        .into_iter()
                        .filter_map(SpellingClass::parse)
                        .collect(),
                },
                span_of(e.span()),
            )
        })
        .labelled("morpheme declaration")
        .as_context()
        .boxed();

    // ── Item enum ────────────────────────────────────────────────────────
    #[derive(Clone)]
    enum Item {
        Directive(AstSpanned<Directive>),
        Declaration(AstSpanned<Declaration>),
        /// Comma-separated sugar: one `type A, B, C.` → multiple decls.
        MultiDecl(Vec<AstSpanned<Declaration>>),
    }

    let directive_item = choice((
        import_directive.map_with(|d, e| (Item::Directive(d), e.span())),
        extend_directive.map_with(|d, e| (Item::Directive(d), e.span())),
    ))
    .boxed();

    let multi_decl_item = single_type_decl
        .map_with(|decls, e| (Item::MultiDecl(decls), e.span()))
        .boxed();

    let single_decl_item = choice((
        type_decl,
        sort_decl,
        morpheme_decl,
        namespace_decl,
        subtype_decl,
        sort_member_decl,
        production_decl,
        atom_decl,
    ))
    .map_with(|d, e| (Item::Declaration(d), e.span()))
    .boxed();

    // ── Top-level ────────────────────────────────────────────────────────
    let item = choice((directive_item, multi_decl_item, single_decl_item)).boxed();

    item.repeated()
        .collect::<Vec<(Item, ChSpan)>>()
        .map(|items| {
            let mut directives = Vec::new();
            let mut declarations = Vec::new();
            for (item, _span) in items {
                match item {
                    Item::Directive(d) => directives.push(d),
                    Item::Declaration(d) => declarations.push(d),
                    Item::MultiDecl(ds) => declarations.extend(ds),
                }
            }
            MontFile {
                directives,
                declarations,
            }
        })
}
