//! Name resolver for `.mont` files.
//!
//! Takes a [`MontFile`] AST plus a [`Registry`] and produces a typed lexicon
//! with populated subtype lattice.

use std::collections::HashSet;

use montague_core::registry::Registry;
use montague_core::subtyping::SubtypeLattice;
use montague_core::types::LambekType;

use crate::ast::{AtomEntry, Declaration, MontFile, ProductionEntry, Span, TypeExpr};
use crate::error::ResolveError;

/// The output of name resolution — a typed lexicon ready for parsing.
///
/// Basic types are represented as `String`. Atom entries store resolved
/// `LambekType<String>` values.
#[derive(Debug, Clone)]
pub struct ResolvedLexicon {
    pub atoms: Vec<AtomEntry<LambekType<String>>>,
    pub productions: Vec<ProductionEntry>,
    pub lattice: SubtypeLattice<String>,
}

/// Resolve a [`MontFile`] AST against a [`Registry`], producing a typed lexicon
/// and subtype lattice using `String` as the basic-type representation.
///
/// Returns `ResolvedLexicon<LambekType<String>>` — each atom entry stores its
/// resolved Lambek type.
pub fn resolve(file: &MontFile, reg: &Registry) -> Result<ResolvedLexicon, Vec<ResolveError>> {
    let mut errors = Vec::new();

    // -- Pass 1: collect known types --
    let mut known_types: HashSet<String> = HashSet::new();
    for decl in &file.declarations {
        if let Declaration::TypeDecl(types) = &decl.item {
            for t in types {
                known_types.insert(t.clone());
            }
        }
    }

    // -- Pass 2: subtype lattice --
    let mut lattice = SubtypeLattice::new();
    for decl in &file.declarations {
        if let Declaration::SubtypeDecl { sub, sup } = &decl.item {
            if !known_types.contains(sub) {
                errors.push(ResolveError::SubtypeUnknownType {
                    name: sub.clone(),
                    span: decl.span,
                });
                continue;
            }
            if !known_types.contains(sup) {
                errors.push(ResolveError::SubtypeUnknownType {
                    name: sup.clone(),
                    span: decl.span,
                });
                continue;
            }
            lattice.add_subtype(sub.clone(), sup.clone());
        }
    }

    // -- Pass 3: atom declarations --
    let mut atoms = Vec::new();
    for decl in &file.declarations {
        if let Declaration::AtomDecl { doc, entity, ty } = &decl.item {
            match resolve_type_expr(&ty.item, &known_types, reg, &mut errors, ty.span) {
                Some(resolved) => {
                    atoms.push(AtomEntry {
                        entity: entity.clone(),
                        doc: doc.clone(),
                        type_expr: resolved,
                        span: decl.span,
                    });
                }
                None => {
                    // Error already emitted in resolve_type_expr
                }
            }
        }
    }

    // -- Pass 4: production declarations --
    let mut productions = Vec::new();
    for decl in &file.declarations {
        if let Declaration::ProductionDecl { words, entity } = &decl.item {
            // Production targets any entity name — it may be declared later
            // or defined in a different context. We just record it.
            productions.push(ProductionEntry {
                words: words.clone(),
                entity: entity.clone(),
                span: decl.span,
            });
        }
    }

    if errors.is_empty() {
        Ok(ResolvedLexicon {
            atoms,
            productions,
            lattice,
        })
    } else {
        Err(errors)
    }
}

/// Resolve a type expression AST node to a `LambekType<String>`.
#[allow(clippy::only_used_in_recursion)]
fn resolve_type_expr(
    expr: &TypeExpr,
    known_types: &HashSet<String>,
    reg: &Registry,
    errors: &mut Vec<ResolveError>,
    span: Span,
) -> Option<LambekType<String>> {
    match expr {
        TypeExpr::TypeIdent(name) => {
            if known_types.contains(name) {
                Some(LambekType::Basic(name.clone()))
            } else {
                errors.push(ResolveError::UnknownType {
                    name: name.clone(),
                    span,
                });
                None
            }
        }
        TypeExpr::Qualified(path) => {
            // Qualified name — try to find in registry as a custom connective
            let name = path.join(".");
            // For now, treat qualified names as unresolved
            errors.push(ResolveError::UnresolvedConnective { name, span });
            None
        }
        TypeExpr::CustomApp { path, args: _ } => {
            // Custom connective application — v2 feature (deferred).
            let name = path.join(".");
            errors.push(ResolveError::UnresolvedConnective { name, span });
            None
        }
        TypeExpr::LeftArrow(a, b) => {
            let a = resolve_type_expr(&a.item, known_types, reg, errors, a.span)?;
            let b = resolve_type_expr(&b.item, known_types, reg, errors, b.span)?;
            Some(LambekType::LeftArrow(Box::new(a), Box::new(b)))
        }
        TypeExpr::RightArrow(a, b) => {
            let a = resolve_type_expr(&a.item, known_types, reg, errors, a.span)?;
            let b = resolve_type_expr(&b.item, known_types, reg, errors, b.span)?;
            Some(LambekType::RightArrow(Box::new(a), Box::new(b)))
        }
        TypeExpr::Union(a, b) => {
            let a = resolve_type_expr(&a.item, known_types, reg, errors, a.span)?;
            let b = resolve_type_expr(&b.item, known_types, reg, errors, b.span)?;
            Some(LambekType::Disj(Box::new(a), Box::new(b)))
        }
    }
}

// ---------------------------------------------------------------------------
// Semantics builder: ResolvedLexicon → Semantics (bridge to engine)
// ---------------------------------------------------------------------------

use montague_core::semantics::Semantics;
use montague_core::types::{AnnotatedTerm, NonDet, Term};

/// Build a [`Semantics`] from a resolved lexicon using `String` atoms and types.
///
/// - `type_of_atom`: looks up each entity in the atom list and returns its type.
/// - `parse_term`: for each surface word, looks up productions and returns
///   `Term::Atom(entity)` for each matching entity.
/// - `interp`: identity (return the annotated term as-is).
pub fn build_semantics(
    lexicon: &ResolvedLexicon,
) -> Semantics<String, String, AnnotatedTerm<String, String>> {
    let atoms = lexicon.atoms.clone();
    let productions = lexicon.productions.clone();

    Semantics::new(
        // type_of_atom: entity name → its LambekType
        move |entity: &String| -> NonDet<LambekType<String>> {
            atoms
                .iter()
                .filter(|a| &a.entity == entity)
                .map(|a| a.type_expr.clone())
                .collect()
        },
        // parse_term: surface word → Term::Atom(entity) for matching productions
        move |word: &str| -> NonDet<Term<String>> {
            productions
                .iter()
                .filter(|p| p.words.iter().any(|w| w == word))
                .map(|p| Term::Atom(p.entity.clone()))
                .collect()
        },
        // interp: identity
        |at| at,
    )
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;

    fn resolve_str(src: &str) -> Result<ResolvedLexicon, Vec<ResolveError>> {
        let (ast, errs) = parser::parse(src);
        assert!(errs.is_empty(), "parse errors: {errs:?}");
        let reg = Registry::empty();
        resolve(&ast, &reg)
    }

    #[test]
    fn resolve_simple_lexicon() {
        let src = "Type = Noun | Sentence | Person | Adjective.\n\
                   Person :< Noun.\n\
                   socrates: Person.\n\
                   mortal: Adjective.\n\
                   man, men --> Man.";
        let lex = resolve_str(src).unwrap();

        assert_eq!(lex.atoms.len(), 2);
        assert_eq!(lex.atoms[0].entity, "socrates");
        assert_eq!(
            lex.atoms[0].type_expr,
            LambekType::Basic("Person".to_string())
        );
        assert_eq!(lex.atoms[1].entity, "mortal");
        assert_eq!(
            lex.atoms[1].type_expr,
            LambekType::Basic("Adjective".to_string())
        );

        assert_eq!(lex.productions.len(), 1);
        assert_eq!(lex.productions[0].words, vec!["man", "men"]);
        assert_eq!(lex.productions[0].entity, "Man");

        assert!(lex.lattice.leq(&"Person".to_string(), &"Noun".to_string()));
        assert!(!lex.lattice.leq(&"Noun".to_string(), &"Person".to_string()));
    }

    #[test]
    fn resolve_arrow_types() {
        let src = "Type = S | NP | N.\n\
                   walk: NP \\ S.";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.atoms.len(), 1);
        // walk: NP \ S
        assert_eq!(
            lex.atoms[0].type_expr,
            LambekType::RightArrow(
                Box::new(LambekType::Basic("NP".to_string())),
                Box::new(LambekType::Basic("S".to_string())),
            )
        );
    }

    #[test]
    fn resolve_unknown_type_errors() {
        let src = "Type = Noun.\n\
                   socrates: Person.";
        let errs = resolve_str(src).unwrap_err();
        assert!(!errs.is_empty());
        match &errs[0] {
            ResolveError::UnknownType { name, .. } => assert_eq!(name, "Person"),
            _ => panic!("expected UnknownType"),
        }
    }

    #[test]
    fn resolve_subtype_cycle_detected() {
        let src = "Type = A | B | C.\nA :< B.\nB :< C.\nC :< A.";
        let lex = resolve_str(src).unwrap();
        // The lattice handles cycles gracefully — the DFS has cycle protection.
        // All three become mutually ≤ each other, which is the conservative answer.
        assert!(lex.lattice.leq(&"A".to_string(), &"B".to_string()));
        assert!(lex.lattice.leq(&"C".to_string(), &"A".to_string()));
    }

    #[test]
    fn resolve_compound_type_arrow() {
        let src = "Type = S | NP | N.\n\
                   the: NP / N.";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.atoms.len(), 1);
        assert_eq!(
            lex.atoms[0].type_expr,
            LambekType::LeftArrow(
                Box::new(LambekType::Basic("NP".to_string())),
                Box::new(LambekType::Basic("N".to_string())),
            )
        );
    }

    #[test]
    fn resolve_union_type() {
        let src = "Type = A | B | C.\n\
                   x: A | B.";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.atoms.len(), 1);
        assert!(matches!(lex.atoms[0].type_expr, LambekType::Disj(..)));
    }
}
