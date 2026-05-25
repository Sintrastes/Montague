//! Name resolver for `.mont` files.
//!
//! Takes a [`MontFile`] AST plus a [`Registry`] and produces a typed lexicon
//! with populated subtype lattice.

use std::collections::HashSet;

use montague_core::registry::Registry;
use montague_core::subtyping::SubtypeLattice;
use montague_core::types::LambekType;

use crate::ast::{AtomEntry, Declaration, MontFile, MorphemeEntry, ProductionEntry, Span, TypeExpr};
use crate::error::ResolveError;

/// The output of name resolution — a typed lexicon ready for parsing.
///
/// Basic types are represented as `String`. Atom entries store resolved
/// `LambekType<String>` values.
#[derive(Debug, Clone)]
pub struct ResolvedLexicon {
    pub atoms: Vec<AtomEntry<LambekType<String>>>,
    pub productions: Vec<ProductionEntry>,
    pub morphemes: Vec<MorphemeEntry<LambekType<String>>>,
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

    // -- Pass 3b: morpheme declarations --
    let mut morphemes = Vec::new();
    for decl in &file.declarations {
        if let Declaration::MorphemeDecl { surface, ty, strips } = &decl.item {
            match resolve_type_expr(&ty.item, &known_types, reg, &mut errors, ty.span) {
                Some(resolved) => {
                    let entity = morpheme_entity_name(surface);
                    let strips = if strips.is_empty() {
                        default_strips(surface)
                    } else {
                        strips.clone()
                    };
                    morphemes.push(MorphemeEntry {
                        surface: surface.clone(),
                        entity,
                        type_expr: resolved,
                        strips,
                        span: decl.span,
                    });
                }
                None => {}
            }
        }
    }
    dedup_morpheme_entities(&mut morphemes);

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
            morphemes,
            lattice,
        })
    } else {
        Err(errors)
    }
}

/// Derive a unique entity name from a morpheme surface form.
/// Strips `+` and `'`, appends `_suffix`. If the name collides with an
/// already-registered entity, appends a counter.
fn morpheme_entity_name(surface: &str) -> String {
    if surface == "+'s" {
        return "poss_suffix".to_string();
    }
    let base = surface.strip_prefix('+').unwrap_or(surface);
    format!("{base}_suffix")
}

/// Ensure entity names are unique across morphemes. Appends `_N` to
/// duplicates.
fn dedup_morpheme_entities(morphemes: &mut [MorphemeEntry<LambekType<String>>]) {
    let mut seen: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for m in morphemes.iter_mut() {
        let count = seen.entry(m.entity.clone()).or_insert(0);
        if *count > 0 {
            m.entity = format!("{}_{}", m.entity, count);
        }
        *count += 1;
    }
}

/// Derive default spelling-recovery classes from the suffix surface form.
fn default_strips(surface: &str) -> Vec<crate::ast::SpellingClass> {
    use crate::ast::SpellingClass;
    let suffix = surface.strip_prefix('+').unwrap_or(surface);
    if suffix == "'s" {
        return vec![SpellingClass::Poss];
    }
    // Vowel-initial suffixes: `+ed`, `+ing` → e-deletion + consonant doubling
    if suffix.starts_with('e') || suffix.starts_with('i') {
        return vec![SpellingClass::EDeletion, SpellingClass::ConsonantDoubling];
    }
    // `+ly` → y→i reversal
    if suffix.starts_with('l') && suffix.contains('y') || suffix == "ly" {
        return vec![SpellingClass::YToI];
    }
    vec![]
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

use montague_core::morph::{MorphemeInfo, MorphSegmenter, SpellingClass as CoreSpellingClass};
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
    let morphemes = lexicon.morphemes.clone();
    let morphemes2 = morphemes.clone();

    Semantics::new(
        // type_of_atom: entity name → its LambekType
        move |entity: &String| -> NonDet<LambekType<String>> {
            let from_atoms: Vec<_> = atoms
                .iter()
                .filter(|a| &a.entity == entity)
                .map(|a| a.type_expr.clone())
                .collect();
            if !from_atoms.is_empty() {
                return from_atoms;
            }
            morphemes
                .iter()
                .filter(|m| &m.entity == entity)
                .map(|m| m.type_expr.clone())
                .collect()
        },
        // parse_term: surface word → Term::Atom(entity) for matching productions/morphemes
        move |word: &str| -> NonDet<Term<String>> {
            let mut results: Vec<Term<String>> = productions
                .iter()
                .filter(|p| p.words.iter().any(|w| w == word))
                .map(|p| Term::Atom(p.entity.clone()))
                .collect();
            // Also match morpheme surface forms (e.g., "+s", "+ing")
            results.extend(
                morphemes2
                    .iter()
                    .filter(|m| m.surface == word)
                    .map(|m| Term::Atom(m.entity.clone())),
            );
            results
        },
        // interp: identity
        |at| at,
    )
}

/// Build a [`MorphSegmenter`] from the morpheme entries in a resolved lexicon.
///
/// Converts `montague_mont::ast::SpellingClass` → `montague_core::morph::SpellingClass`.
pub fn build_segmenter(lexicon: &ResolvedLexicon) -> MorphSegmenter {
    let morphemes: Vec<MorphemeInfo> = lexicon
        .morphemes
        .iter()
        .map(|m| MorphemeInfo {
            surface: m.surface.clone(),
            entity: m.entity.clone(),
            ty: m.type_expr.clone(),
            strips: m
                .strips
                .iter()
                .map(|c| match c {
                    crate::ast::SpellingClass::EDeletion => CoreSpellingClass::EDeletion,
                    crate::ast::SpellingClass::ConsonantDoubling => CoreSpellingClass::ConsonantDoubling,
                    crate::ast::SpellingClass::YToI => CoreSpellingClass::YToI,
                    crate::ast::SpellingClass::Poss => CoreSpellingClass::Poss,
                })
                .collect(),
        })
        .collect();
    MorphSegmenter::new(morphemes)
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

    #[test]
    fn resolve_morph_decl_basic() {
        let src = "Type = S | NP | N.\n\
                   MORPH +s : (NP \\ S) \\ (NP \\ S).";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.morphemes.len(), 1);
        assert_eq!(lex.morphemes[0].surface, "+s");
        assert_eq!(lex.morphemes[0].entity, "s_suffix");
        assert!(lex.morphemes[0].strips.is_empty());
    }

    #[test]
    fn resolve_morph_duplicate_surfaces() {
        let src = "Type = S | NP | N.\n\
                   MORPH +s : (NP \\ S) \\ (NP \\ S).\n\
                   MORPH +s : NP \\ N STRIPS e.";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.morphemes.len(), 2);
        // First gets normal name, second gets disambiguated
        assert_eq!(lex.morphemes[0].surface, "+s");
        assert_eq!(lex.morphemes[1].surface, "+s");
        // Entities should be different
        assert_ne!(lex.morphemes[0].entity, lex.morphemes[1].entity);
    }

    #[test]
    fn resolve_morph_decl_default_strips() {
        let src = "Type = S | NP | N.\n\
                   MORPH +ed : (NP \\ S) \\ (NP \\ S).";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.morphemes.len(), 1);
        assert_eq!(lex.morphemes[0].strips.len(), 2);
        assert_eq!(lex.morphemes[0].strips[0], crate::ast::SpellingClass::EDeletion);
        assert_eq!(lex.morphemes[0].strips[1], crate::ast::SpellingClass::ConsonantDoubling);
    }

    #[test]
    fn resolve_morph_decl_possessive_entity() {
        let src = "Type = NP | N.\n\
                   MORPH +'s : (NP / N) \\ NP STRIPS poss.";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.morphemes.len(), 1);
        assert_eq!(lex.morphemes[0].entity, "poss_suffix");
        assert_eq!(lex.morphemes[0].strips.len(), 1);
    }

    #[test]
    fn resolve_morph_in_semantics() {
        let src = "Type = S | NP | N.\n\
                   MORPH +s : (NP \\ S) \\ (NP \\ S).\n\
                   run: NP \\ S.\n\
                   run --> run.";
        let lex = resolve_str(src).unwrap();
        let sem = build_semantics(&lex);
        // The morpheme surface "+s" should produce a term via parse_term
        let terms = (sem.parse_term)("+s");
        assert_eq!(terms.len(), 1);
        // The morpheme entity should have a type via type_of_atom
        let types = (sem.type_of_atom)(&"s_suffix".to_string());
        assert_eq!(types.len(), 1);
    }
}
