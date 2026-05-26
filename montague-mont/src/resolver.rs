//! Name resolver for `.mont` files.
//!
//! Takes a [`MontFile`] AST plus a [`Registry`] and produces a typed lexicon
//! with populated subtype lattice.

use std::collections::{HashMap, HashSet};

use montague_core::registry::Registry;
use montague_core::subtyping::SubtypeLattice;
use montague_core::types::{AtomType, LambekType, SortArg, SortVarId};

use crate::ast::{AtomEntry, Declaration, Directive, MontFile, MorphemeEntry, ProductionEntry, Span, TypeExpr};
use crate::error::ResolveError;
use crate::sort::SortRegistry;

/// The output of name resolution — a typed lexicon ready for parsing.
///
/// Basic types are represented as `String`. Atom entries store resolved
/// `LambekType<String>` values.
#[derive(Debug, Clone)]
pub struct ResolvedLexicon {
    pub atoms: Vec<AtomEntry<LambekType<AtomType>>>,
    pub productions: Vec<ProductionEntry>,
    pub morphemes: Vec<MorphemeEntry<LambekType<AtomType>>>,
    pub lattice: SubtypeLattice<AtomType>,
    /// Namespace declared in this file (if any).
    pub namespace: Option<Vec<String>>,
    /// Type names declared in this file (via `type A.` or `Type = A | B.`).
    pub type_names: HashSet<String>,
    /// Sort registry — per-sort members and subtyping lattices.
    pub sorts: SortRegistry,
    /// Type parameter arities: type name → list of sort names per parameter.
    pub type_arity: HashMap<String, Vec<String>>,
}

impl ResolvedLexicon {
    /// Merge another resolved lexicon into this one.
    ///
    /// Concatenates atoms, productions, morphemes, and merges subtype lattices.
    /// Returns errors for entity name collisions.
    pub fn extend(&mut self, other: ResolvedLexicon) -> Result<(), Vec<ResolveError>> {
        let mut errors = Vec::new();

        // Check for entity name collisions
        let existing_entities: HashSet<&str> = self.atoms.iter().map(|a| a.entity.as_str()).collect();
        for a in &other.atoms {
            if existing_entities.contains(a.entity.as_str()) {
                errors.push(ResolveError::DuplicateEntity {
                    entity: a.entity.clone(),
                    original_span: a.span,
                    span: a.span,
                });
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        self.atoms.extend(other.atoms);
        self.productions.extend(other.productions);
        self.morphemes.extend(other.morphemes);
        self.lattice.union(&other.lattice);

        // Merge sort registries
        if let Err(sort_errs) = self.sorts.extend(&other.sorts) {
            for msg in sort_errs {
                errors.push(ResolveError::UnknownNamespace {
                    name: msg,
                    span: Span::new(0, 0),
                });
            }
        }

        // Merge type arity maps — check for conflicts
        for (name, params) in &other.type_arity {
            if let Some(existing) = self.type_arity.get(name) {
                if existing != params {
                    errors.push(ResolveError::UnknownNamespace {
                        name: format!(
                            "conflicting arity for type `{name}`: {:?} vs {:?}",
                            existing, params
                        ),
                        span: Span::new(0, 0),
                    });
                }
            } else {
                self.type_arity.insert(name.clone(), params.clone());
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(())
    }
}

// ---------------------------------------------------------------------------
// File resolver trait
// ---------------------------------------------------------------------------

/// Trait for resolving external file references (extend directives).
pub trait FileResolver {
    /// Resolve a namespace to source content.
    /// E.g., namespace `["montague", "en_grammar_basic"]` → file `en_grammar_basic.mont`
    /// in the extending file's base directory.
    fn resolve_namespace(
        &self,
        namespace: &[String],
        base_dir: &str,
    ) -> Result<String, ResolveError>;

    /// Resolve a URI (file://, https://) to source content.
    fn resolve_uri(&self, uri: &str) -> Result<String, ResolveError>;
}

/// File-system based resolver. Looks for module files in the same directory
/// as the extending file.
pub struct FsFileResolver;

impl FileResolver for FsFileResolver {
    fn resolve_namespace(
        &self,
        namespace: &[String],
        base_dir: &str,
    ) -> Result<String, ResolveError> {
        let last = namespace
            .last()
            .ok_or_else(|| ResolveError::UnknownNamespace {
                name: namespace.join("."),
                span: Span::new(0, 0),
            })?;
        let path = std::path::Path::new(base_dir).join(format!("{last}.mont"));
        std::fs::read_to_string(&path).map_err(|_| ResolveError::UnknownNamespace {
            name: namespace.join("."),
            span: Span::new(0, 0),
        })
    }

    fn resolve_uri(&self, uri: &str) -> Result<String, ResolveError> {
        if let Some(path) = uri.strip_prefix("file://") {
            std::fs::read_to_string(path).map_err(|_| ResolveError::UnknownNamespace {
                name: uri.to_string(),
                span: Span::new(0, 0),
            })
        } else {
            Err(ResolveError::UnknownNamespace {
                name: uri.to_string(),
                span: Span::new(0, 0),
            })
        }
    }
}

/// Resolve a [`MontFile`] AST against a [`Registry`], producing a typed lexicon
/// and subtype lattice using `String` as the basic-type representation.
///
/// Returns `ResolvedLexicon<LambekType<String>>` — each atom entry stores its
/// resolved Lambek type.
pub fn resolve(file: &MontFile, reg: &Registry) -> Result<ResolvedLexicon, Vec<ResolveError>> {
    resolve_with_resolver(file, reg, &FsFileResolver, "")
}

/// Resolve with an explicit [`FileResolver`] and base directory for extend
/// resolution.
pub fn resolve_with_resolver(
    file: &MontFile,
    reg: &Registry,
    resolver: &dyn FileResolver,
    base_dir: &str,
) -> Result<ResolvedLexicon, Vec<ResolveError>> {
    let mut errors = Vec::new();
    let mut seen_namespaces: HashSet<Vec<String>> = HashSet::new();

    // -- Pass 0: namespace declaration --
    let mut namespace: Option<Vec<String>> = None;
    for decl in &file.declarations {
        if let Declaration::NamespaceDecl(parts) = &decl.item {
            if namespace.is_some() {
                errors.push(ResolveError::DuplicateEntity {
                    entity: parts.join("."),
                    original_span: decl.span,
                    span: decl.span,
                });
            } else {
                namespace = Some(parts.clone());
            }
        }
    }

    // -- Pass 0b: resolve extends early so their types are available --
    let mut extended_lexicons: Vec<ResolvedLexicon> = Vec::new();
    let mut extended_namespaces: HashSet<Vec<String>> = HashSet::new();
    for dir in &file.directives {
        let (ext_ns, ext_src): (Option<Vec<String>>, String) = match &dir.item {
            Directive::Extend { namespace } => {
                if extended_namespaces.contains(namespace) || seen_namespaces.contains(namespace) {
                    continue;
                }
                seen_namespaces.insert(namespace.clone());
                extended_namespaces.insert(namespace.clone());
                let src = match resolver.resolve_namespace(namespace, base_dir) {
                    Ok(s) => s,
                    Err(e) => { errors.push(e); return Err(errors); }
                };
                (Some(namespace.clone()), src)
            }
            Directive::ExtendBy { ref uri } => {
                let src = match resolver.resolve_uri(uri) {
                    Ok(s) => s,
                    Err(e) => { errors.push(e); return Err(errors); }
                };
                (None, src)
            }
            _ => continue,
        };
        let (sub_ast, parse_errs) = crate::parser::parse(&ext_src);
        if !parse_errs.is_empty() {
            for _e in &parse_errs {
                errors.push(ResolveError::UnknownNamespace {
                    name: ext_ns.as_ref().map(|v| v.join(".")).unwrap_or_else(|| "(uri)".into()),
                    span: dir.span,
                });
            }
            return Err(errors);
        }
        match resolve_with_resolver(&sub_ast, reg, resolver, base_dir) {
            Ok(l) => extended_lexicons.push(l),
            Err(mut es) => { errors.append(&mut es); return Err(errors); }
        }
    }

    // -- Pass 1: collect known types + type arities (local + extended) --
    let mut known_types: HashSet<String> = HashSet::new();
    let mut type_arity: HashMap<String, Vec<String>> = HashMap::new();
    for decl in &file.declarations {
        match &decl.item {
            Declaration::TypeDecl(types) => {
                for t in types { known_types.insert(t.clone()); }
            }
            Declaration::SingleTypeDecl { name, params } => {
                known_types.insert(name.clone());
                if !params.is_empty() {
                    type_arity.insert(name.clone(), params.clone());
                }
            }
            _ => {}
        }
    }
    // Add types from extended lexicons (type_names tracks declared types)
    for el in &extended_lexicons {
        for t in &el.type_names {
            known_types.insert(t.clone());
        }
        // Also collect type names from extended lattice (keys are AtomType)
        for (sub, sups) in el.lattice.direct_supertypes_iter() {
            let sub_name = sub.name.clone();
            if !known_types.contains(&sub_name) { known_types.insert(sub_name); }
            for s in sups {
                let s_name = s.name.clone();
                if !known_types.contains(&s_name) { known_types.insert(s_name); }
            }
        }
    }

    // -- Pass 1.5: sort declarations --
    let mut sorts = SortRegistry::new();
    // Start with extended sorts
    for el in &extended_lexicons {
        if let Err(sort_errs) = sorts.extend(&el.sorts) {
            for msg in sort_errs {
                errors.push(ResolveError::UnknownNamespace {
                    name: msg,
                    span: Span::new(0, 0),
                });
            }
        }
        // Merge extended type arities
        for (name, params) in &el.type_arity {
            if let Some(existing) = type_arity.get(name) {
                if existing != params {
                    errors.push(ResolveError::UnknownNamespace {
                        name: format!("type `{name}` redeclared with different arity"),
                        span: Span::new(0, 0),
                    });
                }
            } else {
                type_arity.insert(name.clone(), params.clone());
            }
        }
    }
    // Collect local sort declarations
    for decl in &file.declarations {
        match &decl.item {
            Declaration::SortDecl(name) => {
                if let Err(msg) = sorts.add_sort(name.clone()) {
                    errors.push(ResolveError::UnknownNamespace {
                        name: msg,
                        span: decl.span,
                    });
                }
            }
            Declaration::SortMemberDecl { sort, members } => {
                for member in members {
                    if let Err(msg) = sorts.add_member(sort, member.clone()) {
                        errors.push(ResolveError::UnknownNamespace {
                            name: msg,
                            span: decl.span,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    // -- Pass 2: subtype lattice + sort-internal edges --
    let mut lattice = SubtypeLattice::new();
    // Start with extended lattices
    for el in &extended_lexicons {
        lattice.union(&el.lattice);
    }
    // Classify and add local subtype declarations
    for decl in &file.declarations {
        if let Declaration::SubtypeDecl { sub, sup } = &decl.item {
            // Check if both are sort members of the same sort → sort-internal edge
            match (sorts.member_of(sub), sorts.member_of(sup)) {
                (Some(sa), Some(sb)) if sa == sb => {
                    if let Err(msg) = sorts.add_subtype(sub, sup) {
                        errors.push(ResolveError::UnknownNamespace {
                            name: msg,
                            span: decl.span,
                        });
                    }
                    continue;
                }
                (Some(_), Some(_)) => {
                    errors.push(ResolveError::UnknownNamespace {
                        name: format!("`{sub}` and `{sup}` belong to different sorts — cross-sort subtyping is not allowed"),
                        span: decl.span,
                    });
                    continue;
                }
                (Some(_), None) | (None, Some(_)) => {
                    errors.push(ResolveError::UnknownNamespace {
                        name: format!("`{sub}` or `{sup}` is a sort member while the other is a grammatical type — mixed edges are not allowed"),
                        span: decl.span,
                    });
                    continue;
                }
                _ => {}
            }
            // Both are grammatical types — add to grammatical lattice
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
            lattice.add_subtype(AtomType::new(sub), AtomType::new(sup));
        }
    }

    // -- Pass 3: atom declarations --
    let mut atoms = Vec::new();
    let mut entry_var_counter: u32 = 0;
    for decl in &file.declarations {
        if let Declaration::AtomDecl { doc, entity, ty } = &decl.item {
            let mut var_map: HashMap<String, SortVarId> = HashMap::new();
            match resolve_type_expr(&ty.item, &known_types, &type_arity, &sorts, reg, &mut errors, ty.span, &mut var_map, &mut entry_var_counter) {
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
            let mut var_map: HashMap<String, SortVarId> = HashMap::new();
            match resolve_type_expr(&ty.item, &known_types, &type_arity, &sorts, reg, &mut errors, ty.span, &mut var_map, &mut entry_var_counter) {
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

    // Build the base lexicon for this file
    if !errors.is_empty() {
        return Err(errors);
    }

    let mut lex = ResolvedLexicon {
        atoms,
        productions,
        morphemes,
        lattice,
        namespace: namespace.clone(),
        type_names: known_types.clone(),
        sorts,
        type_arity,
    };

    // -- Pass 5: merge pre-resolved extended lexicons --
    for el in extended_lexicons {
        // Filter out entities that already exist in the local file.
        let existing_entities: HashSet<&str> = lex.atoms.iter().map(|a| a.entity.as_str()).collect();
        let new_atoms: Vec<_> = el.atoms.into_iter()
            .filter(|a| !existing_entities.contains(a.entity.as_str()))
            .collect();
        let new_productions: Vec<_> = el.productions.into_iter()
            .filter(|p| !lex.productions.iter().any(|lp| lp.words == p.words && lp.entity == p.entity))
            .collect();
        let existing_morph_entities: HashSet<&str> = lex.morphemes.iter().map(|m| m.entity.as_str()).collect();
        let new_morphemes: Vec<_> = el.morphemes.into_iter()
            .filter(|m| !existing_morph_entities.contains(m.entity.as_str()))
            .collect();
        lex.atoms.extend(new_atoms);
        lex.productions.extend(new_productions);
        lex.morphemes.extend(new_morphemes);
    }

    Ok(lex)
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
fn dedup_morpheme_entities(morphemes: &mut [MorphemeEntry<LambekType<AtomType>>]) {
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

/// Resolve a type expression AST node to a `LambekType<AtomType>`.
///
/// Flat (non-parametric) types like `NP` become `LambekType::Basic(AtomType::new("NP"))`.
/// Parametric types like `NP[Person, Nominative]` become
/// `LambekType::Basic(AtomType { name: "NP", args: [...] })`.
#[allow(clippy::only_used_in_recursion)]
fn resolve_type_expr(
    expr: &TypeExpr,
    known_types: &HashSet<String>,
    type_arity: &HashMap<String, Vec<String>>,
    sorts: &SortRegistry,
    reg: &Registry,
    errors: &mut Vec<ResolveError>,
    span: Span,
    var_map: &mut HashMap<String, SortVarId>,
    next_var_id: &mut u32,
) -> Option<LambekType<AtomType>> {
    match expr {
        TypeExpr::ParamApp { name, args } => {
            if !known_types.contains(name) {
                errors.push(ResolveError::UnknownType {
                    name: name.clone(),
                    span,
                });
                return None;
            }
            let expected_sorts = type_arity.get(name).cloned().unwrap_or_default();
            if args.len() != expected_sorts.len() {
                errors.push(ResolveError::UnknownType {
                    name: format!(
                        "type `{name}` expects {} args (sorts {:?}), got {}",
                        expected_sorts.len(),
                        expected_sorts,
                        args.len()
                    ),
                    span,
                });
                return None;
            }
            let mut resolved_args = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                match &arg.item {
                    crate::ast::TypeArg::Concrete(member) => {
                        let expected_sort = &expected_sorts[i];
                        match sorts.member_of(member) {
                            Some(s) if s == expected_sort => {
                                resolved_args.push(SortArg::Concrete {
                                    sort: expected_sort.clone(),
                                    member: member.clone(),
                                });
                            }
                            Some(s) => {
                                errors.push(ResolveError::UnknownType {
                                    name: format!(
                                        "`{member}` is in sort `{s}`, expected sort `{expected_sort}`"
                                    ),
                                    span: arg.span,
                                });
                                return None;
                            }
                            None => {
                                errors.push(ResolveError::UnknownType {
                                    name: format!(
                                        "`{member}` is not a declared sort member of `{expected_sort}`"
                                    ),
                                    span: arg.span,
                                });
                                return None;
                            }
                        }
                    }
                    crate::ast::TypeArg::Var(v) => {
                        // Variable sort is inferred from the position's declared sort.
                        // Multiple occurrences of the same var name in one entry share an ID.
                        let id = *var_map.entry(v.clone()).or_insert_with(|| {
                            let fresh = SortVarId(*next_var_id);
                            *next_var_id += 1;
                            fresh
                        });
                        resolved_args.push(SortArg::Var(id));
                    }
                }
            }
            Some(LambekType::Basic(AtomType::with_args(name, resolved_args)))
        }
        TypeExpr::TypeIdent(name) => {
            if known_types.contains(name) {
                Some(LambekType::Basic(AtomType::new(name)))
            } else {
                errors.push(ResolveError::UnknownType {
                    name: name.clone(),
                    span,
                });
                None
            }
        }
        TypeExpr::Qualified(path) => {
            let name = path.join(".");
            errors.push(ResolveError::UnresolvedConnective { name, span });
            None
        }
        TypeExpr::CustomApp { path, args: _ } => {
            let name = path.join(".");
            errors.push(ResolveError::UnresolvedConnective { name, span });
            None
        }
        TypeExpr::LeftArrow(a, b) => {
            let a = resolve_type_expr(&a.item, known_types, type_arity, sorts, reg, errors, a.span, var_map, next_var_id)?;
            let b = resolve_type_expr(&b.item, known_types, type_arity, sorts, reg, errors, b.span, var_map, next_var_id)?;
            Some(LambekType::LeftArrow(Box::new(a), Box::new(b)))
        }
        TypeExpr::RightArrow(a, b) => {
            let a = resolve_type_expr(&a.item, known_types, type_arity, sorts, reg, errors, a.span, var_map, next_var_id)?;
            let b = resolve_type_expr(&b.item, known_types, type_arity, sorts, reg, errors, b.span, var_map, next_var_id)?;
            Some(LambekType::RightArrow(Box::new(a), Box::new(b)))
        }
        TypeExpr::Union(a, b) => {
            let a = resolve_type_expr(&a.item, known_types, type_arity, sorts, reg, errors, a.span, var_map, next_var_id)?;
            let b = resolve_type_expr(&b.item, known_types, type_arity, sorts, reg, errors, b.span, var_map, next_var_id)?;
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

/// Build a [`Semantics`] from a resolved lexicon using `String` atoms and `AtomType` types.
///
/// - `type_of_atom`: looks up each entity in the atom list and returns its type.
/// - `parse_term`: for each surface word, looks up productions and returns
///   `Term::Atom(entity)` for each matching entity.
/// - `interp`: identity (return the annotated term as-is).
pub fn build_semantics(
    lexicon: &ResolvedLexicon,
) -> Semantics<String, AtomType, AnnotatedTerm<String, AtomType>> {
    let atoms = lexicon.atoms.clone();
    let productions = lexicon.productions.clone();
    let morphemes = lexicon.morphemes.clone();
    let morphemes2 = morphemes.clone();

    Semantics::new(
        // type_of_atom: entity name → its LambekType<AtomType>
        move |entity: &String| -> NonDet<LambekType<AtomType>> {
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
            LambekType::Basic(AtomType::new("Person"))
        );
        assert_eq!(lex.atoms[1].entity, "mortal");
        assert_eq!(
            lex.atoms[1].type_expr,
            LambekType::Basic(AtomType::new("Adjective"))
        );

        assert_eq!(lex.productions.len(), 1);
        assert_eq!(lex.productions[0].words, vec!["man", "men"]);
        assert_eq!(lex.productions[0].entity, "Man");

        assert!(lex.lattice.leq(&AtomType::new("Person"), &AtomType::new("Noun")));
        assert!(!lex.lattice.leq(&AtomType::new("Noun"), &AtomType::new("Person")));
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
                Box::new(LambekType::Basic(AtomType::new("NP"))),
                Box::new(LambekType::Basic(AtomType::new("S"))),
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
        assert!(lex.lattice.leq(&AtomType::new("A"), &AtomType::new("B")));
        assert!(lex.lattice.leq(&AtomType::new("C"), &AtomType::new("A")));
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
                Box::new(LambekType::Basic(AtomType::new("NP"))),
                Box::new(LambekType::Basic(AtomType::new("N"))),
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

    #[test]
    fn resolve_single_type_decl() {
        let src = "type S.\ntype NP.\nwalk: NP \\ S.";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.atoms.len(), 1);
    }

    #[test]
    fn resolve_namespace_decl() {
        let src = "namespace montague.test.\ntype S.\ntype NP.\nwalk: NP \\ S.";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.namespace, Some(vec!["montague".into(), "test".into()]));
        assert_eq!(lex.atoms.len(), 1);
    }

    #[test]
    fn resolve_mixed_type_syntax() {
        let src = "Type = S | NP.\ntype N.\nwalk: NP \\ S.\nman: N.";
        let lex = resolve_str(src).unwrap();
        assert_eq!(lex.atoms.len(), 2);
    }
}
