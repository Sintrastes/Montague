//! AST types for `.mont` files.
//!
//! These are purely syntactic — no name resolution, no registry lookups.
//! Every node carries a [`Span`] for error reporting.

use std::fmt;

/// Spelling-change reversal class for morpheme segmentation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpellingClass {
    /// Reverse e-deletion: try appending `e` to stripped stem.
    EDeletion,
    /// Reverse consonant doubling: try un-doubling final char.
    ConsonantDoubling,
    /// Reverse y→i: replace terminal `i` with `y` at the boundary.
    YToI,
    /// Possessive: strip `'s`, also try without trailing `s`.
    Poss,
}

impl SpellingClass {
    /// Parse from a STRIPS-class keyword string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "e" => Some(SpellingClass::EDeletion),
            "CC" => Some(SpellingClass::ConsonantDoubling),
            "y_i" => Some(SpellingClass::YToI),
            "poss" => Some(SpellingClass::Poss),
            _ => None,
        }
    }
}

/// Byte-range span in the source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    /// Merge two spans into one covering both.
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// A value with its source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Spanned { item, span }
    }
}

// ---------------------------------------------------------------------------
// Top-level
// ---------------------------------------------------------------------------

/// A parsed `.mont` file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MontFile {
    pub directives: Vec<Spanned<Directive>>,
    pub declarations: Vec<Spanned<Declaration>>,
}

// ---------------------------------------------------------------------------
// Directives
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    /// `import qualified.name`
    Import {
        path: Vec<String>,
        alias: Option<String>,
    },
}

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration {
    /// `Type = A | B | C.`
    TypeDecl(Vec<String>),
    /// `A :< B.`
    SubtypeDecl { sub: String, sup: String },
    /// `-- | doc\nentity : type_expr.`
    AtomDecl {
        doc: Option<String>,
        entity: String,
        ty: Spanned<TypeExpr>,
    },
    /// `word1, word2 --> entity.`
    ProductionDecl { words: Vec<String>, entity: String },
    /// `MORPH +s : type [STRIPS class,...].`
    MorphemeDecl {
        surface: String,
        ty: Spanned<TypeExpr>,
        strips: Vec<SpellingClass>,
    },
}

// ---------------------------------------------------------------------------
// Type expressions
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExpr {
    /// A bare identifier: `Noun`
    TypeIdent(String),
    /// `qual.name` — qualified name
    Qualified(Vec<String>),
    /// `qual.name(A, B)` — custom connective application
    CustomApp {
        path: Vec<String>,
        args: Vec<Spanned<TypeExpr>>,
    },
    /// `a / b` — left arrow
    LeftArrow(Box<Spanned<TypeExpr>>, Box<Spanned<TypeExpr>>),
    /// `b \ a` or `b -> a` — right arrow
    RightArrow(Box<Spanned<TypeExpr>>, Box<Spanned<TypeExpr>>),
    /// `a | b` — union
    Union(Box<Spanned<TypeExpr>>, Box<Spanned<TypeExpr>>),
}

// ---------------------------------------------------------------------------
// Display (for round-trip testing)
// ---------------------------------------------------------------------------

impl fmt::Display for MontFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for d in &self.directives {
            writeln!(f, "{}", d.item)?;
        }
        if !self.directives.is_empty() && !self.declarations.is_empty() {
            writeln!(f)?;
        }
        for (i, d) in self.declarations.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", d.item)?;
        }
        Ok(())
    }
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Directive::Import { path, alias } => {
                write!(f, "import {}", path.join("."))?;
                if let Some(a) = alias {
                    write!(f, " as {a}")?;
                }
                write!(f, ".")
            }
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Declaration::TypeDecl(types) => {
                write!(f, "Type = {}", types.join(" | "))?;
                write!(f, ".")
            }
            Declaration::SubtypeDecl { sub, sup } => write!(f, "{sub} :< {sup}."),
            Declaration::AtomDecl { doc, entity, ty } => {
                if let Some(d) = doc {
                    writeln!(f, "-- | {d}")?;
                }
                write!(f, "{entity}: {}.", ty.item)
            }
            Declaration::ProductionDecl { words, entity } => {
                write!(f, "{} --> {entity}.", words.join(", "))
            }
            Declaration::MorphemeDecl { surface, ty, strips } => {
                write!(f, "MORPH {surface}: {}", ty.item)?;
                if !strips.is_empty() {
                    let classes: Vec<&str> = strips
                        .iter()
                        .map(|c| match c {
                            SpellingClass::EDeletion => "e",
                            SpellingClass::ConsonantDoubling => "CC",
                            SpellingClass::YToI => "y_i",
                            SpellingClass::Poss => "poss",
                        })
                        .collect();
                    write!(f, " STRIPS {}", classes.join(", "))?;
                }
                write!(f, ".")
            }
        }
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::TypeIdent(s) => write!(f, "{s}"),
            TypeExpr::Qualified(path) => write!(f, "{}", path.join(".")),
            TypeExpr::CustomApp { path, args } => {
                write!(f, "{}(", path.join("."))?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", a.item)?;
                }
                write!(f, ")")
            }
            TypeExpr::LeftArrow(a, b) => write!(f, "({} / {})", a.item, b.item),
            TypeExpr::RightArrow(a, b) => write!(f, "({} \\ {})", a.item, b.item),
            TypeExpr::Union(a, b) => write!(f, "({} | {})", a.item, b.item),
        }
    }
}

// ---------------------------------------------------------------------------
// Resolved lexicon types (used by the resolver)
// ---------------------------------------------------------------------------

/// A single atom entry in the resolved lexicon.
#[derive(Debug, Clone)]
pub struct AtomEntry<T> {
    pub entity: String,
    pub doc: Option<String>,
    pub type_expr: T,
    pub span: Span,
}

/// A single production entry.
#[derive(Debug, Clone)]
pub struct ProductionEntry {
    pub words: Vec<String>,
    pub entity: String,
    pub span: Span,
}

/// A resolved morpheme entry.
#[derive(Debug, Clone)]
pub struct MorphemeEntry<T> {
    pub surface: String,
    pub entity: String,
    pub type_expr: T,
    pub strips: Vec<SpellingClass>,
    pub span: Span,
}
