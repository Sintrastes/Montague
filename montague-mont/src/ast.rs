//! AST types for `.mont` files.
//!
//! These are purely syntactic — no name resolution, no registry lookups.
//! Every node carries a [`Span`] for error reporting.

use std::fmt;

use montague_core::types::{AtomType, Term};

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
    pub fn parse(s: &str) -> Option<Self> {
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

impl MontFile {
    /// An empty file with no directives or declarations.
    pub fn empty() -> Self {
        MontFile {
            directives: Vec::new(),
            declarations: Vec::new(),
        }
    }
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
    /// `extend <namespace>.` — bulk-import another module by namespace.
    Extend { namespace: Vec<String> },
    /// `extend by "<uri>".` — bulk-import by URI.
    ExtendBy { uri: String },
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
    /// `-- | doc\nentity : type_expr [:: sem_term].`
    AtomDecl {
        doc: Option<String>,
        entity: String,
        ty: Spanned<TypeExpr>,
        sem: Option<Spanned<SemTermExpr>>,
    },
    /// `word1, word2 --> entity.`
    ProductionDecl { words: Vec<String>, entity: String },
    /// `MORPH +s : type [STRIPS class,...] [:: sem_term].`
    MorphemeDecl {
        surface: String,
        ty: Spanned<TypeExpr>,
        strips: Vec<SpellingClass>,
        sem: Option<Spanned<SemTermExpr>>,
    },
    /// `type A[sort1, sort2].` — single type declaration with optional params.
    /// `params` is empty for 0-arity types. `type A.` → `SingleTypeDecl { name: "A", params: [] }`.
    SingleTypeDecl { name: String, params: Vec<String> },
    /// `namespace foo.bar.baz.`
    NamespaceDecl(Vec<String>),
    /// `sort entity.` — declares a new sort namespace.
    SortDecl(String),
    /// `entity Person.` or `entity Person, Animate, Inanimate.` — declares
    /// one or more members of a sort.
    SortMemberDecl { sort: String, members: Vec<String> },
}

// ---------------------------------------------------------------------------
// Type expressions
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeArg {
    /// A concrete sort-member reference: `Nominative`, `Person`.
    Concrete(String),
    /// A polymorphic sort variable: `a`, `b`. Lowercase first letter.
    Var(String),
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
    /// `Name[arg, arg]` — parametric type application (e.g. `NP[Person, Nominative]`).
    ParamApp {
        name: String,
        args: Vec<Spanned<TypeArg>>,
    },
    /// `a / b` — left arrow
    LeftArrow(Box<Spanned<TypeExpr>>, Box<Spanned<TypeExpr>>),
    /// `b \ a` or `b -> a` — right arrow
    RightArrow(Box<Spanned<TypeExpr>>, Box<Spanned<TypeExpr>>),
    /// `a | b` — union
    Union(Box<Spanned<TypeExpr>>, Box<Spanned<TypeExpr>>),
}

// ---------------------------------------------------------------------------
// Semantic term expressions
// ---------------------------------------------------------------------------

/// A raw semantic term expression parsed from `:: <term>.` syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemTermExpr {
    /// Variable reference: `x`, `P`, `Q`
    Var(String),
    /// Integer literal: `42`
    IntLit(i64),
    /// String literal: `"hello"`
    StringLit(String),
    /// Function application: `f(a, b)` or `f a`
    App(Box<Spanned<SemTermExpr>>, Vec<Spanned<SemTermExpr>>),
    /// Lambda abstraction: `λx. body` or `\x. body`
    Lambda(Vec<String>, Box<Spanned<SemTermExpr>>),
    /// Conjunction: `P ∧ Q` or `P /\ Q`
    And(Box<Spanned<SemTermExpr>>, Box<Spanned<SemTermExpr>>),
    /// Disjunction: `P ∨ Q` or `P \/ Q`
    Or(Box<Spanned<SemTermExpr>>, Box<Spanned<SemTermExpr>>),
    /// Negation: `¬P` or `~P`
    Not(Box<Spanned<SemTermExpr>>),
    /// Implication: `P → Q` or `P => Q`
    Implies(Box<Spanned<SemTermExpr>>, Box<Spanned<SemTermExpr>>),
    /// Universal quantification: `∀x. P(x)` or `forall x. P(x)`
    Forall(String, Box<Spanned<SemTermExpr>>),
    /// Existential quantification: `∃x. P(x)` or `exists x. P(x)`
    Exists(String, Box<Spanned<SemTermExpr>>),
    /// Equality: `x = y`
    Eq(Box<Spanned<SemTermExpr>>, Box<Spanned<SemTermExpr>>),
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
            Directive::Extend { namespace } => {
                write!(f, "extend {}.", namespace.join("."))
            }
            Directive::ExtendBy { uri } => {
                write!(f, "extend by \"{uri}\".")
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
            Declaration::AtomDecl {
                doc,
                entity,
                ty,
                sem,
            } => {
                if let Some(d) = doc {
                    writeln!(f, "-- | {d}")?;
                }
                write!(f, "{entity}: {}", ty.item)?;
                if let Some(s) = sem {
                    write!(f, " :: {}", s.item)?;
                }
                write!(f, ".")
            }
            Declaration::ProductionDecl { words, entity } => {
                write!(f, "{} --> {entity}.", words.join(", "))
            }
            Declaration::MorphemeDecl {
                surface,
                ty,
                strips,
                sem,
            } => {
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
                if let Some(s) = sem {
                    write!(f, " :: {}", s.item)?;
                }
                write!(f, ".")
            }
            Declaration::SingleTypeDecl { name, params } => {
                write!(f, "type {name}")?;
                if !params.is_empty() {
                    write!(f, "[{}]", params.join(", "))?;
                }
                write!(f, ".")
            }
            Declaration::NamespaceDecl(parts) => {
                write!(f, "namespace {}", parts.join("."))?;
                write!(f, ".")
            }
            Declaration::SortDecl(name) => write!(f, "sort {name}."),
            Declaration::SortMemberDecl { sort, members } => {
                write!(f, "{sort} {}", members.join(", "))?;
                write!(f, ".")
            }
        }
    }
}

impl fmt::Display for SemTermExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemTermExpr::Var(s) => write!(f, "{s}"),
            SemTermExpr::IntLit(n) => write!(f, "{n}"),
            SemTermExpr::StringLit(s) => write!(f, "\"{s}\""),
            SemTermExpr::App(fun, args) => {
                write!(f, "{}(", fun.item)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", a.item)?;
                }
                write!(f, ")")
            }
            SemTermExpr::Lambda(binders, body) => {
                write!(f, "λ")?;
                for b in binders {
                    write!(f, "{b}")?;
                }
                write!(f, ". {}", body.item)
            }
            SemTermExpr::And(l, r) => write!(f, "({} ∧ {})", l.item, r.item),
            SemTermExpr::Or(l, r) => write!(f, "({} ∨ {})", l.item, r.item),
            SemTermExpr::Not(x) => write!(f, "¬{}", x.item),
            SemTermExpr::Implies(l, r) => write!(f, "({} → {})", l.item, r.item),
            SemTermExpr::Forall(v, body) => write!(f, "∀{v}. {}", body.item),
            SemTermExpr::Exists(v, body) => write!(f, "∃{v}. {}", body.item),
            SemTermExpr::Eq(l, r) => write!(f, "({} = {})", l.item, r.item),
        }
    }
}

impl fmt::Display for TypeArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeArg::Concrete(s) | TypeArg::Var(s) => write!(f, "{s}"),
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
            TypeExpr::ParamApp { name, args } => {
                write!(f, "{name}[")?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", a.item)?;
                }
                write!(f, "]")
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
    pub sem_term: Term<AtomType>,
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
    pub sem_term: Term<AtomType>,
}
