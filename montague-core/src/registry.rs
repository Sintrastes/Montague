//! Identity and namespacing for Montague.
//!
//! Every interned identifier in Montague (operators, binders, predicates,
//! syntactic connectives, reduction rules) shares one canonical scheme based
//! on [`QName`]: a `u32` index into a global table maintained by [`Registry`].
//!
//! ### Three conflict rules
//!
//! 1. Within one canonical namespace, `local` names are unique.
//! 2. Across canonical namespaces, `local` names freely collide. Multiple
//!    operators sharing a local name with different semantics is a feature.
//! 3. Alias collisions are non-fatal but disambiguation-forcing at lookup time.
//!
//! ### Usage
//!
//! ```
//! use montague_core::registry::{Registry, owner_info};
//!
//! let mut reg = Registry::empty();
//! let fol_ns = reg
//!     .namespace()
//!     .canonical("montague.fol")
//!     .alias("fol")
//!     .owner(owner_info!())
//!     .create()
//!     .unwrap();
//! let and_op = reg.op(fol_ns, "and").arity_fixed(2).register().unwrap();
//! assert_eq!(reg.lookup_op(fol_ns, "and"), Some(and_op));
//! ```

use std::borrow::Cow;
use std::collections::HashMap;

use thiserror::Error;

// ---------------------------------------------------------------------------
// Identity types
// ---------------------------------------------------------------------------

/// Globally-interned qualified name. Cheap to copy, hash, compare.
///
/// Backed by a `u32` index into the [`Registry`]'s table. Two `QName`s
/// are equal iff they refer to the same registered entry.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct QName(pub u32);

/// Interned namespace identifier.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct NamespaceId(pub u32);

/// Discriminates the structural role of a registered name.
///
/// All registered names share the [`QName`] interning table; the kind
/// distinguishes how the name should be used at the type-system or
/// semantic level.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum IdKind {
    /// Non-binding operator (∧, ∨, ¬, →, G, F, ⊗, …).
    Op,
    /// Variable-binding operator (∀, ∃, ι, …).
    Binder,
    /// Predicate symbol (`man`, `likes`, …).
    Pred,
    /// Individual constant (`socrates`, `berlin`, …).
    Const,
    /// `LambekType::Custom` tag (pregroup adjoint, multimodal slash, …).
    SyntacticConnective,
    /// Reduction-rule identifier (so diagnostics can name which rule fired).
    ReductionRule,
}

/// Arity declaration for an operator or predicate.
///
/// v1 supports only fixed arities. Variadic arity can be added later if a
/// use case appears (mostly affects predicates, which can be declared
/// per-call until then).
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Arity {
    Fixed(u8),
}

/// Provenance information attached to every registered name.
///
/// Populated automatically by the [`owner_info!`] macro at the registration
/// call site. Used purely for diagnostics — registrations are otherwise
/// identified by their [`QName`].
#[derive(Debug, Clone)]
pub struct OwnerInfo {
    /// `CARGO_PKG_NAME` at the call site.
    pub crate_name: &'static str,
    /// `file!():line!()` at the call site.
    pub registered_at: &'static str,
    /// `CARGO_PKG_VERSION` at the call site. Resolver currently ignores this.
    pub version: Option<&'static str>,
}

/// Storage for a single registered name.
#[derive(Debug, Clone)]
pub struct QNameEntry {
    pub namespace: NamespaceId,
    pub local: Cow<'static, str>,
    pub owner: OwnerInfo,
    pub kind: IdKind,
    pub arity: Arity,
    /// Display form (defaults to `local` when `None`).
    pub pretty: Option<Cow<'static, str>>,
    /// Only meaningful for `IdKind::Binder` — whether the binder accepts a
    /// restrictor expression (generalized-quantifier style: `∀x: P. Q`).
    pub with_restrictor: bool,
}

/// Storage for a single registered namespace.
#[derive(Debug, Clone)]
pub struct NamespaceEntry {
    pub canonical: Cow<'static, str>,
    pub aliases: Vec<Cow<'static, str>>,
    pub owner: OwnerInfo,
    pub members: Vec<QName>,
}

// ---------------------------------------------------------------------------
// Domain newtypes
// ---------------------------------------------------------------------------
//
// Each kind has its own QName-wrapping newtype to preserve domain-typing in
// the public API. Internally they share the QName table.
//
// Conversion to QName is free; conversion *from* QName is only available
// through Registry lookups that verify the kind.

macro_rules! qname_newtype {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
        pub struct $name(pub(crate) QName);

        impl $name {
            /// The underlying [`QName`] — share-able across newtypes for
            /// diagnostics, serialization, and registry lookups.
            pub fn qname(self) -> QName {
                self.0
            }
        }

        impl From<$name> for QName {
            fn from(x: $name) -> QName {
                x.0
            }
        }
    };
}

qname_newtype! {
    /// Identifier for a non-binding or binding operator (`SemTerm::Op`,
    /// `SemTerm::Binder`). The kind is recoverable via [`Registry::kind_of`].
    OpId
}
qname_newtype! {
    /// Identifier for a predicate symbol (`SemTerm::Pred.head`).
    PredId
}
qname_newtype! {
    /// Identifier for an individual constant (`SemTerm::Const`).
    ConstId
}
qname_newtype! {
    /// Identifier for a `LambekType::Custom` tag.
    CustomTag
}
qname_newtype! {
    /// Identifier for a reduction rule. Diagnostics use this to name which
    /// rule produced a given derivation.
    RuleId
}

// ---------------------------------------------------------------------------
// Resolution result types
// ---------------------------------------------------------------------------

/// Outcome of looking up a namespace by canonical name or alias.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NamespaceResolution {
    Found(NamespaceId),
    NotFound,
    /// Alias matched multiple namespaces. Caller must disambiguate by
    /// canonical name.
    Ambiguous(Vec<NamespaceId>),
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Error)]
pub enum RegisterError {
    #[error("namespace builder missing canonical name")]
    MissingCanonical,

    #[error("namespace builder missing owner info (call .owner(owner_info!()))")]
    MissingOwner,

    #[error("operator builder missing arity")]
    MissingArity,

    #[error("namespace `{canonical}` already registered by `{by_crate}` at {at}")]
    NamespaceExists {
        canonical: String,
        by_crate: &'static str,
        at: &'static str,
    },

    #[error(
        "duplicate {kind:?} `{local}` in namespace `{ns}`; already registered by `{by_crate}` at {at}"
    )]
    DuplicateInNamespace {
        ns: String,
        local: String,
        kind: IdKind,
        by_crate: &'static str,
        at: &'static str,
    },
}

// ---------------------------------------------------------------------------
// Registry
// ---------------------------------------------------------------------------

/// The runtime registry of all namespaces and identifiers.
///
/// Mutable during the registration phase (typically at process startup) and
/// effectively immutable thereafter. v1 does not support unregistering or
/// hot-reloading entries; the registry is append-only.
#[derive(Default, Debug)]
pub struct Registry {
    // Append-only storage.
    qnames: Vec<QNameEntry>,
    namespaces: Vec<NamespaceEntry>,

    // Lookup indices.
    /// (NamespaceId, local-name) → QName.
    by_qualified: HashMap<(NamespaceId, String), QName>,
    /// canonical-name → NamespaceId.
    by_canonical: HashMap<String, NamespaceId>,
    /// alias → all NamespaceIds that registered this alias.
    by_alias: HashMap<String, Vec<NamespaceId>>,
}

impl Registry {
    /// Create an empty registry.
    pub fn empty() -> Self {
        Self::default()
    }

    // -- Builders --------------------------------------------------------

    /// Begin building a new namespace.
    pub fn namespace(&mut self) -> NamespaceBuilder<'_> {
        NamespaceBuilder {
            reg: self,
            canonical: None,
            aliases: Vec::new(),
            owner: None,
        }
    }

    /// Begin building a non-binding operator (`SemTerm::Op`).
    pub fn op(&mut self, ns: NamespaceId, local: impl Into<Cow<'static, str>>) -> OpBuilder<'_> {
        OpBuilder {
            reg: self,
            ns,
            local: local.into(),
            kind: IdKind::Op,
            arity: None,
            pretty: None,
            owner_override: None,
            with_restrictor: false,
        }
    }

    /// Begin building a binding operator (`SemTerm::Binder`).
    pub fn binder(
        &mut self,
        ns: NamespaceId,
        local: impl Into<Cow<'static, str>>,
    ) -> BinderBuilder<'_> {
        BinderBuilder {
            inner: OpBuilder {
                reg: self,
                ns,
                local: local.into(),
                kind: IdKind::Binder,
                // Binders are typically arity-1 over their body; the
                // builder accepts an arity but defaults to 1.
                arity: Some(Arity::Fixed(1)),
                pretty: None,
                owner_override: None,
                with_restrictor: false,
            },
        }
    }

    /// Begin building a predicate symbol.
    pub fn pred(
        &mut self,
        ns: NamespaceId,
        local: impl Into<Cow<'static, str>>,
    ) -> PredBuilder<'_> {
        PredBuilder {
            inner: OpBuilder {
                reg: self,
                ns,
                local: local.into(),
                kind: IdKind::Pred,
                arity: None,
                pretty: None,
                owner_override: None,
                with_restrictor: false,
            },
        }
    }

    /// Begin building an individual constant (`SemTerm::Const`).
    pub fn const_(
        &mut self,
        ns: NamespaceId,
        local: impl Into<Cow<'static, str>>,
    ) -> ConstBuilder<'_> {
        ConstBuilder {
            inner: OpBuilder {
                reg: self,
                ns,
                local: local.into(),
                kind: IdKind::Const,
                arity: Some(Arity::Fixed(0)),
                pretty: None,
                owner_override: None,
                with_restrictor: false,
            },
        }
    }

    /// Begin building a `LambekType::Custom` connective tag.
    pub fn syntactic_connective(
        &mut self,
        ns: NamespaceId,
        local: impl Into<Cow<'static, str>>,
    ) -> SyntacticBuilder<'_> {
        SyntacticBuilder {
            inner: OpBuilder {
                reg: self,
                ns,
                local: local.into(),
                kind: IdKind::SyntacticConnective,
                arity: None,
                pretty: None,
                owner_override: None,
                with_restrictor: false,
            },
        }
    }

    /// Begin building a reduction-rule identifier.
    pub fn rule(
        &mut self,
        ns: NamespaceId,
        local: impl Into<Cow<'static, str>>,
    ) -> RuleBuilder<'_> {
        RuleBuilder {
            inner: OpBuilder {
                reg: self,
                ns,
                local: local.into(),
                kind: IdKind::ReductionRule,
                // Rules are nullary identifiers; arity isn't meaningful here.
                arity: Some(Arity::Fixed(0)),
                pretty: None,
                owner_override: None,
                with_restrictor: false,
            },
        }
    }

    // -- Lookup ----------------------------------------------------------

    /// Look up an [`OpId`] (kind `Op` or `Binder`) by namespace + local name.
    pub fn lookup_op(&self, ns: NamespaceId, local: &str) -> Option<OpId> {
        let q = self.lookup_qname(ns, local)?;
        match self.qnames[q.0 as usize].kind {
            IdKind::Op | IdKind::Binder => Some(OpId(q)),
            _ => None,
        }
    }

    /// Look up a [`PredId`] by namespace + local name.
    pub fn lookup_pred(&self, ns: NamespaceId, local: &str) -> Option<PredId> {
        let q = self.lookup_qname(ns, local)?;
        if self.qnames[q.0 as usize].kind == IdKind::Pred {
            Some(PredId(q))
        } else {
            None
        }
    }

    /// Look up a [`ConstId`] by namespace + local name.
    pub fn lookup_const(&self, ns: NamespaceId, local: &str) -> Option<ConstId> {
        let q = self.lookup_qname(ns, local)?;
        if self.qnames[q.0 as usize].kind == IdKind::Const {
            Some(ConstId(q))
        } else {
            None
        }
    }

    /// Look up a [`CustomTag`] by namespace + local name.
    pub fn lookup_custom_tag(&self, ns: NamespaceId, local: &str) -> Option<CustomTag> {
        let q = self.lookup_qname(ns, local)?;
        if self.qnames[q.0 as usize].kind == IdKind::SyntacticConnective {
            Some(CustomTag(q))
        } else {
            None
        }
    }

    /// Look up a [`RuleId`] by namespace + local name.
    pub fn lookup_rule(&self, ns: NamespaceId, local: &str) -> Option<RuleId> {
        let q = self.lookup_qname(ns, local)?;
        if self.qnames[q.0 as usize].kind == IdKind::ReductionRule {
            Some(RuleId(q))
        } else {
            None
        }
    }

    /// Kind-agnostic lookup by `(NamespaceId, local-name)`.
    pub fn lookup_qname(&self, ns: NamespaceId, local: &str) -> Option<QName> {
        self.by_qualified.get(&(ns, local.to_string())).copied()
    }

    /// Resolve a namespace by canonical name *or* alias.
    ///
    /// Returns [`NamespaceResolution::Ambiguous`] if `name` is an alias
    /// shared by multiple namespaces. Canonical-name matches always win
    /// over alias matches.
    pub fn resolve_namespace(&self, name: &str) -> NamespaceResolution {
        if let Some(&id) = self.by_canonical.get(name) {
            return NamespaceResolution::Found(id);
        }
        match self.by_alias.get(name) {
            None => NamespaceResolution::NotFound,
            Some(v) if v.len() == 1 => NamespaceResolution::Found(v[0]),
            Some(v) => NamespaceResolution::Ambiguous(v.clone()),
        }
    }

    // -- Entry accessors -------------------------------------------------

    pub fn qname_entry(&self, q: QName) -> &QNameEntry {
        &self.qnames[q.0 as usize]
    }

    pub fn namespace_entry(&self, ns: NamespaceId) -> &NamespaceEntry {
        &self.namespaces[ns.0 as usize]
    }

    pub fn kind_of(&self, q: QName) -> IdKind {
        self.qnames[q.0 as usize].kind
    }

    /// Fully-qualified display name (`"<namespace>.<local>"`).
    pub fn qualified_name(&self, q: QName) -> String {
        let entry = self.qname_entry(q);
        let ns = self.namespace_entry(entry.namespace);
        format!("{}.{}", ns.canonical, entry.local)
    }

    /// Number of registered namespaces (used by tests / diagnostics).
    pub fn namespace_count(&self) -> usize {
        self.namespaces.len()
    }

    /// Number of registered names across all namespaces.
    pub fn qname_count(&self) -> usize {
        self.qnames.len()
    }
}

// ---------------------------------------------------------------------------
// Builders
// ---------------------------------------------------------------------------

/// Builder returned by [`Registry::namespace`].
#[must_use = "namespace builder does nothing until .create() is called"]
pub struct NamespaceBuilder<'r> {
    reg: &'r mut Registry,
    canonical: Option<Cow<'static, str>>,
    aliases: Vec<Cow<'static, str>>,
    owner: Option<OwnerInfo>,
}

impl<'r> NamespaceBuilder<'r> {
    pub fn canonical(mut self, name: impl Into<Cow<'static, str>>) -> Self {
        self.canonical = Some(name.into());
        self
    }

    pub fn alias(mut self, name: impl Into<Cow<'static, str>>) -> Self {
        self.aliases.push(name.into());
        self
    }

    pub fn owner(mut self, info: OwnerInfo) -> Self {
        self.owner = Some(info);
        self
    }

    pub fn create(self) -> Result<NamespaceId, RegisterError> {
        let canonical = self.canonical.ok_or(RegisterError::MissingCanonical)?;
        let owner = self.owner.ok_or(RegisterError::MissingOwner)?;

        if let Some(&existing) = self.reg.by_canonical.get(canonical.as_ref()) {
            let prev = &self.reg.namespaces[existing.0 as usize];
            return Err(RegisterError::NamespaceExists {
                canonical: canonical.into_owned(),
                by_crate: prev.owner.crate_name,
                at: prev.owner.registered_at,
            });
        }

        let id = NamespaceId(self.reg.namespaces.len() as u32);
        self.reg
            .by_canonical
            .insert(canonical.as_ref().to_string(), id);
        for alias in &self.aliases {
            self.reg
                .by_alias
                .entry(alias.as_ref().to_string())
                .or_default()
                .push(id);
        }
        self.reg.namespaces.push(NamespaceEntry {
            canonical,
            aliases: self.aliases,
            owner,
            members: Vec::new(),
        });
        Ok(id)
    }
}

// All op-shaped builders share this internal builder; thin wrappers expose
// only the methods that make sense for each kind.
#[must_use = "builder does nothing until .register() is called"]
pub struct OpBuilder<'r> {
    reg: &'r mut Registry,
    ns: NamespaceId,
    local: Cow<'static, str>,
    kind: IdKind,
    arity: Option<Arity>,
    pretty: Option<Cow<'static, str>>,
    owner_override: Option<OwnerInfo>,
    with_restrictor: bool,
}

impl<'r> OpBuilder<'r> {
    pub fn arity_fixed(mut self, n: u8) -> Self {
        self.arity = Some(Arity::Fixed(n));
        self
    }

    pub fn pretty(mut self, s: impl Into<Cow<'static, str>>) -> Self {
        self.pretty = Some(s.into());
        self
    }

    pub fn owner(mut self, info: OwnerInfo) -> Self {
        self.owner_override = Some(info);
        self
    }

    /// Internal registration helper used by all op-shaped builders.
    fn register_inner(self) -> Result<QName, RegisterError> {
        let arity = self.arity.ok_or(RegisterError::MissingArity)?;
        let owner = self
            .owner_override
            .unwrap_or_else(|| self.reg.namespaces[self.ns.0 as usize].owner.clone());

        let key = (self.ns, self.local.as_ref().to_string());
        if let Some(&existing) = self.reg.by_qualified.get(&key) {
            let prev = &self.reg.qnames[existing.0 as usize];
            let ns_name = &self.reg.namespaces[self.ns.0 as usize].canonical;
            return Err(RegisterError::DuplicateInNamespace {
                ns: ns_name.as_ref().to_string(),
                local: self.local.into_owned(),
                kind: prev.kind,
                by_crate: prev.owner.crate_name,
                at: prev.owner.registered_at,
            });
        }

        let q = QName(self.reg.qnames.len() as u32);
        self.reg.by_qualified.insert(key, q);
        self.reg.qnames.push(QNameEntry {
            namespace: self.ns,
            local: self.local,
            owner,
            kind: self.kind,
            arity,
            pretty: self.pretty,
            with_restrictor: self.with_restrictor,
        });
        self.reg.namespaces[self.ns.0 as usize].members.push(q);
        Ok(q)
    }

    /// Register as the kind set at construction. Returns the raw [`QName`].
    /// Use the kind-specific wrappers ([`BinderBuilder`] etc.) to obtain a
    /// typed newtype directly.
    pub fn register(self) -> Result<OpId, RegisterError> {
        debug_assert_eq!(self.kind, IdKind::Op);
        self.register_inner().map(OpId)
    }
}

#[must_use = "builder does nothing until .register() is called"]
pub struct BinderBuilder<'r> {
    inner: OpBuilder<'r>,
}

impl<'r> BinderBuilder<'r> {
    pub fn with_restrictor(mut self, b: bool) -> Self {
        self.inner.with_restrictor = b;
        self
    }

    pub fn pretty(mut self, s: impl Into<Cow<'static, str>>) -> Self {
        self.inner = self.inner.pretty(s);
        self
    }

    pub fn owner(mut self, info: OwnerInfo) -> Self {
        self.inner = self.inner.owner(info);
        self
    }

    pub fn register(self) -> Result<OpId, RegisterError> {
        self.inner.register_inner().map(OpId)
    }
}

#[must_use = "builder does nothing until .register() is called"]
pub struct PredBuilder<'r> {
    inner: OpBuilder<'r>,
}

impl<'r> PredBuilder<'r> {
    pub fn arity_fixed(mut self, n: u8) -> Self {
        self.inner = self.inner.arity_fixed(n);
        self
    }

    pub fn pretty(mut self, s: impl Into<Cow<'static, str>>) -> Self {
        self.inner = self.inner.pretty(s);
        self
    }

    pub fn owner(mut self, info: OwnerInfo) -> Self {
        self.inner = self.inner.owner(info);
        self
    }

    pub fn register(self) -> Result<PredId, RegisterError> {
        self.inner.register_inner().map(PredId)
    }
}

#[must_use = "builder does nothing until .register() is called"]
pub struct ConstBuilder<'r> {
    inner: OpBuilder<'r>,
}

impl<'r> ConstBuilder<'r> {
    pub fn pretty(mut self, s: impl Into<Cow<'static, str>>) -> Self {
        self.inner = self.inner.pretty(s);
        self
    }

    pub fn owner(mut self, info: OwnerInfo) -> Self {
        self.inner = self.inner.owner(info);
        self
    }

    pub fn register(self) -> Result<ConstId, RegisterError> {
        self.inner.register_inner().map(ConstId)
    }
}

#[must_use = "builder does nothing until .register() is called"]
pub struct SyntacticBuilder<'r> {
    inner: OpBuilder<'r>,
}

impl<'r> SyntacticBuilder<'r> {
    pub fn arity_fixed(mut self, n: u8) -> Self {
        self.inner = self.inner.arity_fixed(n);
        self
    }

    pub fn pretty(mut self, s: impl Into<Cow<'static, str>>) -> Self {
        self.inner = self.inner.pretty(s);
        self
    }

    pub fn owner(mut self, info: OwnerInfo) -> Self {
        self.inner = self.inner.owner(info);
        self
    }

    pub fn register(self) -> Result<CustomTag, RegisterError> {
        self.inner.register_inner().map(CustomTag)
    }
}

#[must_use = "builder does nothing until .register() is called"]
pub struct RuleBuilder<'r> {
    inner: OpBuilder<'r>,
}

impl<'r> RuleBuilder<'r> {
    pub fn pretty(mut self, s: impl Into<Cow<'static, str>>) -> Self {
        self.inner = self.inner.pretty(s);
        self
    }

    pub fn owner(mut self, info: OwnerInfo) -> Self {
        self.inner = self.inner.owner(info);
        self
    }

    pub fn register(self) -> Result<RuleId, RegisterError> {
        self.inner.register_inner().map(RuleId)
    }
}

// ---------------------------------------------------------------------------
// owner_info!() macro
// ---------------------------------------------------------------------------

/// Capture an [`OwnerInfo`] at the call site.
///
/// Records the calling crate's package name, package version, and the
/// `file!():line!()` position. Use this whenever passing `.owner(...)`
/// to a registry builder.
#[macro_export]
macro_rules! owner_info {
    () => {
        $crate::registry::OwnerInfo {
            crate_name: env!("CARGO_PKG_NAME"),
            registered_at: concat!(file!(), ":", line!()),
            version: Some(env!("CARGO_PKG_VERSION")),
        }
    };
}

// Re-export at the module level so `use montague_core::registry::owner_info;`
// works alongside the types it depends on.
pub use crate::owner_info;

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn owner() -> OwnerInfo {
        OwnerInfo {
            crate_name: "test",
            registered_at: "test.rs:0",
            version: None,
        }
    }

    fn ns(reg: &mut Registry, canonical: &'static str, alias: Option<&'static str>) -> NamespaceId {
        let mut b = reg.namespace().canonical(canonical).owner(owner());
        if let Some(a) = alias {
            b = b.alias(a);
        }
        b.create().unwrap()
    }

    #[test]
    fn create_namespace_and_op() {
        let mut reg = Registry::empty();
        let fol = ns(&mut reg, "montague.fol", Some("fol"));
        let and = reg.op(fol, "and").arity_fixed(2).register().unwrap();
        assert_eq!(reg.lookup_op(fol, "and"), Some(and));
        assert_eq!(reg.kind_of(and.qname()), IdKind::Op);
        assert_eq!(reg.qualified_name(and.qname()), "montague.fol.and");
    }

    #[test]
    fn duplicate_namespace_errors() {
        let mut reg = Registry::empty();
        ns(&mut reg, "montague.fol", None);
        let err = reg
            .namespace()
            .canonical("montague.fol")
            .owner(owner())
            .create()
            .unwrap_err();
        assert!(matches!(err, RegisterError::NamespaceExists { .. }));
    }

    #[test]
    fn duplicate_op_in_namespace_errors() {
        let mut reg = Registry::empty();
        let fol = ns(&mut reg, "montague.fol", None);
        reg.op(fol, "and").arity_fixed(2).register().unwrap();
        let err = reg.op(fol, "and").arity_fixed(2).register().unwrap_err();
        assert!(matches!(err, RegisterError::DuplicateInNamespace { .. }));
    }

    #[test]
    fn cross_namespace_no_conflict() {
        let mut reg = Registry::empty();
        let a = ns(&mut reg, "montague.fol", None);
        let b = ns(&mut reg, "montague.linear", None);
        let a_implies = reg.op(a, "implies").arity_fixed(2).register().unwrap();
        let b_implies = reg.op(b, "implies").arity_fixed(2).register().unwrap();
        assert_ne!(a_implies, b_implies);
        assert_eq!(reg.lookup_op(a, "implies"), Some(a_implies));
        assert_eq!(reg.lookup_op(b, "implies"), Some(b_implies));
    }

    #[test]
    fn alias_resolution() {
        let mut reg = Registry::empty();
        let fol = ns(&mut reg, "montague.fol", Some("fol"));
        assert_eq!(
            reg.resolve_namespace("montague.fol"),
            NamespaceResolution::Found(fol)
        );
        assert_eq!(
            reg.resolve_namespace("fol"),
            NamespaceResolution::Found(fol)
        );
        assert_eq!(
            reg.resolve_namespace("does-not-exist"),
            NamespaceResolution::NotFound
        );
    }

    #[test]
    fn alias_collision_is_ambiguous() {
        let mut reg = Registry::empty();
        let a = ns(&mut reg, "montague.fol", Some("fol"));
        let b = ns(&mut reg, "alice.fol", Some("fol"));
        match reg.resolve_namespace("fol") {
            NamespaceResolution::Ambiguous(v) => {
                assert_eq!(v.len(), 2);
                assert!(v.contains(&a));
                assert!(v.contains(&b));
            }
            other => panic!("expected Ambiguous, got {other:?}"),
        }
        // Canonical names still resolve unambiguously.
        assert_eq!(
            reg.resolve_namespace("montague.fol"),
            NamespaceResolution::Found(a)
        );
        assert_eq!(
            reg.resolve_namespace("alice.fol"),
            NamespaceResolution::Found(b)
        );
    }

    #[test]
    fn binder_kind_is_preserved() {
        let mut reg = Registry::empty();
        let fol = ns(&mut reg, "montague.fol", None);
        let forall = reg
            .binder(fol, "forall")
            .with_restrictor(true)
            .register()
            .unwrap();
        assert_eq!(reg.kind_of(forall.qname()), IdKind::Binder);
        // lookup_op accepts both Op and Binder kinds (both share OpId).
        assert_eq!(reg.lookup_op(fol, "forall"), Some(forall));
        // Predicates/customs/rules don't.
        assert_eq!(reg.lookup_pred(fol, "forall"), None);
    }

    #[test]
    fn const_roundtrip_and_kind() {
        let mut reg = Registry::empty();
        let std_ns = ns(&mut reg, "montague.standard", None);
        let socrates = reg.const_(std_ns, "socrates").register().unwrap();
        assert_eq!(reg.kind_of(socrates.qname()), IdKind::Const);
        assert_eq!(reg.lookup_const(std_ns, "socrates"), Some(socrates));
        // lookup_pred and lookup_op should reject consts.
        assert_eq!(reg.lookup_pred(std_ns, "socrates"), None);
        assert_eq!(reg.lookup_op(std_ns, "socrates"), None);
    }

    #[test]
    fn pred_and_custom_tag_distinct() {
        let mut reg = Registry::empty();
        let std_ns = ns(&mut reg, "montague.standard", None);
        let man = reg.pred(std_ns, "man").arity_fixed(1).register().unwrap();
        let left_adj = reg
            .syntactic_connective(std_ns, "left_adj")
            .arity_fixed(1)
            .register()
            .unwrap();
        assert_eq!(reg.kind_of(man.qname()), IdKind::Pred);
        assert_eq!(reg.kind_of(left_adj.qname()), IdKind::SyntacticConnective);
        assert_eq!(reg.lookup_pred(std_ns, "man"), Some(man));
        assert_eq!(reg.lookup_pred(std_ns, "left_adj"), None);
        assert_eq!(reg.lookup_custom_tag(std_ns, "left_adj"), Some(left_adj));
    }

    #[test]
    fn owner_info_macro_captures_call_site() {
        let info = owner_info!();
        assert_eq!(info.crate_name, "montague-core");
        assert!(info.registered_at.contains("registry.rs"));
        assert!(info.version.is_some());
    }

    #[test]
    fn namespace_members_track_registrations() {
        let mut reg = Registry::empty();
        let fol = ns(&mut reg, "montague.fol", None);
        let and = reg.op(fol, "and").arity_fixed(2).register().unwrap();
        let or = reg.op(fol, "or").arity_fixed(2).register().unwrap();
        let entry = reg.namespace_entry(fol);
        assert_eq!(entry.members, vec![and.qname(), or.qname()]);
    }

    #[test]
    fn missing_canonical_or_owner_errors() {
        let mut reg = Registry::empty();
        let err = reg.namespace().owner(owner()).create().unwrap_err();
        assert!(matches!(err, RegisterError::MissingCanonical));

        let err = reg
            .namespace()
            .canonical("alice.modal")
            .create()
            .unwrap_err();
        assert!(matches!(err, RegisterError::MissingOwner));
    }

    #[test]
    fn missing_arity_errors_for_op() {
        let mut reg = Registry::empty();
        let fol = ns(&mut reg, "montague.fol", None);
        let err = reg.op(fol, "and").register().unwrap_err();
        assert!(matches!(err, RegisterError::MissingArity));
    }
}

// ---------------------------------------------------------------------------
// Property tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    fn owner() -> OwnerInfo {
        OwnerInfo {
            crate_name: "proptest",
            registered_at: "proptest.rs:0",
            version: None,
        }
    }

    // A name regex narrow enough to be valid identifiers without exhausting
    // proptest's case budget on weird Unicode.
    const IDENT: &str = "[a-z][a-z0-9_]{0,15}";
    const QUALIFIED: &str = "[a-z][a-z0-9_]{0,15}(\\.[a-z][a-z0-9_]{0,15}){0,3}";

    proptest! {
        #[test]
        fn register_then_lookup_roundtrip(
            ns_name in QUALIFIED,
            op_name in IDENT,
        ) {
            let mut reg = Registry::empty();
            let ns = reg
                .namespace()
                .canonical(ns_name.clone())
                .owner(owner())
                .create()
                .unwrap();
            let op = reg.op(ns, op_name.clone()).arity_fixed(2).register().unwrap();
            prop_assert_eq!(reg.lookup_op(ns, &op_name), Some(op));
            prop_assert_eq!(reg.kind_of(op.qname()), IdKind::Op);
            prop_assert_eq!(reg.qualified_name(op.qname()), format!("{ns_name}.{op_name}"));
        }

        #[test]
        fn duplicate_registration_errors(
            ns_name in IDENT,
            op_name in IDENT,
        ) {
            let mut reg = Registry::empty();
            let ns = reg
                .namespace()
                .canonical(ns_name)
                .owner(owner())
                .create()
                .unwrap();
            reg.op(ns, op_name.clone()).arity_fixed(2).register().unwrap();
            let result = reg.op(ns, op_name).arity_fixed(2).register();
            let is_duplicate = matches!(result, Err(RegisterError::DuplicateInNamespace { .. }));
            prop_assert!(is_duplicate);
        }

        #[test]
        fn cross_namespace_does_not_conflict(
            ns_a in IDENT,
            ns_b in IDENT,
            op_name in IDENT,
        ) {
            prop_assume!(ns_a != ns_b);
            let mut reg = Registry::empty();
            let a = reg.namespace().canonical(ns_a).owner(owner()).create().unwrap();
            let b = reg.namespace().canonical(ns_b).owner(owner()).create().unwrap();
            let op_a = reg.op(a, op_name.clone()).arity_fixed(2).register().unwrap();
            let op_b = reg.op(b, op_name).arity_fixed(2).register().unwrap();
            prop_assert_ne!(op_a, op_b);
        }

        #[test]
        fn many_registrations_preserve_unique_qnames(
            ops in proptest::collection::vec(IDENT, 0..30)
        ) {
            let mut reg = Registry::empty();
            let ns = reg
                .namespace()
                .canonical("montague.test")
                .owner(owner())
                .create()
                .unwrap();
            let mut registered: std::collections::HashSet<OpId> = Default::default();
            for name in ops.into_iter().collect::<std::collections::BTreeSet<_>>() {
                let op = reg.op(ns, name).arity_fixed(2).register().unwrap();
                prop_assert!(registered.insert(op), "QName was reused");
            }
        }
    }
}
