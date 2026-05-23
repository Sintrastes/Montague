//! Proc-macros for authoring Montague profiles.
//!
//! ## `#[derive(MontagueProfile)]`
//!
//! Generates a `register()` method on the struct.
//!
//! ```ignore
//! #[derive(MontagueProfile)]
//! #[montague(namespace = "montague.fol", alias = "fol")]
//! pub struct Fol {
//!     #[op(name = "and", arity = 2)]
//!     pub and: OpId,
//!     #[binder(name = "forall", restrictor)]
//!     pub forall: OpId,
//! }
//! ```

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Lit, Token};

// ---------------------------------------------------------------------------
// Named-argument list: `(key = "val", key2 = 3, flag1, flag2)`
// ---------------------------------------------------------------------------

/// A single entry: either `key = value` or bare `key` (a flag).
enum AttrEntry {
    KeyValue { key: syn::Ident, value: Lit },
    Flag { key: syn::Ident },
}

impl syn::parse::Parse for AttrEntry {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let key: syn::Ident = input.parse()?;
        if input.peek(Token![=]) {
            let _eq: Token![=] = input.parse()?;
            let value: Lit = input.parse()?;
            Ok(AttrEntry::KeyValue { key, value })
        } else {
            Ok(AttrEntry::Flag { key })
        }
    }
}

struct AttrList(Vec<AttrEntry>);

impl AttrList {
    fn get_str(&self, key: &str) -> Option<String> {
        self.0.iter().find_map(|e| match e {
            AttrEntry::KeyValue {
                key: k,
                value: Lit::Str(s),
            } if *k == key => Some(s.value()),
            _ => None,
        })
    }

    fn get_int(&self, key: &str) -> Option<u64> {
        self.0.iter().find_map(|e| match e {
            AttrEntry::KeyValue {
                key: k,
                value: Lit::Int(i),
            } if *k == key => i.base10_parse().ok(),
            _ => None,
        })
    }

    fn has_flag(&self, key: &str) -> bool {
        self.0
            .iter()
            .any(|e| matches!(e, AttrEntry::Flag { key: k } if k == key))
    }
}

impl syn::parse::Parse for AttrList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // parse_args() already strips the outer `(...)`.
        let mut entries = Vec::new();
        while !input.is_empty() {
            entries.push(input.parse()?);
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
        Ok(AttrList(entries))
    }
}

// ---------------------------------------------------------------------------
// Struct-level #[montague(...)]
// ---------------------------------------------------------------------------

struct MontagueStructAttrs {
    namespace: String,
    alias: Option<String>,
}

fn parse_struct_attrs(attrs: &[syn::Attribute]) -> syn::Result<MontagueStructAttrs> {
    for attr in attrs {
        if !attr.path().is_ident("montague") {
            continue;
        }
        let args: AttrList = attr.parse_args()?;
        let namespace = args.get_str("namespace").ok_or_else(|| {
            syn::Error::new(attr.span(), "required: #[montague(namespace = \"...\")]")
        })?;
        let alias = args.get_str("alias");
        return Ok(MontagueStructAttrs { namespace, alias });
    }
    Err(syn::Error::new(
        proc_macro2::Span::call_site(),
        "missing #[montague(namespace = \"...\")] attribute",
    ))
}

// ---------------------------------------------------------------------------
// Field-level #[op(...)] / #[binder(...)]
// ---------------------------------------------------------------------------

struct FieldSpec {
    field_name: syn::Ident,
    name: String,
    arity: Option<u64>,
    is_binder: bool,
    restrictor: bool,
}

fn parse_field_spec(field: &syn::Field) -> syn::Result<Option<FieldSpec>> {
    let field_name = field.ident.clone().unwrap();
    for attr in &field.attrs {
        let is_op = attr.path().is_ident("op");
        let is_binder = attr.path().is_ident("binder");
        if !is_op && !is_binder {
            continue;
        }
        let args: AttrList = attr.parse_args()?;
        let name = args.get_str("name").ok_or_else(|| {
            syn::Error::new(attr.span(), "required: #[op/binder(name = \"...\")]")
        })?;
        let arity = args.get_int("arity");
        let restrictor = args.has_flag("restrictor");
        return Ok(Some(FieldSpec {
            field_name,
            name,
            arity,
            is_binder,
            restrictor: restrictor && is_binder,
        }));
    }
    Ok(None)
}

// ---------------------------------------------------------------------------
// The macro itself
// ---------------------------------------------------------------------------

#[proc_macro_derive(MontagueProfile, attributes(montague, op, binder))]
pub fn derive_montague_profile(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand(input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = &input.ident;
    let sa = parse_struct_attrs(&input.attrs)?;
    let namespace_str = &sa.namespace;
    let alias_str = sa.alias.as_ref();

    let named_fields = match &input.data {
        syn::Data::Struct(s) => match &s.fields {
            syn::Fields::Named(nf) => &nf.named,
            _ => {
                return Err(syn::Error::new(
                    struct_name.span(),
                    "MontagueProfile requires a struct with named fields",
                ))
            }
        },
        _ => {
            return Err(syn::Error::new(
                struct_name.span(),
                "MontagueProfile can only be derived on structs",
            ))
        }
    };

    let field_specs: Vec<_> = named_fields
        .iter()
        .filter_map(|f| parse_field_spec(f).transpose())
        .collect::<syn::Result<_>>()?;

    let alias_call = if let Some(a) = alias_str {
        quote! { .alias(#a) }
    } else {
        quote! {}
    };

    let struct_field_inits: Vec<_> = named_fields
        .iter()
        .filter(|f| f.ident.as_ref().unwrap() != "ns")
        .map(|f| {
            let ident = f.ident.as_ref().unwrap();
            if let Some(spec) = field_specs.iter().find(|s| &s.field_name == ident) {
                let name = &spec.name;
                if spec.is_binder {
                    let r = spec.restrictor;
                    quote! {
                        #ident: reg.binder(__ns, #name)
                            .with_restrictor(#r)
                            .register()?,
                    }
                } else {
                    let n = spec.arity.unwrap_or(1) as u8;
                    quote! {
                        #ident: reg.op(__ns, #name)
                            .arity_fixed(#n)
                            .register()?,
                    }
                }
            } else {
                quote! { #ident: Default::default(), }
            }
        })
        .collect();

    let gen = quote! {
        impl #struct_name {
            pub fn register(
                reg: &mut ::montague_core::registry::Registry,
            ) -> ::std::result::Result<Self, ::montague_core::registry::RegisterError> {
                let __ns = reg
                    .namespace()
                    .canonical(#namespace_str)
                    #alias_call
                    .owner(::montague_core::registry::owner_info!())
                    .create()?;
                Ok(#struct_name {
                    ns: __ns,
                    #(#struct_field_inits)*
                })
            }
        }
    };

    Ok(gen)
}
