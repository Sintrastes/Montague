use crate::types::{AnnotatedTerm, LambekType, LatticeOrd};
use crate::semantics::Semantics;

/// Given a set of partially-parsed annotated terms, suggest atoms that could
/// extend the parse by filling a pending `LeftArrow` argument slot.
///
/// Mirrors `Montague.Autocomplete.getAutocomplete`.
pub fn get_autocomplete<A, T, X>(
    sem: &Semantics<A, T, X>,
    partial: &[AnnotatedTerm<A, T>],
    all_atoms: &[A],
) -> Vec<A>
where
    T: LatticeOrd + Clone + PartialEq,
    A: Clone,
{
    let mut suggestions = Vec::new();
    for annotated in partial {
        if let LambekType::LeftArrow(_, needed_ty) = &annotated.ty {
            for atom in all_atoms {
                let atom_types = (sem.type_of_atom)(atom);
                for ty in &atom_types {
                    if ty.leq(needed_ty) {
                        suggestions.push(atom.clone());
                        break;
                    }
                }
            }
        }
    }
    suggestions
}
