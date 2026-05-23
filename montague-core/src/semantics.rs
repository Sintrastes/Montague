use crate::types::{AnnotatedTerm, LambekType, NonDet, Term};

type TypeFn<A, T> = Box<dyn Fn(&A) -> NonDet<LambekType<T>>>;
type ParseFn<A> = Box<dyn Fn(&str) -> NonDet<Term<A>>>;

/// Defines the semantics for parsing a language.
///
/// - `A`: atom type (semantic entities — Bob, Likes, …)
/// - `T`: base type enum (Noun, Person, Sentence, …)
/// - `X`: interpretation output type
pub struct Semantics<A, T, X> {
    /// Map an atom to all possible Lambek types it can have.
    pub type_of_atom: TypeFn<A, T>,
    /// Map a surface word to all possible terms.
    pub parse_term: ParseFn<A>,
    /// Interpret a fully-typed term into the output type.
    pub interp: Box<dyn Fn(AnnotatedTerm<A, T>) -> X>,
}

impl<A, T, X> Semantics<A, T, X> {
    pub fn new(
        type_of_atom: impl Fn(&A) -> NonDet<LambekType<T>> + 'static,
        parse_term: impl Fn(&str) -> NonDet<Term<A>> + 'static,
        interp: impl Fn(AnnotatedTerm<A, T>) -> X + 'static,
    ) -> Self {
        Self {
            type_of_atom: Box::new(type_of_atom),
            parse_term: Box::new(parse_term),
            interp: Box::new(interp),
        }
    }
}
