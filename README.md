# Montague

<p align="center">
  <img src="https://people.umass.edu/scable/LING797M-FA19/Montague.jpg" width=230/>
</p>

 > "I reject the contention that an important theoretical difference exists between formal and natural languages." 
 >       
 >    -- Richard Montague

<p align="center">
  <a href="https://www.haskell.org/">
    <img src="https://img.shields.io/badge/Language-Haskell-blue">
  </a>
  <img src="https://img.shields.io/badge/Hackage-TODO-red">
  <img src="https://img.shields.io/badge/License-MIT-blue">
</p>

Montague (named in honor of mathematician and philosopher [Richard Montague](https://en.wikipedia.org/wiki/Richard_Montague)) is a library for non-deterministically parsing natural language expressions into a structured form, so that they can be analyzed, or so that inferences may be made from them.

It is loosely based on the general ideas of [Montague Semantics](https://plato.stanford.edu/entries/montague-semantics/#ComMonSem), and uses a simple [lambek calculus](https://en.wikipedia.org/wiki/Categorial_grammar) with subtyping, and a lexicon translating from strings of text (possibly nondeterministically) to a term of a particular type of the configured lambek grammar.

Inspiration
-----------


This (as far as I know of) novel approach was first formulated in 2018 when I 
 was in grad school studying type theory and categorial grammar. 
 The idea was, rather than having a small number of basic types (such as 
 `noun`, `verb`, `noun phrase`, and `sentence`) combined with the two "lambek arrows"
 `/` and `\` to denote grammatical categories (which allows for nonsense productions
 such as `colorless green ideas sleep furiously`), we could extend this system with subtyping, and
 more apecific semantic categories such as `person <: noun`.
 
 Prohibiting such 
 "nonsense productions" with this idea then both simplifies the process of parsing a 
 sequence of words into a structured representation by pruning the possible 
 search space, as well as facilitates transforming such structured output 
 into a meaningful logical format (for instance, queries in a typed logic programming 
 language). Montague is an attempt to put these ideas in practice.

Basic Usage
----------- 

In order to support the largest amount of use cases, Montague does not perscribe any specific format for it's output, but some example output types might include prolog, datalog, or some other logic programming language clauses. The two core types in Montague are `LambekType t` and `Term a t`, where `t` is a user-defined tye of "base types" 
to be used in production (such as `noun`, `person`), and `a` is a user-defined type 
of "atomic terms" reresenting the type of structured output produced by Montague.




For example, some basic definitions of `a` and `t` might include:

```haskell
data MyType =
    Noun
  | NounPhrase
  | Sentence
  | Person
  | Place
  | Thing

data MyAtom =
    Bob
  | Alice
  | NewYork
  | LosAngeles
  | Car
  | Owns
  | LivesIn
```

Given such types, the typeclass `MontagueSemantics a t x` is used
 to determine an interpretation of `Term a t` into
 x, where x is the user's target structured output, as opposed to 
 `Term a t`, which is only used internally by Montague.

```haskell
class (Eq x, PartialOrd t) => MontagueSemantics a t x | a -> t, a -> x where
    typeOf    :: Term a t -> MontagueType t
    parseTerm :: String -> MontagueTerm a t
    interp    :: AnnotatedTerm a t -> x
```

For instance, an example implementation of this is:

```haskell
instance MontagueSemantics MyAtom MyType (AnnotatedTerm MyAtom MyType) where
    typeOf = \case
        Atom Bob        -> pure Person
        Atom Alice      -> pure Person
        Atom NewYork    -> pure Place
        Atom LosAngeles -> pure Place
        Atom Car        -> pure Thing
        Atom Owns       -> pure $ Person `RightArrow` Sentence `LeftArrow` Thing
        Atom LivesIn    -> pure $ Person `RightArrow` Sentence `LeftArrow` Place
        _ -> mempty
    parseTerm = \case
        "bob"         -> pure Bob
        "alice"       -> pure Alice
        "new york"    -> pure NewYork
        "los angeles" -> pure LosAngeles
        "car"         -> pure Car
        "owns"        -> pure Owns
        "lives in"    -> pure LivesIn
        _             -> mempty
    interp = id
```

Todo
----

Montague is in it's early stages and I am not an expert, so there is much that 
remains to be done. All contributions, both from experts in the field, or interested 
beginners/novices are welcome.

  * The type system of Montague is pretty simple, and does not support 
   some very important features of montague semantics such as quantifiers.
  * The examples are all in English. Add some more examples in other languages. Particularly 
    interesting to me is the encoding of case-heavy languages like Polish. This may require some more advanced type system features in order to implement.

   
