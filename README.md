# montague

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

It is loosely based on the general ideas of [Montague Semantics](https://plato.stanford.edu/entries/montague-semantics/#ComMonSem), and uses a simple [lambek grammar](https://en.wikipedia.org/wiki/Categorial_grammar) and a lexicon translating from strings of text (possibly nondeterministically) to a term of a particular type of the configured lambek grammer. 

In order to support the largest amount of use cases, montague does not perscribe any specific format for it's output, but some example output types might include prolog, datalog, or some other logic programming language clauses.
