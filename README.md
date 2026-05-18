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
  <a href="https://github.com/Sintrastes/Montague/actions/workflows/build.yml">
    <img src="https://github.com/Sintrastes/Montague/actions/workflows/build.yml/badge.svg">
  </a>
  <img src="https://img.shields.io/badge/Hackage-TODO-red">
  <img src="https://img.shields.io/badge/License-AGPL v3.0-blue">
</p>

Montague (named in honor of mathematician and philosopher [Richard Montague](https://en.wikipedia.org/wiki/Richard_Montague)) is a library and domain specific language for non-deterministically parsing natural language expressions into a structured form, so that they can be analyzed, or so that inferences may be made from them.

It is loosely based on the general ideas of [Montague Semantics](https://plato.stanford.edu/entries/montague-semantics/#ComMonSem), and uses a simple [lambek calculus](https://en.wikipedia.org/wiki/Categorial_grammar) equipped with subtyping, together with a user-provided lexicon translating from strings of text (possibly nondeterministically) to a term of a particular type of the configured lambek grammar.

To try it out for yorself, see [montague-reflex](https://github.com/Sintrastes/montague-reflex) for a web and mobile interface for Montague.

**Note**: Montague was originally prototyped in Haskell, but is currently in the process of being re-written in Rust. While Haskell is a great language, I think that Rust is the better choice for this project in the long term, as it makes it a lot easier to integrate Montague into various potential end-user applications.

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

Montague in The AI Era
-----------------------

I originally developed Montague right before the explosion in interest in generative AI, the transformer architecture, and LLMs. My goal with the project was to accomplish things that [LLMs can now very readily do](https://github.com/safishamsi/graphify). So what now is the point of Montague?

One of the major concerns people have over the rise of AI systems is their large resource requirements. Another concern (though this is becoming less and less of a concern with each generation of frontier models) are things like inaccuracies and "hallucinations".

I think that one way we might work to address these concerns, but still build useful AI systems for everyday people and knowledge workers designed to enchance (rather than replace) intellegence is to incorporate small language models with "good-old-fashioned AI" (i.e. logic programming, symbolic reasoning). 

Moving forward, I think Montague (or something like it) could prove incredibly useful as a "first pass" for AI-based query engines (e.x. first try to use Montague to parse and analyze a sentence, if it fails, invoke an LLM, or use a small language model to refine the grammatical schema based on the failed query). There are other fascinating possibilities as well (for instance, the use of Montague to encourage AI agents to be more percise in analyzing user queries, prompting them to ask clarifying questions to the user if Montague determines an important sentence ambiguity).

There are probably hundreds of other potentially interesting applications and techniques riffing on the techniques from Montague. I hope at the very least this project inspires other to build bigger and better things!

**AI Disclosure**: As much as I enjoy software development for it's own sake, and appreciate some good human-written hand-crafted code -- I'm also a fan of getting things done, and have limited free time to work on this (and other) projects. The ideas in this project (unless otherwise cited) are all my own, but I make use of agentic AI to help assist me in the development of Montague. 

Domain specific language
------------------------

Although Montague has a set of Rust (and Haskell) APIs to work with directly, Montague also provides a domain-specific language (similar to happy or alex) for defining schemas for parsing natural language syntax into structured formated for a specific domain. Schemas can be defined in `.mont` files, and are formatted by providing a header section with definitions of types and atoms, followed by a list of sequents denoting which strings map to whics atoms:

```
% example.mont
% types

MyType =
    Noun
  | Sentence
  | Question
  | Person
  | Place
  | DefiniteNoun
  | Determiner

Person :< Noun.
Place :< Noun.
DefiniteNoun :< Noun.
Question :< Noun.

% atoms

Nate: Person.
Berlin: Place.
Apple: DefiniteNoun.
Mother: Noun.

% productions

like, love  --> Like.
i           --> I.
killed      --> Killed.
the         --> The.
man, person --> Man. 
with        --> With.
spoon       --> Spoon.
```

Todo
----

Montague is in it's early stages and I am not an expert, so there is much that 
remains to be done. All contributions, both from experts in the field, or interested 
beginners/novices are welcome.

  * The type system of Montague is pretty simple, and does not support 
   some very important features of montague semantics such as quantifiers.
  * Indexicals probably deserve specail treatement.
  * Add support for better error reporting.
  * Needs support for associativity.
  * The examples are all in English. Add some more examples in other languages. Particularly 
    interesting to me is the encoding of case-heavy languages like Polish. This may require some more advanced type system features in order to implement.

   
