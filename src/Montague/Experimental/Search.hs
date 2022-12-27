{-# LANGUAGE ExistentialQuantification, DataKinds #-}

module Montague.Experimental.Search where
import Data.Row
import Montague.Experimental.Typed
import Montague.Experimental.LambekType

-- Research Question: Is there actually anything we can do here
-- when generalizing over truth values? This would lead to a kind
-- of logic programing with integrated Kripke-type semantics
-- (at least in the case of (w -> Bool)), but I'm not sure if there's
-- a type of "rule" with any sort of semantics other than structural
-- unificaton.

-- I guess what this might look like in practice is that rules can refer to
--  various things in the "world" -- for instance, indexicals (current_user), or
--  things like the current time and so on. And I think this is probably similar
--  to earlier ideas I had for how to handle indexicals.

-- Just throwing ideas around here, but maybe what will end up making sense is
--  using the different truth values for "side-conditions" -- i.e. only actually
--  trying to evaluate them when they are constant values. Otherwise, we can 
--  treat them structurally.

-- This also brings up the question of whether or not to allow structural matching
--  on any sort of modal operator that is tied to the semantics. My initial
--  guess is probably not. However, I do have some ideas about how to potentially
--  incorporate matching with standard quantifiers in a ruleset -- for instance,
--  we can fully evaluate a universally quantified statement as "true" if there
--  exists a universally quantified rule that matches it.

type RuleHead _Ω xs = Row xs -> Term (T _Ω)
type RuleBody _Ω xs = Row xs -> [Term (T _Ω)]

data Rule _Ω = forall xs. Rule (RuleHead _Ω xs) (RuleBody _Ω xs)

