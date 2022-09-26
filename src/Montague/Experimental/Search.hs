{-# LANGUAGE ExistentialQuantification #-}

module Montague.Experimental.Search where
import Data.Row
import Montague.Experimental.Typed

type RuleHead _Ω xs = Row xs -> Term _Ω _Ω
type RuleBody _Ω xs = Row xs -> [Term _Ω _Ω]

data Rule _Ω = forall xs. Rule (RuleHead _Ω xs) (RuleBody _Ω xs)

