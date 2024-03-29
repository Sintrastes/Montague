
module Montague.Display where

import Montague.Types

-- | Helper function to display a parsed term as a prolog term.
displayAsPrologTerm :: Show a => Term a -> String
displayAsPrologTerm x = show x

-- | Helper function to display a parsed term as an s-expression.
displayAsSExp :: Show a => Term a -> String
displayAsSExp (Atom xs) = show xs
displayAsSExp (App x xs) = "(" ++ show x ++ (foldr1 (\x -> \y -> x ++ "," ++ y) (map show xs)) ++ ")"