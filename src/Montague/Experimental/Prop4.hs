
module Montague.Experimental.Prop4 where

-- 4-valued logic from the paper
-- "Quickstrom: Property-based Acceptance Testing with 
--   LTL Specifications"
data Prop4 =
   -- Definitevely true
   DefTrue
   -- Definitively false
 | DefFalse
   -- Presumptively true
 | PreTrue
   -- Presumtively false
 | PreFalse