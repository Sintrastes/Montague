
module Montague.Experimental.ProofRelevant where

{-

Consider the sentence "Would you like coffee, or tea?".

This could be interpreted as a linear logic statement as:

  1. Coffee & Tea -- Do you want either coffee or tea (You don't get to choose which one)
  2. Coffee ⊕ Tea -- Do you want either coffee or tea (I'm asking you to make the choice)
  3. Coffee ⊗ Tea -- Do you want coffee and tea? (We interpret the or as non-exlcusive)

This leads to the question if we could encode this kind of pragmatic information in a dialog
 via linear logic, potentially with either game or some other type of resource semantics 
 (see https://mathoverflow.net/questions/155626/models-of-intuitionistic-linear-logic-that-reflect-the-resource-interpretation)

Note: "par" also has a really interesting interpretation in terms of resource managemnet
 (see https://ncatlab.org/nlab/show/multiplicative+disjunction)

Note that these "statements" could be interpreted also as requesting the other participant
 in the conversation an element of one of these terms -- thus the conncection to proof relevance
 here.

However, somewhat more generally, I think that proof relevant would be very important in a dialog
 system, for instance if someone said "It used to be hot", one could ask for evidence (a proof term,
 in this case a specific date where this held true).

This also brings up the queston of how dependent types play into natural language semantics and 
 categorial grammar.
-}