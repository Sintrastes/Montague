
module Montague.Lexicon where

import Montague.Types

-- | Raw representation of a parsed montague lexicon file.
data LexiconFile a t = LexiconFile {
    entries :: [LexiconEntry a t]
}

-- Representation of an entry in a montague lexicon.
data LexiconEntry a t = LexiconEntry {
    words :: [String],
    types :: [Term a t]
}

-- | Load a montague lexicon from a file.
getLexicon :: FilePath -> IO (Lexicon a t)
getLexicon = undefined

parseLexicon :: (String -> a) -> String -> Maybe (LexiconFile a t)
parseLexicon parseAtom x = undefined

parseLexiconEntry :: (String -> a) -> String -> Maybe (LexiconEntry a t)
parseLexiconEntry parseAtom x = undefined
