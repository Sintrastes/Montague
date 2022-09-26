
module Montague.Experimental.Dialog where
import Montague.Experimental.Typed

-- An experimental module for creating chat/dialog systems.

-- | Takes a method of deriving the initial "world" from IO,
-- together with a method for responding to user input, and 
-- constructs a dialog system.
dialog :: IO w -> (Term () -> w -> (String, w)) -> IO ()
dialog getWorld respond = undefined

-- Note: The simplest chat bot (assistant) would simply
-- take as fact anything that the user tells them.
--
-- For instance "My name is nate" would make the 
-- bot store the fact name(user, "nate"), or saying
-- "I am 28 years old" would store the fact age(user, years(28)).

