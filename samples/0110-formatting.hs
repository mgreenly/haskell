{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- cabal install formatting --lib --global
--
--   https://hackage.haskell.org/package/formatting
--
--   specifically take not of the formatters and combinators
--

import Formatting           ((%), format, fprintLn, int, later, lpadded, now, surrounded, text)

main = do
    fprintLn text "hi"
    --
    -- The function 'fprintLn' takes a single argument which, in this case, is another function called 'text'.
    --
    -- The function 'text' returns a value of type Formatter.
    --
    -- The function 'fprintLn' runs the formatter.
    --
    -- Running (fprintLn text) produces another function that expects to consume one text value and then print it
    -- to stdout.
    --
    -- So while it looks like fprintLn is taking two arguments it's not.
    --
    -- It takes a single argument and produces a function that takes a single argument.


    --
    -- multiple formatters can be combined with combinators
    --
    -- Combinators are just functions that take other functions as parameters and produce new functions
    --
    -- The Formatting package provides many combinators and one very basic one is % which provides concatenation.
    --
    fprintLn (text % text) "abc" "xyz"
    --
    -- Remember (text % text) is evaluated as ((%) text text) because % is a symbol.
    --
    -- So % is a function that takes two formatters as arguments and returns a new formatter that concatenates the
    -- results of it's arguments.
    --
    -- There are many interesting combinators, lpadded provides left padding.  So in this example it will pad the
    -- result of the formatter that is it's third argument with spaces so the final result is at least 12 characters
    -- wide.
    --
    fprintLn (lpadded 12 ' ' text) "hi"
    --
    -- again the point of combinators is that they can be used to combine formatters into increasingly complex
    -- formatters.
    --
    fprintLn ((surrounded "*" text) % (surrounded "+" text)) "Hi" "Bye"
