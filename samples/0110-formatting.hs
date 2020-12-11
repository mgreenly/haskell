{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- cabal install formatting --lib --global
--
--   https://hackage.haskell.org/package/formatting
--
--   specifically take not of the formatters and combinators
--

import Prelude              (($), (+), (*))
import Data.Text.Lazy.IO    (putStrLn)
import Formatting           ((%), format, text, int)

main = do
    let result1 = format template1 "Tim" 37
    let result2 = format template1 "Sam" 42
    let result3 = format template2 1

    putStrLn result1
    putStrLn result2
    putStrLn result3

  where
    template1 = text % " says the number is " % int % "."
    template2 = "this template only took one param it it was " % int % "."
