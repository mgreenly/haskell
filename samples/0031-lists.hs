--
-- I want to introduce the basics of pattern matching with lists
--


sampleList = [ 1, 2, 3 ]

-- lets implement some of the functions that Data.List provided

-- much like the show example earlier.  The first function takes a single argument, but that argument is being
-- deconstructed.  The value-constructor for a list is the : operator.  So we use that value-constructor to
-- destrucure hte list.
--
-- The 'first' funciton is our version of 'head.  In this funciton we only care about the first element of
-- argument so we give that the name 'x' and use the underscore to indicate we don't care about the second value.
first (x:_) = x

-- The 'rest' function is our version of 'tail'.  In this function we only care about everything after the first
-- elment so we give it the name 'xs', that's just x plural, a common idiom in haskell code.
rest (_:xs) = xs



main = do
    print sampleList                 -- [1,2,3]
    print (first sampleList)         -- 1
    print (rest sampleList)          -- [2,3]


--
-- There are definitely problems with the code above.  It doesn't handle empty lists well for example.
-- We'll learn more about that shortly but it shows the basics of destructuring a list
--
