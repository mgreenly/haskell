--
-- 0020-first-type.sh is not idiomatic haskell.
--
-- It took some liberties to ensure the concepts werer easily understood.
--
-- Typically in haskell we'd always use the same value for the type and the value-constructor.
--
-- We'd also allow the compiler to automatically derive the show function for us.
--
-- We'd may define the type like this
--

data Point =
    Point Integer Integer
    deriving Show

--
-- The function print calls show on it's argument for us, instead of us having to do it if we use putStrLn
-- so I'd probobally write this
--

main = do
    let point = Point 5 7
    print point
