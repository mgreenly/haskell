--
-- we don't actually have to write the `show` instance for every type, Hasklel can figure out how to do that for
-- us if we ask it with `deriving Show` in the type definition.
--

data Point =
    Point Integer Integer
    deriving Show


main = do
    let point = Point 5 7
    print point
