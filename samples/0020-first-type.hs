-- I think it's best to start with types when learning Haskell.

-- This creates a new type called `Point`.

data Point = MkPoint Integer Integer

-- It defines a value constructor called `MkPoint` that takes 2 `Integer` values as arguments and returns a new
-- value of type `Point`.  Notice the type, value-constructor and the types of it's arguments all have there
-- first letters capitalized.
--
-- In Haskell types use upper-camelcase and values use lower-camelcase.



-- Haskell has something called type-classes.
--
-- A Haskell type-class is very similar to an interface in an object oriented language.
--
-- In this case we're going to use the `Show` type-class.  To belong to it a type must provide an implemenation of
-- the `show` method.  It's purpose is to convert a value of some type into a string.  So in our case we need
-- a `show` function that takes a `Point` value and returns a `String`.
--

instance Show Point where
    show (MkPoint x y) = "Point" ++ " " ++ (show x) ++ " " ++ (show y)

-- The first line just says the stuff indented below it is an instance method of `Show` for the `Point` type.
--
-- The second line is the implementation of the `show` function for `Point` values.
--
-- It's written like any other funciton in Haskell.
--
-- everything to the left of the `=` is the pattern.
--
-- everything to the right of the `=' is the body.
--
-- Lets look at the pattern first.
--
-- In Haskell parentheses only provide grouping, they have no other syntactical meaning.
--
-- In the case of this pattern the parens say that `(MkPoint x y)` is one thing.  So `show (MkPoint x y)` is a pattern
-- that starts with the `show` function call and takes one arguemnt, but The stuff inside the parens says the argument
-- must have been constructed with the `MkPoint` value constructor and two `Integer` values.  Further when that `Point`
-- value is deconstructed `x` will be bound to the first integer and `y` the second while in the body of the function.
--
-- Now lets look at everything to the right of the `=`, the funcitons body.
--
-- `++` is a function that concatenates strings.  So all we're doing is turning x and y into strings and then joining
-- them together with an empty string between them.


main = do
    let point = MkPoint 7 11
    putStrLn(show point)

-- the output is "Point 7 11"
