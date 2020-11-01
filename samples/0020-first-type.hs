-- I think it's best to first establish a basic understanding of types when learning Haskell.  This creates a new type called `Point`.

data Point = MkPoint Integer Integer

-- This defines a value-constructor called 'MkPoint' that takes 2 Integer values as arguments and returns a new
-- data type called Point.
--
-- Integer is a predefined data type, Haskell has a number of predefined types inlcuding; Bool, Char,
-- Strings, Integer, List, Tuple, and others.
--
-- Notice the new data type, the value-constructor and the types arguments all have their first letters capaitalized.
--
-- Haskell uses upper-camelcase for types and lower-camelcase for values.
--
-- Traditionally the value-constructor is given the same names as the data type it's creating.
--
-- In this example I'm using Point and MkPoint just to make it obvious which is the type and which is the
-- value-constructor.



-- Haskell has something called type-classes.
--
-- A Haskell type-class is very similar to an interface in an object oriented language.
--
-- To demonstrate this I'm going to look at the "Show" type class.
--
-- It's used to convert a value of some data type into a string.
--
-- The "Show" type classes is defined in the base library and looks more or less like this:
--
--      class Show a where
--        show :: a -> String
--
-- It says that any type 'a' can belong to the 'Show' type class by defining an instance function called "show" that
-- takes a value of type 'a' and returns a String.
--
-- A lower case letter in a type definition, like the 'a' above, is a type variable.  It's just a place holder.
-- They're used when you need a type expression to have the same type in multiple places but don't care what the
-- type is.
--
-- In the above example it tells the compiler that if an instance function called 'show', which takes an argument of
-- type 'a' and returns String, is defined then "class Show a" is true.  Meaning the type 'a' is a member of the
-- class "Show".
--



-- This is how we write the type instance "show" for our Point data type

instance Show Point where
    show (MkPoint x y) = "Point " ++ (show x) ++ " " ++ (show y)

--
-- The first line says that all the following indented lines are instance methods of the "Show" type-class for the
-- Point data type.
--
-- The second line is the implementation of the "show" function.
--
-- It's written like any other function in Haskell.
--
-- everything to the left of the `=` is the pattern.
--
-- everything to the right of the `=' is the body.
--
--      PATERN = BODY
--
-- Lets look at the pattern first.
--
-- Keep in mind in Haskell parentheses only provide grouping, they have never have any other meaning.
--
-- In the above pattern, the parentheses around "MkPoint x y" indicate it's a single value.
--
-- In this context it means the "show" function takes a single argument.
--
-- But, inside that single argument we provide a pattern to deconstruct the value.
--
-- A deconstruction always starts with a value-constructor, 'MkPoint' in this example, followed by names
-- or constant values, where the value-constructors arguments should be.
--
-- In the example above when the function "show" is called with a Point value, that value is deconstructed so
-- that the arguments it was originally created with are assigned to x and y
--
-- So given this example
--
--     let point = MkPoint 5 7
--     putStrLn (show point)
--
-- when show is called, the point value passed to it is deconstructed.
--
-- The deconsruction assigns the value 5 to the name x and 7 to name y for the scope of the functions body.
--
-- Notice the perfect symmetry between argument construction and deconstruction.
--
--    to construct         MkPoint 5 7
--    to deconstruct       MKpoint x y
--
-- The symmetry goes deeper, when you call a funciton you do this
--
--    show point
--
-- When you define a function you do this
--
--    show point = ....
--
-- Except we want to desconstruct point so that we can access it's inner values so we do this
--
--   show (MkPoint x y) = ...
--
-- The syntax of calling a function has a perfect symmetry with the pattern used when declaring a funciton and
-- deconstructing the values of it's arguments
--
-- Now the functions body, the part to the right of the =
--
--    "Point " ++ (show x) ++ " " ++ (show y)
--
-- ++ is an operaor that concatenates strings.
--
-- We already know show is a function that takes a value and returns it's String representation
--
-- So (show x) and (show y) each return a String that represents thier corresponding values.
--
-- So we start with a constant string that matches our type "Point " and tack on the space separated
-- representations of the x and y values
--
-- so given this
--

main = do
    let point = MkPoint 5 7
    putStrLn (show point)

-- The output would be this
--
--     "Point 5 7"
