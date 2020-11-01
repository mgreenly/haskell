--
-- Lists are a very common data type in haskell
--

--
-- The most fundemental kind of list is an emtpy one.  It's expressed as an empty pair of square brackets.
--
emptyList = []

-- The single colon is the cons operator.  It will prepend a value to an empty list.
oneElementList = 1 : []

-- the : operator will also append a value to a non empty list.  So you can build up a longer list
threeElementList = 1 : 2 : 3 : []

-- keep in mind that the 'c' was the first thing added to the list and the 'a' was the last thing added to the list

-- It's important to understand the : operator works because we use it when deconstructing lists but normally you'd
-- create a list with this syntax.  Square brackets with comma separated values.

altList = [ 1, 2, 3 ]



main = do
    print oneElementList                 -- [1]
    print threeElementList               -- [1,2,3]
    print altList                        -- [1,2,3]

    -- you can test if a list is empty print (null emptyList)               -- True print (null threeElementList)        -- False

    -- you can get the the length of a list
    print (length threeElementList)      -- 3

    -- there's four closely related functions that can be used to extract information from a list
    print (head threeElementList)        -- 1
    print (tail threeElementList)        -- [2,3]
    print (init threeElementList)        -- [1,2]
    print (last threeElementList)        -- 3

    -- The module Data.List provides a large number of functions that you can apply to lists.  I highly
    -- recomend you're aware of all the functionality this module provides.  Knowing how to manipulate
    -- lists in haskell is important.
