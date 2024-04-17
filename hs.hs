import Distribution.Simple.PackageIndex (SearchResult(None))
-- Author: Alex Wang --
-- Part 1: Haskell Functions --

{-
1. rotate takes three elements and a list.
It returns a list that is the same as the input
list except that each occurrence of the first
element is replaced by the second, the second
element is replaced by the third, and the third
is replaced by the first.
-}
rotate _ _ _ []    = []
rotate a b c (h:t)
    | h == a       = b : rotate a b c t
    | h == b       = c : rotate a b c t
    | h == c       = a : rotate a b c t
    | otherwise    = h : rotate a b c t

{-
2. squareroot takes two numbers, a value and
an iteration. The iteration will be an integer
greater than or equal to 0. The function will
compute the squareroot of the value using
iteration rounds of Newton's method, starting
with an initial value equal to the input value.
Newton's method is:
    new = old - ((old * old) - value) / (2 * old)
-}
squareroot n 0 return = return n
squareroot n i return = squareroot n (i - 1) (\old -> return (old - ((old * old) - n) / (2 * old)))

-- Author: Alex Wang -- 
-- Part 2: Haskell Higher Order Functions --

{-
3. listmax takes a non-empty list of numbers and
returns the maximum value in the list.
-}
listmax l = foldl max (head l) l

{-
4. removedups takes a list and returns the list
with all consecutive duplicate values removed.
-}
removedups :: (Eq a) => [a] -> [a]
removedups = foldr (\x acc -> if null acc || x /= head acc then x : acc else acc) []

-- Author: Alex Wang --
-- Part 2: Haskell Types --

{-
5. Create a type that allows us to have nested
lists. Your type should have two kinds of values,
elements and sublists. For example, the following
will be a valid list:

[Element 1, Element 3, SubList [Element 4, SubList
    [SubList [Element 5], SubList []]], Element 6]
-}
data NestedList t = Element t | SubList [NestedList t] deriving (Show)

{-
5. grotate takes three values and list containing
elements and sublists and returns a list with
the same structure, but if any "element" is the
first input, it is replaced by the second, if an
"element" is the second input, it is replaced by
the third, and if it is the "third" input, it is
replaced by the first.
-}
grotate :: (Eq t) => t -> t -> t -> NestedList t -> NestedList t
grotate a b c (Element x)
    | a == x        = Element b
    | b == x        = Element c
    | c == x        = Element a
    | otherwise     = Element x
grotate a b c (SubList []) = SubList []
-- grotate a b c (SubList l) = SubList (map (grotate a b c) l)

{-
6. removeMin that takes a Tree as input. Assuming
the tree is in proper order (all values in the
left child are smaller than the value in the node,
and all the values in the right child are equal or
larger than the node), the function will return a
new tree with the smallest value of the tree
removed.
-}
data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving (Show)

removeMin Empty                         = Empty    -- min cannot be removed from Empty
removeMin (Leaf _)                      = Empty    -- min removed from Leaf is Empty
removeMin (InnerNode val Empty right)   = right    -- min removed from InnerNode w/o left is right
removeMin (InnerNode val (Leaf left) _) = Leaf val -- min removed from InnerNode w/ Leaf left is the InnerNode
removeMin (InnerNode val left right)    = InnerNode val (removeMin left) right