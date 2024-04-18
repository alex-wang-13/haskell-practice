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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
rotate :: Eq a => a -> a -> a -> [a] -> [a]
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
squareroot :: (Eq t1, Num t1, Fractional t2) => t2 -> t1 -> (t2 -> t3) -> t3
squareroot n 0 return = return n
squareroot n i return = squareroot n (i - 1) (\old -> return (old - ((old * old) - n) / (2 * old)))

-- Author: Alex Wang -- 
-- Part 2: Haskell Higher Order Functions --

{-
3. listmax takes a non-empty list of numbers and
returns the maximum value in the list.
-}
listmax :: Ord a => [a] -> a
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
grotate :: Eq t => t -> t -> t -> [NestedList t] -> [NestedList t]
grotate a b c [] = []
grotate a b c l  = map (_grotate a b c) l

{-
_grotate is a needed helper function for grotate.
grotate takes [NestedList] and gives [NestedList]
while _grotate takes a NestedList and gives a
NestedList. While this difference is subtle, it
shows why we need two different functions because
the output of one function cannot depend on the
input to that function.
-}
_grotate :: Eq t => t -> t -> t -> NestedList t -> NestedList t
_grotate a b c (Element x)
    | a == x        = Element b
    | b == x        = Element c
    | c == x        = Element a
    | otherwise     = Element x
_grotate a b c (SubList []) = SubList []
_grotate a b c (SubList l)  = SubList (map (_grotate a b c) l)

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

removeMin :: BinaryTree t -> BinaryTree t
removeMin Empty                             = Empty    -- min cannot be removed from Empty
removeMin (Leaf _)                          = Empty    -- min removed from Leaf is Empty
removeMin (InnerNode val Empty right)       = right    -- min removed from InnerNode w/o left is right
removeMin (InnerNode val (Leaf left) Empty) = Leaf val -- min removed from InnerNode w/ Leaf left and Empty right
removeMin (InnerNode val left right)        = InnerNode val (removeMin left) right -- removeMin from the left

-- Author: Alex Wang --
-- Part 3: Haskell Monads --

{-
7. Using the Maybe monad of Haskell, create a
function that has the following type:

function:: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]

The function takes a value of some type, a list
of the same type (as a monad), and a test function
and returns a list (in a monad). If the character
passes the test, the character is appended to the
monad list. Otherwise the result is Nothing.
-}
myfunction :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
myfunction a l f
    | f a           = l >>= (\x -> return (x++[a]))
    | otherwise     = Nothing

{-
7. checklist takes a list and a function and returns
Nothing if the elements in the list fail to past the
function and the list (embedded in a Maybe) if
all the elements pass.
-}
checklist :: [a] -> (a -> Bool) -> Maybe [a]
checklist [] f = Just []
checklist l  f
    | all f l   = Just l
    | otherwise = Nothing

{-
8. checkappend takes two Maybe lists and a test function
and appends the first list to the second only if all
characters of the first list pass the test. The return
is Nothing if any character of the first list does not
pass the test. The second list does not have to pass a
test.
-}
checkappend :: Maybe [a] -> Maybe [a] -> (a -> Bool) -> Maybe [a]
checkappend m1 m2 f = do
    l1 <- m1
    l2 <- m2
    if all f l1 then Just (l1++l2) else Nothing

{-
9. sum_of_maxes that takes two lists of lists of numbers
and creates a single list of numbers. The kth value of
the output list is the largest value of the kth sublist
of the first input list added to the largest value of
the kth sublist of the second input list. This routine
should return Nothing if any sublist is empty, or if each
input list has a different number of sublists.
-}
sum_of_maxes :: (Ord a, Num a) => [[a]] -> [[a]] -> Maybe [a]
sum_of_maxes l1 l2
    | length l1 /= length l2     = Nothing -- if list lengths are different
    | any null l1 || any null l2 = Nothing -- if any sublist is empty
    | otherwise = Just (zipWith (+) (map maximum l1) (map maximum l2))

{-
10. Create a list monad that generalizes a list. This will
not be a Haskell Monad type, but instead one of our own
creation like the Value type from lecture. For example,
the following is a valid "list":

Pair 4 (Pair 5 (Pair 6 Null))

Then create a binding function lbind and a return function
lreturn to make a list monad. The code should work so that:

> (Pair 4 (Pair 5 (Pair 6 Null))) `lbind` (\x -> lreturn (2 * x))   
Pair 8 (Pair 10 (Pair 12 Null))
-}
data MyList l = Null | Pair l (MyList l) deriving (Show)

{-
append takes two MyLists and attaches the second
to the end of the first.
-}
append :: MyList a -> MyList a -> MyList a
append Null t = t
append (Pair h t1) t2 = Pair h (t1 `append` t2)

{-
lbind performs a function on each element of MyList.
-}
lbind :: MyList a -> (a -> MyList b) -> MyList b
lbind Null _ = Null
lbind (Pair h t) f = f h `append` (t `lbind` f)

{-
lreturn gives a new MyList without a "tail."
-}
lreturn :: a -> MyList a
lreturn x = Pair x Null