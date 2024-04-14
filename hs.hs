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

