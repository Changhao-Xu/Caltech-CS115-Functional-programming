{- Changhao Xu CS115 Lab1 -}
-- Part A: Exercises
module Lab1 where
import Data.List

-- 1.a
(+*) :: Double -> Double -> Double
(+*) x y = x*x + y*y
infixl 7 +*

-- 1.b
(^||) :: Bool -> Bool -> Bool
False ^|| y = y {- If first argument is false, return the second argument -}
True ^|| y = not y {- If first argument is true, return the reverse of second argument -}
infixr 3 ^||

-- 2
rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y
  | x > y     = error "Error: the first argument is greater than the second argument"
  | x == y    = x
  | otherwise = x * rangeProduct (x+1) y

-- 3
prod :: [Integer] -> Integer
prod = foldr (*) 1

rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 x y
    | x > y     = error "Error: the first argument is greater than the second argument"
    | otherwise = prod [x .. y]

--4.a
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = [] {- Init: empty list -}
map2 _ _ [] = []
map2 f (x : xs) (y : ys) = f x y : map2 f xs ys

--4.b
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] _ _ = []
map3 _ _ [] _ = []
map3 _ _ _ [] = []
map3 f (x : xs) (y : ys) (z : zs) = f x y z : map3 f xs ys zs

--5
{-
 dot lst1 lst2 =                                        -- given dot = (sum .) . map2 (*)
 (sum .) . map2 (*) lst1 lst2
 (sum .) . (map2 (*) lst1 lst2)                         -- . operator has high precedence 9 but function application is still higher
 \lst1 lst2 -> ((sum .) (map2 (*) lst1 lst2))           -- dot product def in Lec4: f . g = \x -> f (g x)
 \lst1 lst2 -> ((\x -> sum . x) (map2 (*) lst1 lst2))   -- (sum .) = \x -> sum . x
 \lst1 lst2 -> (sum . (map2 (*) lst1 lst2))             -- same as above, (sum .) = \x -> sum . x
 \lst1 lst2 -> (sum (map2 (*) lst1 lst2))               -- therefore, (sum .) . map2 (*) is equivalent to sum (map2 (*) lst1 lst2)
-}

--6
sum1000 :: Integer
sum1000 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-- 233168

--7
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x : xs) = x : sieve [y | y <- xs, y `mod` x /= 0]
primes :: [Integer]
primes = sieve [2..]
sum10000 :: Integer
sum10000 = sum $ takeWhile (<10000) primes
--5736396

--8.a
balancedParentheses :: String -> Bool                                           -- equal open and close parentheses && close never occurs before its open
balancedParentheses lst = helper lst 0                                          -- Init count 0
  where
    helper :: String -> Int -> Bool                                             -- helper function: list, count, return Bool
    helper [] count = count == 0                                                -- If final count == 0, return True; otherwise return False
    helper ('(':xs) count = helper xs (count + 1)
    helper (')':xs) count
        | count > 0 = helper xs (count - 1)
        | otherwise = False                                                     -- If count <= 0 return False
    helper (_:xs) count = helper xs count                                       -- Ignore other characters

--8.b
{-
balancedParentheses2 :: String -> Bool
balancedParentheses2 lst = foldl' helper2 0 lst == 0                            -- foldl' helper2 0 lst (reverse the order of helper I wrote in 8.a) returns sum
  where
    helper2 :: Int -> Char -> Int
    helper2 count '(' = count + 1                                               -- count is existing imbalance count
    helper2 count ')'
        | count > 0 = count - 1
        | otherwise = minBound :: Int                                           -- this makes sure once count is negative, it remains negative; but not elegant
    helper2 count _   = count
-}
balancedParentheses2 :: String -> Bool
balancedParentheses2 lst = foldl' helper2 0 lst == 0                            -- foldl' helper2 0 lst (reverse the order of helper I wrote in 8.a) returns sum
  where
    helper2 :: Int -> Char -> Int
    helper2 count '('                                                           -- count is existing imbalance count
        | count < 0 = count                                                     -- this makes sure once count is negative, it remains negative
        | otherwise = count + 1                                           
    helper2 count ')' = count - 1
    helper2 count _   = count

--8.c
balancedParentheses3 :: String -> Bool
balancedParentheses3 lst = test $ scanl' (+) 0 (map ctoi lst)
  where
    ctoi :: Char -> Int
    ctoi '(' = 1
    ctoi ')' = -1
    ctoi _   = 0
    test :: [Int] -> Bool
    test lst
        | null lst         = True
        | last lst == 0    = all (>= 0) lst
        | otherwise        = False

-- Part B: Pitfalls
-- 1
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs
{- The original code used 'head lst' and 'tail lst',
and is a bad style to use head or tail functions when we can use pattern matching instead.
Therefore, I use pattern matching (x:xs) to deconstruct a list.
-}

--2
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max x $ largest xs
{- The original code used 'head lst' and 'tail lst',
and is a bad style to use head or tail functions when we can use pattern matching instead.
In addition, the original code used 'length xs == 0' and 'length xs == 1' to identify an empty list and a single-element list,
which used 'length' function too many times and not computational efficient.
Therefore, I replace 'length xs == 0' with '[]', and 'length xs == 1' with [x]
-}

-- Part C: Evaluation
--1
{-
fib 3
fib (3-1) + fib (3-2)
fib 2 + fib (3-2)
(fib (2-1) + fib (2-2)) + fib (3-2)
(fib 1 + fib (2-2)) + fib (3-2)
(1 + fib (2-2)) + fib (3-2)
(1 + fib 0) + fib (3-2)
(1 + 0) + fib (3-2)
1 + fib (3-2)
1 + fib 1
1 + 1
2
-}

--2
{-
fact 3
3 * fact (3 - 1)
3 * ((3 - 1) * fact ((3 - 1) - 1))
3 * ((3 - 1) * (((3 - 1) - 1) * fact (((3 - 1) - 1) - 1)))
3 * ((3 - 1) * (((3 - 1) - 1) * ((((3 - 1) - 1) - 1) * fact ((((3 - 1) - 1) - 1) - 1))))

The original code put "fact 0 = 1" after "fact n = n * fact (n - 1)",
due to pattern matching "(3 - 1) - 1) -1)" will not get evaluated to match fact 0.
Therefore, there exists an infinite loop and evaluation will not stop.
To fix the error, I move the "fact 0 = 1" before "fact n = n * fact (n - 1)":

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

fact 3
3 * fact (3 - 1)
3 * fact 2                        -- pattern matching here requires that we evaluate (3 – 1) so we can tell if this matches 0 or not
3 * (2 * fact (2 - 1))
3 * (2 * fact 1)
3 * (2 * (1 * fact (1 - 1)))
3 * (2 * (1 * fact 0))
3 * (2 * (1 * 1))
3 * (2 * 1)
3 * 2
6
-}

--3
{-
reverse [1,2,3]
iter [1,2,3] []
iter [2,3] (1:[])
iter [3] (2:(1:[]))
iter [] (3:(2:(1:[])))
(3:(2:(1:[])))
3:2:1:[]
[3,2,1]

The time complexity of this function is O(n).
Explain: When the length of the input list is n, we apply iter that extracted each element from the list head which takes O(n),
and combining all elements after ite takes O(1). Therefore, in total O(n).
-}

--4
{-
reverse [1, 2, 3]
reverse [2, 3] ++ [1]
(reverse [3] ++ [2]) ++ [1]
((reverse [] ++ [3]) ++ [2]) ++ [1]
(([] ++ [3]) ++ [2]) ++ [1]
([3] ++ [2]) ++ [1]
(3 : ([] ++ [2])) ++ [1]
(3 : [2]) ++ [1]
[3, 2] ++ [1]
3 : ([2] ++ [1])
3 : (2 : ([] ++ [1]))
3 : (2 : [1])
3 : [2, 1]
[3, 2, 1]

Ben Bitfiddle is wrong because constructing the result list from "[] ++ [3] ++ [2] ++ [1]" is not linear.
The ++ operator used recursive definition,
for each concatenation it takes O(n) which is linear in the length of the input list,
summing up all of them n times recursively takes O(n^2).
Therefore, the actual asymptotic time complexity of this version is O(n^2).
-}

--5
{-
head (isort [3, 1, 2, 5, 4])
head (insert 3 (isort [1, 2, 5, 4]))
head (insert 3 (insert 1 (isort[2, 5, 4])))
head (insert 3 (insert 1 (insert 2 (isort[5, 4]))))
head (insert 3 (insert 1 (insert 2 (insert 5 (isort[4])))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort[]))))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
head (insert 3 (insert 1 (2 : (4 : insert 5 []))))
head (insert 3 (1 : (2 : (4 : insert 5 []))))
head (1 : (insert 3 (2 : (4 : insert 5 []))))
1
-}

--6
{-
foldr max 0 [1, 5, 3, -2, 4]
max 1 (foldr max 0 [5, 3, -2, 4])
max 1 (max 5 (foldr max 0 [3, -2, 4]))
max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
max 1 (max 5 (max 3 (max -2 (max 4 0))))
max 1 (max 5 (max 3 (max -2 4)))
max 1 (max 5 (max 3 4))
max 1 (max 5 4)
max 1 5
5

foldl max 0 [1, 5, 3, -2, 4]
foldl max (max 0 1) [5, 3, -2, 4]
foldl max (max (max 0 1) 5) [3, -2, 4]
foldl max (max (max (max 0 1) 5) 3) [-2, 4]
foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
max (max (max (max (max 0 1) 5) 3) -2) 4
max (max (max (max 1 5) 3) -2) 4
max (max (max 5 3) -2) 4
max (max 5 -2) 4
max 5 4
5

The space complexity of foldr and foldl are the same. Due to lazy evaluation,
both foldr and foldl accumulates all the elements in the list before calculating max to reduce the list.
As shown in the evaluation examples above, both has 5 max evaluations.
-}