module Lab5 where

import Control.Monad

-- Part A
-- 1
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)] -- Lec 15, P39
hr_solutions = do
    i <- [1..]
    j <- [1..i-1]
    k <- [1..j-1]
    l <- [1..k-1]
    guard $ i^3 + l^3 == j^3 + k^3
    return ((i, l), (j, k), i^3 + l^3)

-- 2
{-
sum1000 :: Integer
sum1000 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-}
sum1 :: Integer
sum1 = sum(do 
        x <- [1..999]
        guard $ x `mod` 3 == 0 || x `mod` 5 == 0
        return x)

sum2 :: Integer
sum2 = sum (do
        x <- [1..999]
        if x `mod` 3 == 0 || x `mod` 5 == 0 
            then return x
            else mzero)

-- 3
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

largestPalindrome :: Integer
largestPalindrome = maximum(do 
                        x <- [100..999]
                        y <- [x..999] -- Originally used [100..999], change to [x..999] to avoid double counting and enhance efficiency
                        guard $ isPalindrome (x * y)
                        return (x * y))
-- result: 906609

-- 4
type Expr = [Item]

data Item = N Int | O Op
  deriving Show

data Op = Add | Sub | Cat
  deriving Show

ops :: [Item]
ops = [O Add, O Sub, O Cat]

-- 4.A
exprs :: [Expr]
exprs = do
    op1 <- ops
    op2 <- ops
    op3 <- ops
    op4 <- ops
    op5 <- ops
    op6 <- ops
    op7 <- ops
    op8 <- ops
    return [N 1, op1, N 2, op2, N 3, op3, N 4, op4, N 5, op5, N 6, op6, N 7, op7, N 8, op8, N 9]

-- 4.B
normalize :: Expr -> Expr
normalize ((N i) : []) = [N i]
normalize ((N i) : (O Cat) : (N j) : es) = normalize ((N (10 * i + j)) : es)
normalize ((N i) : (O Add) : es) = (N i) : (O Add) : (normalize es)
normalize ((N i) : (O Sub) : es) = (N i) : (O Sub) : (normalize es)
normalize _ = error "illegitimate patterns/invalid subexpressions"

-- 4.C
evaluate :: Expr -> Int
evaluate ((N i) : []) = i
evaluate ((N i) : (O Add) : (N j) : es) = evaluate ((N (i + j)) : es)
evaluate ((N i) : (O Sub) : (N j) : es) = evaluate ((N (i - j)) : es)
evaluate _ = error "illegitimate patterns/invalid subexpressions"

-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs


-- Part B
-- 1
{-
Q: Desugar
do n1 <- [1..6]
   n2 <- [1..6]
   []
   return (n1, n2) -- Lec 15, P40; Lec 11, P27

A:
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> ([] >> return (n1, n2)))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> ([] >>= (\_ -> return (n1, n2))))          -- Def: mv >>= f = concatMap f mv
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> (concatMap (\_ -> return (n1, n2)) []))    -- concatMap <anything> [] is the empty list
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> [])                                        -- Def: mv >>= f = concatMap f mv
[1..6] >>= \n1 -> concatMap (\n2 -> []) [1..6]                                  -- concatMap (\n -> []) <anything> is the empty list
[1..6] >>= \n1 -> []                                                            -- Def: mv >>= f = concatMap f mv
concatMap (\n1 -> []) [1..6]                                                    -- concatMap (\n -> []) <anything> is the empty list
[]
-}

-- 2
{-
Q: Desugar
do n1 <- [1..6]
   n2 <- [1..6]
   return <anything>
   return (n1, n2)
&& Desugar
do n1 <- [1..6]
   n2 <- [1..6]
   return (n1, n2)

A:
First case:
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> (return <anything> >> return (n1, n2)))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> (return <anything> >>= (\_ -> return (n1, n2))))       -- Def: mv >>= f = concatMap f mv
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> concatMap (\_ -> return (n1, n2)) (return <anything>))
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> concatMap (\_ -> return (n1, n2)) <anything>)
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat (map (\_ -> return (n1, n2)) <anything>)        -- Def: concatMap f lst = concat (map f lst)
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat (return (n1, n2) <anything>))                   -- Kept the <anything> placeholder
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> concat (return (n1, n2)))                              -- Then simplified step-by-step
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> return (n1, n2))


Second case:
[1..6] >>= \n1 -> ([1..6] >>= \n2 -> return (n1, n2))

Therefore, the two expressions return the same thing.
-}

-- 3
{-
Q: Desugar
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s
     return [c1, c2]

A:
["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>=                                         -- Def: mv >>= f = concatMap f mv
  \x -> case x of
      ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
      _ -> fail "pattern match failure"
concatMap                                                                                   -- Def: concatMap f lst = concat (map f lst)
      (\x -> case x of
          ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
          _ -> fail "pattern match failure")
      ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]
concat (map (\x -> case x of
                  ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
                  _ -> fail "pattern match failure")
          ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])
concat [return ['x', 'y'], return ['z', 'w'], [], return ['c', 'c'], []]                    -- fail _ = []
concat [["xy"], ["zw"], [], ["cc"], []]
["xy", "zw", "cc"]

If instead fail for the list monad was: fail s = error s,
then the evaluation will stop at "foobar" instead of giving empty list,
and the error message "pattern match failure" will pop out.
-}

-- 4 -- Lec 15, P14
{-
Given m = [x1, x2, ...],
foldr ((++) . k) [] m
foldr (\x -> (++) (k x)) [] m
foldr (\y -> (\x -> (k x) ++ y)) [] m
foldr (\x y -> (k x) ++ y) [] [x1, x2, ...]
(k x1) ++ (foldr (\x y -> (k x) ++ y) [] [x2, ...])
(k x1) ++ (k x2) ++ ... ++ []
concat [k x1, k x2, ...]
concat (map k [x1, x2, ...])
concat (map k m)

Given m = [],
foldr ((++) . k) [] m
foldr (\x y -> (k x) ++ y) [] []
[]
concat (map k [])
concat []
[]

Therefore, foldr ((++) . k) [] m and concat (map k m) evaluate to the same thing.
-}