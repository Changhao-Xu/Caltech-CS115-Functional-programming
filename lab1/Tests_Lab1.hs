--
-- Test script for lab 1.
--

{-# LANGUAGE ImplicitParams #-}

--
-- To run this, you must install the HUnit and HUnit-approx packages:
--
--   cabal install --lib HUnit
--   cabal install --lib HUnit-approx
--

-- NOTE:
-- This uses language features which haven't been covered in class yet.

module Main where

import Test.HUnit
import Test.HUnit.Approx
import Control.Monad
import Control.Exception

import Lab1

assertError :: a -> Test
assertError x = TestCase $ do
  raisesError <-
    catch 
      ((return $! x) >> return False) 
      (\(_ :: SomeException) -> return True)
  if raisesError then
    return ()
  else
    assertFailure "expected error"

makeTests :: String -> [Test] -> Test
makeTests label tests =
  TestList (map (TestLabel (" " ++ label)) tests)

testsSumsq :: Test
testsSumsq = 
  let ?epsilon = 1.0e-8 in
    makeTests "operator (+*)" [
        0.0 +* 0.0 ~?~ 0.0,
        1.4 +* 2.8 ~?~ 9.8,
        (-1.4) +* 2.8 ~?~ 9.8,
        (-1.4) +* (-2.8) ~?~ 9.8,
        1.4 +* (-2.8) ~?~ 9.8,
        3.0 +* 4.0 ~?~ 25.0
      ]

testsXor :: Test
testsXor =
  makeTests "operator (^||)" [
      True  ^|| True  ~=? False,
      True  ^|| False ~=? True,
      False ^|| True  ~=? True,
      False ^|| False ~=? False
    ]

testsRangeProduct :: Test
testsRangeProduct =
  makeTests "rangeProduct" [
    rangeProduct    0    0  ~=?  0,
    rangeProduct    0   10  ~=?  0,
    rangeProduct    1   10  ~=?  3628800,
    rangeProduct    5   10  ~=?  151200,
    rangeProduct (-10) (-6) ~=? -30240,
    assertError (rangeProduct 10 0)
  ]

testsProd :: Test
testsProd =
  makeTests "prod" [
    prod [] ~=? 1,
    prod [2..5] ~=? 120,
    prod [1..10] ~=? 3628800
  ]

testsRangeProduct2 :: Test
testsRangeProduct2 =
  makeTests "rangeProduct" [
    rangeProduct2    0    0  ~=?  0,
    rangeProduct2    0   10  ~=?  0,
    rangeProduct2    1   10  ~=?  3628800,
    rangeProduct2    5   10  ~=?  151200,
    rangeProduct2 (-10) (-6) ~=? -30240
  ]

testsMap2 :: Test
testsMap2 =
  makeTests "map2" [
    map2 (+) [] [] ~=? ([] :: [Integer]),
    map2 (+) [] [1,2,3] ~=? ([] :: [Integer]),
    map2 (+) [1,2,3] [] ~=? ([] :: [Integer]),
    map2 (+) [1,2,3] [4,5,6] ~=? ([5,7,9] :: [Integer]),
    map2 (+) [1,2,3,4] [4,5,6] ~=? ([5,7,9] :: [Integer]),
    map2 (+) [1,2,3] [4,5,6,7] ~=? ([5,7,9] :: [Integer])
  ]


testsMap3 :: Test
testsMap3 =
  let add3 x y z = x + y + z in
    makeTests "map2" [
      map3 add3 [] [] [] ~=? ([] :: [Integer]),
      map3 add3 [] [] [1,2,3] ~=? ([] :: [Integer]),
      map3 add3 [] [1,2,3] [] ~=? ([] :: [Integer]),
      map3 add3 [1,2,3] [] [] ~=? ([] :: [Integer]),
      map3 add3 [1,2,3] [4,5,6] [7,8,9] ~=? ([12,15,18] :: [Integer]),
      map3 add3 [1,2,3,4] [4,5,6] [7,8,9] ~=? ([12,15,18] :: [Integer]), 
      map3 add3 [1,2,3] [4,5,6,7] [7,8,9] ~=? ([12,15,18] :: [Integer]),
      map3 add3 [1,2,3] [4,5,6] [7,8,9,10] ~=? ([12,15,18] :: [Integer])
    ]

balancedParenthesesCases :: [(String, Bool)]
balancedParenthesesCases =
    [("", True),
     ("foo", True),
     ("(", False),
     (")", False),
     ("()", True),
     ("(foo)", True),
     (")(", False),
     (")a(", False),
     ("()()()", True),
     ("()()(", False),
     ("(()())", True),
     ("(()))(()", False),
     ("(((())))", True),
     ("(((()())()))", True)]

makeTestsBalancedParentheses :: String -> (String -> Bool) -> Test
makeTestsBalancedParentheses name ib =
  makeTests name (map (\(s, b) -> ib s ~=? b) balancedParenthesesCases)

testsBalancedParentheses :: Test
testsBalancedParentheses =
  makeTestsBalancedParentheses "balancedParentheses" balancedParentheses

testsBalancedParentheses2 :: Test
testsBalancedParentheses2 =
  makeTestsBalancedParentheses "balancedParentheses2" balancedParentheses2

testsBalancedParentheses3 :: Test
testsBalancedParentheses3 =
  makeTestsBalancedParentheses "balancedParentheses3" balancedParentheses3

allTests :: Test
allTests = TestList [
    testsSumsq,
    testsXor,
    testsRangeProduct,
    testsProd,
    testsRangeProduct2,
    testsMap2,
    testsMap3,
    testsBalancedParentheses,
    testsBalancedParentheses2,
    testsBalancedParentheses3
  ]

main :: IO ()
main = do
  putStrLn "\nTesting lab 1 code..."
  void $ runTestTT allTests
  putStrLn "Done testing.\n"
