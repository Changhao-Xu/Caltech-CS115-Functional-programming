--
-- SignedNatTests.hs
--
-- Tests of signed natural number datatype.
--

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.HUnit
import Test.QuickCheck
import Lab3ab

----------------------------------------------------------------------
-- HUnit tests.
----------------------------------------------------------------------

one1 :: Nat1
one1 = Succ1 Zero1

one2 :: Nat2
one2 = Succ2 Zero2

five1 :: Nat1
five1 = Succ1 (Succ1 (Succ1 (Succ1 (Succ1 Zero1))))

five2 :: Nat2
five2 = Succ2 (Succ2 (Succ2 (Succ2 (Succ2 Zero2))))

test1 :: Test
test1 = TestCase $
  assertEqual "Nat1 Show instance 1"
    (show Zero1) "Zero1"

test2 :: Test
test2 = TestCase $
  assertEqual "Nat1 Show instance 2"
    (show one1) "Succ1 Zero1"

test3 :: Test
test3 = TestCase $
  assertEqual "Nat1 Show instance 3"
    (show five1) "Succ1 (Succ1 (Succ1 (Succ1 (Succ1 Zero1))))"

test4 :: Test
test4 = TestCase $
  assertBool "Nat1 Eq instance 1"
    (Zero1 == Zero1)

test5 :: Test
test5 = TestCase $
  assertBool "Nat1 Eq instance 2"
    (one1 == one1)

test6 :: Test
test6 = TestCase $
  assertBool "Nat1 Eq instance 3"
    (five1 == five1)

test7 :: Test
test7 = TestCase $
  assertBool "Nat1 Eq instance 4"
    (Zero1 /= one1)

test8 :: Test
test8 = TestCase $
  assertBool "Nat1 Eq instance 5"
    (Zero1 /= five1)

test9 :: Test
test9 = TestCase $
  assertBool "Nat1 Eq instance 6"
    (one1 /= five1)


test10 :: Test
test10 = TestCase $
  assertEqual "Nat2 Show instance 1"
    (show Zero2) "Zero2"

test11 :: Test
test11 = TestCase $
  assertEqual "Nat2 Show instance 2"
    (show one2) "Succ2 Zero2"

test12 :: Test
test12 = TestCase $
  assertEqual "Nat2 Show instance 3"
    (show five2) "Succ2 (Succ2 (Succ2 (Succ2 (Succ2 Zero2))))"

test13 :: Test
test13 = TestCase $
  assertBool "Nat2 Eq instance 1"
    (Zero2 == Zero2)

test14 :: Test
test14 = TestCase $
  assertBool "Nat2 Eq instance 2"
    (one2 == one2)

test15 :: Test
test15 = TestCase $
  assertBool "Nat2 Eq instance 3"
    (five2 == five2)

test16 :: Test
test16 = TestCase $
  assertBool "Nat2 Eq instance 4"
    (Zero2 /= one2)

test17 :: Test
test17 = TestCase $
  assertBool "Nat2 Eq instance 5"
    (Zero2 /= five2)

test18 :: Test
test18 = TestCase $
  assertBool "Nat2 Eq instance 6"
    (one2 /= five2)


test19 :: Test
test19 = TestCase $
  assertBool "Nat2 Ord instance 1"
    (Zero2 <= Zero2)

test20 :: Test
test20 = TestCase $
  assertBool "Nat2 Ord instance 2"
    (Zero2 <= one2)

test21 :: Test
test21 = TestCase $
  assertBool "Nat2 Ord instance 3"
    (Zero2 <= five2)

test22 :: Test
test22 = TestCase $
  assertBool "Nat2 Ord instance 4"
    (not (one2 <= Zero2))

test23 :: Test
test23 = TestCase $
  assertBool "Nat2 Ord instance 5"
    (one2 <= one2)

test24 :: Test
test24 = TestCase $
  assertBool "Nat2 Ord instance 6"
    (one2 <= five2)

test25 :: Test
test25 = TestCase $
  assertBool "Nat2 Ord instance 7"
    (not (five2 <= Zero2))

test26 :: Test
test26 = TestCase $
  assertBool "Nat2 Ord instance 8"
    (not (five2 <= one2))

test27 :: Test
test27 = TestCase $
  assertBool "Nat2 Ord instance 9"
    (five2 <= five2)


tests :: Test
tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ,TestLabel "test4" test4
                 ,TestLabel "test5" test5
                 ,TestLabel "test6" test6
                 ,TestLabel "test7" test7
                 ,TestLabel "test8" test8
                 ,TestLabel "test9" test9
                 ,TestLabel "test10" test10
                 ,TestLabel "test11" test11
                 ,TestLabel "test12" test12
                 ,TestLabel "test13" test13
                 ,TestLabel "test14" test14
                 ,TestLabel "test15" test15
                 ,TestLabel "test16" test16
                 ,TestLabel "test17" test17
                 ,TestLabel "test18" test18
                 ,TestLabel "test19" test19
                 ,TestLabel "test20" test20
                 ,TestLabel "test21" test21
                 ,TestLabel "test22" test22
                 ,TestLabel "test23" test23
                 ,TestLabel "test24" test24
                 ,TestLabel "test25" test25
                 ,TestLabel "test26" test26
                 ,TestLabel "test27" test27
                 ]

----------------------------------------------------------------------
-- Helper functions.
----------------------------------------------------------------------

make_prop_eq :: Eq a => (Integer -> a) -> (Integer, Integer) -> Property
make_prop_eq fromint (imin, imax) = do
  forAll twoIntegers check
  where
    twoIntegers :: Gen (Integer, Integer)
    twoIntegers = do
      i <- choose (imin, imax)
      j <- choose (imin, imax)
      return (i, j)

    check :: (Integer, Integer) -> Bool
    check (i, j) = 
      let
        k  = i == j
        i' = fromint i
        j' = fromint j
        k' = i' == j'
      in k' == k

make_prop_eq_with_first :: Eq a => 
  (Integer -> a) -> (Integer, Integer) -> a -> Integer -> Property
make_prop_eq_with_first fromint (imin, imax) first firstint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = firstint == i
        i' = fromint i
        j' = first == i'
      in j' == j

make_prop_eq_with_second :: Eq a => 
  (Integer -> a) -> (Integer, Integer) -> a -> Integer -> Property
make_prop_eq_with_second fromint (imin, imax) second secondint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = i == secondint
        i' = fromint i
        j' = i' == second
      in j' == j

make_prop_le :: Ord a => (Integer -> a) -> (Integer, Integer) -> Property
make_prop_le fromint (imin, imax) = do
  forAll twoIntegers check
  where
    twoIntegers :: Gen (Integer, Integer)
    twoIntegers = do
      i <- choose (imin, imax)
      j <- choose (imin, imax)
      return (i, j)

    check :: (Integer, Integer) -> Bool
    check (i, j) = 
      let
        k  = i <= j
        i' = fromint i
        j' = fromint j
        k' = i' <= j'
      in k' == k

make_prop_le_with_first :: Ord a => 
  (Integer -> a) -> (Integer, Integer) -> a -> Integer -> Property
make_prop_le_with_first fromint (imin, imax) first firstint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = firstint <= i
        i' = fromint i
        j' = first <= i'
      in j' == j

make_prop_le_with_second :: Ord a => 
  (Integer -> a) -> (Integer, Integer) -> a -> Integer -> Property
make_prop_le_with_second fromint (imin, imax) second secondint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = i <= secondint
        i' = fromint i
        j' = i' <= second
      in j' == j

-- Test that converting int->nat->int gives the same number back.
make_prop_convert :: Num a => (a -> Integer) -> (Integer, Integer) -> Property
make_prop_convert toint (imin, imax) = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = toint (fromInteger i) == i

-- Test that a unary function on a numeric type gives the same results as a
-- unary function on Integers.
make_prop_unary_op :: Num a =>
  (a -> a) -> (Integer -> Integer) -> (a -> Integer) -> 
  (Integer, Integer) -> Property
make_prop_unary_op f fint toint (imin, imax) = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = fint i
        i' = fromInteger i
        j' = f i'
      in toint j' == j

-- Test that a binary function on a numeric type gives the same results as a
-- binary function on Integers.
make_prop_binary_op :: Num a =>
  (a -> a -> a) -> (Integer -> Integer -> Integer) -> (a -> Integer) -> 
  (Integer, Integer) -> Property
make_prop_binary_op f fint toint (imin, imax) = do
  forAll twoIntegers check
  where
    twoIntegers :: Gen (Integer, Integer)
    twoIntegers = do
      i <- choose (imin, imax)
      j <- choose (imin, imax)
      return (i, j)

    check :: (Integer, Integer) -> Bool
    check (i, j) = 
      let
        k  = fint i j
        i' = fromInteger i
        j' = fromInteger j
        k' = f i' j'
      in toint k' == k

-- The same as above, but supplying a fixed argument for the first argument
-- to the binary operator.
make_prop_binary_op_with_first :: Num a =>
  (a -> a -> a) -> (Integer -> Integer -> Integer) -> (a -> Integer) -> 
  (Integer, Integer) -> a -> Integer -> Property
make_prop_binary_op_with_first f fint toint (imin, imax) first firstint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = fint firstint i
        i' = fromInteger i
        j' = f first i'
      in toint j' == j

-- The same as above, but supplying a fixed argument for the first argument
-- to the binary operator.
make_prop_binary_op_with_second :: Num a =>
  (a -> a -> a) -> (Integer -> Integer -> Integer) -> (a -> Integer) -> 
  (Integer, Integer) -> a -> Integer -> Property
make_prop_binary_op_with_second f fint toint (imin, imax) second secondint = do
  forAll anInteger check
  where
    anInteger :: Gen Integer
    anInteger = do
      i <- choose (imin, imax)
      return i

    check :: Integer -> Bool
    check i = 
      let
        j  = fint i secondint
        i' = fromInteger i
        j' = f i' second
      in toint j' == j

----------------------------------------------------------------------
-- Properties to test.
-- NOTE: These depend on signedNatToInteger, which may not depend
-- on either Neg Zero or Pos Zero, so we test specifically for these.
----------------------------------------------------------------------

prop_eq :: Property
prop_eq =
  make_prop_eq (fromInteger :: Integer -> SignedNat) (-10, 10)

prop_eq_neg_zero_first :: Property
prop_eq_neg_zero_first =
  make_prop_eq_with_first (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Neg Zero) 0

prop_eq_neg_zero_second :: Property
prop_eq_neg_zero_second =
  make_prop_eq_with_second (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Neg Zero) 0

prop_eq_pos_zero_first :: Property
prop_eq_pos_zero_first =
  make_prop_eq_with_first (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Pos Zero) 0

prop_eq_pos_zero_second :: Property
prop_eq_pos_zero_second =
  make_prop_eq_with_second (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Pos Zero) 0

prop_le :: Property
prop_le =
  make_prop_le (fromInteger :: Integer -> SignedNat) (-10, 10)

prop_le_neg_zero_first :: Property
prop_le_neg_zero_first =
  make_prop_le_with_first (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Neg Zero) 0

prop_le_neg_zero_second :: Property
prop_le_neg_zero_second =
  make_prop_le_with_second (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Neg Zero) 0

prop_le_pos_zero_first :: Property
prop_le_pos_zero_first =
  make_prop_le_with_first (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Pos Zero) 0

prop_le_pos_zero_second :: Property
prop_le_pos_zero_second =
  make_prop_le_with_second (fromInteger :: Integer -> SignedNat) (-10, 10)
    (Pos Zero) 0

prop_plus :: Property
prop_plus = 
  make_prop_binary_op (+) (+) signedNatToInteger (-50, 50)

prop_plus_neg_zero_first :: Property
prop_plus_neg_zero_first = 
  make_prop_binary_op_with_first (+) (+) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_plus_neg_zero_second :: Property
prop_plus_neg_zero_second = 
  make_prop_binary_op_with_second (+) (+) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_plus_pos_zero_first :: Property
prop_plus_pos_zero_first = 
  make_prop_binary_op_with_first (+) (+) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_plus_pos_zero_second :: Property
prop_plus_pos_zero_second = 
  make_prop_binary_op_with_second (+) (+) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_minus :: Property
prop_minus = 
  make_prop_binary_op (-) (-) signedNatToInteger (-50, 50)

prop_minus_neg_zero_first :: Property
prop_minus_neg_zero_first = 
  make_prop_binary_op_with_first (-) (-) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_minus_neg_zero_second :: Property
prop_minus_neg_zero_second = 
  make_prop_binary_op_with_second (-) (-) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_minus_pos_zero_first :: Property
prop_minus_pos_zero_first = 
  make_prop_binary_op_with_first (-) (-) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_minus_pos_zero_second :: Property
prop_minus_pos_zero_second = 
  make_prop_binary_op_with_second (-) (-) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_times :: Property
prop_times = 
  make_prop_binary_op (*) (*) signedNatToInteger (-50, 50)

prop_times_neg_zero_first :: Property
prop_times_neg_zero_first = 
  make_prop_binary_op_with_first (*) (*) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_times_neg_zero_second :: Property
prop_times_neg_zero_second = 
  make_prop_binary_op_with_second (*) (*) signedNatToInteger (-50, 50) 
    (Neg Zero) 0

prop_times_pos_zero_first :: Property
prop_times_pos_zero_first = 
  make_prop_binary_op_with_first (*) (*) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_times_pos_zero_second :: Property
prop_times_pos_zero_second = 
  make_prop_binary_op_with_second (*) (*) signedNatToInteger (-50, 50) 
    (Pos Zero) 0

prop_negate :: Property
prop_negate = 
  make_prop_unary_op negate negate signedNatToInteger (-50, 50)

prop_abs :: Property
prop_abs = 
  make_prop_unary_op abs abs signedNatToInteger (-50, 50)

prop_signum :: Property
prop_signum = 
  make_prop_unary_op signum signum signedNatToInteger (-50, 50)

prop_convert :: Property
prop_convert =
  make_prop_convert signedNatToInteger (-50, 50)

----------------------------------------------------------------------
-- Run all the tests.
----------------------------------------------------------------------

-- Run all the quickcheck tests.
-- This uses Template Haskell; see
-- https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-All.html
-- for more details.
return []
runTests = $quickCheckAll

main :: IO ()
main = do
  success <- runTests
  if success
     then putStrLn "\nALL TESTS PASSED!\n"
     else putStrLn "\nERROR: SOME TESTS FAILED!\n"
  putStrLn "Running HUnit tests...\n"
  counts <- runTestTT tests
  print counts
  putStrLn ""

