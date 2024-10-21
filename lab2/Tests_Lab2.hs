--
-- Tests_Lab2_TAs.hs
--

{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified RedBlackTree as R

import qualified Data.List as L
import qualified Data.Set as S

import Test.HUnit
import Test.QuickCheck

---------------------------------------------------------------------------
-- Utility functions.
---------------------------------------------------------------------------

-- Structural equality on `Color` and `Tree` types.
-- These could have been derived.

eqColor :: R.Color -> R.Color -> Bool
eqColor R.Red R.Red = True
eqColor R.Black R.Black = True
eqColor _ _ = False

eqTreeInt :: R.Tree Int -> R.Tree Int -> Bool
eqTreeInt R.Leaf R.Leaf = True
eqTreeInt (R.Node c1 lt1 i1 rt1) (R.Node c2 lt2 i2 rt2) =
  eqColor c1 c2 && i1 == i2 && eqTreeInt lt1 lt2 && eqTreeInt rt1 rt2
eqTreeInt _ _ = False

---------------------------------------------------------------------------
-- Sample trees (both valid and invalid) for testing.
---------------------------------------------------------------------------

-- t1, t2, etc. are all valid trees.

t1 :: R.Tree Int
t1 = R.Leaf

t2 :: R.Tree Int
t2 = R.Node R.Red R.Leaf 1 R.Leaf

t3 :: R.Tree Int
t3 = R.Node R.Black R.Leaf 2 R.Leaf

t4 :: R.Tree Int
t4 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)

t5 :: R.Tree Int
t5 = R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)

t6 :: R.Tree Int
t6 = R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf)

t7 :: R.Tree Int
t7 = R.Node R.Black
      (R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf))
      4
      (R.Node R.Black (R.Node R.Black R.Leaf 5 R.Leaf) 6 (R.Node R.Black R.Leaf 7 R.Leaf))

t8 :: R.Tree Int
t8 = R.Node R.Black
      (R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf))
      4
      (R.Node R.Red (R.Node R.Black R.Leaf 5 R.Leaf) 6 (R.Node R.Black R.Leaf 7 R.Leaf))

t9 :: R.Tree Int
t9 = R.Node R.Black
     (R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf))
     4
     (R.Node R.Black (R.Node R.Red R.Leaf 5 R.Leaf) 6 (R.Node R.Red R.Leaf 7 R.Leaf))

-- These are valid trees used for testing the `minDepth` and `maxDepth` functions.

d0 :: R.Tree Int
d0 = R.Leaf

d1 :: R.Tree Int
d1 = R.Node R.Black R.Leaf 1 R.Leaf

d2 :: R.Tree Int
d2 = R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 R.Leaf

d3 :: R.Tree Int
d3 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)

d4 :: R.Tree Int
d4 = R.Node R.Black (R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 R.Leaf) 3
      (R.Node R.Black R.Leaf 4 R.Leaf)

d5 :: R.Tree Int
d5 = R.Node R.Black (R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf) 2
      (R.Node R.Black R.Leaf 3 R.Leaf)) 4 (R.Node R.Black R.Leaf 5 R.Leaf)

d6 :: R.Tree Int
d6 = R.Node R.Black (R.Node R.Red (R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 R.Leaf) 3
      (R.Node R.Black R.Leaf 4 R.Leaf)) 5 (R.Node R.Black R.Leaf 6 R.Leaf)

d7 :: R.Tree Int
d7 = R.Node R.Black (R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2
      (R.Node R.Black R.Leaf 3 R.Leaf)) 4 (R.Node R.Black (R.Node R.Black R.Leaf 5 R.Leaf)
      6 (R.Node R.Black R.Leaf 7 R.Leaf))

d8 :: R.Tree Int
d8 = R.Node R.Black (R.Node R.Black (R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 R.Leaf)
      3 (R.Node R.Black R.Leaf 4 R.Leaf))
      5 (R.Node R.Black (R.Node R.Black R.Leaf 6 R.Leaf) 7 (R.Node R.Black R.Leaf 8 R.Leaf))

d9 :: R.Tree Int
d9 = R.Node R.Black (R.Node R.Black (R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf) 2
      (R.Node R.Black R.Leaf 3 R.Leaf)) 4 (R.Node R.Black R.Leaf 5 R.Leaf)) 6
      (R.Node R.Black (R.Node R.Black R.Leaf 7 R.Leaf) 8 (R.Node R.Black R.Leaf 9 R.Leaf))

d10 :: R.Tree Int
d10 = R.Node R.Black (R.Node R.Black (R.Node R.Red (R.Node R.Black (R.Node R.Red (R.Node R.Black (R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 R.Leaf) 3 (R.Node R.Black R.Leaf 4 R.Leaf)) 5 (R.Node R.Black (R.Node R.Black R.Leaf 6 R.Leaf) 7 (R.Node R.Black R.Leaf 8 R.Leaf))) 9 (R.Node R.Black (R.Node R.Black R.Leaf 10 R.Leaf) 11 (R.Node R.Black R.Leaf 12 R.Leaf))) 13 (R.Node R.Black (R.Node R.Black (R.Node R.Black R.Leaf 14 R.Leaf) 15 (R.Node R.Black R.Leaf 16 R.Leaf)) 17 (R.Node R.Black (R.Node R.Black R.Leaf 18 R.Leaf) 19 (R.Node R.Black R.Leaf 20 R.Leaf)))) 21 (R.Node R.Black (R.Node R.Black (R.Node R.Black R.Leaf 22 R.Leaf) 23 (R.Node R.Black R.Leaf 24 R.Leaf)) 25 (R.Node R.Black (R.Node R.Black R.Leaf 26 R.Leaf) 27 (R.Node R.Black R.Leaf 28 R.Leaf)))) 29 (R.Node R.Black (R.Node R.Black (R.Node R.Black (R.Node R.Black R.Leaf 30 R.Leaf) 31 (R.Node R.Black R.Leaf 32 R.Leaf)) 33 (R.Node R.Black (R.Node R.Black R.Leaf 34 R.Leaf) 35 (R.Node R.Black R.Leaf 36 R.Leaf))) 37 (R.Node R.Black (R.Node R.Black (R.Node R.Black R.Leaf 38 R.Leaf) 39 (R.Node R.Black R.Leaf 40 R.Leaf)) 41 (R.Node R.Black (R.Node R.Black R.Leaf 42 R.Leaf) 43 (R.Node R.Black R.Leaf 44 R.Leaf))))

-- These are valid trees for testing the `insert` function.

in0 :: R.Tree Int
in0 = R.Leaf

in1 :: R.Tree Int
in1 = R.Node R.Black R.Leaf 1 R.Leaf

in2 :: R.Tree Int
in2 = R.Node R.Black R.Leaf 1 (R.Node R.Red R.Leaf 2 R.Leaf)

in3 :: R.Tree Int
in3 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)

in4 :: R.Tree Int
in4 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 (R.Node R.Red R.Leaf 4 R.Leaf))

in5 :: R.Tree Int
in5 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Red (R.Node R.Black R.Leaf 3 R.Leaf) 4 (R.Node R.Black R.Leaf 5 R.Leaf))

in6 :: R.Tree Int
in6 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Red (R.Node R.Black R.Leaf 3 R.Leaf) 4 (R.Node R.Black R.Leaf 5 (R.Node R.Red R.Leaf 6 R.Leaf)))

in7 :: R.Tree Int
in7 = R.Node R.Black (R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)) 4 (R.Node R.Black (R.Node R.Black R.Leaf 5 R.Leaf) 6 (R.Node R.Black R.Leaf 7 R.Leaf))

in8 :: R.Tree Int
in8 = R.Node R.Black (R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)) 4 (R.Node R.Black (R.Node R.Black R.Leaf 5 R.Leaf) 6 (R.Node R.Black R.Leaf 7 (R.Node R.Red R.Leaf 8 R.Leaf)))

in9 :: R.Tree Int
in9 = R.Node R.Black (R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)) 4 (R.Node R.Black (R.Node R.Black R.Leaf 5 R.Leaf) 6 (R.Node R.Red (R.Node R.Black R.Leaf 7 R.Leaf) 8 (R.Node R.Black R.Leaf 9 R.Leaf)))

-- These are valid trees for testing the `eqSet` function.
-- All are derived from the numbers 1, 2, 3, 4, 5 in various orders.

is1 :: R.Tree Int
is1 = R.Node R.Black (R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf)
      2 (R.Node R.Black R.Leaf 3 R.Leaf)) 4 (R.Node R.Black R.Leaf 5 R.Leaf)

is2 :: R.Tree Int
is2 = R.Node R.Black (R.Node R.Black (R.Node R.Red R.Leaf 1 R.Leaf) 2 R.Leaf)
      3 (R.Node R.Black R.Leaf 4 (R.Node R.Red R.Leaf 5 R.Leaf))

is3 :: R.Tree Int
is3 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2
      (R.Node R.Red (R.Node R.Black R.Leaf 3 R.Leaf) 4 (R.Node R.Black R.Leaf 5 R.Leaf))

-- These are invalid trees.
-- The name prefix is a label.
-- "o" -- violates ordering constraint.
-- "c" -- violates color constraint.
-- "b" -- violates black count constraints.

ot1 :: R.Tree Int
ot1 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 1 (R.Node R.Black R.Leaf 2 R.Leaf)

ot2 :: R.Tree Int
ot2 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 2 R.Leaf)

ot3 :: R.Tree Int
ot3 = R.Node R.Black (R.Node R.Black R.Leaf 2 R.Leaf) 1 (R.Node R.Black R.Leaf 3 R.Leaf)

ot4 :: R.Tree Int
ot4 = R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 3 (R.Node R.Black R.Leaf 2 R.Leaf)

ot5 :: R.Tree Int
ot5 = R.Node R.Black
      (R.Node R.Black (R.Node R.Black R.Leaf 3 R.Leaf) 2 (R.Node R.Black R.Leaf 4 R.Leaf))
      1
      (R.Node R.Black (R.Node R.Black R.Leaf 6 R.Leaf) 5 (R.Node R.Black R.Leaf 7 R.Leaf))

-- Additional tree suggested by Kriti Devasenapathy:
ot6 :: R.Tree Int
ot6 = R.Node R.Black
      (R.Node R.Black (R.Node R.Black R.Leaf 1 R.Leaf) 3 (R.Node R.Black R.Leaf 6 R.Leaf))
      5
      (R.Node R.Black (R.Node R.Black R.Leaf 7 R.Leaf) 9 (R.Node R.Black R.Leaf 14 R.Leaf))

cbt6 :: R.Tree Int
cbt6 = R.Node R.Red (R.Node R.Black R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf)

cbt7 :: R.Tree Int
cbt7 = R.Node R.Red (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf)

ct8 :: R.Tree Int
ct8 = R.Node R.Red (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf)

cbt9 :: R.Tree Int
cbt9 = R.Node R.Black
       (R.Node R.Red (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Black R.Leaf 3 R.Leaf))
       4
       (R.Node R.Red (R.Node R.Red R.Leaf 5 R.Leaf) 6 (R.Node R.Black R.Leaf 7 R.Leaf))

ct10 :: R.Tree Int
ct10 = R.Node R.Black
       (R.Node R.Red (R.Node R.Red R.Leaf 1 R.Leaf) 2 (R.Node R.Red R.Leaf 3 R.Leaf))
       4
       (R.Node R.Red (R.Node R.Red R.Leaf 5 R.Leaf) 6 (R.Node R.Red R.Leaf 7 R.Leaf))

---------------------------------------------------------------------------
-- Unit tests on red-black trees (non-randomized).
---------------------------------------------------------------------------

-- 1) Unit tests on valid trees.

-- Test the test invariants.

validTreeUnitTest1a = TestCase $ assertBool "t1 should satisfy invariant 1" (R.testInvariant1 t1)
validTreeUnitTest1b = TestCase $ assertBool "t1 should satisfy invariant 2" (R.testInvariant2 t1)
validTreeUnitTest1c = TestCase $ assertBool "t1 should satisfy invariant 3" (R.testInvariant3 t1)

validTreeUnitTest2a = TestCase $ assertBool "t2 should satisfy invariant 1" (R.testInvariant1 t2)
validTreeUnitTest2b = TestCase $ assertBool "t2 should satisfy invariant 2" (R.testInvariant2 t2)
validTreeUnitTest2c = TestCase $ assertBool "t2 should satisfy invariant 3" (R.testInvariant3 t2)

validTreeUnitTest3a = TestCase $ assertBool "t3 should satisfy invariant 1" (R.testInvariant1 t3)
validTreeUnitTest3b = TestCase $ assertBool "t3 should satisfy invariant 2" (R.testInvariant2 t3)
validTreeUnitTest3c = TestCase $ assertBool "t3 should satisfy invariant 3" (R.testInvariant3 t3)

validTreeUnitTest4a = TestCase $ assertBool "t4 should satisfy invariant 1" (R.testInvariant1 t4)
validTreeUnitTest4b = TestCase $ assertBool "t4 should satisfy invariant 2" (R.testInvariant2 t4)
validTreeUnitTest4c = TestCase $ assertBool "t4 should satisfy invariant 3" (R.testInvariant3 t4)

validTreeUnitTest5a = TestCase $ assertBool "t5 should satisfy invariant 1" (R.testInvariant1 t5)
validTreeUnitTest5b = TestCase $ assertBool "t5 should satisfy invariant 2" (R.testInvariant2 t5)
validTreeUnitTest5c = TestCase $ assertBool "t5 should satisfy invariant 3" (R.testInvariant3 t5)

validTreeUnitTest6a = TestCase $ assertBool "t6 should satisfy invariant 1" (R.testInvariant1 t6)
validTreeUnitTest6b = TestCase $ assertBool "t6 should satisfy invariant 2" (R.testInvariant2 t6)
validTreeUnitTest6c = TestCase $ assertBool "t6 should satisfy invariant 3" (R.testInvariant3 t6)

validTreeUnitTest7a = TestCase $ assertBool "t7 should satisfy invariant 1" (R.testInvariant1 t7)
validTreeUnitTest7b = TestCase $ assertBool "t7 should satisfy invariant 2" (R.testInvariant2 t7)
validTreeUnitTest7c = TestCase $ assertBool "t7 should satisfy invariant 3" (R.testInvariant3 t7)

validTreeUnitTest8a = TestCase $ assertBool "t8 should satisfy invariant 1" (R.testInvariant1 t8)
validTreeUnitTest8b = TestCase $ assertBool "t8 should satisfy invariant 2" (R.testInvariant2 t8)
validTreeUnitTest8c = TestCase $ assertBool "t8 should satisfy invariant 3" (R.testInvariant3 t8)

validTreeUnitTest9a = TestCase $ assertBool "t9 should satisfy invariant 1" (R.testInvariant1 t9)
validTreeUnitTest9b = TestCase $ assertBool "t9 should satisfy invariant 2" (R.testInvariant2 t9)
validTreeUnitTest9c = TestCase $ assertBool "t9 should satisfy invariant 3" (R.testInvariant3 t9)

-- Test `minDepth` and `maxDepth`.

validMinDepth0  = TestCase $ assertEqual "d0 should have the correct minDepth"  (R.minDepth d0)  0
validMinDepth1  = TestCase $ assertEqual "d1 should have the correct minDepth"  (R.minDepth d1)  1
validMinDepth2  = TestCase $ assertEqual "d2 should have the correct minDepth"  (R.minDepth d2)  1
validMinDepth3  = TestCase $ assertEqual "d3 should have the correct minDepth"  (R.minDepth d3)  2
validMinDepth4  = TestCase $ assertEqual "d4 should have the correct minDepth"  (R.minDepth d4)  2
validMinDepth5  = TestCase $ assertEqual "d5 should have the correct minDepth"  (R.minDepth d5)  2
validMinDepth6  = TestCase $ assertEqual "d6 should have the correct minDepth"  (R.minDepth d6)  2
validMinDepth7  = TestCase $ assertEqual "d7 should have the correct minDepth"  (R.minDepth d7)  3
validMinDepth8  = TestCase $ assertEqual "d8 should have the correct minDepth"  (R.minDepth d8)  3
validMinDepth9  = TestCase $ assertEqual "d9 should have the correct minDepth"  (R.minDepth d9)  3
validMinDepth10 = TestCase $ assertEqual "d10 should have the correct minDepth" (R.minDepth d10) 5

validMaxDepth0  = TestCase $ assertEqual "d0 should have the correct maxDepth"  (R.maxDepth d0)  0
validMaxDepth1  = TestCase $ assertEqual "d1 should have the correct maxDepth"  (R.maxDepth d1)  1
validMaxDepth2  = TestCase $ assertEqual "d2 should have the correct maxDepth"  (R.maxDepth d2)  2
validMaxDepth3  = TestCase $ assertEqual "d3 should have the correct maxDepth"  (R.maxDepth d3)  2
validMaxDepth4  = TestCase $ assertEqual "d4 should have the correct maxDepth"  (R.maxDepth d4)  3
validMaxDepth5  = TestCase $ assertEqual "d5 should have the correct maxDepth"  (R.maxDepth d5)  3
validMaxDepth6  = TestCase $ assertEqual "d6 should have the correct maxDepth"  (R.maxDepth d6)  4
validMaxDepth7  = TestCase $ assertEqual "d7 should have the correct maxDepth"  (R.maxDepth d7)  3
validMaxDepth8  = TestCase $ assertEqual "d8 should have the correct maxDepth"  (R.maxDepth d8)  4
validMaxDepth9  = TestCase $ assertEqual "d9 should have the correct maxDepth"  (R.maxDepth d9)  4
validMaxDepth10 = TestCase $ assertEqual "d10 should have the correct maxDepth" (R.maxDepth d10) 8

-- Test `insert`.

validInsert1 = TestCase $ assertBool "insert 1 in0 should equal in1"
  (eqTreeInt (R.insert 1 in0) in1)
validInsert2 = TestCase $ assertBool "insert 2 in1 should equal in2"
  (eqTreeInt (R.insert 2 in1) in2)
validInsert3 = TestCase $ assertBool "insert 3 in2 should equal in3"
  (eqTreeInt (R.insert 3 in2) in3)
validInsert4 = TestCase $ assertBool "insert 4 in3 should equal in4"
  (eqTreeInt (R.insert 4 in3) in4)
validInsert5 = TestCase $ assertBool "insert 5 in4 should equal in5"
  (eqTreeInt (R.insert 5 in4) in5)
validInsert6 = TestCase $ assertBool "insert 6 in5 should equal in6"
  (eqTreeInt (R.insert 6 in5) in6)
validInsert7 = TestCase $ assertBool "insert 7 in6 should equal in7"
  (eqTreeInt (R.insert 7 in6) in7)
validInsert8 = TestCase $ assertBool "insert 8 in7 should equal in8"
  (eqTreeInt (R.insert 8 in7) in8)
validInsert9 = TestCase $ assertBool "insert 9 in8 should equal in9"
  (eqTreeInt (R.insert 9 in8) in9)

-- Test `eqSet`.

validEqSet1 = TestCase $ assertBool "is1 should be equal to is2" (R.eqSet is1 is2)
validEqSet2 = TestCase $ assertBool "is2 should be equal to is1" (R.eqSet is2 is1)
validEqSet3 = TestCase $ assertBool "is1 should be equal to is3" (R.eqSet is1 is3)
validEqSet4 = TestCase $ assertBool "is3 should be equal to is1" (R.eqSet is3 is1)
validEqSet5 = TestCase $ assertBool "is2 should be equal to is3" (R.eqSet is2 is3)
validEqSet6 = TestCase $ assertBool "is3 should be equal to is2" (R.eqSet is3 is2)

validTreeUnitTests = TestList
  [ TestLabel "1a" validTreeUnitTest1a
  , TestLabel "1b" validTreeUnitTest1b
  , TestLabel "1c" validTreeUnitTest1c

  , TestLabel "2a" validTreeUnitTest2a
  , TestLabel "2b" validTreeUnitTest2b
  , TestLabel "2c" validTreeUnitTest2c

  , TestLabel "3a" validTreeUnitTest3a
  , TestLabel "3b" validTreeUnitTest3b
  , TestLabel "3c" validTreeUnitTest3c

  , TestLabel "4a" validTreeUnitTest4a
  , TestLabel "4b" validTreeUnitTest4b
  , TestLabel "4c" validTreeUnitTest4c

  , TestLabel "5a" validTreeUnitTest5a
  , TestLabel "5b" validTreeUnitTest5b
  , TestLabel "5c" validTreeUnitTest5c

  , TestLabel "6a" validTreeUnitTest6a
  , TestLabel "6b" validTreeUnitTest6b
  , TestLabel "6c" validTreeUnitTest6c

  , TestLabel "7a" validTreeUnitTest7a
  , TestLabel "7b" validTreeUnitTest7b
  , TestLabel "7c" validTreeUnitTest7c

  , TestLabel "8a" validTreeUnitTest8a
  , TestLabel "8b" validTreeUnitTest8b
  , TestLabel "8c" validTreeUnitTest8c

  , TestLabel "9a" validTreeUnitTest9a
  , TestLabel "9b" validTreeUnitTest9b
  , TestLabel "9c" validTreeUnitTest9c

  , TestLabel "10_0"  validMinDepth0
  , TestLabel "10_1"  validMinDepth1
  , TestLabel "10_2"  validMinDepth2
  , TestLabel "10_3"  validMinDepth3
  , TestLabel "10_4"  validMinDepth4
  , TestLabel "10_5"  validMinDepth5
  , TestLabel "10_6"  validMinDepth6
  , TestLabel "10_7"  validMinDepth7
  , TestLabel "10_8"  validMinDepth8
  , TestLabel "10_9"  validMinDepth9
  , TestLabel "10_10" validMinDepth10

  , TestLabel "10_0"  validMaxDepth0
  , TestLabel "10_1"  validMaxDepth1
  , TestLabel "10_2"  validMaxDepth2
  , TestLabel "10_3"  validMaxDepth3
  , TestLabel "10_4"  validMaxDepth4
  , TestLabel "10_5"  validMaxDepth5
  , TestLabel "10_6"  validMaxDepth6
  , TestLabel "10_7"  validMaxDepth7
  , TestLabel "10_8"  validMaxDepth8
  , TestLabel "10_9"  validMaxDepth9
  , TestLabel "10_10" validMaxDepth10

  , TestLabel "11_1"  validInsert1
  , TestLabel "11_2"  validInsert2
  , TestLabel "11_3"  validInsert3
  , TestLabel "11_4"  validInsert4
  , TestLabel "11_5"  validInsert5
  , TestLabel "11_6"  validInsert6
  , TestLabel "11_7"  validInsert7
  , TestLabel "11_8"  validInsert8
  , TestLabel "11_9"  validInsert9

  , TestLabel "12_1"  validEqSet1
  , TestLabel "12_2"  validEqSet2
  , TestLabel "12_3"  validEqSet3
  , TestLabel "12_4"  validEqSet4
  , TestLabel "12_5"  validEqSet5
  , TestLabel "12_6"  validEqSet6

  ]

-- 2) Unit tests on invalid trees.

invalidTreeUnitTest1 = TestCase $
  assertBool "ot1 should fail to satisfy invariant 1" (not (R.testInvariant1 ot1))
invalidTreeUnitTest2 = TestCase $
  assertBool "ot2 should fail to satisfy invariant 1" (not (R.testInvariant1 ot2))
invalidTreeUnitTest3 = TestCase $
  assertBool "ot3 should fail to satisfy invariant 1" (not (R.testInvariant1 ot3))
invalidTreeUnitTest4 = TestCase $
  assertBool "ot4 should fail to satisfy invariant 1" (not (R.testInvariant1 ot4))
invalidTreeUnitTest5 = TestCase $
  assertBool "ot5 should fail to satisfy invariant 1" (not (R.testInvariant1 ot5))
invalidTreeUnitTest6 = TestCase $
  assertBool "ot6 should fail to satisfy invariant 1" (not (R.testInvariant1 ot6))

invalidTreeUnitTest6a = TestCase $
  assertBool "cbt6 should fail to satisfy invariant 2" (not (R.testInvariant2 cbt6))
invalidTreeUnitTest6b = TestCase $
  assertBool "cbt6 should fail to satisfy invariant 3" (not (R.testInvariant3 cbt6))
invalidTreeUnitTest7a = TestCase $
  assertBool "cbt7 should fail to satisfy invariant 2" (not (R.testInvariant2 cbt7))
invalidTreeUnitTest7b = TestCase $
  assertBool "cbt7 should fail to satisfy invariant 3" (not (R.testInvariant3 cbt7))
invalidTreeUnitTest8 = TestCase $
  assertBool "ct8 should fail to satisfy invariant 2" (not (R.testInvariant2 ct8))
invalidTreeUnitTest9a = TestCase $
  assertBool "cbt9 should fail to satisfy invariant 2" (not (R.testInvariant2 cbt9))
invalidTreeUnitTest9b = TestCase $
  assertBool "cbt9 should fail to satisfy invariant 3" (not (R.testInvariant3 cbt9))
invalidTreeUnitTest10 = TestCase $
  assertBool "ct10 should fail to satisfy invariant 2" (not (R.testInvariant2 ct10))

invalidTreeUnitTests = TestList
  [ TestLabel "i1"  invalidTreeUnitTest1
  , TestLabel "i2"  invalidTreeUnitTest2
  , TestLabel "i3"  invalidTreeUnitTest3
  , TestLabel "i4"  invalidTreeUnitTest4
  , TestLabel "i5"  invalidTreeUnitTest5
  , TestLabel "i6"  invalidTreeUnitTest6
  , TestLabel "i6a" invalidTreeUnitTest6a
  , TestLabel "i6b" invalidTreeUnitTest6b
  , TestLabel "i7a" invalidTreeUnitTest7a
  , TestLabel "i7b" invalidTreeUnitTest7b
  , TestLabel "i8"  invalidTreeUnitTest8
  , TestLabel "i9a" invalidTreeUnitTest9a
  , TestLabel "i9b" invalidTreeUnitTest9b
  , TestLabel "i10" invalidTreeUnitTest10
  ]

runRBUnitTests :: IO ()
runRBUnitTests = do
  putStrLn "validTreeUnitTests:\n"
  counts <- runTestTT validTreeUnitTests
  print counts
  putStrLn ""
  putStrLn "invalidTreeUnitTests:\n"
  counts <- runTestTT invalidTreeUnitTests
  print counts
  putStrLn ""

---------------------------------------------------------------------------
-- Randomized tests on red-black trees.
---------------------------------------------------------------------------

-- Test that all elements of a Tree constructed from a list
-- are in fact in the Tree.
prop_member :: [Int] -> Bool
prop_member lst =
  let t = R.fromList lst in
    all (\x -> R.member x t) lst

-- Test that converting a list to a tree and back is the same
-- as removing all duplicates from the list.
prop_list_tree_list :: [Int] -> Bool
prop_list_tree_list lst =
  let
    t = R.fromList lst
    lst' = R.toList t
    lst'' = L.nub lst
  in
    length lst' == length lst'' &&
    all (\x -> x `elem` lst') lst''

-- Test that the maximum depth of a red-black tree is no worst
-- than twice the minimum depth, where "depth" here means
-- the distance from the root to a leaf.
prop_depth :: [Int] -> Bool
prop_depth lst =
  let
    t = R.fromList lst
    minD = R.minDepth t
    maxD = R.maxDepth t
  in
    maxD >= minD && maxD <= 2 * minD

-- Test that tree invariants (ordering constraint) hold true for all trees.

prop_inv1 :: [Int] -> Bool
prop_inv1 lst = R.testInvariant1 (R.fromList lst)

prop_inv2 :: [Int] -> Bool
prop_inv2 lst = R.testInvariant2 (R.fromList lst)

prop_inv3 :: [Int] -> Bool
prop_inv3 lst = R.testInvariant3 (R.fromList lst)

---------------------------------------------------------------------------
-- Randomized tests on Sets.
---------------------------------------------------------------------------

-- Test that the `isSubset` function gives the same result
-- as the `isSubsetOf` function from the `Set` type in `Data.Set`.
prop_isSubset_valid :: [Int] -> Bool
prop_isSubset_valid lst =
  let
    sublst  = take (length lst `div` 2) lst
    hset    = S.fromList lst
    hsubset = S.fromList sublst
    set     = R.toSet lst
    subset  = R.toSet sublst
  in
    S.isSubsetOf hsubset hset == R.isSubset subset set

-- Test that the `isSubset` function gives the correct result
-- when sets are actually subsets.
prop_isSubset_true :: [Int] -> Bool
prop_isSubset_true lst =
  let
    sublst  = take (length lst `div` 2) lst
    set     = R.toSet lst
    subset  = R.toSet sublst
  in
    R.isSubset subset set

-- Test that the `isSubset` function gives the correct result
-- when sets are *not* subsets.
prop_isSubset_false :: [Int] -> Bool
prop_isSubset_false lst =
  let
    maxInt    = maximum lst
    sublst    = take (length lst `div` 2) lst
    set       = R.toSet lst
    notSubset = R.toSet $ sublst ++ [maxInt + 1]
  in
    not (R.isSubset notSubset set)

-- Test that the `eqSet` function is correct by comparing it to the
-- `==` operator on the set type in `Data.Set`.
prop_eqSet_valid :: [Int] -> [Int] -> Bool
prop_eqSet_valid lst1 lst2 =
  let
    hset1 = S.fromList lst1
    hset2 = S.fromList lst2
    set1  = R.toSet lst1
    set2  = R.toSet lst2
  in
    ((hset1 == hset2) == R.eqSet set1 set2) &&
    R.eqSet set1 set1 && R.eqSet set2 set2

-- Test that the union of an empty set with another set gives the other set.
prop_union_empty :: [Int] -> Bool
prop_union_empty lst =
  let s = R.toSet lst in
    R.eqSet (R.union R.empty s) s &&
    R.eqSet (R.union s R.empty) s

-- Test that (union s1 s2) == (union s2 s1).
prop_union_symmetric :: [Int] -> [Int] -> Bool
prop_union_symmetric lst1 lst2 =
  let
    s1 = R.toSet lst1
    s2 = R.toSet lst2
  in
    R.eqSet (R.union s1 s2) (R.union s2 s1)

-- Test that all elements of s1 and s2 are in (union s1 s2).
-- Test that all elements of (union s1 s2) are in either s1 or s2.
prop_union_complete :: [Int] -> [Int] -> Bool
prop_union_complete lst1 lst2 =
  let
    s1 = R.toSet lst1
    s2 = R.toSet lst2
    u  = R.union s1 s2
    ulist = R.toList u
  in
    all (\x -> R.member x s1 || R.member x s2) ulist &&
    all (\x -> R.member x u) lst1 &&
    all (\x -> R.member x u) lst2

-- Test that the intersection of an empty set with another set gives the empty set.
prop_intersection_empty :: [Int] -> Bool
prop_intersection_empty lst =
  let s = R.toSet lst in
    R.eqSet (R.intersection R.empty s) R.empty &&
    R.eqSet (R.intersection s R.empty) R.empty

-- Test that (intersection s1 s2) == (intersection s2 s1).
prop_intersection_symmetric :: [Int] -> [Int] -> Bool
prop_intersection_symmetric lst1 lst2 =
  let
    s1 = R.toSet lst1
    s2 = R.toSet lst2
  in
    R.eqSet (R.intersection s1 s2) (R.intersection s2 s1)

-- Test that all elements of (intersection s1 s2) are in both s1 and s2.
-- Test that all elements which are in both s1 and s2 are in (intersection s1 s2).
prop_intersection_complete :: [Int] -> [Int] -> Bool
prop_intersection_complete lst1 lst2 =
  let
    s1 = R.toSet lst1
    s2 = R.toSet lst2
    i  = R.intersection s1 s2
    ilist = R.toList i
  in
    all (\x -> R.member x s1 && R.member x s2) ilist &&
    all (\x -> if x `elem` lst2 then R.member x i else not (R.member x i)) lst1 &&
    all (\x -> if x `elem` lst1 then R.member x i else not (R.member x i)) lst2

-- Test that the difference of a set and the empty set is the first set.
-- Test that the difference of an empty set and a set is the empty set.
prop_difference_empty :: [Int] -> Bool
prop_difference_empty lst =
  let s = R.toSet lst in
    R.eqSet (R.difference R.empty s) R.empty &&
    R.eqSet (R.difference s R.empty) s

-- Test that all elements of (difference s1 s2) are in s1 but not s2.
-- Test that all elements that are in s1 but not s2 are in (difference s1 s2).
prop_difference_complete :: [Int] -> [Int] -> Bool
prop_difference_complete lst1 lst2 =
  let
    s1 = R.toSet lst1
    s2 = R.toSet lst2
    i  = R.difference s1 s2
    ilist = R.toList i
  in
    all (\x -> R.member x s1 && not (R.member x s2)) ilist &&
    all (\x -> if x `notElem` lst2 then R.member x i else not (R.member x i)) lst1 &&
    all (\x -> not (R.member x i)) lst2


-- Run all the quickcheck tests.
-- This uses Template Haskell; see
-- https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-All.html
-- for more details.
return []
allRandomizedTests = $quickCheckAll

runRandomizedTests :: IO ()
runRandomizedTests = do
  success <- allRandomizedTests
  if success
     then putStrLn "ALL RANDOMIZED TESTS PASSED!\n"
     else putStrLn "ERROR: SOME TESTS FAILED!\n"
  putStrLn ""

-- Run all the tests.
main :: IO ()
main = do
  putStrLn "Running randomized QuickCheck tests...\n"
  runRandomizedTests
  putStrLn "Running HUnit tests...\n"
  runRBUnitTests

