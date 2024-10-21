--
-- Tests_Lab6.hs
--

{-# LANGUAGE TemplateHaskell #-}

module Main where

import State
import Test.HUnit
import System.IO.Unsafe -- to test IO-returning functions

---------------------------------------------------------------------------
-- Unit tests.
---------------------------------------------------------------------------

-- factIO

factIOUnitTest1a =
  TestCase $ assertEqual "factorial 0" 1
    (unsafePerformIO (factIO 0))
factIOUnitTest1b =
  TestCase $ assertEqual "factorial 1" 1
    (unsafePerformIO (factIO 1))
factIOUnitTest1c =
  TestCase $ assertEqual "factorial 2" 2
    (unsafePerformIO (factIO 2))
factIOUnitTest1d =
  TestCase $ assertEqual "factorial 5" 120
    (unsafePerformIO (factIO 5))
factIOUnitTest1e =
  TestCase $ assertEqual "factorial 10" 3_628_800
    (unsafePerformIO (factIO 10))

factIOUnitTests = TestList
  [ TestLabel "1a" factIOUnitTest1a
  , TestLabel "1b" factIOUnitTest1b
  , TestLabel "1c" factIOUnitTest1c
  , TestLabel "1d" factIOUnitTest1d
  , TestLabel "1e" factIOUnitTest1e
  ]

-- factState

factStateUnitTest2a =
  TestCase $ assertEqual "factorial 0" 1 (factState 0)
factStateUnitTest2b =
  TestCase $ assertEqual "factorial 1" 1 (factState 1)
factStateUnitTest2c =
  TestCase $ assertEqual "factorial 2" 2 (factState 2)
factStateUnitTest2d =
  TestCase $ assertEqual "factorial 5" 120 (factState 5)
factStateUnitTest2e =
  TestCase $ assertEqual "factorial 10" 3_628_800 (factState 10)

factStateUnitTests = TestList
  [ TestLabel "2a" factStateUnitTest2a
  , TestLabel "2b" factStateUnitTest2b
  , TestLabel "2c" factStateUnitTest2c
  , TestLabel "2d" factStateUnitTest2d
  , TestLabel "2e" factStateUnitTest2e
  ]

-- fibIO

fibIOUnitTest3a =
  TestCase $ assertEqual "fibonacci 0" 0
    (unsafePerformIO (fibIO 0))
fibIOUnitTest3b =
  TestCase $ assertEqual "fibonacci 1" 1
    (unsafePerformIO (fibIO 1))
fibIOUnitTest3c =
  TestCase $ assertEqual "fibonacci 2" 1
    (unsafePerformIO (fibIO 2))
fibIOUnitTest3d =
  TestCase $ assertEqual "fibonacci 5" 5
    (unsafePerformIO (fibIO 5))
fibIOUnitTest3e =
  TestCase $ assertEqual "fibonacci 10" 55
    (unsafePerformIO (fibIO 10))
fibIOUnitTest3f =
  TestCase $ assertEqual "fibonacci 20" 6765
    (unsafePerformIO (fibIO 20))
fibIOUnitTest3g =
  TestCase $ assertEqual "fibonacci 100" 354224848179261915075 
    (unsafePerformIO (fibIO 100))

fibIOUnitTests = TestList
  [ TestLabel "3a" fibIOUnitTest3a
  , TestLabel "3b" fibIOUnitTest3b
  , TestLabel "3c" fibIOUnitTest3c
  , TestLabel "3d" fibIOUnitTest3d
  , TestLabel "3e" fibIOUnitTest3e
  , TestLabel "3f" fibIOUnitTest3f
  , TestLabel "3g" fibIOUnitTest3g
  ]

-- fibState

fibStateUnitTest4a =
  TestCase $ assertEqual "fibonacci 0" 0 (fibState 0)
fibStateUnitTest4b =
  TestCase $ assertEqual "fibonacci 1" 1 (fibState 1)
fibStateUnitTest4c =
  TestCase $ assertEqual "fibonacci 2" 1 (fibState 2)
fibStateUnitTest4d =
  TestCase $ assertEqual "fibonacci 5" 5 (fibState 5)
fibStateUnitTest4e =
  TestCase $ assertEqual "fibonacci 10" 55 (fibState 10)
fibStateUnitTest4f =
  TestCase $ assertEqual "fibonacci 20" 6765 (fibState 20)
fibStateUnitTest4g =
  TestCase $ assertEqual "fibonacci 100" 354224848179261915075 (fibState 100)

fibStateUnitTests = TestList
  [ TestLabel "4a" fibStateUnitTest4a
  , TestLabel "4b" fibStateUnitTest4b
  , TestLabel "4c" fibStateUnitTest4c
  , TestLabel "4d" fibStateUnitTest4d
  , TestLabel "4e" fibStateUnitTest4e
  , TestLabel "4f" fibStateUnitTest4f
  , TestLabel "4g" fibStateUnitTest4g
  ]

-- Test runner.

runUnitTests :: IO ()
runUnitTests = do
  putStrLn "Testing factIO..."
  counts <- runTestTT factIOUnitTests
  print counts
  putStrLn ""

  putStrLn "Testing factState..."
  counts <- runTestTT factStateUnitTests
  print counts
  putStrLn ""

  putStrLn "Testing fibIO..."
  counts <- runTestTT fibIOUnitTests
  print counts
  putStrLn ""

  putStrLn "Testing fibState..."
  counts <- runTestTT fibStateUnitTests
  print counts
  putStrLn ""


-- Run all the tests.
main :: IO ()
main = do
  putStrLn "Running HUnit tests...\n"
  runUnitTests

