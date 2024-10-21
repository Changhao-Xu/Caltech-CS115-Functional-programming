module Lab4a where
-- Need to import this to get the definition of toUpper:
import Data.Char

-- Part A
-- 1
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = putChar c >> myPutStrLn cs -- Lec 11 P19

-- 2
greet :: String -> IO ()
greet name = do putStrLn ("Hello, " ++ name ++ "!")
-- do is redundant

-- 3
{-
-- Ask the user for his/her name, then print a greeting.
greet2 :: IO ()
greet2 = do
  putStr "Enter your name: "
  name <- getLine
  putStr "Hello, "
  putStr name
  putStrLn "!"
-}

-- way 1: simple desugaring, Lec 11 P27
greet2a :: IO ()
greet2a = putStr "Enter your name: " >>
          getLine >>= \name -> (putStr "Hello, " >>
                                putStr name >>
                                putStrLn "!")

-- way 2: complicated desugaring involving "case" statement, Lec 11 P36
greet2b :: IO ()
greet2b = putStr "Enter your name: " >>
          getLine >>= \name -> case name of _ -> (putStr "Hello, " >>
                                                  putStr name >>
                                                  putStrLn "!")
{-
Complex desugaring behave the same as simple desugaring here. 
This is because there is not pattern match failure in the code.
Note that in greet2b, if we intentionally create another pattern match
    _ -> fail "Pattern match failure in do expression"
The complier will actually give an warning saying that pattern match is redundant.
-}

-- 4
{-
-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
greet3 :: IO ()
greet3 = do
  putStr "Enter your name: "
  (n:ns) <- getLine
  let name = toUpper n : ns
  putStr "Hello, "
  putStr name
  putStrLn "!"
-}

-- way 1: simple desugaring, Lec 11 P27
greet3a :: IO()
greet3a = putStr "Enter your name: " >>
          getLine >>= \(n : ns) -> 
            let name = toUpper n : ns in 
                putStr "Hello, " >>
                putStr name >>
                putStrLn "!"

-- way 2: complicated desugaring involving "case" statement, Lec 11 P36
greet3b :: IO()
greet3b = putStr "Enter your name: " >> 
          getLine >>= \name -> case name of 
            (n : ns) -> 
                let name = toUpper n : ns in 
                    putStr "Hello, " >>
                    putStr name >>
                    putStrLn "!"
            _ -> fail "Pattern match failure in do expression"

{-
In this case, the more complex desugaring (way 2) will check if there is pattern match failure. 
For example, if input is empty, it will then fail with supplied error message, 
which is not handled properly in simple desugaring (way 1).
-}