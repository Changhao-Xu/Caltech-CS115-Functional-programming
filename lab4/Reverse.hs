module Main where

import Prelude                                              -- reverse, lines, mapM_, readFile in Prelude module
import System.Environment                                   -- getProgName, getArgs in System.Environment module
import System.Exit                                          -- exitFailure, exitSuccess in System.Exit module

main :: IO ()
main = 
    do args <- getArgs                                      -- returns a list of the program's command line arguments
--       progName <- getProgName                            -- returns the name of the program 
       if length args /= 1                                  -- If there are too many or too few command-line arguments (should only consist of file.txt)
           then do putStrLn ("usage: reverse filename")     -- usage: reverse filename, ++ progName also is okay
                   exitFailure
           else do fileText <- readFile (head args)
                   mapM_ putStrLn (reverse (lines fileText))
                   exitSuccess