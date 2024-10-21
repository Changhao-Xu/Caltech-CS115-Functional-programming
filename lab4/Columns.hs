module Main where

import Prelude                                              -- read, words, all, mapM, the !! operator
import System.Environment                                   -- getProgName, getArgs in System.Environment module
import System.Exit                                          -- exitFailure, exitSuccess in System.Exit module
-- import Control.Monad                                        -- guard
import Data.Char                                            -- isDigit
-- import Data.List                                            -- intercalate, unwords
-- import Data.Maybe                                           -- mapMaybe
import System.IO                                            -- stdin, hGetContents

main :: IO ()
main = 
{- This part is to debug: args = 1 3 myfile2.txt, progName = columns.exe
    do
        args <- getArgs
        putStrLn "Command-line arguments:"
        mapM_ putStrLn args
        progName <- getProgName
        putStrLn "program name:"
        putStrLn progName
-}
    do args <- getArgs
    --    progName <- getProgName
       if validInput (init args)                                           -- all the elements of args except for the last filename
           then do let fileText | (last args) == "-" = hGetContents stdin  -- filename is -, read from stdin (standard input i.e. the terminal)
                                | otherwise = readFile (last args)         -- otherwise, read from file
                   text <- fileText
                   let cols = map (\x -> read x) (init args)               -- build col list
                   let newText = map unwords (map (getWord cols) (map words (lines text)))
                   mapM_ putStrLn newText
                   exitSuccess
           else do putStrLn ("usage: columns n1 n2 ... filename, n1 n2 etc are positive integers") -- no need progName, as answered on Piazza
                   exitFailure

-- Helper function checking whether all input n is valid: is numbers && > 0.
validInput :: [String] -> Bool -- [String] is the input argument w/o progName
validInput [] = True
validInput (x:xs) = all isDigit x && (read x) > 0 && validInput xs

-- Helper function returning words of the col index, use !! operator. 
getWord :: [Int] -> [String] -> [String]
getWord (x:xs) y | x <= length y = [y !! (x - 1)] ++ getWord xs y -- !! access elements in a list by their index, starting from 1 for the first item
                 | otherwise = []                                 -- if exceed length, return []
getWord _ _ = []