----------------------------------------------------------------------
-- S-expression parser.
----------------------------------------------------------------------

-- We call this module `Main` for testing purposes.
-- In a real-world scenario, it would be called `Sexpr`.
module Main where

import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA  Bool
  | IntA   Integer
  | FloatA Double
  | IdA    String  -- identifier
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

-- 4: Better floats
parseExpHelper :: Parser [Char] -- Helper function to parse floating-point numbers with exponents
parseExpHelper = do
  exp <- oneOf "eE"
  sign <- option "" (string "-" <|> string "+")
  digits <- many1 digit
  return ([exp] ++ sign ++ digits)

parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  exp <- option "" parseExpHelper
  return (read (sign ++ digits ++ "." ++ f ++ exp) :: Double)
  <?> "floating-point number"

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

-- 5: Adding strings
parseString :: Parser String
parseString = do
  char '\"' -- use escape character \ to handle backslash escapes, similar with parseQuote
  str <- many (noneOf "\"")
  char '\"'
  return str
  <?> "string"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> try (parseString >>= return . StringA) -- handle strings after integers but before identifiers
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- 2: Generalized parentheses
parseListHelper :: Char -> Char -> Parser [Sexpr]
parseListHelper char1 char2 = do
  char char1
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char char2
  return ss
  <?> "delimiter S-expressions"

-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.

parseList :: Parser [Sexpr]
parseList = do
  parseListHelper '(' ')'
  <|> parseListHelper '[' ']'
  <|> parseListHelper '{' '}'
  <?> "list of S-expressions"

-- 3: Why not try?
{-
This is because we do not experience backtrack.
In the event that we tried to parse one kind of delimiter and failed,
the parser fails without consuming any input,
so <|> is sufficient and we don't need to use the try combinator.
-}


-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = char '\'' >> parseSexpr
  <?> "quoted S-expression"

-- 1: Simplifying the Sexpr datatype
-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> (parseQuote >>= return . (\x -> ListS [AtomS (IdA "quote"), x])) -- replace QuoteS with AtomS and ListS
  <?> "S-expression"

-- Parse a series of Sexprs from a string
-- representing the entire contents of a file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ "AtomS[" ++ show a ++ "]"
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

usage :: String -> IO ()
usage s = do
  hPutStrLn stderr $ "usage: " ++ s ++ " filename"

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [filename] -> runPpSexpr filename >> exitSuccess
    _ -> usage progName >> exitFailure
