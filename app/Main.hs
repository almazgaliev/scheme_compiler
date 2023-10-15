module Main where

import Control.Monad (when)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Numeric (readFloat)
import System.Environment
import Text.Parsec (string')
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.ReadPrec (reset)
import Text.Read (readMaybe)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | IntegerNumber Integer
  | FloatNumber Float
  | String String
  | Bool Bool
  | Character Char
  deriving (Show)

parseString' :: Parser LispVal
parseString' = do
  char '"'
  x <- many stringChar
  char '"'
  return $ String x

stringChar :: Parser Char
stringChar = noneOf "\""

anyEscaped :: Parser Char
anyEscaped = do
  char '\\'
  char <- choice [char 't', char 'n', char '\\', char '"']
  return $ case char of
    't' -> '\t'
    'n' -> '\n'
    '\\' -> '\\'
    '"' -> '"'

stringContent :: Parser String
-- stringContent = many stringChar
stringContent = f <|> return ""
 where
  f = do
    str <- anyEscaped <|> stringChar
    rest <- stringContent
    return $ str : rest

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- stringContent
  char '"'
  return $ String x

res = parse parseString "error" "\" abnafk\\nja\\\bn \""

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = IntegerNumber . read <$> many1 digit

parseFloat :: Parser LispVal
parseFloat = do
  beforeDot' <- many digit
  let beforeDot = if null beforeDot' then "0" else beforeDot'
  char '.'
  afterDot <- (if null beforeDot then many1 else many) digit
  let a = readMaybe $ beforeDot ++ "." ++ afterDot
  maybe (fail "no matching floating number") (return . FloatNumber) a

testFloat = parse parseFloat "error" "."

-- >>> testFloat
-- Left "error" (line 1, column 2):
-- unexpected end of input
-- expecting digit
-- no matching floating number

lowerCaseCharacter :: [Parser String]
lowerCaseCharacter = string' . (: []) <$> ['a' .. 'z']

upperCaseCharacter :: [Parser String]
upperCaseCharacter = string' . (: []) <$> ['A' .. 'Z']

digitCharacter :: Parser String
digitCharacter = (: []) <$> digit

parseCharacter :: Parser LispVal
parseCharacter = do
  string "#\\"
  ch <- choice $ [string' "newline", string' "space", string' " ", digitCharacter] ++ lowerCaseCharacter ++ upperCaseCharacter
  return $ Character $ case ch of
    "newline" -> '\n'
    "space" -> ' '
    chars -> head chars

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

testCharacter = parse (many parseCharacter) "error" "#\\newline#\\5"

-- >>> testCharacter
-- Right [Character '\n',Character '5']

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseCharacter
    <|> parseString
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x
    -- <|> parseFloat -- TODO fix
    <|> parseNumber

-- spaces :: Parser ()
-- spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

test = readExpr "(dotted a . list)"

-- >>> test
-- "No match: \"lisp\" (line 1, column 12):\nunexpected \" \"\nexpecting digit\nno matching floating number"

main :: IO ()
-- main = putStrLn $ readExpr "\n \n123"
main = do
  args <- getArgs
  putStrLn
    $ if null args
      then "no args"
      else readExpr (head args)

-- TODO Измените parseNumber для поддержки стандарта Scheme для разных оснований. Вы, возможно, найдёте readOct и readHex функции полезными.
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4

-- TODO https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
-- TODO https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1
