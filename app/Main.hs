module Main where

import Control.Monad (when)
import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.ReadPrec (reset)
import Data.Maybe (isJust, fromMaybe)
import Data.Either (fromRight)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving Show

-- parseString = char '"' >> (many (noneOf "\"") >>= \ x -> char '"' >> return (String x))
parseString' :: Parser LispVal
parseString' = do
  char '"'
  x <- many stringChar
  char '"'
  return $ String x


stringChar :: Parser Char
stringChar = noneOf "\""

-- backslashEscaped :: Parser String
-- backslashEscaped = string "\\\\" -- \\

-- dqEscaped :: Parser String
-- dqEscaped = string "\\\"" -- \"

-- nEscaped :: Parser String
-- nEscaped = string "\\n" -- \n

-- rEscaped :: Parser String
-- rEscaped = string "\\r" -- \r

-- tabEscaped :: Parser String
-- tabEscaped = string "\\t" -- \t

anyEscaped :: Parser Char
anyEscaped = do
  char '\\'
  char <- choice [char 't', char 'n', char '\\', char '"']
  case char of
    't' ->  return '\t'
    'n' ->  return '\n'
    '\\' -> return '\\'
    '"' ->  return '"'


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

-- TODO Измените parseNumber для поддержки стандарта Scheme для разных оснований. Вы, возможно, найдёте readOct и readHex функции полезными.
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
parseNumber :: Parser LispVal
-- parseNumber = do
--   s <- many1 digit
--   let result = read s
--   return $ Number result
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

-- spaces :: Parser ()
-- spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
-- main = putStrLn $ readExpr "\n \n123"
main = do
  args <- getArgs
  putStrLn
    $ if null args
      then "no args"
      else readExpr (head args)
