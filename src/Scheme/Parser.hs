module Scheme.Parser (parseExpr, LispVal (..), unwordsList, PrettyPrint (..)) where

import Control.Monad (when)
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Numeric (readFloat)
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
  deriving (Show, Eq)

-- >>> PrettyPrint $ List [List [String "a"], String "b"]
-- (("a") "b")

newtype PrettyPrint = PrettyPrint {getPrintable :: LispVal}

instance Show PrettyPrint where
  show = costyl . getPrintable
   where
    costyl (List xs) = "(" ++ unwords (map (show . PrettyPrint) xs) ++ ")"
    costyl x = showVal x

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (IntegerNumber contents) = show contents
showVal (FloatNumber contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character '\n') = "#\\n"
showVal (Character ch) = "#\\" ++ [ch]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

-- instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map (show . PrettyPrint)

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
parseNumber = try parseFloat <|> (IntegerNumber . read <$> many1 digit)

-- TODO implement s f d l e literals
parseFloat :: Parser LispVal
parseFloat = try $ do
  beforeDot <- many1 digit
  char '.'
  afterDot <- many1 digit
  let a = readMaybe $ beforeDot ++ "." ++ afterDot
  maybe (fail "no matching floating number") (return . FloatNumber) a

testFloat = parse parseFloat "error" "1.2"

-- >>> testFloat
-- Right 1.2

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
    chars -> head chars -- ???

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

testCharacter = parse (many parseCharacter) "error" "#\\newline#\\5"

-- >>> testCharacter
-- Right [#\n,#\5]

parseList :: Parser LispVal
-- parseList = List <$> sepBy parseExpr spaces
parseList = do
  result <- sepBy parseExpr spaces
  return $ List result

parseDottedList :: Parser LispVal
parseDottedList = do
  init <- endBy parseExpr spaces
  last <- char '.' >> spaces >> parseExpr
  return $ DottedList init last

parseListInBrackets :: Parser LispVal
parseListInBrackets = do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseCharacter
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> parseListInBrackets

-- >>> :t (<|>)
-- (<|>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
