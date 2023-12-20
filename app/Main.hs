module Main where

import Scheme.Evaluator (eval)
import Scheme.Parser (LispVal, parseExpr)
import System.Environment ( getArgs )
import Text.Parsec ( parse, ParseError )


-- spaces :: Parser ()
-- spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

readExpr' :: String -> Either ParseError LispVal
readExpr' = parse parseExpr "lisp"

test = readExpr "(a a . 2)"

main :: IO ()
main = do
  args <- getArgs
  print
    $ if null args
      then "no args"
      else show $ eval <$> readExpr' (head args)

-- TODO Измените parseNumber для поддержки стандарта Scheme для разных оснований. Вы, возможно, найдёте readOct и readHex функции полезными.
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4

-- TODO https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
-- TODO https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1
