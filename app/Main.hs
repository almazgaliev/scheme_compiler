module Main where

import Text.Parsec
import Scheme.Parser (parseExpr)
import System.Environment

-- spaces :: Parser ()
-- spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

test = readExpr "(a a . 2)"

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
