module Main where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (liftM)
import Control.Monad.Except (MonadError (throwError), catchError)
import Scheme.Errors (LispError (Parser), ThrowsError, extractValue, trapError)
import Scheme.Evaluator (eval)
import Scheme.LispVal (LispVal, PrettyPrint (PrettyPrint))
import Scheme.Parser (parseExpr)

import System.Environment (getArgs)
import Text.Parsec (ParseError, parse)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr' :: String -> Either ParseError LispVal
readExpr' = parse parseExpr "lisp"

main :: IO ()
main = do
  args <- getArgs
  putStrLn $
    if null args
      then "No arguments provided"
      else
        let evaled = show . PrettyPrint <$> (eval =<< readExpr (head args))
         in extractValue $ trapError evaled

-- TODO Измените parseNumber для поддержки стандарта Scheme для разных оснований. Вы, возможно, найдёте readOct и readHex функции полезными.
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
-- TODO https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
-- TODO https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1

symbolTests =
  [ ("(symbol? 'foo)", "#t")
  , ("(symbol? (car '(a b)))", "#t")
  , ("(symbol? \"bar\")", "#f")
  , ("(symbol? 'nil)", "#t")
  , ("(symbol? '())", "#f")
  , ("(symbol? #f)", "#f")
  ]

symbolTestResults = map (\(x, y) -> eval <$> readExpr' x) symbolTests

-- >>> symbolTestResults
-- [Right (Bool False),Right (Bool False),Right (Bool False),Right (Bool False),Right (Bool False),Right (Bool False)]

-- >>> readExpr' "('a)"
-- Right (List [List [Atom "quote",Atom "a"]])
