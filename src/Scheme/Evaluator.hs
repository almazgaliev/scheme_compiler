module Scheme.Evaluator (eval) where

import Scheme.Parser (LispVal (..))

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(IntegerNumber _) = val
eval val@(FloatNumber _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

-- numericBinop (+) :: [LispVal] -> LispVal
-- numericBinop :: (a -> a -> a) -> [LispVal] -> LispVal
numericBinop :: (Num a) => (a -> a -> a) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- либо все числа сделать Number
-- либо обрабатывать как то IntegerNumber и FloatNumber отдельно