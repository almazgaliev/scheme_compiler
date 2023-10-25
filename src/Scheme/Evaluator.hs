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
  , ("remainder", numericBinop rem) -- TODO float number operators
  ]

-- numericBinop (+) :: [LispVal] -> LispVal
-- numericBinop :: (a -> a -> a) -> [LispVal] -> LispVal
numericBinop op params = IntegerNumber $ value
 where
  floatParams = map unpackNum params
  value = foldl1 op floatParams

unpackNum (IntegerNumber val) = fromIntegral val
unpackNum (FloatNumber val) = floor val
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then 0
        else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

unpackFloatNum :: LispVal -> Float
unpackFloatNum (FloatNumber val) = val
unpackFloatNum (IntegerNumber val) = fromIntegral val
unpackFloatNum (String n) =
  let parsed = reads n :: [(Float, String)]
   in if null parsed
        then 0
        else fst $ head parsed
unpackFloatNum (List [n]) = unpackFloatNum n
unpackFloatNum _ = 0

numericCast :: LispVal -> LispVal
numericCast (IntegerNumber num) = FloatNumber (fromIntegral num)
numericCast a = a
