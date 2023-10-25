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
  , ("not", notOp)
  , ("boolean?", checkBool)
  , ("string?", checkString)
  -- , ("car", carOp) -- TODO fix
  -- , ("cdr", cdrOp) -- TODO fix
  -- , ("symbol?", checkSymbol) -- TODO implement
  -- , ("number?", checkNumber) -- TODO implement
  ]

notOp :: [LispVal] -> LispVal
notOp [Bool False] = Bool True
notOp [Bool True] = Bool False
notOp [_] = Bool False
notOp a = error $ "wrong number of arguments: not requires 1, but got " ++ show (length a)

checkBool :: [LispVal] -> LispVal
checkBool [Bool _] = Bool True
checkBool [_] = Bool False
checkBool a = error $ "wrong number of arguments for #<subr (boolean? obj)> (required 1, got " ++ show (length a) ++ ")"

checkString :: [LispVal] -> LispVal
checkString [String _] = Bool True
checkString [_] = Bool False
checkString a = error $ " wrong number of arguments: string? requires 1, but got " ++ show (length a)

-- checkSymbol :: [LispVal] -> LispVal
-- checkSymbol [Atom _] = Bool True
-- checkSymbol [_] = Bool False
-- checkSymbol a = error $ "wrong number of arguments for #<subr (boolean? obj)> (required 1, got " ++ show (length a) ++ ")"

carOp :: [LispVal] -> LispVal
carOp xs
  | not (null xs) = head xs
  | otherwise = error "pair required, but got ()"

cdrOp :: [LispVal] -> LispVal
cdrOp xs
  | not (null xs) = List $ tail xs
  | otherwise = error "pair required, but got ()"

-- TODO https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3 untill 6.5

-- numericBinop (+) :: [LispVal] -> LispVal
-- numericBinop :: (a -> a -> a) -> [LispVal] -> LispVal
numericBinop op params = IntegerNumber value
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
