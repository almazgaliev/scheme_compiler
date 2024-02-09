module Scheme.Evaluator (eval) where

import Control.Monad.Except (MonadError (throwError))
import Debug.Trace
import Scheme.Errors (LispError (..), ThrowsError)
import Scheme.Parser (LispVal (..))

-- = Atom String
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(IntegerNumber _) = return val
eval val@(FloatNumber _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = do
  args' <- mapM eval args
  apply func args'
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  case lookup func primitives of
    Just func' -> func' args
    Nothing -> throwError $ NotFunction "not found function" func

-- TODO implement ADL

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", floatBinop (+))
  , ("-", floatBinop (-))
  , ("*", floatBinop (*))
  , ("/", integerBinop div)
  , ("mod", integerBinop mod)
  , ("quotient", integerBinop quot)
  , ("remainder", integerBinop rem) -- TODO float number operators
  , ("not", notOp)
  , ("boolean?", checkBool)
  , ("string?", checkString)
  , ("list?", checkList)
  , ("number?", checkNumber)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  -- ("symbol?", checkSymbol) -- TODO implement
  -- , ("car", carOp)
  -- , ("cdr", cdrOp)
  ]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [left', right'] = do
  left <- unpacker left'
  right <- unpacker right'
  return $ Bool $ left `op` right
boolBinop unpacker _ args = throwError $ NumArgs 2 args

numBoolBinop :: (Float -> Float -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackFloatNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackString

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString (FloatNumber s) = return $ show s
unpackString (IntegerNumber s) = return $ show (fromIntegral s :: Float)
unpackString (Bool s) = return $ show s
unpackString val = throwError $ TypeMismatch "String" val

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool v) = return v
unpackBool val = throwError $ TypeMismatch "Bool" val

notOp :: [LispVal] -> ThrowsError LispVal
notOp [Bool False] = return $ Bool True
notOp [Bool True] = return $ Bool False
notOp [_] = return $ Bool False
notOp a = throwError $ NumArgs 1 a

checkBool :: [LispVal] -> ThrowsError LispVal
checkBool [Bool _] = return $ Bool True
checkBool [_] = return $ Bool False
checkBool a = throwError $ NumArgs 1 a

checkString :: [LispVal] -> ThrowsError LispVal
checkString [String _] = return $ Bool True
checkString [_] = return $ Bool False
checkString a = throwError $ NumArgs 1 a

checkList :: [LispVal] -> ThrowsError LispVal
checkList [List _] = return $ Bool True
checkList [_] = return $ Bool False
checkList a = throwError $ NumArgs 1 a

-- checkSymbol :: [LispVal] -> ThrowsError LispVal
-- checkSymbol [Atom _] = return $ Bool True
-- checkSymbol [_] = return $ Bool False
-- checkSymbol a = throwError $ NumArgs 1 a

checkNumber :: [LispVal] -> ThrowsError LispVal
checkNumber [IntegerNumber _] = return $ Bool True
checkNumber [FloatNumber _] = return $ Bool True
checkNumber [_] = return $ Bool False
checkNumber a = throwError $ NumArgs 1 a

checkSymbol :: [LispVal] -> LispVal
checkSymbol = undefined

carOp :: [LispVal] -> LispVal
carOp xs
  | not (null xs) = head xs
  | otherwise = error "pair required, but got ()"

cdrOp :: [LispVal] -> LispVal
cdrOp xs
  | not (null xs) = List $ tail xs
  | otherwise = error "pair required, but got ()"

-- TODO https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3 untill 6.5

floatBinop :: (Float -> Float -> Float) -> [LispVal] -> ThrowsError LispVal
floatBinop op val@[] = throwError $ NumArgs 2 val
floatBinop op val@[_] = throwError $ NumArgs 2 val
floatBinop op params =
  do
    floatParams <- mapM unpackFloatNum params
    let value = foldl1 op floatParams
    return $ FloatNumber value

integerBinop op val@[] = throwError $ NumArgs 2 val
integerBinop op val@[_] = throwError $ NumArgs 2 val
integerBinop op params = do
  intParams <- mapM unpackIntNum params
  let value = foldl1 op intParams
  return $ IntegerNumber value

unpackFloatNum :: LispVal -> ThrowsError Float
unpackFloatNum (IntegerNumber val) = return $ fromIntegral val
unpackFloatNum (FloatNumber val) = return val
unpackFloatNum val@(String n) =
  let parsed = reads n :: [(Float, String)]
   in if null parsed
        then throwError $ TypeMismatch "Float" val
        else return . fst $ head parsed
unpackFloatNum (List [n]) = unpackFloatNum n
unpackFloatNum val = throwError $ TypeMismatch "Float" val

-- TODO remove
unpackIntNum :: LispVal -> ThrowsError Integer
unpackIntNum (IntegerNumber val) = return $ fromIntegral val
unpackIntNum (FloatNumber val) = return $ floor val
unpackIntNum val@(String n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then throwError $ TypeMismatch "Int" val
        else return . fst $ head parsed
unpackIntNum (List [n]) = unpackIntNum n
unpackIntNum val = throwError $ TypeMismatch "Float" val

numericCast :: LispVal -> LispVal
numericCast (IntegerNumber num) = FloatNumber (fromIntegral num)
numericCast a = a
