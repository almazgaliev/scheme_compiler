module Scheme.LispVal where

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