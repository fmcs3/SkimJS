module Value (Value (..), getIds, getStatements, getList) where
import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Function Id [Id] [Statement]
    | Break
    | Nil
    | List [Value] deriving (Eq)
    
   

--
-- Pretty Printer
--
instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show (Break) = "Break"
  show Nil = "undefined"
  show (Function (Id id) args blockFunction) = "function: " ++ id
  show (List list) = show list
  


  
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)

--
-- Funções para ajuda
--
getIds :: Value -> [Id]
getIds (Function id ids functionBlock) = ids


getStatements :: Value -> [Statement]
getStatements (Function id ids functionBlock) = functionBlock

getList :: Value -> [Value]
getList (List list) = list