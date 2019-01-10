-- CMPS 253, Fall 2017
-- Sara Sholes, 1153681
-- Final Project

module Datatypes where

import Printing

import Language.Haskell.Exts.Simple
   
    
-- Types an expression can be --
    
data BaseTypes = IntType
    | BoolType
    | StringType
     deriving (Eq)
      
data AllTypes = Base BaseTypes
    | Error
    | TypeVar Int
    | ParamType Int
    | ArrowType AllTypes AllTypes
    | ListType AllTypes
      deriving (Eq)
      
instance Show AllTypes where 
    show allType =
        case allType of
            Base concreteType ->
                case concreteType of
                    IntType -> "Int"
                    BoolType -> "Bool"
                    StringType -> "String"
            ArrowType inType outType ->
                (show inType) ++ " -> " ++ (show outType)
            ListType elemType ->
                "[" ++ (show elemType) ++ "]"
            ParamType idNum ->
                "T_" ++ (show idNum)
            TypeVar idNum ->
                "X_" ++ (show idNum)
            Error ->
                "*ERROR*"      
      

-- Values that an expression can evaluate to --       

data Value = IntVal Integer
    | StringVal String
    | BoolVal Bool
    | ListVal [Value]
    | LambdaVal String Exp
    | ErrValue String

instance Show Value where 
    show value =
        case value of
            IntVal intVal -> show intVal
            StringVal stringVal -> "\"" ++ stringVal ++ "\""
            BoolVal boolVal -> show boolVal
            ListVal val -> "[" ++ (listPrint (ListVal val)) ++ "]"
            LambdaVal inputName body -> "\\" ++ inputName ++ " -> " ++ (printOrigExpr body)
            ErrValue msg -> "*ERROR: " ++ msg
           
listPrint :: Value -> String 
listPrint val = 
    case val of 
        ListVal [] -> ""
        ListVal (hVal:[ListVal []]) -> (show hVal)
        ListVal (hVal:[tVal]) -> (show hVal) ++ ", " ++ (listPrint tVal)
        otherVal -> (show otherVal)

      
-- Defining Context for Type Evaluation --
      
data GlobalContext = 
    GlobalContext [AllTypes] [(AllTypes, AllTypes)]
    | ErrorContext String
     deriving (Show)

data OverallContext = 
    OverallContext [(Name, AllTypes)] GlobalContext  -- Note change Name to a string
     deriving (Show)
     
 

-- Unification Result --  
     
data UnifyResult = 
    TypeVarAssignment [(AllTypes, AllTypes)]
    | UnificationErr String
     deriving (Show)
