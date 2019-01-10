-- CMPS 253, Fall 2017
-- Sara Sholes, 1153681
-- Final Project

module Main where


import Language.Haskell.Exts.Simple
import Data.Typeable
import Data.List

import Datatypes
import Unification
import Preprocess
import TypeEval
import Eval
import Printing


getDeclType :: Decl -> IO()
getDeclType decl = 
    case decl of
        SpliceDecl expr -> 
            let origContext = initalizeContext () in 
            let desugaredExpr = (desugar expr) in
            let (programType, context) = (getType desugaredExpr origContext) in 
            let (retValue) = (eval desugaredExpr) in
            case context of
                (GlobalContext usedVarNames contraints) ->
                    let unifiedConsts = (unify contraints) in
                        case unifiedConsts of
                            UnificationErr errMsg -> putStrLn ( "ERROR: Unification Error: " ++ errMsg)
                            TypeVarAssignment typeVarAssign -> 
                                let returnType = (getReturnType typeVarAssign programType) in
                                    putStrLn ( "Evaluates to:\n\t" ++ show returnType ++ "\nWith return val: " ++ show retValue)
                (ErrorContext errMsg) -> putStrLn ("Has ERROR:\n\t " ++ errMsg)
        otherwise -> putStrLn "ERROR: Unrecognized declaration type"                   

getDeclTypes :: [Decl] -> IO()        
getDeclTypes declList =
    case declList of
        [] -> putStrLn " "
        decl:tl -> do
            putStrLn ("\nOriginal Statement:\n\t" ++ (printOrigDecl decl))
            getDeclType decl
            getDeclTypes tl

        

evalFile testfile = do
    x <- readFile testfile
    case (parseFileContents x) of
        ParseFailed srcLoc errStr -> putStrLn ("ERROR: Invalid parsing at " ++ show srcLoc)
        ParseOk loadedMod -> 
            case loadedMod of
                Module Nothing pragmas importDecls declList -> (getDeclTypes declList)
                otherwise -> putStrLn ("ERROR: Unrecognized module type" ++ show loadedMod)

main = putStrLn("Run evalFile {testfile}\n")
--main = parseme 
 