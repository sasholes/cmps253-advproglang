-- CMPS 253, Fall 2017
-- Sara Sholes, 1153681
-- Final Project

module Preprocess where

import Language.Haskell.Exts.Simple
import Data.List

import Datatypes

desugar :: Exp -> Exp 
desugar expr =
    case expr of     
        List exprList -> List (desugarList exprList)
        InfixApp expr1 op expr2 -> InfixApp (desugar expr1) op (desugar expr2)
        If expr1 expr2 expr3 -> If (desugar expr1) (desugar expr2) (desugar expr3)
        Let (BDecls binds) expr1 -> let newBinds = (letFunsToLambdas binds) in
            (Let (BDecls newBinds) (subBinds newBinds (desugar expr1)))
        Lambda patts body -> curryLambdas patts (desugar body)
        App lambda input -> App (desugar lambda) (desugar input)
        Case expr1 altlist -> desugarCaselist (desugar expr1) altlist
        Paren expr1 -> desugar expr1 
        otherwise -> expr  

-- List: Desugar list element by element       
desugarList :: [Exp] -> [Exp]
desugarList exprList = 
    case exprList of
        [] -> []
        expr:tail -> (desugar expr):(desugarList tail)
  

-- Case Statement: Desugar case altnative by case alternative
desugarCaselist :: Exp -> [Alt] -> Exp
desugarCaselist ctrlExpr altList = 
    let newCtrlExpr = desugar ctrlExpr in
    let newAltlist = desugarCases altList in
    Case newCtrlExpr newAltlist 

desugarCases :: [Alt] -> [Alt]
desugarCases altList = 
    case altList of
        [] -> []
        hd:tail -> (desugarCase hd):(desugarCases tail)
        
desugarCase :: Alt -> Alt  
desugarCase alt =
    let (Alt pat (UnGuardedRhs rhs) someval) = alt in
        (Alt pat (UnGuardedRhs (desugar rhs)) someval)
        
        
-- Lambdas: Ensure all lambdas take a single input    
curryLambdas :: [Pat] -> Exp -> Exp
curryLambdas patts body = 
    case patts of
        patt:[] -> (Lambda [patt] body)
        patt:other_patts -> -- Want to nest the lambdas
            Lambda [patt] (curryLambdas other_patts body)
          
-- Convert function let statements to let- lambda statements
-- Ex. 'let foo x -> body...' becomes 'let foo = \x -> body ..'            
letFunsToLambdas :: [Decl] -> [Decl]     
letFunsToLambdas binds = case binds of
    [] -> []
    bind:tail -> (letFunToLambda bind):(letFunsToLambdas tail)

letFunToLambda :: Decl -> Decl
letFunToLambda bind  = 
    case bind of
        FunBind [matching] ->
            case matching of    
                Match funId paramPatts (UnGuardedRhs funBody) Nothing ->
                    let rhs = Lambda paramPatts (desugar funBody) in
                        (PatBind (PVar funId) (UnGuardedRhs (desugar rhs)) Nothing)
        PatBind (PVar (Ident id)) (UnGuardedRhs rhs) Nothing -> (PatBind (PVar (Ident id)) (UnGuardedRhs (desugar rhs)) Nothing)
        otherwise -> bind 


-- Substituting in the actual expression               
subBinds :: [Decl] -> Exp -> Exp      
subBinds binds expr = case binds of
    [bind] -> subBind bind expr
    bind:tail -> subBinds tail (subBind bind expr)  --print "other-----"
    otherwise -> expr
 
subBind :: Decl -> Exp -> Exp
subBind bind expr = 
    case bind of
        PatBind (PVar (Ident id)) (UnGuardedRhs rhs) Nothing -> subExpr id rhs expr
        otherwise -> expr
    
subExpr :: String -> Exp -> Exp -> Exp       
subExpr id idBind expr =
    case expr of
        Lit literal -> expr
        Var (UnQual (Ident exprId)) -> 
            if (id == exprId)
            then idBind
            else expr
        Con stuff -> expr
        List exprList -> List (subExprList id idBind exprList)
        InfixApp expr1 op expr2 -> InfixApp (subExpr id idBind expr1)  op (subExpr id idBind expr2)
        If expr1 expr2 expr3 -> If (subExpr id idBind expr1) 
                                    (subExpr id idBind expr2) 
                                    (subExpr id idBind expr3)
        Let (BDecls binds2) expr1 -> Let (BDecls (subExprBinds id idBind binds2)) (subExpr id idBind expr1)
        Lambda patts body -> Lambda patts (subExpr id idBind body)
        App lambda input -> App (subExpr id idBind lambda) (subExpr id idBind input)
        Case expr1 altList -> subExprCaselist id idBind expr1 altList
        Paren expr1 -> Paren (subExpr id idBind expr1)
        otherwise -> expr

-- List
subExprList :: String-> Exp -> [Exp] -> [Exp]
subExprList id idBind exprList =
    case exprList of
        [] -> []
        expr:tail -> (subExpr id idBind expr):(subExprList id idBind tail)

-- Case
subExprCaselist :: String -> Exp -> Exp -> [Alt] -> Exp
subExprCaselist id idBind ctrlExpr altList = 
    let newCtrlExpr = (subExpr id idBind ctrlExpr) in
    let newAltlist = subExprCases id idBind altList in
        Case newCtrlExpr newAltlist 

subExprCases :: String -> Exp -> [Alt] -> [Alt]
subExprCases id idBind altList = 
    case altList of
        [] -> []
        hd:tail -> (subExprCase id idBind hd):(subExprCases id idBind tail)

        
        
subExprCase :: String -> Exp -> Alt -> Alt  
subExprCase id idBind alt =
    let (Alt pat (UnGuardedRhs rhs) someval) = alt in
        (Alt pat (UnGuardedRhs (subExpr id idBind rhs)) someval)        
----  
        
subExprBinds :: String -> Exp -> [Decl] -> [Decl]       
subExprBinds id idBind binds2 = 
    case binds2 of
        [bind] ->
            case bind of
                PatBind (PVar (Ident id2)) (UnGuardedRhs rhs) Nothing -> 
                    [PatBind (PVar (Ident id2)) (UnGuardedRhs (subExpr id idBind rhs)) Nothing]
        bind:tail ->
            case bind of
                PatBind (PVar (Ident id2)) (UnGuardedRhs rhs) Nothing ->
                    PatBind (PVar (Ident id2)) (UnGuardedRhs (subExpr id idBind rhs)) Nothing:(subExprBinds id idBind tail)
        otherwise -> binds2        
