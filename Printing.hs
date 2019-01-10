-- CMPS 253, Fall 2017
-- Sara Sholes, 1153681
-- Final Project

module Printing where

import Language.Haskell.Exts.Simple
import Data.List

printOrigDecl :: Decl -> String
printOrigDecl decl =
    case decl of
        SpliceDecl expr -> printOrigExpr expr
        otherwise -> show decl

printOrigExpr :: Exp -> String
printOrigExpr expr =
    case expr of
        Lit literal -> 
            case literal of
                Int val -> show val
                String val -> show val
                otherwise -> "?_Lit_?"
        Con (UnQual (Ident conStr)) -> conStr 
        Var (UnQual (Ident varNam)) -> varNam
        List exprList -> "[" ++ (printOrigList exprList) ++ "]"
        InfixApp expr1 op expr2 -> 
            case op of 
                QVarOp (UnQual (Symbol opSym)) ->
                    (printOrigExpr expr1) ++ " " ++ opSym ++ " " ++ (printOrigExpr expr2)
                QConOp (Special Cons) ->
                    (printOrigExpr expr1) ++ ":" ++ (printOrigExpr expr2)
        If expr1 expr2 expr3 -> 
            "if " ++ (printOrigExpr expr1) ++ " then " ++ (printOrigExpr expr2) ++ " else " ++ (printOrigExpr expr3)
        Let (BDecls [bind]) expr1 -> 
            "let " ++ (printBinds bind) ++ " in " ++ (printOrigExpr expr1)
        Lambda patts body -> 
            ("\\" ++ (printOrigPatts patts) ++ " -> " ++ (printOrigExpr body))
        App lambda input -> 
            (printOrigExpr lambda) ++ " " ++ (printOrigExpr input) 
        Case expr1 altList -> 
            "\n\t\tcase " ++ (printOrigExpr expr1) ++ " of " ++ printOrigAlts altList
        Paren expr1 -> "(" ++ printOrigExpr expr1 ++ ")"
        otherwise -> "?_" ++ show expr ++ "_?"
 
printOrigList :: [Exp] -> String 
printOrigList exprList =
    case exprList of
        expr:[] -> (printOrigExpr expr)
        expr:tail -> (printOrigExpr expr) ++ ", " ++ (printOrigList tail)
        [] -> ""

printBinds :: Decl -> String        
printBinds bind =
    case bind of
        FunBind [Match (Ident funId) paramPatts (UnGuardedRhs funBody) Nothing] -> 
            funId ++ " " ++ (printOrigPatts paramPatts) ++ " = " ++ (printOrigExpr funBody)
        PatBind (PVar (Ident id)) (UnGuardedRhs rhs) Nothing -> id ++ " = " ++ (printOrigExpr rhs)
 
printOrigPatts :: [Pat] -> String 
printOrigPatts patts = 
    case patts of 
        patt:[] -> printOrigPatt patt
        patt:tail -> printOrigPatt patt ++ ", " ++ (printOrigPatts tail)
        [] -> ""

printOrigPatt :: Pat -> String        
printOrigPatt patt =
    case patt of
        PVar (Ident varName) -> varName
        PLit sign literal -> 
            case literal of
                Int val -> show val
                String val -> show val
                otherwise -> "Lit?"
        PList plist -> "[" ++ (printOrigPatts plist) ++ "]"
        PInfixApp p1 (Special Cons) p2 -> (printOrigPatt p1) ++ ":" ++ (printOrigPatt p2)
        PWildCard -> "_"

printOrigAlts :: [Alt] -> String        
printOrigAlts atlList =
    case atlList of
        [] -> ""
        alt:tail -> (printOrigAlt alt) ++ (printOrigAlts tail)

printOrigAlt :: Alt -> String        
printOrigAlt alt =
    let (Alt patt (UnGuardedRhs rhs) (Nothing)) = alt in
        "\n\t\t\t" ++ (printOrigPatt patt) ++ " -> " ++ (printOrigExpr rhs)
        

 