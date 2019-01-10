-- CMPS 253, Fall 2017
-- Sara Sholes, 1153681
-- Final Project

module Eval where

import Language.Haskell.Exts.Simple
import Data.List

import Preprocess
import Datatypes


eval :: Exp -> Value 
eval expr  = 
    case expr of
        Lit literal -> 
            case literal of
                Int val -> IntVal val
                String val -> StringVal val
                otherwise -> (ErrValue "Bad literal type")
        Con (UnQual (Ident boolStr)) -> 
            case boolStr of
                "True" -> BoolVal True
                otherwise -> BoolVal False
        Var (UnQual (Ident varName)) -> (ErrValue ("Variable " ++ varName ++ " not recognized."))
        List exprList -> evList exprList
        InfixApp expr1 op expr2 -> eInfixOpType expr1 op expr2
        If expr1 expr2 expr3 -> eIf expr1 expr2 expr3
        Let (BDecls [bind]) expr1 -> eval expr1 --eLet bind expr1
        Lambda [patt] body -> eLambda patt body
        App lambda input -> eApp lambda input
        Case expr1 altlist -> (eCaselist expr1 altlist)
        Paren expr1 -> eval expr1
        otherwise -> (ErrValue ("Expression " ++ (show expr) ++ "Not handled") )      
        

eLambda :: Pat -> Exp -> Value
eLambda patt body = 
    let (PVar (Ident id)) = patt in
        LambdaVal id body
                       
eApp :: Exp -> Exp -> Value
eApp lambda input = 
    let vLambda = (eval lambda) in
    case vLambda of
        (ErrValue msg) -> (ErrValue msg)
        (LambdaVal id body) ->
            let lambda_eval = (subExpr id input body) in
                (eval lambda_eval)
        otherwise ->  ErrValue ("lambda is: " ++ (show lambda))



eIf :: Exp -> Exp -> Exp -> Value 
eIf expr1 expr2 expr3 = 
    let condition = (eval expr1) in
    case condition of
        (ErrValue msg) -> (ErrValue msg)
        (BoolVal True) -> (eval expr2)
        otherwise -> (eval expr3)


eInfixOpType :: Exp -> QOp -> Exp -> Value   
eInfixOpType expr1 op expr2 =
    let val1 = (eval expr1) in
    let val2 = (eval expr2) in
    case op of
        QVarOp (UnQual opp) ->
            let val2 = (eval expr2) in
            case (val1, val2) of
                (ErrValue msg, _) -> ErrValue msg
                (_, ErrValue msg) -> ErrValue msg
                (IntVal v1, IntVal v2) ->
                    case opp of
                        Symbol "+" -> IntVal (v1 + v2) 
                        Symbol "-" -> IntVal (v1 - v2) 
                        Symbol "*" -> IntVal (v1 * v2) 
                        Symbol "<" -> BoolVal (v1 < v2) 
                        Symbol ">" -> BoolVal (v1 > v2)
                        Symbol "==" -> BoolVal (v1 == v2)
                        otherwise -> (ErrValue "Unrecognized binary op")                        
                (BoolVal v1, BoolVal v2) ->
                    case opp of
                        Symbol "||" -> BoolVal (v1 || v2)
                        Symbol "&&" -> BoolVal (v1 && v2)
                        Symbol "<" -> BoolVal (v1 < v2) 
                        Symbol ">" -> BoolVal (v1 > v2)
                        Symbol "==" -> BoolVal (v1 == v2)
                        otherwise -> (ErrValue "Unrecognized binary op")
                (StringVal v1, StringVal v2) ->
                    case opp of
                        Symbol "++" -> StringVal (v1 ++ v2)
                        Symbol "<" -> BoolVal (v1 < v2) 
                        Symbol ">" -> BoolVal (v1 > v2)
                        Symbol "==" -> BoolVal (v1 == v2)
                        otherwise -> (ErrValue "Unrecognized binary op")
                otherwise -> (ErrValue ("Unrecognized binary op " ++ show opp))
        QConOp (Special Cons) ->
            ListVal (val1:[val2])
        otherwise -> (ErrValue "eInfixOpType failed")
 
evList :: [Exp] -> Value
evList exprList = 
    case exprList of
        [] -> ListVal []
        h:tl -> let hVal = (eval h) in 
            case hVal of
                ErrValue msg -> ErrValue msg 
                otherwise -> ListVal (hVal:[(evList tl)])

   
-- Case Evaluation --
   
eCaselist :: Exp -> [Alt] -> Value
eCaselist expr altList = 
    case altList of -- For first match found, return results
        [alt] -> 
            let altRes = (eCase expr alt) in
            case altRes of
                Just val -> val
                otherwise -> ErrValue "unmatched case"
        alt:tl -> -- if not keep on going...
            let altRes = (eCase expr alt) in
            case altRes of
                Just val -> val
                otherwise -> eCaselist expr tl


eCase :: Exp -> Alt -> Maybe Value
eCase ctrlExp alt =  
    let (Alt pat (UnGuardedRhs rhs) (Nothing)) = alt in
    let bindedRhs = (ePat pat ctrlExp rhs) in
        case bindedRhs of
            Nothing -> Nothing
            Just expr -> Just (eval expr)
 
ePat :: Pat -> Exp -> Exp -> Maybe Exp  
ePat patt ctrlExp rhs =
    case patt of
        (PList []) -> 
            case ctrlExp of
                List [] -> Just rhs
                otherwise -> Nothing
        (PList [patt1]) -> 
            case ctrlExp of
                List [val] -> (ePat patt1 val rhs)
                otherwise -> Nothing
        (PList pattList) -> (ePatList pattList ctrlExp rhs)
        (PInfixApp patt1 (Special Cons) patt2) ->
            case ctrlExp of
                (InfixApp e1 (QConOp (Special Cons)) e2) ->
                    let e1Binded = (ePat patt1 e1 rhs) in
                    case e1Binded of
                        Just e1New -> (ePat patt2 e2 e1New)
                        otherwise -> Nothing 
                (List (e1:e_tail)) ->
                    let e1Binded = (ePat patt1 e1 rhs) in
                    case e1Binded of
                        Just e1New -> (ePat patt2 (List e_tail) e1New)
                        otherwise -> Nothing 
                otherwise -> Nothing
        (PLit sign literal) ->  
            let evaledCtrl = (eval ctrlExp) in 
            case (literal, evaledCtrl) of
                (Int v1, IntVal v2) ->  if (v1 == v2) then Just rhs else Nothing
                (String v1, StringVal v2) ->  if (v1 == v2) then Just rhs else Nothing
                otherwise -> Nothing
        (PVar (Ident id)) -> Just (subExpr id ctrlExp rhs) 
        PWildCard -> Just rhs
        otherwise -> Nothing

        
     
ePatList :: [Pat] -> Exp -> Exp -> Maybe Exp 
ePatList pattList ctrlExp rhs = 
    case pattList of
        [hd] -> 
            case ctrlExp of
                List [val] -> (ePat hd val rhs)
                otherwise -> Nothing
        hd:tl -> 
            case ctrlExp of
                (InfixApp e1 (QConOp (Special Cons)) e2) ->
                    let e1Binded = (ePat hd e1 rhs) in
                    case e1Binded of
                        Just e1New -> (ePat (PList tl) e2 e1New)
                        otherwise -> Nothing 
                (List (expr:expr_tl)) ->
                    let e1Binded = (ePat hd expr rhs) in
                    case e1Binded of
                        Just e1New -> (ePat (PList tl) (List expr_tl) e1New)
                        otherwise -> Nothing 
                otherwise -> Nothing
        
 


 