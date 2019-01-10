-- CMPS 253, Fall 2017
-- Sara Sholes, 1153681
-- Final Project

module TypeEval where

import Language.Haskell.Exts.Simple
import Data.List

import Datatypes


-- Methods used within type evaluation
initalizeContext :: () -> OverallContext
initalizeContext () = 
    let trueBool = (Ident "True", Base BoolType) in
    let falseBool = (Ident "False", Base BoolType) in
    let env = [trueBool, falseBool] in
    OverallContext env (GlobalContext [] [])
    
    
genVarType :: GlobalContext -> (AllTypes, GlobalContext)
genVarType global_context =
    case global_context of
        (GlobalContext usedVarNames contraints) ->
            let returnFun = (\new -> (new, (GlobalContext (new:usedVarNames) contraints))) in 
                case usedVarNames of
                    [] ->  returnFun (TypeVar 0)
                    (TypeVar lastNew):other -> returnFun (TypeVar (lastNew + 1))
        (ErrorContext errMsg) -> (Error, ErrorContext errMsg)

addConstraint :: GlobalContext -> (AllTypes, AllTypes) -> GlobalContext 
addConstraint global_context rule = 
    case global_context of
        GlobalContext varTypeNames contraints -> GlobalContext varTypeNames (rule:contraints)  
        ErrorContext errMsg -> ErrorContext errMsg



getType :: Exp -> OverallContext -> (AllTypes, GlobalContext)  
getType expr context = let (OverallContext env global_context) = context in
    case global_context of
        ErrorContext errMsg -> (Error, ErrorContext errMsg)
        otherwise ->
            case expr of
                Lit literal -> 
                    (case literal of
                        Int val -> (Base IntType, global_context)
                        String val -> (Base StringType, global_context)
                        otherwise -> (Error, ErrorContext "Undefined literal type"))
                Con (UnQual (Ident boolStr)) -> (Base BoolType, global_context)
                Var varNam -> ctVar varNam context
                List exprList -> ctList exprList context
                InfixApp expr1 op expr2 -> ctInfixOpType expr1 op expr2 context
                If expr1 expr2 expr3 -> tIf expr1 expr2 expr3 context
                Let (BDecls binds) expr1 -> ctLet binds expr1 context 
                Lambda [patts] body -> tLambda (Lambda [patts] body) context
                App lambda input -> tApp lambda input context
                Case expr1 altlist -> (ctCaselist expr1 altlist context)
                Paren expr1 -> getType expr1 context
                otherwise -> (Error, ErrorContext "Parsing error, term not recognized")        
        
-- Based on textbook's CT-ABSINF  
tLambda :: Exp -> OverallContext -> (AllTypes, GlobalContext) 
tLambda pattBody context = 
    case pattBody of 
        (Lambda (patt:[]) body) ->
            let (PVar id) = patt in 
            let (OverallContext env global_context) = context in 
            let (inTypeVar, new_global_context) = (genVarType global_context) in
            let (bodyType, bodyContext) = (getType body (OverallContext ((id, inTypeVar):env) new_global_context)) in
                ((ArrowType inTypeVar bodyType), bodyContext)

              
tApp :: Exp -> Exp -> OverallContext -> (AllTypes, GlobalContext) 
tApp lambda input context = 
    let (OverallContext env global_context) = context in
        let (lambdaType, global_context_2) = (getType lambda context) in
        let (inputType, global_context_3) = (getType input (OverallContext env global_context_2)) in
        let (resTypeVar, res_global_context) = (genVarType global_context_3) in
        let newrule = (lambdaType,(ArrowType inputType resTypeVar)) in
        let res_context = (addConstraint res_global_context newrule) in
            (resTypeVar, res_context)


tIf :: Exp -> Exp -> Exp -> OverallContext -> (AllTypes, GlobalContext) 
tIf expr1 expr2 expr3 context = 
    let (OverallContext env global_context) = context in
    let (condType, global_context_1) = (getType expr1 context) in
    let (iftrueType, global_context_2) = (getType expr2 (OverallContext env global_context_1)) in
    let (iffalseType, global_context_3) = (getType expr3 (OverallContext env global_context_2)) in
    let newrule1 = (condType, (Base BoolType)) in
    let newrule2 = (iftrueType, iffalseType) in
    let res_context = (addConstraint (addConstraint global_context_3 newrule1) newrule2) in
        (iftrueType, res_context)

ctLet :: [Decl] -> Exp -> OverallContext -> (AllTypes, GlobalContext)
ctLet binds expr context = 
    let (OverallContext env global_context) = context in
    let (_, overall_context_2) = (ctBinds binds context) in
        (getType expr overall_context_2)

ctBinds :: [Decl] -> OverallContext -> (AllTypes, OverallContext)   
ctBinds binds context = let (OverallContext env global_context) = context in
    case binds of
        [bind] -> ctBind bind context
        bind:tail -> 
            let (bType, overall_context_2) = (ctBind bind context) in 
                ctBinds tail overall_context_2  
        otherwise -> (Error, (OverallContext env (ErrorContext "Empty binding")))
                  
       
ctBind :: Decl -> OverallContext -> (AllTypes, OverallContext)   
ctBind bind context = let (OverallContext env global_context) = context in
    case bind of
        FunBind [matching] -> (Error, (OverallContext env  (ErrorContext "let function should be replaced")))
        PatBind (PVar ident) (UnGuardedRhs rhs) Nothing -> 
            let (rhsTypeVar, global_context_2) = genVarType global_context in -- Enables recursive functions to type check
            let (rhsType, global_context_3) = getType rhs (OverallContext ((ident, rhsTypeVar):env) global_context_2) in 
            let rule = (rhsTypeVar, rhsType) in
            let global_context_4 = (addConstraint global_context_3 rule) in
                (rhsType, (OverallContext ((ident, rhsType):env) global_context_4))
        otherwise -> (Error, (OverallContext env (ErrorContext ("Unrecognized binding: " ++ show bind)))) --print "otherOther..."


ctInfixOpType :: Exp -> QOp -> Exp -> OverallContext -> (AllTypes, GlobalContext)   
ctInfixOpType expr1 op expr2 context = 
    case op of
        QVarOp (UnQual opp) ->  
            case opp of
                Symbol "+" -> ctBoolArithExp expr1 expr2 (Base IntType) context
                Symbol "-" -> ctBoolArithExp expr1 expr2 (Base IntType) context
                Symbol "*" -> ctBoolArithExp expr1 expr2 (Base IntType) context
                Symbol "<" -> ctCmpExp expr1 expr2 context
                Symbol ">" -> ctCmpExp expr1 expr2 context
                Symbol "==" -> ctCmpExp expr1 expr2 context
                Symbol "||" -> ctBoolArithExp expr1 expr2 (Base BoolType) context
                Symbol "&&" -> ctBoolArithExp expr1 expr2 (Base BoolType) context
                Symbol "++" -> ctBoolArithExp expr1 expr2 (Base StringType) context
                otherwise -> (Error, ErrorContext ("Unrecognized binary op" ++ show opp)) 
        QConOp (Special Cons) -> ctConsExp expr1 expr2 context
        otherwise -> (Error, ErrorContext ("Unrecognized binary op" ++ show op)) 
    

ctVar :: QName -> OverallContext -> (AllTypes, GlobalContext)
ctVar xIn context =
    let (UnQual x) = xIn in
        case context of 
        (OverallContext env global_context) ->
            case (lookup x env) of
                Nothing -> (Error, ErrorContext ("Undefined var: " ++ show x)) -- Actually want to create type variable??
                Just t -> (t, global_context)
 

ctCmpExp :: Exp -> Exp -> OverallContext -> (AllTypes, GlobalContext) 
ctCmpExp in1 in2 context = let retType = (Base BoolType) in
    case context of
        (OverallContext env global_context) ->
            let (t1, global_context_1) = (getType in1 context) in
            let (t2, global_context_2) = (getType in2 (OverallContext env global_context_1))  
                in (retType, (addConstraint global_context_2 (t1, t2))) 
                
ctBoolArithExp :: Exp -> Exp -> AllTypes -> OverallContext -> (AllTypes, GlobalContext) 
ctBoolArithExp in1 in2 cType context = case context of
    (OverallContext env global_context) ->
        let (t1, global_context_1) = (getType in1 context) in
            let (t2, global_context_2) = (getType in2 (OverallContext env global_context_1)) 
            -- (getType in2 (OverallContext env (addConstraint global_context_1 (t1, cType))))  
                in (cType, (addConstraint (addConstraint global_context_2 (t1, cType)) (t2, cType)))
        
ctList :: [Exp] -> OverallContext -> (AllTypes, GlobalContext)        
ctList exprList context = let (OverallContext env global_context) = context in
    case exprList of
        [] -> 
            let (typeVar, global_context_1) = genVarType global_context in
                (ListType typeVar, global_context_1)
        [exp] -> ctConsExp exp (List []) context
            -- let (t1, global_context_1) = (getType exp context) in
                -- (ListType t1, global_context_1)
        exp:tail -> ctConsExp exp (List tail) context
            -- let (t1, global_context_1) = (getType exp context) in
            -- let (t2, global_context_2) = (ctList tail (OverallContext env global_context_1)) in
                -- (t2, (addConstraint global_context_2 (t2, ListType t1)))
 
 
ctConsExp :: Exp -> Exp -> OverallContext -> (AllTypes, GlobalContext)  
ctConsExp in1 in2 context = case context of
    (OverallContext env global_context) ->
        let (t1, global_context_1) = (getType in1 context) in
        let (t2, global_context_2) = (getType in2 (OverallContext env global_context_1)) in
            (t2, (addConstraint global_context_2 (t2, (ListType t1))))

ctCaselist :: Exp -> [Alt] -> OverallContext -> (AllTypes, GlobalContext)
ctCaselist expr altList context = 
    let (OverallContext env global_context) = context in
    let (exprType, global_context_1) = (getType expr context) in 
    case altList of
        [hd] ->
            (ctCase exprType hd (OverallContext env global_context_1))
        hd:tail -> 
            let (hdType, global_context_2) = ctCase exprType hd (OverallContext env global_context_1) in
            let (tailType, global_context_3) = ctCaselist expr tail (OverallContext env global_context_2) in
            let newrule = (hdType, tailType) in
            let global_context_4 = (addConstraint global_context_3 newrule) in
                (hdType, global_context_4)
        
    
ctPat :: Pat -> OverallContext -> (AllTypes, OverallContext)   
ctPat patt context = let (OverallContext env global_context) = context in
    case patt of
        (PList []) -> 
            let (typeVar, global_context_1) = genVarType global_context in
                (ListType typeVar, OverallContext env global_context_1)
        (PList [patt1]) -> 
            let (p1Type, new_context) = (ctPat patt1 context) in
                ((ListType p1Type), new_context)
        (PList pattList) -> ctPatList pattList context 
        (PInfixApp patt1 (Special Cons) patt2) ->
            let (p1Type, context_1) = (ctPat patt1 context) in
            let (p2Type, (OverallContext env2 global_context_2)) = (ctPat patt2 context_1) in
            let newrule = (p2Type, ListType p1Type) in
            let new_global_context = addConstraint global_context_2 newrule in
                ((ListType p1Type), (OverallContext env2 new_global_context))
        (PLit sign literal) ->  
            case literal of
                Int val -> (Base IntType, context)
                String val -> (Base StringType, context)
                otherwise -> (Error, (OverallContext env (ErrorContext ("Bad literal pattern " ++ show literal))))
        (PVar id) -> ctPVar id context
        PWildCard -> ctWildCard context
        otherwise -> (Error, (OverallContext env (ErrorContext ("Unmatched pattern: " ++ show patt))))
 
ctPatList :: [Pat] -> OverallContext -> (AllTypes, OverallContext)
ctPatList pattList context = let (OverallContext env global_context) = context in
    case pattList of
        [hd] -> 
            let (p1Type, new_context) = (ctPat hd context) in
                ((ListType p1Type), new_context)
        hd:tl -> 
            let (p1Type, context_1) = (ctPat hd context) in
            let (p2Type, (OverallContext env2 global_context_2)) = (ctPat (PList tl) context_1) in
            let newrule = (p2Type, ListType p1Type) in
            let new_global_context = addConstraint global_context_2 newrule in
                ((ListType p1Type), (OverallContext env2 new_global_context))
 
ctWildCard context =
    case context of 
        (OverallContext env global_context) ->
            let (typeVar, global_context_1) = (genVarType global_context) in
                (typeVar, (OverallContext env global_context_1))
 
ctPVar varName context = 
    case context of 
        (OverallContext env global_context) ->
            let (typeVar, global_context_1) = (genVarType global_context) in
            let newenv = (varName, typeVar):env in
                (typeVar, (OverallContext newenv global_context_1))
 
-- Do the binding stuff, then evaluate RHS 
ctCase :: AllTypes -> Alt -> OverallContext -> (AllTypes, GlobalContext) 
ctCase exprType alt context = let (OverallContext env global_context) = context in
    let (Alt pat (UnGuardedRhs rhs) (Nothing)) = alt in
    let (patType, (OverallContext env_1 global_context_1)) = (ctPat pat context) in
    let newrule = (patType, exprType) in
    let global_context_2 = (addConstraint global_context_1 newrule) in
        (getType rhs (OverallContext env_1 global_context_2))

