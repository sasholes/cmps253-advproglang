-- CMPS 253, Fall 2017
-- Sara Sholes, 1153681
-- Final Project

module Unification where

import Datatypes



isEq :: AllTypes -> AllTypes -> Bool    
isEq t1 t2 = case (t1, t2) of 
    (Error, _) -> False
    (_, Error) -> False
    (TypeVar x, TypeVar y) -> (x == y)
    (Base x, Base y) ->
        case (x, y) of
            (IntType, IntType) -> True
            (BoolType, BoolType) -> True
            (StringType, StringType) -> True
            otherwise -> False
    (ArrowType a1 b1, ArrowType a2 b2) -> (isEq a1 a2) && (isEq b1 b2)        
    otherwise -> False

sub :: AllTypes -> AllTypes -> [(AllTypes, AllTypes)] -> [(AllTypes, AllTypes)] 
sub x xnew tv_constrs = case tv_constrs of
    [] -> []
    tv_constr:other_constrs -> (subConstr x xnew tv_constr):(sub x xnew other_constrs)

subConstr :: AllTypes -> AllTypes -> (AllTypes, AllTypes) -> (AllTypes, AllTypes)  
subConstr x xnew tv_constr = case tv_constr of
    (s, t) -> ((subType x xnew s), (subType x xnew t)) 
    
subType :: AllTypes -> AllTypes -> AllTypes -> AllTypes
subType x xnew typeExpr = case typeExpr of
    (ArrowType a b) -> (ArrowType (subType x xnew a) (subType x xnew b))
    (ListType a) -> (ListType (subType x xnew a))
    a | (isEq x typeExpr) -> xnew
    otherwise -> typeExpr


conTVAssign :: (AllTypes, AllTypes) -> UnifyResult -> UnifyResult --[(AllTypes, AllTypes)] 
conTVAssign tvAssign subUnifyResult = 
    case subUnifyResult of
        TypeVarAssignment other_constrs -> TypeVarAssignment (tvAssign:other_constrs)
        UnificationErr errMsg -> UnificationErr errMsg


-- UNIFICATION -- 
unify :: [(AllTypes, AllTypes)] -> UnifyResult--[(AllTypes, AllTypes)]
unify tv_constrs = case tv_constrs of
    [] -> TypeVarAssignment [] --(done)
    (type1, type2):other_constrs ->
        case (type1, type2) of 
            (s, t) | (isEq s t) -> (unify other_constrs)
            (TypeVar x, t) -> 
                (conTVAssign (TypeVar x, t) (unify (sub (TypeVar x) t other_constrs)))
            (s, TypeVar y) ->
                (conTVAssign (TypeVar y, s) (unify (sub (TypeVar y) s other_constrs)))
            ((ArrowType s1 s2), (ArrowType t1 t2)) -> 
                unify ([(s1, t1),(s2, t2)] ++ other_constrs)
            ((ListType s), (ListType t)) -> 
                unify ((s, t):other_constrs)  
            otherwise -> UnificationErr ("Cannot unify " ++ show type1 ++ " and " ++ show type2)
 

getReturnType unifiedConsts outT = 
    case outT of
        Base t -> outT
        ArrowType t1 t2 -> ArrowType (getReturnType unifiedConsts t1) (getReturnType unifiedConsts t2)
        TypeVar t -> 
            let findType = lookup outT unifiedConsts in
                case findType of
                    Just s -> (getReturnType unifiedConsts s)
                    Nothing -> ParamType t -- Ensures no collision of distinct parametric types)
        ListType t -> ListType (getReturnType unifiedConsts t)
            