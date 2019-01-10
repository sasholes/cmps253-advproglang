{-# LANGUAGE TemplateHaskell #-}
 
case ["g"] of
        [] -> "Empty"
        ["hi"] -> "Contains hi"
        1:tl -> "Numeric list"
        otherwise -> "Other"  
-- ERROR: Unification Error: Cannot unify Int and String

\e -> case e of
        [] -> "Empty"
        ["hi"] -> "Contains hi"
        1:tl -> "Numeric list"
        otherwise -> "Other"
-- ERROR: Unification Error: Cannot unify String and Int        
 
let len a = case a of
                [] -> 0
                h:t -> 1 + (len t)
            in len [1, 2, 4, 5]
-- Type: Int, Value: *ERROR: Variable len not recognized.
            
let len a = case a of
                [] -> 0
                h:t -> 1 + (len t)
            in len ["a", "b", "c"]
-- Type: Int, *ERROR: Variable len not recognized.
            
let len a = case a of
                [] -> 0
                h:t -> 1 + (len t)
            in (len [1, 2, 3, 4]) + (len ["a", "b", "c"])
-- ERROR: Unification Error: Cannot unify String and Int
            

