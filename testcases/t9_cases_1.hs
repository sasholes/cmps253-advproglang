{-# LANGUAGE TemplateHaskell #-}
 
 

case [6-5, 2] of
    [1, 2] -> "Matches"
    otherwise -> "No match"
-- Type: String, Value: "Matches"

case [2] of
    [] -> "Empty"
    [2] -> "Contains 2"
    otherwise -> "Other"
-- Type: String, Value: "Contains 2"
    
case [0, 1, 2] of
    [] -> "Empty"
    [2] -> "Contains 2"
    h:tl -> "Not just 2"
    otherwise -> "Other"
-- Type: String, Value: "Not just 2"

\list -> case list of
        [] -> "Empty"
        [2] -> "Contains 2"
        h:tl -> "Not just 2"
        otherwise -> "Other"    
-- Type: [Int] -> String, Value: 
-- \list -> 
        -- case list of 
            -- [] -> "Empty"
            -- [2] -> "Contains 2"
            -- h:tl -> "Not just 2"
            -- otherwise -> "Other"        
    
case [5, 6, 7, 8] of
    [] -> 0
    a:b:tail -> a + b
    a:tail -> a
    otherwise -> 100
-- Type: Int -> String, Value: 11
    
case [5, 6, 7, 8] of
    [] -> [0]
    a:6:tail -> tail
    a:tail -> [a]
    otherwise -> []
-- Type: [Int] -> String, Value: [7, 8]
    
case [5, 6, 7, 8] of
    [] -> [0]
    a:6:tail -> tail
    a:tail -> [a]
    otherwise -> []
-- Type: [Int] -> String, Value: [7, 8]
    
case [5, 6, 7, 8] of
    [] -> [0]
    a:6:tail -> tail
    a:tail -> ["list"]
    otherwise -> []
-- ERROR: Unification Error: Cannot unify String and Int
        
            

