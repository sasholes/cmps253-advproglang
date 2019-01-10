{-# LANGUAGE TemplateHaskell #-}

-- Basic If Statements --
------------------------- 
 
if (False) then 4 else 5
-- Type: Int, Value: 5

if (True) then "4" else "5"
-- Type: String, Value: "4"

if ("abc" == "abc") then 4 else 5
-- Type: Int, Value: 4

if (4 < 7) then 4 else 7
-- Type: Int, Value: 4

if (7) then 1 else 2
-- ERROR: Unification Error: Cannot unify Int and Bool

if (3 < 6) then "4" else 5
-- ERROR: Unification Error: Cannot unify String and Int