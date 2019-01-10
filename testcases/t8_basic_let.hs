{-# LANGUAGE TemplateHaskell #-}

-- Basic Let Statements --
-------------------------

let hello x = 1 in 4 
-- Type: Int, Value: 4

let f = 5 in (2:[f-1, 4])
-- Type: [Int], Value: [2, 4, 4]

let f = "abc" in if (f == "abc") then "hi" else "bye"
-- Type: String, Value: "hi"

let f = ("hi" < 6) in 3 - 2
-- ERROR: Unification Error: Cannot unify String and Int

let x = "hi" in 
    if (x == 4) then 
        let x = 4 in x
    else
        let x = 6 in x
-- ERROR: Unification Error: Cannot unify String and Int

let foo x = if (x == "hi") then 1 else x + 1 in foo
-- ERROR: Unification Error: Cannot unify Int and String

let foo x = if (x == 0) then 1 else foo (x - 1) in foo
-- Type: Int -> Int, Value: \x -> if x == 0 then 1 else foo x - 1

let foo x = if (x == 0) then 1 else foo (x - 1) in foo 5
-- Type: Int, *ERROR: Variable foo not recognized.

let foo x = if (x == "abc") then 1 else foo (x - 1) in foo
-- ERROR: Unification Error: Cannot unify Int and String

let x = "hello " in x ++ y
-- ERROR: Undefined var: Ident () "y"

let x = True in (let x = 3 in x - 2)
-- Type: Int, Value: 1