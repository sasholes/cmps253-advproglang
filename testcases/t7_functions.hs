{-# LANGUAGE TemplateHaskell #-}

(\x y -> x ++ y ) "hello " "world"
-- Type: String, Value: "hello world"

(\x y -> x + y) 4 5
-- Type: Int, Value: 9

let x = True in if x then (\y -> x - 2) else (\y -> y + 2)
-- ERROR: Unification Error: Cannot unify Bool and Int

let x = True in if x then (\y -> x == y) else (\y -> y)
-- Type: Bool -> Bool, Value:  \y -> True == y

let x = True in (x - 2)
-- ERROR: Unification Error: Cannot unify Bool and Int 

let foo x = x in let bar y z = foo y + z in bar 1 7 
-- Type: Int, Value: 8

-- PARAMETRIC FUNCTIONS --
--------------------------
\x y -> (x == y)
-- Type: T_1 -> T_1 -> Bool, Value: \x -> \y -> x == y
 
let foo x = if (x == 0) then 1 else x + 1 in foo
-- Type: Int -> Int , Value: \x -> if x == 0 then 1 else x + 1

let foo x y = if (x == y) then 1 else 15 in foo
-- Type: T_4 -> T_4 -> Int, Value: \x -> \y -> if x == y then 1 else 15

let foo x y z = if (x == y) then z else z in foo
-- Type: T_5 -> T_5 -> T_6 -> T_6, Value: \x -> \y -> \z -> if x == y then z else z

let baz x y = (x == y) in (baz "hi" "bye") || (baz 3 3)
-- Type: Bool, Value: True
