{-# LANGUAGE TemplateHaskell #-}


------- Lists --------
----------------------

[1, 2, 3]
-- Type: [Int] , Value: [1, 2, 3]

[1, 2, 2 - 1 + 4]
-- Type: [Int] , Value: [1, 2, 5]

[True, False && True, False || True]
-- Type: [Bool], Value: [True, False, True]

[]
-- Type: [T_0], Value: [] 

"0: ":["abc", "d" ++ "e" ++ "f", "g"]
-- Type: [String], Value: ["0: ", "abc", "def", "g"]

["hi" + 3]
-- ERROR: Unification Error: Cannot unify String and Int

("a" ++ "b"):[1, 2, 2 - 1 + 4]
-- ERROR: Unification Error: Cannot unify String and Int

[1, 2, 2 - 1 + 4, True, 6]
-- ERROR: Unification Error: Cannot unify Int and Bool

\x -> ["hi", "bye", "yada", x]
-- Type: String -> [String], Value: \x -> ["hi", "bye", "yada", x] 




