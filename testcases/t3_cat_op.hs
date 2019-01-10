{-# LANGUAGE TemplateHaskell #-}


-- Concatenation Operation --
-----------------------------

"Hello" ++ " world"
-- Type: String, Value: "Hello world"

"Hello" ++ 5
-- ERROR: Unification Error: Cannot unify Int and String

"Hello" ++ True
-- ERROR: Unification Error: Cannot unify Bool and String

True ++ 5
-- ERROR: Unification Error: Cannot unify Int and String

True ++ False
-- ERROR: Unification Error: Cannot unify Bool and String

4 ++ 5
-- ERROR: Unification Error: Cannot unify Int and String

