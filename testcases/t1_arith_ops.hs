{-# LANGUAGE TemplateHaskell #-}


-- Testing basic expressions (binary ops):

-- Arithmetic Operations --
---------------------------

1 + 2 + 5
-- Type: Int, Value: 8

6 - 2
-- Type: Int, Value: 4

5*4
-- Type: Int, Value: 20

"hi" * 2
-- ERROR: Unification Error: Cannot unify String and Int

"Hello" + " world"
-- ERROR: Unification Error: Cannot unify String and Int

"Hello" - " world"
-- ERROR: Unification Error: Cannot unify String and Int

"Hello" * " world"
-- ERROR: Unification Error: Cannot unify String and Int

(10 - 1) * 3 - (2 + 3 + (4 - (4 * "hi") + 10) - 1)*3
-- ERROR: Unification Error: Cannot unify String and Int

(10 - 1) * 3 - (2 + 3 + (2 * 2 * 2))
-- Type: Int, Value: 14