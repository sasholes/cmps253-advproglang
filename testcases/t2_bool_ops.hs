{-# LANGUAGE TemplateHaskell #-}

-- Boolean Operations --
------------------------

True && False
-- Type: Bool, Value: False

True && True
-- Type: Bool, Value: True

False && False
-- Type: Bool, Value: False

True || False
-- Type: Bool, Value: True

True || True
-- Type: Bool, Value: True

False || False
-- Type: Bool, Value: False

False || 4 
-- Unification Error: Cannot unify Int and Bool

3 && 4
-- Unification Error: Cannot unify Int and Bool

True && "hello"
-- Unification Error: Cannot unify String and Bool

"Hello" && " world"
-- Unification Error: Cannot unify String and Bool
