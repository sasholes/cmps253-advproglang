{-# LANGUAGE TemplateHaskell #-}

-- Comparison Operations --
---------------------------

4 < 5
-- Type: Bool, Value: True

5 < 4
-- Type: Bool, Value: False

4 > 5
-- Type: Bool, Value: False

5 > "hi"
-- ERROR: Unification Error: Cannot unify Int and String

1 + 3 + 1 == 5
-- Type: Bool, Value: True

1 + 3 - 1 == 5
-- Type: Bool, Value: False

False < True
-- Type: Bool, Value: True

"abc" > "def"
-- Type: Bool, Value: False

"ab"++ "c" == "abc"
-- Type: Bool, Value: True
