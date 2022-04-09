{-# LANGUAGE PatternSynonyms #-}
module Main where

pattern ReqNoProv :: Show a => a -> Maybe a
pattern ReqNoProv{j} = Just j

data A = A deriving Show

p1 = Just True

-- Record updates is desugared to case belowexpression, see #18802.
-- p6 = case p1 of ReqNoProv x -> ReqNoProv A
-- (Detials can be found in Note [Record Updates] defined in Tc.Gen.Expr.hs)

p6 = p1 {j = A}

main = print p6
