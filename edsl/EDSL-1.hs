{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module EDSL where

data Expr a where
  IntLit :: Int -> Expr Int
  StrLit :: String -> Expr String
  BoolLit :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Sub :: Expr Int -> Expr Int -> Expr Int
  SubStr :: Expr String -> Expr Int -> Expr Int -> Expr String
  Len :: Expr String -> Expr Int
  Con :: Expr String -> Expr String -> Expr String
  Equals :: (Eq b) => Expr b -> Expr b -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  -- the type checker now prevents invalid expressions

deriving instance Show (Expr a)

eval :: Expr a -> a
eval (IntLit int) = int
eval (StrLit str) = str
eval (BoolLit bool) = bool
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (SubStr str lo hi) = subStr (eval lo) (eval hi) (eval str)
eval (Len s) = length $ eval s
eval (Con s t) = eval s ++ eval t
eval (Equals x y) = eval x == eval y
eval (If c t e) = if (eval c) then (eval t) else (eval e)
-- this looks a little repetitive

subStr :: Int -> Int -> String -> String
subStr lo hi = take (hi - lo) . drop lo
