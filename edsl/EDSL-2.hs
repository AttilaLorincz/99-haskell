{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
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

deriving instance Show (Expr a)

eval :: Expr a -> a
eval = \case -- there is an equivalent that uses vanilla 'case'
  IntLit int -> int
  StrLit str -> str
  BoolLit bool -> bool
  Add x y -> eval x + eval y
  Sub x y -> eval x - eval y
  SubStr str lo hi -> subStr (eval lo) (eval hi) (eval str)
  Len s -> length $ eval s
  Con s t -> eval s ++ eval t
  Equals x y -> eval x == eval y
  If c t e -> if (eval c) then (eval t) else (eval e)

subStr :: Int -> Int -> String -> String
subStr lo hi = take (hi - lo) . drop lo
