module EDSL where

data Expr
  = IntLit Int
  | StrLit String
  | BoolLit Bool
  | Add Expr Expr
  | Sub Expr Expr
  | SubStr Expr Expr Expr
  | Len Expr
  | Con Expr Expr
  | Equals Expr Expr
  | If Expr Expr Expr
  deriving (Show, Eq)
  -- what about invalid expressions? Phantom types anyone?

data Result
  = IntRes Int
  | StrRes String
  | BoolRes Bool
  | TypeError
  deriving (Show, Eq)

eval :: Expr -> Result
eval (IntLit i) = IntRes i
eval (StrLit s) = StrRes s
eval (BoolLit b) = BoolRes b
eval (Add x y) = case (eval x, eval y) of
  (IntRes x, IntRes y) -> IntRes $ x + y
  _ -> TypeError
eval (Sub x y) = case (eval x, eval y) of
  (IntRes x, IntRes y) -> IntRes $ x - y
  _ -> TypeError
eval (SubStr str lo hi) = case (eval str, eval lo, eval hi) of
  (StrRes str, IntRes lo, IntRes hi) -> StrRes . subStr lo hi $ str
  _ -> TypeError
eval (Len s) = case (eval s) of
  (StrRes s) -> IntRes . length $ s
  _ -> TypeError
eval (Con s t) = case (eval s, eval t) of
  (StrRes s, StrRes t) -> StrRes $ s ++ t
  _ -> TypeError
eval (Equals x y) = BoolRes $ eval x == eval y
  _ -> TypeError
eval (If c t e) = case (eval c, eval t, eval e) of
  (BoolRes c, t, e) -> if c then t else e
  _ -> TypeError

subStr :: Int -> Int -> String -> String
subStr lo hi = take (hi - lo) . drop lo
