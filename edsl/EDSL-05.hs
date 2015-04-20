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
eval (Add x y)
  | (IntRes x, IntRes y) <- (eval x, eval y) = IntRes $ x + y
eval (Sub x y)
  | (IntRes x, IntRes y) <- (eval x, eval y) = IntRes $ x - y
eval (SubStr str lo hi)
  | (StrRes str, IntRes lo, IntRes hi) <- (eval str, eval lo, eval hi) =
    StrRes . subStr lo hi $ str
eval (Len s)
  | (StrRes s) <- eval s = IntRes . length $ s
eval (Con s t)
  | (StrRes s, StrRes t) <- (eval s, eval t) = StrRes $ s ++ t
eval (Equals x y) = BoolRes $ eval x == eval y
eval (If c t e)
  | (BoolRes c, t, e) <- (eval c, eval t, eval e) = if c then t else e
eval _ = TypeError

-- invalid expressions are still a problem

subStr :: Int -> Int -> String -> String
subStr lo hi = take (hi - lo) . drop lo
