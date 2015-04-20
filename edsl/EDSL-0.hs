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
  | Eq Expr Expr
  | If Expr Expr Expr
  deriving (Show, Eq)

eval :: Expr -> Maybe Expr
eval (Add x y) = case (eval x, eval y) of
  (Just (IntLit x), Just (IntLit y)) -> Just . IntLit $ x + y
  _ -> Nothing
eval (Sub x y) = case (eval x, eval y) of
  (Just (IntLit x), Just (IntLit y)) -> Just . IntLit $ x - y
  _ -> Nothing
eval (SubStr str lo hi) = case (eval str, eval lo, eval hi) of
  (Just (StrLit str), Just (IntLit lo), Just (IntLit hi)) ->
    Just . StrLit . subStr lo hi $ str
  _ -> Nothing
eval (Len s) = case (eval s) of
  Just (StrLit s) -> Just . IntLit . length $ s
  _ -> Nothing
eval (Con s t) = case (eval s, eval t) of
  (Just (StrLit s), Just (StrLit t)) -> Just . StrLit $ s ++ t
  _ -> Nothing
eval (Eq x y) = case (eval x, eval y) of
  (Just (IntLit x), Just (IntLit y)) -> Just . BoolLit $ x == y
  _ -> Nothing
eval (If c t e) = case (eval c, eval t, eval e) of
  (Just (BoolLit c), Just t, Just e) -> Just $ if c then t else e
  _ -> Nothing
eval x = Just x

subStr :: Int -> Int -> String -> String
subStr lo hi = take (hi - lo) . drop lo
