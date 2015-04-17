module EDSL where

data Expr
  = IntLit Int
  | StrLit String
  | Add Expr Expr
  | Idx Expr Expr
  | Len Expr
  | Con Expr Expr
  deriving (Show)

eval :: Expr -> Maybe Expr
eval (Add op0 op1) = case (eval op0, eval op1) of
  (Just (IntLit i0), Just (IntLit i1)) -> Just $ IntLit $ i0 + i1
  _ -> Nothing
eval (Idx op0 op1) = case (eval op0, eval op1) of
  (Just (StrLit s), Just (IntLit i)) -> Just $ StrLit $ [s !! i]
  _ -> Nothing
eval (Len op0) = case (eval op0) of
  Just (StrLit s) -> Just $ IntLit $ length s
  _ -> Nothing
eval (Con op0 op1) = case (eval op0, eval op1) of
  (Just (StrLit s0), Just (StrLit s1)) -> Just $ StrLit $ s0 ++ s1
  _ -> Nothing
eval x = Just x
