module Lib (mochi) where

data Expr x a where
  Lit :: a -> Expr () a
  Lift :: (x -> y) -> Expr y a -> Expr x a
  Var :: Expr x x
  App :: Expr x (a -> b) -> Expr x a -> Expr x b
  Lam :: Expr (a, x) b -> Expr x (a -> b)

interpret :: Expr x a -> x -> a
interpret (Lit a) _ = a
interpret (Lift f e) x = interpret e (f x)
interpret Var x = x
interpret (App e1 e2) x = interpret e1 x $ interpret e2 x
interpret (Lam e) x = \a -> interpret e (a, x)

display :: Expr x a -> String
display (Lit _) = "L"
display (Lift _ e) = display e
display Var = "x"
display (App e1 e2) = "(" ++ display e1 ++ " " ++ display e2 ++ ")"
display (Lam e) = "λx. " ++ display e

add :: Expr (Int, Int) Int
add = Lift (const ()) (Lit (+)) `App` (Lift fst Var) `App` (Lift snd Var)

add' :: Expr (Int, Int) Int
add' = App (Lift (const ()) $ Lit (\(x,y) -> x+y)) Var

add23 :: Expr () Int
add23 = Lift (const (2, 3)) add

add23' :: Expr () Int
add23' = Lift (const (2, 3)) add'

mochi :: IO ()
mochi = do
  putStrLn $ display add
  putStrLn $ display add'
  print $ interpret add23 ()
  print $ interpret add23' ()