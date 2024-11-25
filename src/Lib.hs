module Lib (mochi) where

import qualified Data.Map as M
import Data.Map (Map, (!))

newtype Id = Id String
  deriving (Show, Eq, Ord)

data Literal where
  Int :: Int -> Literal
  String :: String -> Literal
  deriving Show

data Ty where
  TInt :: Ty
  TString :: Ty
  TFun :: Ty -> Ty -> Ty
  TObj :: Map Id Ty -> Ty
  TCtx :: Id -> Ty -> Ty -> Ty
  deriving (Eq, Show)

data Def where
  Ctx :: Id -> Ty -> Def -> Def
  Def :: Id -> Expr -> Def
  deriving Show

data Expr where
  Lit :: Literal -> Expr
  Var :: Id -> Expr
  Subst :: Expr -> Expr -> Expr
  Lam :: Id -> Ty -> Expr -> Expr
  App :: Expr -> Expr -> Expr
  Obj :: [Def] -> Expr
  Prop :: Expr -> Id -> Expr
  deriving Show

data Value where
  VInt :: Int -> Value
  VString :: String -> Value
  VClosure :: Map Id Value -> Id -> Expr -> Value
  VObject :: Map Id Value -> Value
  deriving Show

evalDef :: Map Id Value -> Def -> (Id, Value)
evalDef x (Ctx i _ d) = case M.lookup i x of
  Just _ -> evalDef x d
  Nothing -> error "not found"
evalDef x (Def i e) = (i, eval x e)

eval :: Map Id Value -> Expr -> Value
eval _ (Lit (Int n)) = VInt n
eval _ (Lit (String s)) = VString s
eval x (Var i) = x ! i
eval x (Subst e h) = case eval x h of
  VObject o -> eval (M.union o x) e
  _ -> error "not an object"
eval x (Lam i _ e) = VClosure x i e
eval x (App f a) = case eval x f of
  VClosure x' i f' -> eval (M.insert i (eval x a) x') f'
  _ -> error "not a closure"
eval x (Obj ds) = VObject $ M.fromList $ map (evalDef x) ds
eval x (Prop e i) = case eval x e of
  VObject o -> o ! i
  _ -> error "not an object"

typingDef :: Map Id Ty -> Def -> (Id, Ty)
typingDef x (Ctx i t d) = case M.lookup i x of -- hmm...
  Just t' -> if t == t' then (i, t) else error "type mismatch"
  Nothing -> fmap (TCtx i t) $ typingDef x d
typingDef x (Def i e) = (i, typing x e)

typing :: Map Id Ty -> Expr -> Ty
typing _ (Lit (Int _)) = TInt
typing _ (Lit (String _)) = TString
typing x (Var i) = x ! i
typing x (Subst e h) = case typing x h of
  TObj o -> typing (M.union o x) e -- ?
  _ -> error "not an object"
typing x (Lam i t e) = TFun t $ typing (M.insert i t x) e
typing x (App f a) = case typing x f of
  TFun a' b -> if typing x a == a' then b else error "type mismatch"
  _ -> error "not a function"
typing x (Obj ds) = TObj $ M.fromList $ map (typingDef x) ds
typing x (Prop e i) = case typing x e of
  TObj o -> o ! i
  _ -> error "not an object"

p :: Expr
p = Obj [
  Ctx (Id "x") TString $ Def (Id "id") $ Var (Id "x"),
  Def (Id "const") $ Lam (Id "x") TInt $ Lam (Id "y") TInt $ Var (Id "x"),
  Def (Id "h") $ Subst (Var (Id "g")) (Obj [Def (Id "g") $ Lit $ String "a"]),
  Def (Id "f") $ Subst (Obj [Ctx (Id "v") TInt $ Def (Id "u") $ Var (Id "v")]) (Obj [Def (Id "v") $ Lit $ Int 4])]

mochi :: IO ()
mochi = do
  print p
  let
    xt = M.singleton (Id "x") TString
    xv = M.singleton (Id "x") $ VString "a"
  print $ typing xt p
  print $ eval xv p