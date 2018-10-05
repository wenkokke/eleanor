{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
module STLC.Unification where


import Control.Arrow (first, second)
import Control.Applicative (Alternative (..))
import Control.Monad.List (ListT (..))
import Control.Monad.Supply (Supply)
import Data.Maybe (maybeToList)
import Debug.Trace (traceShow)


-- * Types with unification variables

type Name = Int

data Conn
  = Zero
  | Fun
  deriving (Eq)

data Type
  = UVar Name
  | UCon Conn [Type]
  deriving (Eq)

instance Show Type where
  showsPrec _ (UVar n) =
    showChar '?' . shows n
  showsPrec p (UCon Fun [τ1, τ2]) =
    showParen (p >= 9) $ showsPrec 9 τ1 . showString " → " . showsPrec 8 τ2
  showsPrec p (UCon Zero []) =
    showChar '0'


-- * Syntactic sugar for types

(-->) :: Type -> Type -> Type
τ1 --> τ2 = UCon Fun [τ1, τ2]

zero :: Type
zero = UCon Zero []


-- * Substitution

ext :: (Name -> Name) -> (Name -> Name)
ext _ 0 = 0
ext r x = succ (r (pred x))

rename :: (Name -> Name) -> (Type -> Type)
rename r (UVar x) = UVar (r x)
rename r (UCon c xs) = UCon c (rename r <$> xs)

exts :: (Name -> Type) -> (Name -> Type)
exts _ 0 = UVar 0
exts σ x = rename succ (σ (pred x))

subst :: (Name -> Type) -> (Type -> Type)
subst σ (UVar n) = σ n
subst σ (UCon c xs) = UCon c (subst σ <$> xs)

sub :: Name -> Type -> Type -> Type
sub x τ = subst (\y -> if x == y then τ else UVar y)


-- * Unification

type Subst = [(Name, Type)]

apply :: Subst -> Type -> Type
apply [] τ = τ
apply ((x, τ') : σ) τ = apply σ (sub x τ' τ)

occurs :: Name -> Type -> Bool
occurs y (UVar x) = x == y
occurs y (UCon _ xs) = any (occurs y) xs

flexRigid :: Name -> Type -> Maybe Subst
flexRigid x τ =
  if occurs x τ then Nothing else Just [(x, τ)]

flexFlex :: Name -> Name -> Subst
flexFlex x y =
  if x == y then [] else [(x, UVar y)]

unifyAcc :: Type -> Type -> Subst -> Maybe Subst
unifyAcc (UCon c1 τs1) (UCon c2 τs2) σ =
  if c1 == c2 then unifyAccChildren τs1 τs2 σ else Nothing
  where
    unifyAccChildren :: [Type] -> [Type] -> Subst -> Maybe Subst
    unifyAccChildren [] [] σ = Just σ
    unifyAccChildren (τ1 : τs1) (τ2 : τs2) σ =
      unifyAcc τ1 τ2 σ >>= unifyAccChildren τs1 τs2
unifyAcc (UVar x1) (UVar x2) [] = Just (flexFlex x1 x2)
unifyAcc (UVar x1) τ2        [] = flexRigid x1 τ2
unifyAcc τ1        (UVar x2) [] = flexRigid x2 τ1
unifyAcc τ1        τ2        ((x, τ) : σ) =
  ((x, τ) : ) <$> unifyAcc (sub x τ τ1) (sub x τ τ2) σ


unify :: Type -> Type -> Maybe Subst
unify τ1 τ2 = unifyAcc τ1 τ2 []


-- * Expressions

data Expr
  = Var Name
  | Lam Type Expr
  | App Expr Expr
  deriving (Eq)

instance Show Expr where
  showsPrec _ (Var x) = shows x
  showsPrec p (Lam τ e) =
    showParen (p >= 3) $ showString "λ " . showsPrec 2 e
  showsPrec p (App f e) =
    showParen (p >= 5) $ showsPrec 4 f . showChar ' ' . showsPrec 5 e


-- * Enumeration of simply-typed lambda terms

mapTy σ (Lam τ e) = Lam (σ τ) (mapTy σ e)
mapTy σ (App f e) = App (mapTy σ f) (mapTy σ e)
mapTy _ e         = e

split :: [a] -> ([a], [a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:xss) = let (xs,ys) = split xss in (x:xs,y:ys)

closed g s = [mapTy (apply σ) e | (e, σ) <- g s ns [] u0 []]
  where
    u0 = UVar 0
    ns = [1..]

exprs :: Int -> [Name] -> [(Name, Type)] -> Type -> Subst -> [(Expr, Subst)]
exprs s ns γ τ σ = vars ++ lams ++ apps
  where
    vars | s /= 1 = []
         | otherwise = do
             (x, τ') <- γ
             σ' <- maybeToList (unifyAcc τ τ' σ)
             return (Var x, σ')

    lams | s == 0 = []
         | otherwise = do
             let (n1 : n2 : ns') = ns
             let τ1 = UVar n1
             let τ2 = UVar n2
             σ' <- maybeToList (unifyAcc τ (τ1 --> τ2) σ)
             let γ' = (0, τ1) : map (first succ) γ
             (e, σ'') <- exprs (s - 1) ns' γ' τ2 σ'
             return (Lam τ1 e, σ'')

    apps | s == 0 = []
         | otherwise = do
             -- split the names
             let (n1 : ns1, ns2) = split ns
             let τ1 = UVar n1
             -- split the sizes
             s1 <- [0..s - 1]
             let s2 = (s - 1) - s1
             -- continue
             (f, σ1) <- exprs s1 ns1 γ (τ1 --> τ) σ
             (e, σ2) <- exprs s2 ns2 γ τ1 σ1
             return (App f e, σ2)

-- * Enumeration of simply-typed *linear* lambda terms

combinations :: Int -> [a] -> [([a],[a])]
combinations 0 xs = [([],xs)]
combinations _ [] = [([],[])]
combinations n (x:xs) = [ (x:xs,ys) | (xs,ys) <- combinations (n-1) xs ]
                        ++ [ (xs,x:ys) | (xs,ys) <- combinations n xs ]

lexprs :: Int -> [Name] -> [(Name, Type)] -> Type -> Subst -> [(Expr, Subst)]
lexprs s ns γ τ σ
  | s < length γ * 2 - 1 = []
  | null γ && (s + 1) `rem` 3 /= 0 = []
  | otherwise = vars ++ lams ++ apps
  where
    vars | s /= 1 || length γ /= 1 = []
         | otherwise = do
             (x, τ') <- γ
             σ' <- maybeToList (unifyAcc τ τ' σ)
             return (Var x, σ')

    lams | s == 0 = []
         | otherwise = do
             let (n1 : n2 : ns') = ns
             let τ1 = UVar n1
             let τ2 = UVar n2
             let γ' = (0, τ1) : map (first succ) γ
             σ' <- maybeToList (unifyAcc τ (τ1 --> τ2) σ)
             (e, σ'') <- lexprs (s - 1) ns' γ' τ2 σ'
             return (Lam τ1 e, σ'')

    apps | s == 0 = []
         | otherwise = do
             -- split the names
             let (n1 : ns1, ns2) = split ns
             let τ1 = UVar n1
             -- split the sizes
             s1 <- [0..s - 1]
             let s2 = (s - 1) - s1
             -- split the context
             i <- [0..length γ - 1]
             (γ1, γ2) <- combinations i γ
             -- continue
             (f, σ1) <- lexprs s1 ns1 γ1 (τ1 --> τ) σ
             (e, σ2) <- lexprs s2 ns2 γ2 τ1 σ1
             return (App f e, σ2)

-- -}
-- -}
-- -}
-- -}
-- -}
