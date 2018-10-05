{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LLAM.Naive where

import Control.Monad.Supply (MonadSupply(..), Supply, evalSupply)
import Debug.Trace (traceShow)

data Z
  deriving (Eq, Show)

data S n
  = FZ
  | FS n
  deriving (Eq, Show)

data Nat n where
  Z :: Nat Z
  S :: Nat n -> Nat (S n)

data Type
  = Void
  | Type :-> Type

data Term n
  = Var n
  | Lam (Term (S n))
  | App (Term n) (Term n)

terms :: Show n => Int -> [n] -> [Term n]
terms 0  _  = []
terms 1 [n] = [Var n]
terms s env = if s >= smin then lams ++ apps else []
  where
    smin = 2 * length env - 1
    lams = if s >= smin + 3 then Lam <$> terms (s - 1) (FZ : map FS env) else []
    apps = do
      (env1, env2) <- combinations env
      s1 <- [0..s - 1]
      let s2 = (s - 1) - s1
      App <$> terms s1 env1 <*> terms s2 env2

combinations :: [a] -> [([a], [a])]
combinations [] = [([],[])]
combinations (x:xs) =
  [ (x:xs1,   xs2) | (xs1, xs2) <- combinations_xs ] ++
  [ (  xs1, x:xs2) | (xs1, xs2) <- combinations_xs ]
  where
    combinations_xs = combinations xs

closed :: Int -> [Term Z]
closed s = terms s []

-- * Pretty printing

type Name = String

class Fin n where
  toInt :: n -> Int

instance Fin Z where
  toInt _ = undefined

instance Fin n => Fin (S n) where
  toInt FZ = 0
  toInt (FS n) = toInt n + 1

pretty :: Fin n => [Name] -> Int -> Term n -> Supply Name ShowS
pretty γ p (Var n) = do
  let x = γ !! toInt n
  return $
    showString x
pretty γ p (Lam t) = do
  x <- supply
  pretty_t <- pretty (x : γ) 4 t
  return $
    showParen (p >= 5) $
      showChar 'λ' . showString x . showChar '.' . pretty_t
pretty γ p (App f s) = do
  pretty_f <- pretty γ 6 f
  pretty_s <- pretty γ 7 s
  return $
    showParen (p >= 7) $
      pretty_f . showChar ' ' . pretty_s

instance Show (Term Z) where
  showsPrec p t = evalSupply (pretty [] p t) names
    where
      names = [ 'x' : show i | i <- [0..] ]
