{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module STLC.Duregård where


import Prelude hiding (lookup)
import Control.Enumerable
import Control.Monad.Supply (MonadSupply(..), Supply, evalSupply)
import Control.Search


-- * Finite types and contexts

data Z
  deriving (Typeable, Eq, Show)

data S n
  = FZ
  | FS n
  deriving (Typeable, Eq, Show)

instance Enumerable Z where
  enumerate = datatype []

instance Enumerable n => Enumerable (S n) where
  enumerate = share $ aconcat [c0 FZ, c1 FS]

class Eq n => Fin n where
  toInt :: n -> Int

instance Fin Z where
  toInt _ = undefined

instance Fin n => Fin (S n) where
  toInt FZ = 0
  toInt (FS n) = toInt n + 1


-- * Types and terms

data Type
  = Z
  | Type :-> Type
  deriving (Typeable, Eq, Show)

data Term n
  = Var n
  | Lam (Term (S n))
  | App (Term n) (Term n) Type
  deriving (Typeable)


-- * Enumerating types and terms

instance Enumerable Type where
  enumerate = share $ aconcat
    [ pay (c0 Z)
    , pay (c2 (:->))
    ]

instance Enumerable n => Enumerable (Term n) where
  enumerate = share $ aconcat
    [ pay (c1 Var)
    , pay (c1 Lam)
    , pay (c3 App)
    ]


-- * Type checking

check :: Fin n => [Type] -> Type -> Term n -> Cool
check env a         (Var x)     = Atom $ env !! toInt x == a
check env (a :-> b) (Lam t)     = check (a : env) b t
check env b         (App f s a) = check env (a :-> b) f &&& check env b s
check _   _         _           = false

checkClosed :: Type -> Term Z -> Cool
checkClosed = check []


-- * Pretty printing

type Name = String

pretty :: Fin n => [Name] -> Int -> Term n -> Supply Name ShowS
pretty γ _ (Var n) = do
  let x = γ !! toInt n
  return $
    showString x
pretty γ p (Lam t) = do
  x <- supply
  pretty_t <- pretty (x : γ) 4 t
  return $
    showParen (p >= 5) $
      showChar 'λ' . showString x . showChar '.' . pretty_t
pretty γ p (App f s _) = do
  pretty_f <- pretty γ 6 f
  pretty_s <- pretty γ 7 s
  return $
    showParen (p >= 7) $
      pretty_f . showChar ' ' . pretty_s

instance Show (Term Z) where
  showsPrec p t = evalSupply (pretty [] p t) names
    where
      names = [ 'x' : show i | i <- [0..] :: [Int] ]

-- -}
-- -}
-- -}
-- -}
-- -}
