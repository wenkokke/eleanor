module Naive where

data Type
  = Z
  | Type :-> Type
  deriving (Eq, Show)

type Name = String

data Term
  = Var Name
  | Lam Name Term
  | App Term Term
  deriving (Eq, Show)

