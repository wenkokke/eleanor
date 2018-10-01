{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Lib where

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

-- n lams
-- (n - 1) apps
-- n vars
-- every lam after the first one increases the size needed by two
-- first one increases the size by one
-- idea: keep track of the depth


terms :: Int -> Int -> [n] -> [Term n]
terms s d env
  | s < 2 * d - 1 = []
  | otherwise     = vars ++ lams ++ apps
  where
    vars
      | s == 1 && d == 1 = [Var (head env)]
      | otherwise        = []
    lams
      | s >  2           = Lam <$> terms (s - 3) (d + 1) (FZ : map FS env)
      | s == 2 && d == 0 = Lam <$> terms (s - 2)      1  [FZ]
      | otherwise        = []
    apps                 =
      [ App t1 t2
      | s1 <- [0..s - 1] , let s2 = s - s2
      , d1 <- [0..d]     , let d2 = d - d1
      , (env1, env2) <- combinations d1 env
      , t1 <- terms s1 d1 env1
      , t2 <- terms s2 d2 env2
      ]

combinations :: Int -> [a] -> [([a], [a])]
combinations 0 xs = [([], xs)]
combinations n (x:xs) =
  [ (x:xs, ys) | (xs, ys) <- combinations_xs ] ++
  [ (xs, x:ys) | (xs, ys) <- combinations_xs ]
  where
    combinations_xs = combinations (n - 1) xs
