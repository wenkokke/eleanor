module LLAM.Duregård where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import STLC.Duregård hiding (check)

check :: Fin n => IntMap Type -> Type -> Term n -> Bool
check env a t = snd (go env a t)
  where
    go :: Fin n => IntMap Type -> Type -> Term n -> (IntMap Type, Bool)
    go env a (Var x) =
      let ix = toInt x
      in (IM.delete ix env, IM.lookup ix env == Just a)
    go env (a :-> b) (Lam t) =
      let env1          = IM.insert 0 a $ IM.mapKeysMonotonic succ env
          (env2, cond1) = go env1 b t
          cond2         = IM.lookup 0 env2 == Nothing
          env3          = IM.mapKeysMonotonic pred env2
      in (env3, cond1 && cond2)
    go env b (App f s a) =
      let (env1, cond1) = go env (a :-> b) f
          (env2, cond2) = go env1 a s
      in (env2, cond1 && cond2)
    go env _ _ = (env, False)


checkClosed :: Type -> Term Z -> Bool
checkClosed = check IM.empty
