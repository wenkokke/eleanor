import Duregard (Z,S,Type(..),Term(..),check,check')
import Data.Coolean (Cool)
import Control.Search (search)
import Control.Enumerable (global)
import Test.Feat (Enumerate)
import Test.Feat.Access (valuesWith)

main :: IO ()

-- how many lambdas of size <100?
-- main = print (sum (map fst (take 30 (valuesWith ((global :: Enumerate (Term Z)))))))

-- how many lambdas of type Void :-> Void and size <30?
-- main = print . length =<< search 30 (check' [] (Z :-> Z) :: Term Z -> Cool)

-- how many linear lambdas of type Void :-> Void and size <30?
main = print . length =<< search 30 (check [] (Z :-> Z) :: Term Z -> Cool)
