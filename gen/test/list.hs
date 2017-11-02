{-# LANGUAGE DeriveDataTypeable #-}

import Gen
import Data.Data
import Data.Typeable

data Nat = Z | S Nat
  deriving (Eq, Ord, Data, Typeable, Show)

data UnitType = UnitCons
  deriving (Eq, Ord, Data, Typeable, Show)

data ListType a = Nil | Cons a (ListType a)
  deriving (Eq, Ord, Data, Typeable, Show)

main :: IO ()
main = do
  "()"       ->- print $ toOracle (undefined :: ())
  "Nat"      ->- print $ toOracle (undefined :: Nat)
  "[()]"     ->- print $ toOracle (undefined :: [()])
  "List ()"  ->- print $ toOracle (undefined :: ListType UnitType)

-- Infinite loop?
--"[Nat]"    ->- print $ toOracle (undefined :: [Nat])
--"List Nat" ->- print $ toOracle (undefined :: List Nat)

(->-) :: String -> IO () -> IO ()
s ->- a = do
  putStrLn $ "> toOracle (undefined :: " ++ s ++ ")"
  a
  putStrLn ""
infixr 0 ->-

-- Program output follows:
--
-- > toOracle (undefined :: ())
-- fromList [("()",0.9999999999417923)]
-- 
-- > toOracle (undefined :: Nat)
-- fromList [("Nat",8.589933254397974e9)]
-- 
-- > toOracle (undefined :: [()])
-- fromList [("()",0.9999999999417923),("Prelude.[]",8.589933255397791e9)]
-- 
-- > toOracle (undefined :: List ())
-- fromList [("()",0.9999999999417923),("List",8.589933255397791e9)]
--
-- > toOracle (undefined :: [Nat])
-- ... non-termination ...
--
-- > toOracle (undefined :: List Nat)
-- ... non-termination ...
