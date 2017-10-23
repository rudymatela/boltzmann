{-# LANGUAGE DeriveDataTypeable #-}

import Gen
import Test.QuickCheck.Gen

import Data.Data
import Data.Typeable
import qualified Data.Map as M
import Data.Map (Map, singleton)
import Data.Maybe (fromMaybe)

import Combstruct

data Nat = Z | S Nat
  deriving (Eq, Ord, Data, Typeable)

instance Show Nat where
  show = show . natToInt

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = natToInt n + 1

instance Num Nat where
  n + Z     = n
  n + (S m) = S (n + m)
  n * Z     = Z
  n * (S m) = n + (n * m)
  n     - Z     = n
  Z     - (S m) = Z
  (S n) - (S m) = n - m
  abs n = n
  signum Z = Z
  signum (S _) = S Z
  fromInteger 0 = Z
  fromInteger n | n > 0 = S (fromInteger (n - 1))
                | otherwise = error "negative!"



data Expr = Const Double
          | Var String
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
--        | Expr :/: Expr
  deriving (Show, Eq, Ord, Data, Typeable)

eval :: Map String Double -> Expr -> Double
eval m (Const x) = x
eval m (Var s)   = fromMaybe (error $ "unbound varialbe " ++ s)
                 $ M.lookup s m
eval m (e1 :+: e2) = eval m e1 + eval m e2
eval m (e1 :-: e2) = eval m e1 - eval m e2
eval m (e1 :*: e2) = eval m e1 * eval m e2
-- eval m (e1 :/: e2) | eval m e2 == 0.0 = 1
--                    | otherwise      = eval m e1 / eval m e2

fitness :: [(Double,Double)] -> Expr -> Double
fitness xys e = sum fs / fromIntegral (length fs)
  where
  fs = fitness' xys e
  fitness' :: [(Double,Double)] -> Expr -> [Double]
  fitness' []          _ = []
  fitness' ((x,y):xys) e = sq (eval (singleton "x" x) e - y)
                         : fitness' xys e
  sq x = x * x

foo :: Double -> Double
foo x = x ** 6 - 1

fooMap :: [(Double,Double)]
fooMap = [ (-2.0, 63.000000)
         , (-1.5, 10.390625)
         , (-1.0,  0.000000)
         , (-0.5, -0.984375)
         , (-0.0, -1.000000)
         , ( 0.5, -0.984375)
         , ( 1.0,  0.000000)
         , ( 1.5, 10.390625)
         , ( 2.0, 63.000000) ]

main :: IO ()
main = do
  print $ toOracle (undefined :: Expr)

-- > eval M.empty $ Const 2.0 :*: Const 3.0
-- 6.0
-- > eval (M.fromList [("x",2)]) $ Var "x" :*: Const 3.0
-- 6.0
-- > fitness [(2.0,6.0)] $ Const 2.0 :*: Const 3.0
-- 0.0
-- > fitness [(2,6), (3,9)] $ Const 2.0 :*: Const 3.0
-- 4.5
-- > fitness [(2,6), (3,9)] $ Var "x" :*: Const 3.0
-- 0.0
-- > fitness fooMap (Const 0)
-- 906.3186848958334
-- > fitness fooMap (Var "x" :*: Var "x" :*: Var "x" :*: Var "x" :*: Var "x" :*: Var "x" :-: Const 1)
-- 0.0
