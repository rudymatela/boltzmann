{-# LANGUAGE DeriveDataTypeable #-}

import Gen
import Test.QuickCheck.Gen

import Data.Data
import Data.Typeable
import qualified Data.Map as M
import Data.Map (Map, singleton)
import Data.Maybe (fromMaybe)

import Combstruct

import qualified Numeric.Optimization.Algorithms.CMAES as C

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



data Expr = Const Nat
          | Var
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
--        | Expr :/: Expr
  deriving (Show, Eq, Ord, Data, Typeable)

eval :: Nat -> Expr -> Nat
eval n (Const x) = x
eval n Var       = n
eval n (e1 :+: e2) = eval n e1 + eval n e2
eval n (e1 :-: e2) = eval n e1 - eval n e2
eval n (e1 :*: e2) = eval n e1 * eval n e2
-- eval n (e1 :/: e2) | eval n e2 == 0.0 = 1
--                    | otherwise      = eval n e1 / eval n e2

fitness :: [(Nat,Nat)] -> Expr -> Double
fitness xys e = fromIntegral (natToInt $ sum fs) / fromIntegral (length fs)
  where
  fs = fitness' xys e
  fitness' :: [(Nat,Nat)] -> Expr -> [Nat]
  fitness' []          _ = []
  fitness' ((x,y):xys) e = sq (eval x e - y)
                         : fitness' xys e
  sq x = x * x

foo :: Nat -> Nat
foo x = x ^ 6 - 1

fooMap :: [(Nat,Nat)]
fooMap = [ ( 1,     0)
         , ( 2,    63)
--       , ( 3,   728)
--       , ( 4,  4095)
--       , ( 5, 15624)
--       , ( 6, 46655)
         ]

fun :: [Double] -> IO Double
fun [x,y] =  minimum
         <$> sampleFitness 5 (fitness fooMap) (oracleFromList [("Expr",x),("Nat",y)])

expr :: Expr
expr = undefined

main :: IO ()
main = do
  let oracle = toOracle (undefined :: Expr)
  print oracle
  let coefficients = map snd $ oracleToList oracle 
  print coefficients
  print =<< sampleN 1 (toGen expr)
  print =<< sampleFitness 100 (fitness fooMap) oracle
  bestXs <- C.run $ (C.minimizeIO fun coefficients){C.tolStagnation = Just 2}
  putStrLn "end"

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


-- * how to represent doubles?
-- * how to avoid really large values?
--     - discard large values?
