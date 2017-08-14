{-# LANGUAGE DeriveDataTypeable #-}

import           Gen
import           GHC.Generics
import           Test.QuickCheck.Gen

import           Data.Data
import           Data.Typeable

import           Combstruct

data Prog = Prog Dec Stat deriving (Typeable, Data, Show)
data Dec  = Nodec | Ondec Id Type | Manydecs Dec Dec deriving (Typeable, Data, Show)
data Id   = A | B deriving (Typeable, Data, Show)
data Type = Int | Bool deriving (Typeable, Data, Show)
data Stat = Noop | Assign Id Exp | Seq Stat Stat deriving (Typeable, Data, Show)
data Exp  = Zero | Succ Exp deriving (Typeable, Data, Show)

prog :: Prog
prog = error "undefined program (type hack)"

toSingularity :: Data a => a -> (Double, State)
toSingularity = singularity singPrecision valPrecision
              . defsToComb
              . toDefs

(->-) :: String -> IO () -> IO ()
s ->- a = do
  putStrLn $ "> " ++ s
  a
  putStrLn ""
infixr 0 ->-
main = do
  "toDefs prog"              ->- print $ toDefs prog
  "defsToComb (toDefs prog)" ->- print $ defsToComb $ toDefs prog
  "combToVals ..."           ->- print $ combToVals $ defsToComb $ toDefs prog
  "singularity ..."          ->- print $ toSingularity prog
  "toOracle prog"            ->- print $ toOracle prog
  "sample $ toGen prog"      ->- sample $ toGen prog

-- callTree:
--   toGen
--     toOracle
--       toDefs
--       defsToOracle
--         defsToComb
--         combToVals
--           singularity
--             Newton.*
--     toGenAux
--
--
-- Summary of functions:
-- * toDefs takes a type and returns its representation as a Def;
-- * defsToComb just simplifies the representation;
-- * combToVals computes the doubles returned by toOracle;
-- * singularity uses newton to computer the aforementioned double values;
-- * toOracle takes a type and returns a Double value paired with each subtype.
-- * toGenAux takes an Oracle and returns a generator of values
--
-- I am unsure if we need to change values in the oracle, or if we need to
-- expose parameters in toGenAux.
--
-- toDefs, defsToComb, combToVals, singularity and toOracle are deterministic.
--
-- Examples
-- ========
--
-- > > toDefs prog
-- > Defs (fromList
-- >   [ ( "Dec",  (Def (DataType { tycon = "Dec"
-- >                              , datarep = AlgRep [Nodec,Ondec,Manydecs] })
-- >                    (Sum [ Prod []
-- >                         , Prod [ Var 3 "Id"
-- >                                , Var 6 "Type"]
-- >                         , Prod [ Var 1 "Dec"
-- >                                , Var 1 "Dec"] ])
-- >               ,1))
-- >   , ( "Exp",  (Def (DataType { tycon = "Exp"
-- >                              , datarep = AlgRep [Zero,Succ]})
-- >                    (Sum [ Prod []
-- >                         , Prod [Var 2 "Exp"]])
-- >               ,2))
-- >   , ( "Id" ,  (Def (DataType { tycon = "Id"
-- >                              , datarep = AlgRep [A,B]})
-- >                    (Sum [ Prod []
-- >                         , Prod []])
-- >               ,3))
-- >   , ( "Prog", (Def (DataType { tycon = "Prog"
-- >                              , datarep = AlgRep [Prog]})
-- >                    (Sum [Prod [Var 1 "Dec"
-- >                               ,Var 5 "Stat"]])
-- >               ,4))
-- >   , ("Stat",  (Def (DataType {tycon = "Stat", datarep = AlgRep [Noop,Assign,Seq]})
-- >                    (Sum [Prod []
-- >                         ,Prod [Var 3 "Id",Var 2 "Exp"]
-- >                         ,Prod [Var 5 "Stat",Var 5 "Stat"]])
-- >               ,5))
-- >   , ("Type",  (Def (DataType {tycon = "Type"
-- >                              , datarep = AlgRep [Int,Bool]})
-- >                    (Sum [Prod []
-- >                         ,Prod []])
-- >               ,6)
-- >     )
-- >   ])
--
-- > > defsToComb (toDefs prog)
-- > array (1,6) [ (1, (X :+: ((X :*: Y 3) :*: Y 6)) :+: ((X :*: Y 1) :*: Y 1))
-- >             , (2, X :+: (X :*: Y 2))
-- >             , (3, X :+: X)
-- >             , (4, (X :*: Y 1) :*: Y 5)
-- >             , (5, (X :+: ((X :*: Y 3) :*: Y 2)) :+: ((X :*: Y 5) :*: Y 5))
-- >             , (6, X :+: X)
-- >             ]
--
-- > > combToVals (defsToComb (toDefs prog))
-- > [ 1.2720043909696304
-- > , 0.6476519092129175
-- > , 0.786151377717033
-- > , 0.47101815381032025
-- > , 0.9420476080253635
-- > , 0.786151377717033
-- > ]
--
-- > > singularity ...
-- > (0.3930756888585165,NewtonState ...)
--
-- > > toOracle prog
-- > fromList [ ("Dec",  1.2720043909696304)
-- >          , ("Exp",  0.6476519092129175)
-- >          , ("Id",   0.786151377717033)
-- >          , ("Prog", 0.47101815381032025)
-- >          , ("Stat", 0.9420476080253635)
-- >          , ("Type", 0.786151377717033)
-- >          ]
