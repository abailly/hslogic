{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hslogic.Types where

import Text.PrettyPrint(hcat, text, Doc, char,punctuate)
import qualified Data.Map as H
import Data.Map (Map)
import Data.String (IsString (..))
import qualified Data.Map as Map
import Test.QuickCheck
    ( Small(..), Arbitrary(..), oneof, elements, listOf, Gen, choose, sized, resize )

newtype VarName = VarName String
 deriving newtype (Eq, Ord, Read)

instance Show VarName where
  show (VarName s) = s

instance IsString VarName where
  fromString = VarName

instance Arbitrary VarName where
  arbitrary = (VarName . (: [])) <$> choose ('A', 'Z')

data Term
  = Var VarName
  | Fn String [Term]
  deriving (Eq,Read)

instance Arbitrary Term where
  arbitrary = genTerm 5

class Deep d where
  deep :: d -> Int

instance Deep Term where
  deep = \case
    Var {} -> 0
    Fn _ [] -> 1
    Fn _ terms -> maximum (deep <$> terms ) + 1

genTerm :: Int -> Gen Term
genTerm 0 = Var <$> arbitrary
genTerm n = sized $ \ sz -> resize (n `div` 2) -- floor $ sqrt $ fromIntegral sz)
  $ oneof [ Var <$> arbitrary
                  , genFunction n
                  ]

genFunction :: Int -> Gen Term
genFunction n = Fn <$> elements ["foo","bar","baz","qix","quux"] <*> listOf (genTerm (n - 1))

data Clause
  = Clause {
    clauseHead :: Term,
    clausePremises :: [Term]
    } deriving (Eq,Read)

instance Deep Clause where
  deep Clause{clauseHead, clausePremises} = maximum $ deep <$> (clauseHead : clausePremises)

instance Arbitrary Clause where
  arbitrary = Clause <$> genFunction 3 <*> listOf(genTerm 3)

-- | The set of valid goals
--
-- Formulas are (currently) distinct from clauses but they should probably be one and the same
data Formula = T Term
             | Term :-> Formula  -- ^Intuitionistic implication, hypothesis maybe used zero or more times to prove consequence
             | Term :-@ Formula  -- ^Linear implication, hypothesis must be used one and only one time to prove consequence
             | Term :* Formula   -- ^Multiplicative conjunction (in linear context) or more simply conjunction (in intuitionistic context)
               deriving (Eq,Read)

instance Deep Formula where
  deep = \case
    T t -> deep t
    t :-> t' -> deep t `max` deep t'
    t :-@ t' -> deep t `max` deep t'
    t :* t' -> deep t `max` deep t'

instance Arbitrary Formula where
  arbitrary = genFormula 10

genFormula :: Int -> Gen Formula
genFormula 0 = T <$> genTerm 3
genFormula n = oneof [ T <$> genTerm n
                    , (:->) <$> genTerm n <*> genFormula (n - 1)
                    , (:-@) <$> genTerm n <*> genFormula (n - 1)
                    , (:*) <$> genTerm n <*> genFormula (n - 1)
                    ]
newtype Subst = Subst { substMap :: Map VarName Term }
  deriving Eq

toList :: Subst -> [(VarName, Term)]
toList (Subst m) = Map.toList m

class PrettyPrintable a where
  pp :: a -> Doc

instance PrettyPrintable VarName where
  pp v = text $ show v

instance PrettyPrintable Term where
  pp (Var v)       = pp v
  pp (Fn n [])     = text n
  pp (Fn n (t:ts)) = text n
                     <> char '('
                     <> pp t
                     <> hcat [char ',' <> pp t' | t' <- ts ]
                     <> char ')'

instance Show Term where
  show = show . pp

instance PrettyPrintable Clause where
  pp (Clause h []) = pp h <> char '.'
  pp (Clause h (p:ps)) = pp h <> text " <= " <> pp p <> hcat [text ", " <> pp p' | p' <- ps ] <> char '.'

instance Show Clause where
  show = show . pp

-- |Pretty print a term
--
pretty :: Term -> Doc
pretty = pp

instance PrettyPrintable (VarName,Term) where
  pp (k,v) = pp k <> text " -> " <> pp v

instance PrettyPrintable Subst where
  pp s = char '['
       <> hcat (punctuate (char ',') (map pp (H.toList $ substMap s)))
       <> char ']'

instance Show Subst where
  show = show . pp

instance PrettyPrintable Formula where
  pp (T t) = pp t
  pp (t :-> t') = pp t <> text " => "<> pp t'
  pp (t :-@ t') = pp t <> text " -o "<> pp t'
  pp (t :*  t') = pp t <> text " , " <> pp t'

instance Show Formula where
  show = show . pp
