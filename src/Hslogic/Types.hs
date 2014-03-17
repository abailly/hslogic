{-# LANGUAGE FlexibleInstances #-}

module Hslogic.Types where

import Text.PrettyPrint((<>), hcat, text, Doc, char,punctuate)
import Data.Hashable
import qualified Data.HashMap.Lazy as H

data VarName = VarName String deriving (Eq, Read)

instance Show VarName where
  show (VarName v) = v

instance Hashable VarName where
  hash (VarName s) = hash s
  hashWithSalt i (VarName s) = hashWithSalt i s


mk_var :: String -> VarName
mk_var = VarName

data Term
  = Var VarName
  | Fn String [Term]
  deriving (Eq,Read)

data Clause
  = Clause {
    clauseHead :: Term,
    clausePremises :: [Term]
    } deriving (Eq,Read)

newtype Subst = Subst { substMap :: (H.HashMap VarName Term) } deriving Eq


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
  pp (Clause h (p:ps)) = pp h <> text " -: " <> pp p <> hcat [text ", " <> pp p' | p' <- ps ] <> char '.'
  
instance Show Clause where
  show = show . pp

-- |Pretty print a term
--
-- >>> pretty (Fn "install" [ Var (VarName "X") ])
-- install(X)
-- >>> pretty (Fn "copy" [])
-- copy
pretty :: Term -> Doc
pretty t = pp t

instance PrettyPrintable (VarName,Term) where
  pp (k,v) = pp k <> text " -> " <> pp v
  
instance PrettyPrintable Subst where
  pp s = char '['
       <> hcat (punctuate (char ',') (map pp (H.toList $ substMap s)))
       <> char ']'

instance Show Subst where
  show = show . pp
  
