{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Basic unification essentially copied verbatim from http://pragprog.com/magazines/2013-06/unification
module Hslogic.Unify where

import qualified Data.HashMap.Lazy as H
import Data.Hashable
import Text.PrettyPrint((<>), hcat, text, Doc, char)
import Data.List(union)

import Hslogic.Types

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
  pp (Clause h (p:ps)) = pp h <> text " -: " <> pp p <> hcat [text ", " <> pp p' | p' <- ps ]
  
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

class ContainsVars t where
  vars_in :: t -> [VarName]


instance ContainsVars a => ContainsVars [a] where
  vars_in xs = foldl union [] [ vars_in x | x <- xs ]

instance ContainsVars Term where
  vars_in (Var v)     = [v]
  vars_in (Fn _ ts)   = vars_in ts


class Substitution s where
  lookup_var        :: s -> VarName -> Maybe Term
  emptySubstitution :: s
  (+->)             :: VarName -> Term -> s
  extend_with       :: s -> s -> s

instance Hashable VarName where
  hash (VarName s) = hash s
  hashWithSalt i (VarName s) = hashWithSalt i s

newtype Subst = Subst { substMap :: (H.HashMap VarName Term) }

instance Substitution Subst where
  emptySubstitution = Subst H.empty
  lookup_var        = flip H.lookup . substMap
  v +-> t           = Subst $ H.singleton v t
  extend_with s s'  = Subst $ H.union (substMap s) (substMap s') 

instance PrettyPrintable Subst where
  pp s = char '['
       <> hcat (map ( \ (k,v) -> pp k <> text " → " <> pp v) (H.toList $ substMap s))
       <> char ']'

instance Show Subst where
  show = show . pp
  
class Substitutible t where
  apply :: Substitution s => s -> t -> t


instance Substitutible Term where
  apply ss (Var n)   = case lookup_var ss n of
    Just t  -> t
    Nothing -> Var n
  apply ss (Fn n ts) = Fn n (apply ss ts)
 	
instance Substitutible a => Substitutible [a] where
  apply ss = map (apply ss)

class Substitutible t => Unifiable t where
  unify :: Substitution s => t -> t -> Maybe s


instance Unifiable a => Unifiable [a] where
  unify [] [] = Just emptySubstitution
  unify [] _  = Nothing
  unify _  [] = Nothing
  unify (l:ls) (r:rs) = do s1 <- unify l r
                           s2 <- unify (apply s1 ls) (apply s1 rs)
                           return (s1 `extend_with` s2)


instance Unifiable Term where
  unify (Var l_v) r@(Var r_v)                    -- 1
   | l_v == r_v = return $ emptySubstitution     -- 2
   | otherwise  = return $ l_v +-> r             -- 3

  unify (Var l_v)  r@(Fn _ _)                    -- 4
   | l_v `elem` vars_in r = Nothing              -- 5
   | otherwise            = return $ l_v +-> r   -- 6

  unify l@(Fn _ _) (Var r_v)                     -- 7
   | r_v `elem` vars_in l = Nothing              -- 8
   | otherwise            = return $ r_v +-> l   -- 9

  unify (Fn l_n l_ts) (Fn r_n r_ts)              -- 10
   | l_n /= r_n  = Nothing                        -- 11
   | otherwise  = unify l_ts r_ts                -- 12



infixl 8 <->

-- |Unify two unifiable terms 
--
-- >>>  (Fn "install" [ Var (VarName "X") ]) <-> Fn "install" [ Fn "check" [ Var (VarName "Y") ]]
-- Just [X → check(Y)]
(<->) :: Term -> Term -> Maybe Subst
a <-> b = unify a b
