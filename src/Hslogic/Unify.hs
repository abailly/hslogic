{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Basic unification essentially copied verbatim from http://pragprog.com/magazines/2013-06/unification
module Hslogic.Unify where

import qualified Data.HashMap.Lazy as H

import           Data.List         (union, (\\))

import           Hslogic.Parse
import           Hslogic.Types

class ContainsVars t where
  vars_in :: t -> [VarName]


instance ContainsVars a => ContainsVars [a] where
  vars_in xs = foldl union [] [ vars_in x | x <- xs ]

instance ContainsVars Term where
  vars_in (Var v)   = [v]
  vars_in (Fn _ ts) = vars_in ts

instance ContainsVars Formula where
  vars_in (T t)      = vars_in t
  vars_in (t :-> t') = vars_in t ++ vars_in t'

class Substitution s where
  lookup_var        :: s -> VarName -> Maybe Term
  emptySubstitution :: s
  (+->)             :: VarName -> Term -> s
  extend_with       :: s -> s -> s
  -- |Restricts the substitution to the given variables
  (-/-)             :: s -> [VarName] -> s

instance Substitution Subst where
  emptySubstitution = Subst H.empty
  lookup_var        = flip H.lookup . substMap
  v +-> t           = Subst $ H.singleton v t
  extend_with s s'  = Subst $ H.union (substMap s) (substMap s')
  s -/- vs          = Subst $ H.filterWithKey (\ k _ -> k `elem` vs) (substMap s)

-- |Renames bound and free variables of a clause to fresh variables
--
-- >>> fresh 1 (clause "foo(X) <= bar(Z), X, quux(Z), Y.")
-- (4,foo(X1) <= bar(X2), X1, quux(X2), X3.)
fresh :: Int -> Clause -> (Int, Clause)
fresh count (Clause ch cps) = let bound = vars_in ch
                                  free  = vars_in cps \\ bound
                                  count' = count + (length bound + length free)
                                  freshvars = map (Var . mk_var . ('X' :) . show) [count .. count']
                                  mapping   = zipWith (+->) (bound ++ free) freshvars :: [Subst]
                                  subst     = foldl extend_with emptySubstitution mapping
                              in  (count', Clause (subst `apply` ch) (map (subst `apply`) cps))

class Substitutible t where
  apply :: Substitution s => s -> t -> t


instance Substitutible Term where
  apply ss (Var n)   = case lookup_var ss n of
    Just t  -> t
    Nothing -> Var n
  apply ss (Fn n ts) = Fn n (apply ss ts)

instance Substitutible Formula where
  apply ss (T t)      = T $ apply ss t
  apply ss (t :-> t') = apply ss t :-> apply ss t'

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
   | l_v == r_v = return $ emptySubstitution      -- 2
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
-- Just [X -> check(Y)]
(<->) :: Term -> Term -> Maybe Subst
(<->) = unify
