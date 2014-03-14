module Hslogic.Solve where

import Hslogic.Types
import Hslogic.Unify
import Hslogic.Parse

type Clauses = [ Clause ]

fromRight :: Either a b -> b
fromRight (Right b) = b

-- | Select a clause s.t. its head unifies with the given Term
--
-- >>> selectClause (map (fromRight . doParse clauseParser) ["foo(bar) -: qix.", "foo(X) -: baz (X)."]) (fromRight $ doParse termParser "foo(foo)")
-- Just ([X → foo],[baz(foo)],[])
-- >>> selectClause (map (fromRight . doParse clauseParser) ["foo(bar) -: qix.", "foo(X) -: baz (X)."]) (fromRight $ doParse termParser "foo(X)")
-- Just ([X → bar],[qix],[foo(X) -: baz(X)])
selectClause :: Clauses -> Term -> Maybe (Subst, [Term], Clauses)
selectClause []     _ = Nothing
selectClause (c:cs) t = case clauseHead c <-> t of
  Nothing -> selectClause cs t
  Just s  -> Just (s, s `apply` clausePremises c, cs)

sampleClauses = (map (fromRight . doParse clauseParser) [
                    "foo(bar) -: qix.",
                    "foo(X)   -: baz (X).",
                    "baz(quux).",
                    "qix."
                    ])

term = fromRight . doParse termParser

-- |Solves a list of terms (a query) providing a substitution for any variable occuring in it
-- if it succeeds.
--
-- We can produce all substitutions for variables occuring in the query:
-- >>> map fst $ solve sampleClauses (emptySubstitution, [term "foo(X)"])
-- [[X → bar],[X → quux]]
--
-- If not solvable, then empty list is returned:
-- >>> solve sampleClauses (emptySubstitution, [term "foo(qix)"])
-- []
--
-- If auery term is ground then produces a list with no substitution
-- >>> solve sampleClauses (emptySubstitution, [term "foo(quux)"])
-- [([],[])]
solve :: Clauses -> (Subst, [Term]) -> [(Subst, [Term])]
solve _  (s,[])     = [(s,[])]
solve []  _         = []
solve cs (s,terms@(t:ts)) = case selectClause cs t of
  Just (s',ts',cs') -> let s''= s `extend_with` s'
                      in solve cs (s'', ts' ++ ts)
                         ++ solve cs' (s, terms)
  Nothing           -> []
