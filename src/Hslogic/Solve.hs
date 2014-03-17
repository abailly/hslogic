module Hslogic.Solve where

import Data.Tuple.Select
import Data.Maybe
import Hslogic.Types
import Hslogic.Unify
import Hslogic.Parse

type Clauses = [ Clause ]

-- | Select a clause s.t. its head unifies with the given Term
--
-- >>> selectClause 1 (map clause ["foo(bar) -: qix.", "foo(X) -: baz (X)."]) (term "foo(foo)")
-- Just (2,[X1 → foo],[baz(foo)],[])
--
-- >>> selectClause 1 (map clause ["foo(bar) -: qix.", "foo(X) -: baz (X)."]) (term "foo(X)")
-- Just (1,[X → bar],[qix],[foo(X) -: baz(X).])
selectClause :: Int -> Clauses -> Term -> Maybe (Int, Subst, [Term], Clauses)
selectClause _ []     _ = Nothing
selectClause i (c:cs) t = let (i',c') = fresh i c
                          in case clauseHead c' <-> t of
                            Nothing -> selectClause i cs t
                            Just s  -> Just (i', s, s `apply` clausePremises c', cs)

sampleClauses :: Clauses
sampleClauses = (map (fromRight . doParse clauseParser) [
                    "foo(bar) -: qix.",
                    "foo(X)   -: baz (X).",
                    "baz(quux).",
                    "qix."
                    ])

-- |Solves a list of terms (a query) providing a substitution for any variable occuring in it
-- if it succeeds.
--
-- We can produce all substitutions for variables occuring in the query:
-- >>> map sel2 $ catMaybes $ solve sampleClauses (1,emptySubstitution, [term "foo(X)"])
-- [[X → bar],[X1 → X,X → quux]]
--
-- If not solvable, then empty list is returned:
-- >>> map sel2 $ catMaybes $ solve sampleClauses (1,emptySubstitution, [term "foo(qix)"])
-- []
--
-- If auery term is ground then produces a list with no substitution
-- >>> map sel2 $ catMaybes $ solve sampleClauses (1,emptySubstitution, [term "foo(quux)"])
-- [[X1 → quux]]
solve :: Clauses -> (Int, Subst, [Term]) -> [Maybe (Int, Subst, [Term])]
solve _  (i,s,[])           = [Just (i,s,[])]
solve []  _                 = [Nothing]
solve cs (i,s,terms@(t:ts)) =
  case selectClause i cs t of
    Just (i', s',ts',cs') -> let s''= s `extend_with` s'
                            in solve cs (i', s'', ts' ++ map (s'' `apply`) ts)
                             ++ solve cs' (i', s, terms)
    Nothing           -> [Nothing]


-- |Generate all solutions for given query against given clauses
--
-- >>> solutions sampleClauses (map term ["foo(X)", "baz(Y)"])
-- [[Y → quux,X → bar],[Y → quux,X → quux]]
--
-- >>> solutions sampleClauses (map term ["foo(X)", "baz(X)"])
-- [[X → quux]]
solutions :: Clauses -> [Term] -> [Subst]
solutions cs ts = let vars = vars_in ts
                  in  map ((-/- vars) . sel2 . fromJust) (filter ((/= Nothing)) (solve cs (1, emptySubstitution, ts)))
