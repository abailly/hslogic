module Hslogic.Solve where

import Data.Tuple.Select
import Data.Maybe
import Hslogic.Types
import Hslogic.Unify
import Hslogic.Parse

type Clauses = [ Clause ]

-- | Select a clause s.t. its head unifies with the given Term.
--
-- >>> selectClause 1 (map clause ["foo(bar) <= qix.", "foo(X) <= baz (X)."]) (term "foo(foo)")
-- Just (2,[X1 -> foo],[baz(foo)],[])
--
-- >>> selectClause 1 (map clause ["foo(bar) <= qix.", "foo(X) <= baz (X)."]) (term "foo(X)")
-- Just (1,[X -> bar],[qix],[foo(X) <= baz(X).])
--
selectClause :: Int -> Clauses -> Term  -> Maybe (Int, Subst, [Term], Clauses)
selectClause _ []     _       = Nothing
selectClause i (c:cs) t = let (i',c') = fresh i c
                          in case clauseHead c' <-> t of
                            Nothing -> selectClause i cs t
                            Just s  -> Just (i', s, (s `apply` clausePremises c'), cs)

sampleClauses :: Clauses
sampleClauses = (map (fromRight . doParse clauseParser) [
                    "foo(bar) <= qix.",
                    "foo(baz) <= quux.",
                    "foo(X)   <= baz (X).",
                    "baz(quux).",
                    "qix."
                    ])

sampleClauses2 :: Clauses
sampleClauses2 = (map (fromRight . doParse clauseParser) [
                    "took(sue,cs120).",
                    "took(sue,cs121).",
                    "took(sue,cs240).",
                    "took(bob,cs120).",
                    "took(bob,cs370).",
                    "canGraduate(X) <= took(X,cs120), took(X,cs121), took(X,cs240), took(X,cs370)."
                    ])


-- |Solves a list of terms (a query) providing a substitution for any variable occuring in it
-- if it succeeds.
--
-- >>> map sel2 $ catMaybes $ solve sampleClauses (1,emptySubstitution, [formula "foo(X)"])
-- [[X -> bar],[X1 -> X,X -> quux]]
--
-- >>> map sel2 $ catMaybes $ solve sampleClauses (1,emptySubstitution, [formula "foo(qix)"])
-- []
--
-- >>> map sel2 $ catMaybes $ solve sampleClauses (1,emptySubstitution, [formula "foo(quux)"])
-- [[X1 -> quux]]
--
-- >>> map sel2 $ catMaybes $ solve sampleClauses (1,emptySubstitution, [formula "qix"])
-- [[]]
solve :: Clauses -> (Int, Subst, [Formula]) -> [Maybe (Int, Subst, [Formula])]
solve _  (i,s,[])           = [Just (i,s,[])]
solve []  _                 = [Nothing]
solve cs (i,s,terms@(T t:ts)) =
  case selectClause i cs t of
    Just (i', s',ts',cs') -> let s''= s `extend_with` s'
                            in
                              solve cs (i', s'', map T ts' ++ map (s'' `apply`) ts)
                              ++
                              solve cs' (i', s, terms)
    Nothing               -> [Nothing]
solve cs (i,s,(l :-> r):ts) = solve ((Clause l []):cs) (i,s,T r:ts)


-- |Generate all solutions for given query against given clauses
--
-- >>> solutions sampleClauses (map formula ["foo(X)", "baz(Y)"])
-- [[Y -> quux,X -> bar],[Y -> quux,X -> quux]]
--
-- >>> solutions sampleClauses (map formula ["foo(X)", "baz(X)"])
-- [[X -> quux]]
--
-- >>> solutions sampleClauses2 (map formula ["took(sue,cs370) => canGraduate(sue)"])
-- [[]]
solutions :: Clauses -> [Formula] -> [Subst]
solutions cs ts = let vars = vars_in ts
                  in  map ((-/- vars) . sel2 . fromJust) (filter (/= Nothing) (solve cs (1, emptySubstitution, ts)))
