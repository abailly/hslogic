module Hslogic.Solve where

import Data.List
import Data.Tuple.Select
import Data.Maybe
import Hslogic.Types
import Hslogic.Unify
import Hslogic.Parse

type Clauses = [ Clause ]

-- | Maintains a list of goals and a context for generating more goals
data Goal = Goal {
  varIndex :: Int              -- ^Index to use for assigning fresh variables, should increase monotonically to
                              -- ensure uniquenesse of variables name across a run.
  , goalSubstitution :: Subst  -- ^ Substitution leading to that goal
  , goals  :: [Formula]        -- ^ Current list of goals
  , assumptions :: Clauses     -- ^ Clauses used in proving this goal, might be reduced to single clause in case of
                              -- resolution through unification of terms
  } | EmptyGoal               -- ^ An unsolvable goal, eg. 'false'
    deriving (Eq, Show)


-- | Select the first clause s.t. its head unifies with the given Term.
--
-- This is the heart of the solver where it selects a unifiable clause for the given term among
-- all given 'Clauses'.
--
-- First, the clause is instantiated with all its variables /fresh/, eg. s.t. they do not occur anywhere
-- else in the program. Currently this is implemented naively using the 'Int' given parameter which is
-- and index that is incremented for each new fresh variable.
--
-- If the clause's head can be unified with given 'Term' then 'selectClause' returns a new 'Goal'
-- containing an updated variables index, the 'Substitution' resulting from the unification of terms
-- and the clause's premises with inferred substitution applied. It also returns the remaining list of clauses
-- not applied, and the prefix of clauses skipped. eg. it returns the list of clauses split in half with the
-- selected clause removed.
--
-- >>> selectClause 1 (map clause ["foo(bar) <= qix.", "foo(X) <= baz (X)."]) (term "foo(foo)")
-- Just (Goal {varIndex = 2, goalSubstitution = [X1 -> foo], goals = [baz(foo)], assumptions = [foo(X) <= baz(X).]},[])
--
-- >>> selectClause 1 (map clause ["foo(bar) <= qix.", "foo(X) <= baz (X)."]) (term "foo(X)")
-- Just (Goal {varIndex = 1, goalSubstitution = [X -> bar], goals = [qix], assumptions = [foo(bar) <= qix.]},[foo(X) <= baz(X).])
--
selectClause :: Int -> Clauses -> Term -> Maybe (Goal, Clauses)
selectClause _ []      _       = Nothing
selectClause i (c:cs)  t = let (i',c') = fresh i c
                            in case clauseHead c' <-> t of
                              Nothing -> selectClause i cs t
                              Just s  -> Just (Goal i'  s  (map T (s `apply` clausePremises c')) [c],cs)

mkClauses :: [ String ] -> Clauses
mkClauses = map (fromRight . doParse clauseParser)

sampleClauses :: Clauses
sampleClauses = mkClauses [
                    "foo(bar) <= qix.",
                    "foo(baz) <= quux.",
                    "foo(X)   <= baz (X).",
                    "baz(quux).",
                    "qix."
                    ]

sampleClauses2 :: Clauses
sampleClauses2 = mkClauses [
                    "took(sue,cs120).",
                    "took(sue,cs121).",
                    "took(sue,cs240).",
                    "took(bob,cs120).",
                    "took(bob,cs370).",
                    "canGraduate(X) <= took(X,cs120), took(X,cs121), took(X,cs240), took(X,cs370)."
                    ]

cakes :: Clauses
cakes = mkClauses [
  "have(X) <= X.",
  "eat(X)  <= X."
  ]

data Logic = Intuitionistic | Linear deriving (Eq, Show)
  
-- |Solves a list of terms (a query) providing a substitution for any variable occuring in it
-- if it succeeds.
--
-- >>> map goalSubstitution $ filter (/= EmptyGoal) $ solve Intuitionistic sampleClauses (Goal 1 emptySubstitution [formula "foo(X)"] [])
-- [[X -> bar],[X1 -> X,X -> quux]]
--
-- >>> map goalSubstitution $ filter (/= EmptyGoal) $ solve Intuitionistic sampleClauses (Goal 1 emptySubstitution [formula "foo(qix)"] [])
-- []
--
-- >>> map goalSubstitution $ filter (/= EmptyGoal) $ solve Intuitionistic sampleClauses (Goal 1 emptySubstitution [formula "foo(quux)"] [])
-- [[X1 -> quux]]
--
-- >>> map goalSubstitution $ filter (/= EmptyGoal) $ solve Intuitionistic sampleClauses (Goal 1 emptySubstitution [formula "qix"] [])
-- [[]]
-- >>> filter (/= EmptyGoal) $ solve Linear cakes (Goal 1 emptySubstitution [formula "cake -o have(cake),eat(cake)"] [])
-- []
solve :: Logic -> Clauses -> Goal -> [Goal]
solve _ _  c@(Goal _ _ [] _)       = [c]
solve _ []  _                      = [EmptyGoal]
solve _ cs (Goal i s ((l :-> f):ts) us)
                                   = solve Intuitionistic ((Clause l []):cs) (Goal i s (f:ts) us)
solve _ cs (Goal i s ((l :-@ f):ts) us)
                                   = solve Linear         ((Clause l []):cs) (Goal i s (f:ts) us)
solve Intuitionistic cs (Goal i s ((l :* f):ts) us)
                                   = concat [ solve Intuitionistic cs (Goal i' s' (f:ts') us) | (Goal i' s' ts' _) <- solve Intuitionistic cs (Goal i s (T l:ts) us) ]
solve Linear cs (Goal i s ((l :* f):ts) us)  =  concat [ solve Linear (cs \\ us') (Goal i' s' (f:ts') us) | (Goal i' s' ts' us') <- solve Linear cs (Goal i s (T l:ts) us) ]
solve Intuitionistic cs (Goal i s terms@(T t:ts) us) =
  case selectClause i cs t of
    Just (Goal i' s' ts' [u],cs') -> let s''= s `extend_with` s'
                                       in
                                        solve Intuitionistic cs (Goal i' s'' (ts' ++ map (s'' `apply`) ts) (u:us))
                                        ++
                                        solve Intuitionistic cs' (Goal i' s terms us)
    Nothing               -> [ EmptyGoal ]
solve Linear cs (Goal i s terms@(T t:ts) us) =
  case selectClause i (cs \\ us) t of
    Just (Goal i' s' ts' [u],cs') -> let s''= s `extend_with` s'
                                     in
                                      solve Linear (cs \\ (u:us)) (Goal i' s'' (ts' ++ map (s'' `apply`) ts) (u:us))
                                      ++
                                      solve Linear cs' (Goal i' s terms us)
    Nothing               -> [ EmptyGoal ]


-- |Generate all solutions for given query against given clauses.
--
-- >>> solutions sampleClauses (map formula ["foo(X)", "baz(Y)"])
-- [[Y -> quux,X -> bar],[Y -> quux,X -> quux]]
--
-- >>> solutions sampleClauses (map formula ["foo(X)", "baz(X)"])
-- [[X -> quux]]
--
-- >>> solutions sampleClauses2 (map formula ["took(sue,cs370) => canGraduate(sue)"])
-- [[]]
--
-- >>> solutions cakes (map formula ["cake -o have(cake), eat(cake)"])
-- []
-- >>> solutions cakes (map formula ["cake => have(cake), eat(cake)"])
-- [[]]
solutions :: Clauses -> [Formula] -> [Subst]
solutions cs ts = let vars = vars_in ts
                  in  map ((-/- vars) . goalSubstitution) (filter (/= EmptyGoal) (solve Intuitionistic cs (Goal 1 emptySubstitution ts [])))
