module Hslogic.Solve where

import Data.Tuple.Select
import Data.Maybe
import Hslogic.Types
import Hslogic.Unify
import Hslogic.Parse

type Clauses = [ Clause ]

-- | A resolution context
data Goal = Goal {
  varIndex :: Int             -- ^Index to use for assigning fresh variables, should increase monotonically to
                             -- ensure uniquenesse of variables name across a run.
  , goalSubstitution :: Subst  -- ^ Substitution applicable in this context
  , goals  :: [Formula]    -- ^ List of hypothesis in this context  
  } | EmptyGoal           -- ^ A context from which nothing can be inferred
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
-- Just (Goal {varIndex = 2, goalSubstitution = [X1 -> foo], goals = [baz(foo)]},[],[foo(bar) <= qix.])
--
-- >>> selectClause 1 (map clause ["foo(bar) <= qix.", "foo(X) <= baz (X)."]) (term "foo(X)")
-- Just (Goal {varIndex = 1, goalSubstitution = [X -> bar], goals = [qix]},[foo(X) <= baz(X).],[])
--
selectClause :: Int -> Clauses -> Term  -> Maybe (Goal, Clauses, Clauses)
selectClause i cs t = selectClause' i cs [] t

-- Helper function: A version of 'selectClause' with an accumulator
selectClause' :: Int -> Clauses -> Clauses -> Term -> Maybe (Goal, Clauses, Clauses)
selectClause' _ []     _   _       = Nothing
selectClause' i (c:cs) acc t = let (i',c') = fresh i c
                               in case clauseHead c' <-> t of
                                 Nothing -> selectClause' i cs (c:acc) t
                                 Just s  -> Just (Goal i'  s  (map T (s `apply` clausePremises c')),cs, reverse acc)

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
-- >>> map goalSubstitution $ filter (/= EmptyGoal) $ solve sampleClauses (Goal 1 emptySubstitution [formula "foo(X)"])
-- [[X -> bar],[X1 -> X,X -> quux]]
--
-- >>> map goalSubstitution $ filter (/= EmptyGoal) $ solve sampleClauses (Goal 1 emptySubstitution [formula "foo(qix)"])
-- []
--
-- >>> map goalSubstitution $ filter (/= EmptyGoal) $ solve sampleClauses (Goal 1 emptySubstitution [formula "foo(quux)"])
-- [[X1 -> quux]]
--
-- >>> map goalSubstitution $ filter (/= EmptyGoal) $ solve sampleClauses (Goal 1 emptySubstitution [formula "qix"])
-- [[]]
solve :: Clauses -> Goal -> [Goal]
solve _  c@(Goal _ _ []) = [c]
solve []  _                 = [EmptyGoal]
solve cs (Goal i s terms@(T t:ts)) =
  case selectClause i cs t of
    Just (Goal i' s' ts',cs',acc) -> let s''= s `extend_with` s'
                                        in
                                         solve cs (Goal i' s'' (ts' ++ map (s'' `apply`) ts))
                                         ++
                                         solve cs' (Goal i' s terms)
    Nothing               -> [ EmptyGoal ]
solve cs (Goal i s ((l :-> r):ts)) = solve ((Clause l []):cs) (Goal i s (T r:ts))


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
solutions :: Clauses -> [Formula] -> [Subst]
solutions cs ts = let vars = vars_in ts
                  in  map ((-/- vars) . goalSubstitution) (filter (/= EmptyGoal) (solve cs (Goal 1 emptySubstitution ts)))
