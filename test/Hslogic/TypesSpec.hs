{-# LANGUAGE OverloadedStrings #-}

module Hslogic.TypesSpec where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (runStateT)
import Hslogic.Parse (clause, formula, term)
import Hslogic.Solve (Goal (..), cakes, contextWith, courses, runSolver, sampleClauses, selectClause, solutions, starWarsClauses)
import Hslogic.Types (Term (..), VarName (..), pp, pretty, toList)
import Hslogic.Unify (fresh, (<=>))
import Test.Hspec (Spec, describe, it, pending, pendingWith, shouldBe)

spec :: Spec
spec = do
  it "pretty-prints a term" $ do
    show (pretty (Fn "install" [Var (VarName "X")])) `shouldBe` "install(X)"
    show (pretty (Fn "copy" [])) `shouldBe` "copy"

  describe "unify" $ do
    describe "selectClauses" $ do
      it "selects a unifiable clause and returns a goal" $ do
        let Just (goal, clauses) = selectClause 1 (map clause ["foo(bar) :- qix.", "foo(X) :- baz (X)."]) (term "foo(foo)")

        toList (goalSubstitution goal) `shouldBe` [("X1", Fn "foo" [])]

      it "selects another unifiable clause and returns a goal" $ do
        let Just (goal, clauses) = selectClause 1 (map clause ["foo(bar) :- qix.", "foo(X) :- baz (X)."]) (term "foo(X)")

        toList (goalSubstitution goal) `shouldBe` [("X", Fn "bar" [])]

    describe "solutions" $ do
      it "finds query solutions for sampleClauses" $ do
        let solution = solutions sampleClauses (map formula ["foo(X)", "baz(Y)"])
        map (show . pp) solution `shouldBe` ["[X -> bar,Y -> quux]", "[X -> quux,Y -> quux]"]

      it "returns empty list of substitutions given no clause matches" $ do
        let solution = solutions sampleClauses [formula "quuz(X)"]
        map (show . pp) solution `shouldBe` []

      it "finds query solution for ground term for sampleClauses" $ do
        let solution = solutions sampleClauses [formula "baz(quux)"]
        map (show . pp) solution `shouldBe` ["[]"]

      it "finds single substitution matching all formulas" $ do
        let solution = solutions sampleClauses (map formula ["foo(X)", "baz(X)"])
        map (show . pp) solution `shouldBe` ["[X -> quux]"]

      it "validates formula using intuitionistic implication" $ do
        pendingWith "TODO: not implemented"
        let solution = solutions cakes [formula "cake => have(cake), eat(cake)"]
        map (show . pp) solution `shouldBe` ["[]"]

      it "invalidates formula using linear implication" $ do
        pendingWith "TODO: not implemented"
        let solution =
              solutions cakes [formula "cake -o have(cake), eat(cake)"]
        map (show . pp) solution `shouldBe` []

      it "validates formula with courses" $ do
        pendingWith "TODO: not implemented"
        let solution = solutions courses [formula "took(sue,cs370) => canGraduate(sue)"]
        map (show . pp) solution `shouldBe` ["[]"]

      it "finds grandchild solution for star wars" $ do
        let solution = solutions starWarsClauses [formula "grandchild(X, vader)"]
        map (show . pp) solution `shouldBe` ["[X -> kylo,Y -> leia,Z -> vader]"]
