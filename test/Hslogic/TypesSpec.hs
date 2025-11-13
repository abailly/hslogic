{-# LANGUAGE OverloadedStrings #-}

module Hslogic.TypesSpec where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (runStateT)
import Hslogic.Parse (clause, formula, term)
import Hslogic.Solve (Goal (..), cakes, contextWith, courses, runSolver, sampleClauses, selectClause, solutions)
import Hslogic.Types (Term (..), VarName (..), pp, pretty, toList)
import Hslogic.Unify (fresh, (<->))
import Test.Hspec (Spec, describe, it, pending, shouldBe)

spec :: Spec
spec = do
  it "pretty-prints a term" $ do
    show (pretty (Fn "install" [Var (VarName "X")])) `shouldBe` "install(X)"
    show (pretty (Fn "copy" [])) `shouldBe` "copy"

  describe "unify" $ do
    it "unifies two unifiable terms" $ do
      fmap
        toList
        ( Fn "install" [Var (VarName "X")]
            <-> Fn "install" [Fn "check" [Var (VarName "Y")]]
        )
        `shouldBe` Just [("X", Fn "check" [Var "Y"])]

  describe "fresh variables" $ do
    it "renames bound and free variables to fresh ones" $ do
      fresh 1 (clause "foo(X) <= bar(Z), X, quux(Z), Y.")
        `shouldBe` (4, clause "foo(X1) <= bar(X2), X1, quux(X2), X3.")

  describe "selectClauses" $ do
    it "selects a unifiable clause and returns a goal" $ do
      let Just (goal, clauses) = selectClause 1 (map clause ["foo(bar) <= qix.", "foo(X) <= baz (X)."]) (term "foo(foo)")

      toList (goalSubstitution goal) `shouldBe` [("X1", Fn "foo" [])]

    it "selects another unifiable clause and returns a goal" $ do
      let Just (goal, clauses) = selectClause 1 (map clause ["foo(bar) <= qix.", "foo(X) <= baz (X)."]) (term "foo(X)")

      toList (goalSubstitution goal) `shouldBe` [("X", Fn "bar" [])]

  describe "solutions" $ do
    it "finds query solutions for sampleClauses" $ do
      let solution = solutions sampleClauses (map formula ["foo(X)", "baz(Y)"])
      map (show . pp) solution `shouldBe` ["[X -> bar,Y -> quux]", "[X -> quux,Y -> quux]"]

    it "finds single substitution matching all formulas" $ do
      let solution = solutions sampleClauses (map formula ["foo(X)", "baz(X)"])
      map (show . pp) solution `shouldBe` ["[X -> quux]"]

    it "validates formula using intuitionistic implication" $ do
      let solution = solutions cakes [formula "cake => have(cake), eat(cake)"]
      map (show . pp) solution `shouldBe` ["[]"]

    it "invalidates formula using linear implication" $ do
      let solution =
            solutions cakes [formula "cake -o have(cake), eat(cake)"]
      map (show . pp) solution `shouldBe` []

    it "validates formula with courses" $ do
      let solution = solutions courses [formula "took(sue,cs370) => canGraduate(sue)"]
      map (show . pp) solution `shouldBe` ["[]"]
