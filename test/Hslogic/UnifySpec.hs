{-# LANGUAGE OverloadedStrings #-}

module Hslogic.UnifySpec where

import Hslogic.Parse (clause)
import Hslogic.Types (Term (..), toList)
import Hslogic.Unify (fresh, (<=>), unify)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  it "unifies two unifiable terms" $ do
    Fn "install" [Var "X"] <=> Fn "install" [Fn "check" [Var "Y"]]
      `shouldBe` [("X", Fn "check" [Var "Y"])]

  it "fails to unify two terms" $ do
    Fn "install" [Var "X"] <=> Fn "foo" [Fn "bar" []]
      `shouldBe` []

  describe "fresh variables" $ do
    it "renames bound and free variables to fresh ones" $ do
      fresh 1 (clause "foo(X) :- bar(Z), X, quux(Z), Y.")
        `shouldBe` (4, clause "foo(X1) :- bar(X2), X1, quux(X2), X3.")
