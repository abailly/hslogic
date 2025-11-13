module Hslogic.TypesSpec where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (runStateT)
import Hslogic.Parse (formula)
import Hslogic.Solve (contextWith, defaultContext, findSolutions, runSolver, sampleClauses, solutions)
import Hslogic.Types (Term (..), VarName (..), pp, pretty)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  it "pretty-prints a term" $ do
    show (pretty (Fn "install" [Var (VarName "X")])) `shouldBe` "install(X)"
    show (pretty (Fn "copy" [])) `shouldBe` "copy"

  describe "solutions" $ do
    it "finds query solutions for sampleClauses" $ do
      let substs =
            runIdentity $
              runStateT
                ( runSolver $
                    solutions sampleClauses (map formula ["foo(X)", "baz(Y)"])
                )
                (contextWith sampleClauses)
      map (show . pp) (fst substs) `shouldBe` ["[X -> bar,Y -> quux]", "[X -> quux,Y -> quux]"]

    it "finds single substitution matching all formulas" $ do
      let substs = runIdentity $ runStateT (runSolver $ solutions sampleClauses (map formula ["foo(X)", "baz(X)"])) (contextWith sampleClauses)
      map (show . pp) (fst substs) `shouldBe` ["[X -> quux]"]

--
-- >>> solutions sampleClauses2 (map formula ["took(sue,cs370) => canGraduate(sue)"])
-- [[]]
--
-- >>> solutions cakes (map formula ["cake -o have(cake), eat(cake)"])
-- []
-- >>> solutions cakes (map formula ["cake => have(cake), eat(cake)"])
-- [[]]
