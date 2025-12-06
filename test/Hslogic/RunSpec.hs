module Hslogic.RunSpec where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Function ((&))
import Hslogic.Options (Options (..), parseArgs)
import Hslogic.Run (Effects (..), clauses, run)
import Hslogic.Solve (sampleClauses, sampleClausesString)
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, property, (===))

fakeEffects =
  Effects
    { writeString = const $ pure (),
      writeLine = const $ pure (),
      readLine = pure undefined,
      textEffect = const $ pure (),
      loadFile = const $ pure undefined
    }

spec :: Spec
spec = do
  prop "parses files to load option" $ \fileNames ->
    let args = foldr (\f acc -> "-l" : f : acc) [] fileNames
     in case parseArgs args of
          Right (Load files) -> files === fileNames
          Left err ->
            property False
              & counterexample ("expected '" <> show fileNames <> "' but got error: '" <> err <> "'")

  it "loads files passed as argument" $ do
    state <-
      run
        fakeEffects
          { readLine = pure "exit",
            loadFile = \_ -> pure (unlines sampleClausesString)
          }
        ["--load", "somefile"]

    clauses state `shouldBe` sampleClauses
