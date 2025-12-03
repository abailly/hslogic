module Hslogic.RunSpec where

import Data.Function ((&))
import Hslogic.Options (Options (..), parseArgs)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, property, (===))

spec :: Spec
spec =
  prop "parses files to load option" $ \fileNames ->
    let args = foldr (\f acc -> "-l" : f : acc) [] fileNames
     in case parseArgs args of
          Right (Load files) -> files === fileNames
          Left err ->
            property False
              & counterexample ("expected '" <> show fileNames <> "' but got error: '" <> err <> "'")
