{-# LANGUAGE LambdaCase #-}

module Hslogic.RunSpec where

import Control.Exception (throw, throwIO)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Function ((&))
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import Hslogic.Options (Options (..), parseArgs)
import Hslogic.Run (Effects (..), clauses, run)
import Hslogic.Solve (sampleClauses, sampleClausesString)
import System.IO.Error (eofErrorType, mkIOError)
import Test.Hspec (Spec, it, shouldBe, shouldReturn)
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

  it "prints 'failure' given query fails" $ do
    commands <- newIORef ["?- quux(X).", "exit"]
    strings <- newIORef []
    state <-
      run
        fakeEffects
          { readLine = readCommands commands,
            writeLine = \s -> atomicModifyIORef strings $ \strs -> (s : strs, ()),
            loadFile = \_ -> pure (unlines sampleClausesString)
          }
        ["--load", "somefile"]

    readIORef strings `shouldReturn` ["Bye!", "failure"]

  it "returns 'success' given query succeeds" $ do
    commands <- newIORef ["?- baz(quux).", "exit"]
    strings <- newIORef []
    state <-
      run
        fakeEffects
          { readLine = readCommands commands,
            writeLine = \s -> atomicModifyIORef strings $ \strs -> (s : strs, ()),
            loadFile = \_ -> pure (unlines sampleClausesString)
          }
        ["--load", "somefile"]

    readIORef strings `shouldReturn` ["Bye!", "success"]

  it "returns 'success' and can enumerate solution given query succeeds with 1 or more solutions" $ do
    commands <- newIORef ["?- baz(X).", "", "exit"]
    strings <- newIORef []
    state <-
      run
        fakeEffects
          { readLine = readCommands commands,
            writeLine = \s -> atomicModifyIORef strings $ \strs -> (s : strs, ()),
            loadFile = \_ -> pure (unlines sampleClausesString)
          }
        ["--load", "somefile"]

    readIORef strings `shouldReturn` ["Bye!", "[X -> quux]", "success. Type [Enter] to list solutions."]

readCommands :: IORef [String] -> IO String
readCommands ref =
  atomicModifyIORef ref $ \case
    [] -> throw $ mkIOError eofErrorType "" Nothing Nothing
    (line : lines) -> (lines, line)
