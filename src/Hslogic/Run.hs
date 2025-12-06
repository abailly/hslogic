{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hslogic.Run where

import Control.Exception (IOException)
import Control.Exception.Safe (MonadCatch, throw, try)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (MonadTrans (lift), StateT, execStateT, get, gets, modify, put, runStateT)
import Data.Function ((&))
import Data.Functor (($>))
import Hslogic.Options (Options (..), parseArgs)
import Hslogic.Parse (clauseParser, doParse, formulaParser)
import Hslogic.Solve (Clauses, contextWith, ctxTrace, runSolver, solutions, solver)
import Hslogic.Types (Clause, PrettyPrintable (pp), Subst)
import Hslogic.Unify (emptySubstitution)
import System.Console.ANSI (Color (Green, Red), ColorIntensity (Dull), ConsoleLayer (Foreground), SGR (..), setSGR)

data CurrentState = C [Clause] [Subst]

clauses :: CurrentState -> [Clause]
clauses (C c _) = c

color :: (Monad m) => Effects m -> Color -> m a -> m a
color Effects {textEffect} c io = do
  textEffect [SetColor Foreground Dull c]
  a <- io
  textEffect []
  return a

putStrLnPretty :: (PrettyPrintable a) => Effects m -> a -> m ()
putStrLnPretty Effects {writeLine} = writeLine . show . pp

displaySolution :: (Monad m) => Effects m -> StateT CurrentState m ()
displaySolution eff = do
  C cls sols <- get
  case sols of
    s : ss -> do
      lift $ color eff Green $ putStrLnPretty eff s
      put $ C cls ss
    [] -> pure ()

trySolving :: (Monad m) => Effects m -> String -> StateT CurrentState m ()
trySolving eff s = do
  cls <- gets clauses
  case doParse formulaParser s of
    Left e -> lift $ color eff Red $ writeLine eff e
    Right t -> do
      case solutions cls [t] of
        [] -> lift $ color eff Red $ writeLine eff "failure"
        sols -> do
          let substs = filter (/= emptySubstitution) sols
              message =
                if null substs
                  then "success"
                  else "success. Type [Enter] to list solutions."
          lift $ color eff Green $ writeLine eff message
          modify (\(C cls' ss) -> C cls' (ss <> substs))

extendClauses :: (Monad m) => Effects m -> String -> StateT CurrentState m ()
extendClauses eff c = do
  C cls sol <- get
  clauses' <- case doParse clauseParser c of
    Left e -> lift (color eff Red $ writeLine eff e) >> return cls
    Right v -> return $ cls ++ [v]
  put $ C clauses' sol

data Effects m = Effects
  { writeString :: String -> m (),
    writeLine :: String -> m (),
    readLine :: m String,
    textEffect :: [SGR] -> m (),
    loadFile :: FilePath -> m String
  }

ioEffects :: Effects IO
ioEffects =
  Effects
    { writeString = putStr,
      writeLine = putStrLn,
      readLine = getLine,
      textEffect = setSGR,
      loadFile = readFile
    }

data Loop = Stop | Continue

instance Semigroup Loop where
  Stop <> _ = Stop
  _ <> Stop = Stop
  Continue <> Continue = Continue

instance Monoid Loop where
  mempty = Continue

loop :: (MonadCatch m) => Effects m -> StateT CurrentState m ()
loop eff = do
  lift $ writeString "> "
  l <- try @_ @IOException $ lift readLine
  l & either (const $ pure Stop) (interpret eff) >>= \case
    Stop -> pure ()
    Continue -> loop eff
  where
    Effects {readLine, writeString} = eff

interpret :: (MonadCatch m) => Effects m -> String -> StateT CurrentState m Loop
interpret eff input = do
  C cls sol <- get
  case input of
    "?" -> lift $ color eff Green $ mapM_ (writeLine . show . pp) cls $> Continue
    "exit" -> lift (color eff Green (writeLine "Bye!")) $> Stop
    "" -> displaySolution eff $> Continue
    ('?' : '-' : q) -> trySolving eff q $> Continue
    c -> extendClauses eff c $> Continue
  where
    Effects {writeLine} = eff

doLoadFile :: (Monad m, MonadCatch m) => Effects m -> FilePath -> StateT CurrentState m Loop
doLoadFile eff file = do
  content <- lines <$> lift (loadFile eff file)
  mconcat <$> mapM (interpret eff) content

-- * Top-level run

run :: (MonadCatch m) => Effects m -> [String] -> m CurrentState
run eff args = do
  let Load files = either (\err -> error $ "Failed to decode arguments, " <> err) id $ parseArgs args

  state <- execStateT (mapM_ (doLoadFile eff) files) (C [] [])

  execStateT (loop eff) state
