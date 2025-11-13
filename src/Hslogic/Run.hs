{-# LANGUAGE ScopedTypeVariables #-}

module Hslogic.Run where

import Control.Exception (IOException, catch)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (StateT, get, put, runStateT)
import Hslogic.Parse (clauseParser, doParse, formulaParser)
import Hslogic.Solve (Clauses, contextWith, ctxTrace, runSolver, solver)
import Hslogic.Types (Clause, PrettyPrintable (pp), Subst)
import System.Console.ANSI (Color (Green, Red), ColorIntensity (Dull), ConsoleLayer (Foreground), SGR (..), setSGR)
import System.Exit (exitSuccess)

data CurrentState = C [Clause] [Subst]

color :: (MonadIO m) => Color -> m a -> m a
color c io = do
  liftIO $ setSGR [SetColor Foreground Dull c]
  a <- io
  liftIO $ setSGR []
  return a

putStrLnPretty :: (PrettyPrintable a) => a -> IO ()
putStrLnPretty = print . pp

displaySolution :: Clauses -> [Subst] -> StateT CurrentState IO ()
displaySolution clauses (s : ss) = do
  color Green $ liftIO $ putStrLnPretty s
  put $ C clauses ss
displaySolution _ _ = color Red $ liftIO $ putStrLn "??"

trySolving :: Clauses -> String -> StateT CurrentState IO ()
trySolving clauses s = case doParse formulaParser s of
  Left e -> color Red $ liftIO (putStrLn e)
  Right t ->
    let (sols, ctx) = runIdentity $ runStateT (runSolver $ solver [t]) (contextWith clauses)
     in displaySolution clauses sols >> liftIO (print (ctxTrace ctx))

extendClauses :: Clauses -> [Subst] -> String -> StateT CurrentState IO ()
extendClauses clauses sol c = do
  clauses' <- case doParse clauseParser c of
    Left e -> color Red $ liftIO (putStrLn e) >> return clauses
    Right v -> return $ clauses ++ [v]
  put $ C clauses' sol

loop :: StateT CurrentState IO ()
loop = do
  C clauses sol <- get
  liftIO $ putStr "> "
  l <- liftIO $ catch getLine (\(_ :: IOException) -> exitSuccess)
  case l of
    "?" -> color Green $ liftIO $ mapM_ putStrLnPretty clauses
    "exit" -> liftIO $ color Green (putStrLn "Bye!") >> exitSuccess
    "" -> displaySolution clauses sol
    ('?' : '-' : q) -> trySolving clauses q
    c -> extendClauses clauses sol c
  loop

run :: IO ()
run = void (runStateT loop (C [] []))
