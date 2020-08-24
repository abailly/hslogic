{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import           Control.Exception
import           Control.Monad.State
import           Prelude             hiding (getLine, putStr, putStrLn)
import           System.Console.ANSI
import           System.Exit
import           System.IO           (BufferMode (..), hSetBuffering, stdout)
import           System.IO.UTF8
import Prelude hiding(getLine,putStr,putStrLn)
import System.IO.UTF8
import System.IO(stdout,hSetBuffering,BufferMode(..))
import System.Console.ANSI
import System.Exit


import           Hslogic.Parse
import           Hslogic.Solve
import           Hslogic.Types

data CurrentState = C [Clause] [Subst]

color :: (MonadIO m) => Color -> m a -> m a
color c io = do
  liftIO $ setSGR [ SetColor Foreground Dull c ]
  a <- io
  liftIO $ setSGR []
  return a

putStrLnPretty :: (PrettyPrintable a) => a -> IO ()
putStrLnPretty = putStrLn . show . pp

displaySolution :: Clauses -> [Subst] -> StateT CurrentState IO ()
displaySolution clauses (s:ss) = do
  color Green $ liftIO $ putStrLnPretty s
  put $ C clauses ss
displaySolution  _      _      = color Red $ liftIO $ putStrLn "??"

trySolving :: Clauses -> String -> StateT CurrentState IO ()
trySolving clauses s = case doParse formulaParser s of
      Left e  -> color Red $ liftIO (putStrLn e)
      Right t -> let (sols,ctx) = runState (runSolver (solutions clauses [t])) (Context Intuitionistic clauses [])
                 in displaySolution clauses sols >> liftIO (putStrLn $ show (ctxTrace ctx))

extendClauses :: Clauses -> [Subst] -> String -> StateT CurrentState IO ()
extendClauses clauses sol c = do
      clauses' <- case doParse clauseParser c of
        Left e  -> color Red $ liftIO (putStrLn e) >> return clauses
        Right v -> return $ clauses ++ [v]
      put $ C clauses' sol

loop :: StateT CurrentState IO ()
loop = do
  C clauses sol <- get
  liftIO $ putStr "> "
  l <- liftIO $ catch getLine (\ (_ :: IOException) -> exitWith ExitSuccess)
  case l of
    "?"         -> color Green $ liftIO $ mapM_ putStrLnPretty clauses
    "exit"      -> liftIO $ color Green (putStrLn "Bye!") >> exitWith ExitSuccess
    ""          -> displaySolution clauses sol
    ('?':'-':q) -> trySolving clauses q
    c           -> extendClauses clauses sol c
  loop

main :: IO ()
main = hSetBuffering stdout NoBuffering >> runStateT loop (C [] []) >> return ()
