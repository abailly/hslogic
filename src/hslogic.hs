{-# LANGUAGE DoAndIfThenElse #-}
module Main(main) where

import Prelude hiding(getLine,putStr,putStrLn)
import Control.Monad.State
import Hslogic.Types
import Hslogic.Parse
import Hslogic.Solve
import System.IO.UTF8
import System.IO(stdout,hSetBuffering,BufferMode(..))
import System.Console.ANSI

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


loop :: StateT CurrentState IO ()
loop = do
  C clauses sol <- get 
  liftIO $ putStr "> "
  l <- lift getLine
  case l of
    "?"         -> color Green $ liftIO $ mapM_ putStrLnPretty clauses
    ""          -> displaySolution clauses sol
    ('?':'-':s) -> case doParse termParser s of
      Left e  -> color Red $ liftIO (putStrLn e)
      Right t -> displaySolution clauses (solutions clauses [t])
    c           -> do
      clauses' <- case doParse clauseParser c of
        Left e  -> color Red $ liftIO (putStrLn e) >> return clauses
        Right v -> return $ clauses ++ [v]
      put $ C clauses' sol
  loop

main :: IO ()
main = hSetBuffering stdout NoBuffering >> runStateT loop (C [] []) >> return ()
