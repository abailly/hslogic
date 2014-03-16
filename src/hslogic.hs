{-# LANGUAGE DoAndIfThenElse #-}
module Main(main) where

import Prelude hiding(getLine,putStr,putStrLn)
import System.Environment(getArgs)
import Control.Monad.State
import Hslogic.Types
import Hslogic.Parse
import Hslogic.Unify
import Hslogic.Solve
import System.IO.UTF8
import System.IO(stdout,hSetBuffering,BufferMode(..))

loop :: StateT [Clause] IO ()
loop = do
  clauses <- get 
  liftIO $ putStr "> "
  l <- lift getLine
  case l of
    "?"         -> liftIO $ mapM_ (putStrLn . show . pp) clauses
    ('?':'-':s) -> case doParse termParser s of
      Left e  -> liftIO (putStrLn e)
      Right t -> liftIO $ mapM_ (putStrLn . show . pp) (solutions clauses [t])
    c           -> do
      clauses' <- case doParse clauseParser c of
        Left e  -> liftIO (putStrLn e) >> return clauses
        Right v -> return $ clauses ++ [v]
      put clauses'
  loop

main :: IO ()
main = hSetBuffering stdout NoBuffering >> runStateT loop [] >> return ()
