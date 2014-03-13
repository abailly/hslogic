module Main(main) where

import Prelude hiding(getLine,putStr,putStrLn)
import System.Environment(getArgs)
import Hslogic.Parse
import Hslogic.Unify
import System.IO.UTF8
import System.IO(stdout,hSetBuffering,BufferMode(..))

loop :: IO ()
loop = do
  putStr "> "
  t <- getLine >>= return . parse
  putStrLn (show $ pp t)
  loop

main :: IO ()
main = hSetBuffering stdout NoBuffering >> loop
