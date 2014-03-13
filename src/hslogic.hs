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
  l <- getLine
  case parseTerm l of
    Left e  -> putStrLn e
    Right v -> putStrLn (show $ pp v)
  loop

main :: IO ()
main = hSetBuffering stdout NoBuffering >> loop
