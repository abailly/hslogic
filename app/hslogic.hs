import Data.Functor (void)
import Hslogic.Run (ioEffects, run)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  void $ getArgs >>= run ioEffects
