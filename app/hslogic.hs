import Hslogic.Run (run)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  run
