import Test.DocTest
import System.Directory(setCurrentDirectory)

main :: IO ()
main = do
  -- assumes it is run at root of project directory
  setCurrentDirectory "src"
  doctest ["Hslogic.Solve"]
