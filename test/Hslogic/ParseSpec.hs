{-# LANGUAGE ScopedTypeVariables #-}
module Hslogic.ParseSpec where

import Data.Function ((&))
import Hslogic.Parse (doParse, termParser, clauseParser, formulaParser)
import Hslogic.Types (Deep (..), pretty, pp, Formula)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Testable (..), tabulate, (===))
import Hslogic.Types (Clause(..))

spec :: Spec
spec = do
  prop "parse is inverse to pretty (Term)" $ \t ->
    case doParse termParser (show (pretty t)) of
      Left _ -> property False
      Right t' -> t === t' & tabulate "depth" [show $ deep t]

  prop "parse is inverse to pretty (Clause)" $ \ (c :: Clause) ->
    case doParse clauseParser (show (pp c)) of
      Left _ -> property False
      Right c' -> c === c' & tabulate "depth" [show $ deep c]

  prop "parse is inverse to pretty (Formula)" $ \ (f :: Formula) ->
    case doParse formulaParser (show (pp f)) of
      Left _ -> property False
      Right f' -> f === f' & tabulate "depth" [show $ deep f]
