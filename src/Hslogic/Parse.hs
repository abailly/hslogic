module Hslogic.Parse where
import Hslogic.Types
import Text.ParserCombinators.Parsec

doParse :: Parser a -> String -> Either String a
doParse p input = case parse p "" input of
  Left e  -> Left $ show e
  Right v -> Right v

-- | Parse a single variable
--
-- >>> parseTest var "X"
-- X
var :: Parser Term
var = do
  i <- upper
  s <- many alphaNum
  return $ Var (VarName (i : s))

-- | Parse a predicate/function.
--
-- >>> parseTest fun "foo"
-- foo
-- >>> parseTest fun "foo(X)"
-- foo(X)
-- >>> parseTest fun "foo ( Xj, ll )"
-- foo(Xj,ll)
fun :: Parser Term
fun = do
  i <- lower
  s <- many alphaNum
  args <- spaces >> (funArgs <|> return [])
  return $ Fn (i:s) args
  where
    funArgs :: Parser [ Term ]
    funArgs = between (char '(' >> spaces)
                      (spaces >> char ')')
                      (termParser `sepBy`
                       (spaces >> char ',' >> spaces)) 


-- | Parse a term.
--
-- >>> parseTest termParser " foo"
-- foo
termParser :: Parser Term
termParser = spaces >> (var <|> fun)

-- |Parse a clause
--
-- >>> parseTest clauseParser "foo(X) -: bar, qix(X)."
-- foo(X) -: bar, qix(X).
clauseParser :: Parser Clause
clauseParser = do
  h <- spaces >> termParser
  spaces
  cls <- premises <|> return []
  char '.'
  return $ Clause h cls
    where
      premises :: Parser [Term]
      premises = string "-:" >> spaces >> (termParser `sepBy`
                                           (spaces >> char ',' >> spaces))

fromRight :: Either a b -> b
fromRight (Right b) = b

term :: String -> Term
term = fromRight . doParse termParser

clause :: String -> Clause
clause = fromRight . doParse clauseParser

