module Hslogic.Parse where
import Hslogic.Unify
import Text.ParserCombinators.Parsec

parseTerm :: String -> Either String Term
parseTerm  input = case parse termParser "" input of
  Left e  -> Left $ show e
  Right v -> Right v

-- | Parse a single variable
--
-- >>> parseTest var "X"
-- Var X
var :: Parser Term
var = do
  i <- upper
  s <- many alphaNum
  return $ Var (VarName (i : s))

-- | Parse a function
--
-- >>> parseTest fun "foo"
-- Fn "foo" []
-- >>> parseTest fun "foo(X)"
-- Fn "foo" [Var X]
-- >>> parseTest fun "foo ( Xj, ll )"
-- Fn "foo" [Var Xj,Fn "ll" []]
fun :: Parser Term
fun = do
  i <- lower
  s <- many alphaNum
  args <- spaces >> (funArgs <|> return [])
  return $ Fn (i:s) args
  where
    funArgs :: Parser [ Term ]
    funArgs = between (char '(' >> spaces)
                      (spaces >> char ')' >> spaces)
                      (termParser `sepBy`
                       (spaces >> char ',' >> spaces)) 
  
termParser :: Parser Term
termParser = var <|> fun



