module Hslogic.Types where

data VarName = VarName String deriving (Eq, Read)

instance Show VarName where
  show (VarName v) = v

mk_var :: String -> VarName
mk_var = VarName

data Term
  = Var VarName
  | Fn String [Term]
  deriving (Eq,Read)

data Clause
  = Clause {
    clauseHead :: Term,
    clausePremises :: [Term]
    } deriving (Eq,Read)
