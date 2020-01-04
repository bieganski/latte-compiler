module Types where

import qualified AbsLatte as Abs

data Location =
  FunName Abs.Ident | 
  ClassMethod String String


instance Show Location where
  show what = case what of
    FunName (Abs.Ident id) -> "function " ++ id
    _ -> "TODO"
