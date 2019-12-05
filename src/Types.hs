module Types where

import AbsLatte

data Location =
  FunName Ident | 
  ClassMethod String String


instance Show Location where
  show what = case what of
    FunName (Ident id) -> "function " ++ id
    _ -> "TODO"
