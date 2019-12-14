module Expr where

import qualified Type as Type

type Id = String

data Expr =
    Var Id
  | Unit
  | Abs Id Expr
  | Ap Expr Expr
  | Ann Expr Type.Poly
  deriving Show
