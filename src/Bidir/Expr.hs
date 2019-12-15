module Bidir.Expr where

import qualified Bidir.Type as Type

type Id = String

data Expr =
    Var Id
  | Unit
  | Abs Id Expr
  | Ap Expr Expr
  | Ann Expr Type.Poly
  deriving Show
