module Checking where

import TI
import qualified Expr as Expr
import qualified Type as Type
import Context (Ctx)
import qualified Context as Context

check :: Ctx -> Expr.Expr -> Type.Poly -> TI Ctx
check ctx e t = case (e, t) of
    (Expr.Unit, Type.PolyAtom Type.Unit) -> return ctx
