module Checking where

import Control.Monad.Trans (lift)

import TI
import qualified Expr as Expr
import qualified Type as Type
import Context (Ctx, splitTwo)
import qualified Context as Context

check :: Ctx -> Expr.Expr -> Type.Poly -> TI Ctx
check ctx e t = case (e, t) of
    (Expr.Unit, Type.PolyAtom Type.Unit) -> return ctx
    (Expr.Abs x e, Type.PolyArrow t1 t2) -> do
        ctx2 <- check (Context.SolvedVar x t1:ctx) e t2
        (_, ctx3) <- lift $ maybe
            (Left "check arrow")
            Right
            (splitTwo (Context.SolvedVar x t1) ctx2)
        return ctx3
