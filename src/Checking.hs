module Checking (check) where

import Control.Monad.Trans (lift)

import TI
import qualified Expr as Expr
import qualified Type as Type
import Context (Ctx, splitTwo)
import qualified Context as Context
import Inference
import Subtyping

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
    (e, Type.Forall x t) -> do
        ctx2 <- check (Context.TypeVar x:ctx) e t
        (_, ctx3) <- lift $ maybe
            (Left "check forall")
            Right
            (splitTwo (Context.TypeVar "a") ctx2)
        return ctx3
    (e, t) -> do
        (ctx2, t2) <- infer ctx e
        subtype ctx2 (Context.apply ctx2 t2) (Context.apply ctx2 t)
