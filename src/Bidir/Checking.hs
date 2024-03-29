module Bidir.Checking (check, infer, inferAp) where

import Control.Monad.Trans (lift)

import Bidir.TI
import qualified Bidir.Expr as Expr
import qualified Bidir.Type as Type
import Bidir.Context (Ctx, splitTwo)
import qualified Bidir.Context as Context
import Bidir.Subtyping

check :: Ctx -> Expr.Expr -> Type.Poly -> TI Ctx
check ctx e t = case (e, t) of
    (Expr.Unit, Type.PolyAtom Type.Unit) -> return ctx
    (Expr.Abs x e, Type.PolyArrow t1 t2) -> do
        ctx2 <- check (Context.TermVar x t1:ctx) e t2
        (_, ctx3) <- lift $ maybe
            (Left "check arrow")
            Right
            (splitTwo (Context.TermVar x t1) ctx2)
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

infer :: Ctx -> Expr.Expr -> TI (Ctx, Type.Poly)
infer ctx e = case e of
    Expr.Var x -> do
        t <- lift $ maybe
            (Left $ "Could not find var: " ++ x)
            Right
            (Context.lookup ctx x)
        return (ctx, t)
    Expr.Unit -> return (ctx, Type.PolyAtom Type.Unit)
    Expr.Ann e t -> do
        ctx2 <- check ctx e t
        return (ctx2, t)
    Expr.Abs x e -> do
        ta <- fresh
        tr <- fresh
        let ctx2 = Context.TermVar "x" (Type.PolyAtom (Type.Ext ta))
                    : Context.Unsolved tr
                    : Context.Unsolved ta
                    : ctx
        ctx3 <- check ctx2 e (Type.PolyAtom (Type.Ext tr))
        (_, ctx4) <- lift $ maybe
            (Left "infer abs")
            Right
            (splitTwo (Context.TermVar "x" (Type.PolyAtom (Type.Ext ta))) ctx3)
        return (
            ctx4,
            Type.PolyArrow (Type.PolyAtom (Type.Ext ta)) (Type.PolyAtom (Type.Ext tr))
            )
    Expr.Ap e1 e2 -> do
        (ctx2, t1) <- infer ctx e1
        inferAp ctx2 (Context.apply ctx2 t1) e2

inferAp :: Ctx -> Type.Poly -> Expr.Expr -> TI (Ctx, Type.Poly)
inferAp ctx t e = case t of
    Type.PolyArrow t1 t2 -> do
        ctx2 <- check ctx e t1
        return (ctx2, t2)
    Type.Forall x t -> do
        tv <- fresh
        let ctx2 = Context.Unsolved tv:ctx
        inferAp ctx2 (Type.applyVarSub t x (Type.PolyAtom (Type.Ext tv))) e
    Type.PolyAtom (Type.Ext x) -> do
        (ctx1, ctx2) <- lift $ maybe
            (Left "inferAp")
            Right
            (splitTwo (Context.Unsolved x) ctx)
        ta <- fresh
        tr <- fresh
        let ctx3 = ctx1 ++
                    [Context.Unsolved ta] ++
                    [Context.Unsolved tr] ++
                    [Context.Solved
                        x
                        (Type.MonoArrow
                            (Type.MonoAtom (Type.Ext ta))
                            (Type.MonoAtom (Type.Ext tr)))] ++
                    ctx2
        ctx4 <- check ctx3 e (Type.PolyAtom (Type.Ext ta))
        return (
            ctx4,
            Type.PolyArrow (Type.PolyAtom (Type.Ext ta)) (Type.PolyAtom (Type.Ext tr))
            )
    t -> lift $ Left $ "Can not apply to: " ++ show t
