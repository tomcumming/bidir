module Subtyping where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import qualified Data.Set as Set

import Context (Ctx, splitTwo)
import qualified Context as Context
import qualified Type as Type
import TI
import Instantiation

subtype :: Ctx -> Type.Poly -> Type.Poly -> TI Ctx
subtype ctx t1 t2 = case (t1, t2) of
    (Type.PolyAtom (Type.Var x), Type.PolyAtom (Type.Var y)) | x == y -> do
        lift $ when
            (splitTwo (Context.TypeVar x) ctx == Nothing)
            (Left "subtype var")
        return ctx
    (Type.PolyAtom Type.Unit, Type.PolyAtom Type.Unit) -> return ctx
    (Type.PolyAtom (Type.Ext x), Type.PolyAtom (Type.Ext y)) | x == y -> do
        lift $ when
            (splitTwo (Context.UnsolvedExt x) ctx == Nothing)
            (Left "subtype ext")
        return ctx
    (Type.PolyArrow t1a t1r, Type.PolyArrow t2a t2r) -> do
        ctx2 <- subtype ctx t2a t1a
        subtype ctx2 (Context.apply ctx2 t1r) (Context.apply ctx2 t2r)
    (Type.Forall x t1, t2) -> do
        alpha <- fresh
        let ctx2 = Context.UnsolvedExt alpha:Context.Marker alpha:ctx
        ctx3 <- subtype ctx2 (Type.applyVarSub t1 x (Type.PolyAtom (Type.Ext alpha))) t2
        (_, ctx4) <- lift $ maybe
            (Left "subtype forallL")
            Right
            (splitTwo (Context.Marker alpha) ctx3)
        return ctx4
    (t1, Type.Forall x t2) -> do
        ctx2 <- subtype (Context.TypeVar x:ctx) t1 t2
        (_, ctx3) <- lift $ maybe
            (Left "subtype forallr")
            Right
            (splitTwo (Context.TypeVar x) ctx2)
        return ctx3
    (Type.PolyAtom (Type.Ext x), t2) -> do
        lift $ when
            (splitTwo (Context.UnsolvedExt x) ctx == Nothing)
            (Left "subtype ext any ctx")
        lift $ when
            (Set.member x (Type.free t2))
            (Left "subtype ext any occurs")
        instLeft ctx x t2
    (t1, Type.PolyAtom (Type.Ext x)) -> do
        lift $ when
            (splitTwo (Context.UnsolvedExt x) ctx == Nothing)
            (Left "subtype ext any ctx")
        lift $ when
            (Set.member x (Type.free t1))
            (Left "subtype ext any occurs")
        instRight ctx t1 x
