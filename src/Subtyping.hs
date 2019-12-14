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
    (Type.PolyAtom (Type.Ext x), t2) -> do
        lift $ when
            (splitTwo (Context.UnsolvedExt x) ctx == Nothing)
            (Left "subtype ext any ctx")
        lift $ when
            (Set.member x (Type.free t2))
            (Left "subtype ext any occurs")
        instLeft ctx x t2
