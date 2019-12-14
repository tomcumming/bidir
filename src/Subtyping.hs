module Subtyping where

import Control.Monad (when)
import Control.Monad.Trans (lift)

import Context (Ctx, splitTwo)
import qualified Context as Context
import qualified Type as Type
import TI

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
