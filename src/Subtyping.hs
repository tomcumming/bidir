module Subtyping where

import Context (Ctx)
import qualified Type as Type
import TI

subtype :: Ctx -> Type.Poly -> Type.Poly -> TI Ctx
subtype ctx t1 t2 = case (t1, t2) of
    (Type.PolyAtom Type.Unit,Type.PolyAtom Type.Unit) -> return ctx
