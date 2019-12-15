module Inference (infer) where

import Control.Monad.Trans (lift)

import TI
import qualified Expr as Expr
import qualified Type as Type
import Context (Ctx, splitTwo)
import qualified Context as Context

infer :: Ctx -> Expr.Expr -> TI (Ctx, Type.Poly)
infer ctx e = case e of
    Expr.Var x -> do
        t <- lift $ maybe
            (Left $ "Could not find var: " ++ x)
            Right
            (Context.lookup ctx x)
        return (ctx, t)
    Expr.Unit -> return (ctx, Type.PolyAtom Type.Unit)
