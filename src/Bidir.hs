module Bidir where

import Control.Monad.State (evalStateT)

import qualified Bidir.Type as Type
import qualified Bidir.Expr as Expr
import qualified Bidir.Context as Context
import Bidir.Checking

type Error = String

inferType :: Context.Ctx -> Expr.Expr -> Either Error Type.Poly
inferType ctx e = do
    (ctx2, t) <- evalStateT (infer ctx e) 10
    return $ Context.apply ctx2 t
