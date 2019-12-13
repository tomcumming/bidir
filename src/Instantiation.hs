module Instantiation where

import qualified Type as Type
import qualified Context as Context
import Context (Ctx, splitThree)

type InstError = String

instLeft :: Ctx -> Type.ExtId -> Type.Poly -> Either InstError Ctx
instLeft ctx x t = case t of
    Type.Forall y t -> Left "Forall"
    Type.PolyArrow t1 t2 -> Left "Arrow"
    Type.PolyAtom (Type.Ext y) -> instExtExt ctx x y

instExtExt :: Ctx -> Type.ExtId -> Type.ExtId -> Either InstError Ctx
instExtExt ctx a b = case (splitThree b a ctx , splitThree a b ctx) of
  (Just (ctx1, ctx2, ctx3), _) -> Right $ go ctx1 ctx2 ctx3 a b
  (_, Just (ctx1, ctx2, ctx3)) -> Right $ go ctx1 ctx2 ctx3 b a
  _ -> Left $ "instExtExt not in ctx"
  where
    go ctx1 ctx2 ctx3 a b = concat [
        ctx1,
        [Context.SolvedExt b (Type.MonoAtom $ Type.Ext a)],
        ctx2,
        [Context.UnsolvedExt a],
        ctx3
        ]
