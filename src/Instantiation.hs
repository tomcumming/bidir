module Instantiation where

import qualified Type as Type
import qualified Context as Context
import Context (Ctx, splitTwo, splitThree)

type InstError = String

instLeft :: Ctx -> Type.ExtId -> Type.Poly -> Either InstError Ctx
instLeft ctx x t = case t of
    Type.Forall y t -> Left "Forall"
    Type.PolyArrow t1 t2 -> Left "Arrow"
    Type.PolyAtom (Type.Ext y) -> instExtExt ctx x y
    Type.PolyAtom t -> instAtom ctx x t

instExtExt :: Ctx -> Type.ExtId -> Type.ExtId -> Either InstError Ctx
instExtExt ctx a b = do
    let (ae, be) = (Context.UnsolvedExt a, Context.UnsolvedExt b)
    case (splitThree be ae ctx , splitThree ae be ctx) of
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

instAtom :: Ctx -> Type.ExtId -> Type.Atom -> Either InstError Ctx
instAtom ctx x t = case splitTwo (Context.UnsolvedExt x) ctx of
    Just (ctx1, ctx2) -> do
        Context.validate ctx2 (Type.PolyAtom t)
        return $ concat [ctx1, [Context.SolvedExt x (Type.MonoAtom t)], ctx2]
