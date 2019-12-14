module Instantiation (InstError, instLeft, instRight) where

import Control.Monad (when)
import Control.Monad.Trans (lift)

import TI
import qualified Type as Type
import qualified Context as Context
import Context (Ctx, splitTwo, splitThree)

type InstError = String

instLeft :: Ctx -> Type.ExtId -> Type.Poly -> TI Ctx
instLeft ctx x t = case t of
    Type.Forall y t -> instForallL ctx x y t
    Type.PolyArrow t1 t2 -> instLArr ctx x t1 t2
    Type.PolyAtom (Type.Ext y) -> lift $ instExtExt ctx x y
    Type.PolyAtom t -> lift $ instAtom ctx x t

instRight :: Ctx -> Type.Poly -> Type.ExtId -> TI Ctx
instRight ctx t x = case t of
    Type.Forall y t -> instRAllL ctx y t x
    Type.PolyArrow t1 t2 -> instRArr ctx t1 t2 x
    Type.PolyAtom (Type.Ext y) -> lift $ instExtExt ctx x y
    Type.PolyAtom t -> lift $ instAtom ctx x t

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
    Nothing -> Left "instAtom"

instForallL :: Ctx -> Type.ExtId -> Type.Id -> Type.Poly -> TI Ctx
instForallL ctx x y t = do
    lift $ when (splitTwo (Context.UnsolvedExt x) ctx == Nothing) (Left "instForallL")
    ctx2 <- instLeft (Context.TypeVar y:ctx) x t
    (ctx3, ctx4) <- lift $ maybe
        (Left "instForallL tv")
        Right
        (splitTwo (Context.TypeVar y) ctx2)
    return ctx4

instLArr :: Ctx -> Type.ExtId -> Type.Poly -> Type.Poly -> TI Ctx
instLArr ctx x t1 t2 = do
    (ctx1, ctx2) <- lift $ maybe
        (Left "instLArr")
        Right
        (splitTwo (Context.UnsolvedExt x) ctx)
    ta <- fresh
    tr <- fresh
    let ctx3 = ctx1
                ++ [Context.SolvedExt
                    x
                    (Type.MonoArrow
                        (Type.MonoAtom (Type.Ext ta))
                        (Type.MonoAtom (Type.Ext tr)))]
                ++ [Context.UnsolvedExt ta]
                ++ [Context.UnsolvedExt tr]
                ++ ctx2
    ctx4 <- instRight ctx3 t1 ta
    instLeft ctx4 tr (Context.apply ctx4 t2)

instRAllL :: Ctx -> Type.Id -> Type.Poly -> Type.ExtId -> TI Ctx
instRAllL ctx y t x = do
    lift $ when (splitTwo (Context.UnsolvedExt x) ctx == Nothing) (Left "instRAllL")
    beta <- fresh
    let ctx2 = Context.UnsolvedExt beta:Context.Marker beta:ctx
    ctx3 <- instRight ctx2 (Context.apply ctx2 t) x
    (_, ctx4) <- lift $ maybe
        (Left "instRAllL")
        Right
        (splitTwo (Context.Marker beta) ctx3)
    return ctx4

instRArr :: Ctx -> Type.Poly -> Type.Poly -> Type.ExtId -> TI Ctx
instRArr ctx t1 t2 x  = do
    (ctx1, ctx2) <- lift $ maybe
        (Left "instRArr")
        Right
        (splitTwo (Context.UnsolvedExt x) ctx)
    ta <- fresh
    tr <- fresh
    let ctx3 = ctx1
                ++ [Context.SolvedExt
                    x
                    (Type.MonoArrow
                        (Type.MonoAtom (Type.Ext ta))
                        (Type.MonoAtom (Type.Ext tr)))]
                ++ [Context.UnsolvedExt ta]
                ++ [Context.UnsolvedExt tr]
                ++ ctx2
    ctx4 <- instLeft ctx3 ta t1
    instRight ctx4 (Context.apply ctx4 t2) tr
