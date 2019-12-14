module InstantiationTests where

import Test.Hspec
import Control.Monad.State (evalStateT)

import Instantiation
import qualified Context as Context
import Type as Type

tests = hspec $ do
    describe "Instantiate Left" $ do
        instLReach
        instLSolve
        instLAllR

runInst :: Context.Ctx -> Type.ExtId -> Type.Poly -> Either String Context.Ctx
runInst ctx x t = evalStateT (instLeft ctx x t) 10

instLReach = do
    it "handles InstLReach" $ do
        let [alpha, beta] = [1, 2] :: [Type.ExtId]

        let ctx = [
                    Context.TypeVar "x",
                    Context.UnsolvedExt beta,
                    Context.TypeVar "y",
                    Context.UnsolvedExt alpha,
                    Context.TypeVar "z"
                    ]
        let ctxOut = [
                        Context.TypeVar "x",
                        Context.SolvedExt beta (Type.MonoAtom (Type.Ext alpha)),
                        Context.TypeVar "y",
                        Context.UnsolvedExt alpha,
                        Context.TypeVar "z"
                        ]
        runInst ctx alpha (Type.PolyAtom (Type.Ext beta)) `shouldBe` Right ctxOut

instLSolve = do
    it "handles InstLSolve" $ do
        let alpha = 1
        let t = Type.Var "a"
        let ctx = [
                    Context.TypeVar "x",
                    Context.UnsolvedExt alpha,
                    Context.TypeVar "y",
                    Context.TypeVar "a",
                    Context.TypeVar "z"
                    ]
        let ctxOut = [
                        Context.TypeVar "x",
                        Context.SolvedExt alpha (Type.MonoAtom t),
                        Context.TypeVar "y",
                        Context.TypeVar "a",
                        Context.TypeVar "z"
                        ]
        runInst ctx alpha (Type.PolyAtom t) `shouldBe` Right ctxOut

instLAllR = do
    it "handles InstLSolve" $ do
        let alpha = 1
        let ctx = [
                    Context.TypeVar "x",
                    Context.UnsolvedExt alpha,
                    Context.TypeVar "y"
                    ]
        let ctxOut = [
                        Context.TypeVar "x",
                        Context.SolvedExt alpha (Type.MonoAtom Type.Unit),
                        Context.TypeVar "y"
                        ]
        let pt = Type.Forall "b" (PolyAtom Type.Unit)
        runInst ctx alpha pt `shouldBe` Right ctxOut
