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
        instLArr
        instRAllL

runInstL :: Context.Ctx -> Type.ExtId -> Type.Poly -> Either String Context.Ctx
runInstL ctx x t = evalStateT (instLeft ctx x t) 10

runInstR :: Context.Ctx -> Type.Poly -> Type.ExtId -> Either String Context.Ctx
runInstR ctx t x = evalStateT (instRight ctx t x) 10

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
        runInstL ctx alpha (Type.PolyAtom (Type.Ext beta)) `shouldBe` Right ctxOut

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
        runInstL ctx alpha (Type.PolyAtom t) `shouldBe` Right ctxOut

instLAllR = do
    it "handles InstLAllR" $ do
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
        runInstL ctx alpha pt `shouldBe` Right ctxOut

instLArr = do
    it "handles InstLArr" $ do
        let alpha = 1 -- :: freshA -> freshR
        let freshR = 11
        let freshA = 10
        let ctx = [
                    Context.TypeVar "x",
                    Context.UnsolvedExt alpha,
                    Context.TypeVar "a",
                    Context.TypeVar "y"
                    ]
        let ctxOut = [
                        Context.TypeVar "x",
                        Context.SolvedExt
                            alpha
                            (Type.MonoArrow
                                (Type.MonoAtom (Type.Ext freshA))
                                (Type.MonoAtom (Type.Ext freshR))),
                        Context.SolvedExt freshA (Type.MonoAtom (Type.Var "a")),
                        Context.SolvedExt freshR (Type.MonoAtom Type.Unit),
                        Context.TypeVar "a",
                        Context.TypeVar "y"
                        ]
        let pt = Type.PolyArrow (Type.PolyAtom (Type.Var "a")) (Type.PolyAtom Type.Unit)
        runInstL ctx alpha pt `shouldBe` Right ctxOut

instRAllL = do
    it "handles InstRAllL" $ do
        let alpha = 1
        let beta = "b"
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
        let pt = Type.Forall beta (Type.PolyAtom Type.Unit)
        runInstR ctx pt alpha `shouldBe` Right ctxOut
