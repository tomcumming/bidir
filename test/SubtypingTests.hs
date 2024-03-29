module SubtypingTests where

import Test.Hspec
import Control.Monad.State (evalStateT)
import Data.Either (isLeft)

import Bidir.Subtyping
import qualified Bidir.Context as Context
import Bidir.Type as Type

runSubtyping :: Context.Ctx -> Type.Poly -> Type.Poly -> Either String Context.Ctx
runSubtyping ctx t1 t2 = evalStateT (subtype ctx t1 t2) 10

tests = hspec $ do
    describe "Subtyping" $ do
        testUnitUnit
        testVarVar
        testExtExt
        testArrow
        testInstL
        testInstR
        testForallR
        testForallL

testUnitUnit = it "handles unit unit" $ do
    let ctx = [Context.TypeVar "a"]
    runSubtyping ctx (Type.PolyAtom Type.Unit) (Type.PolyAtom Type.Unit)
        `shouldBe` Right ctx

testVarVar = do
    context "when subtyping var var" $ do
        it "handles not in ctx" $ do
            let a = Type.PolyAtom (Type.Var "a")
            let ctx = [Context.TypeVar "b"]
            runSubtyping ctx a a `shouldSatisfy` isLeft
        it "handles in ctx" $ do
            let a = Type.PolyAtom (Type.Var "a")
            let ctx = [Context.TypeVar "b", Context.TypeVar "a", Context.TypeVar "c"]
            runSubtyping ctx a a `shouldBe` Right ctx

testExtExt = do
    context "when subtyping ext ext" $ do
        it "handles not in ctx" $ do
            let alpha = 1
            let t = Type.PolyAtom (Type.Ext alpha)
            let ctx = [Context.TypeVar "b"]
            runSubtyping ctx t t `shouldSatisfy` isLeft
        it "handles in ctx" $ do
            let alpha = 1
            let t = Type.PolyAtom (Type.Ext alpha)
            let ctx = [Context.TypeVar "b", Context.Unsolved alpha, Context.TypeVar "c"]
            runSubtyping ctx t t `shouldBe` Right ctx

testArrow = do
    it "handles arrow arrow" $ do
        let alpha = 1
        let beta = 2
        let ctx = [
                    Context.TypeVar "a",
                    Context.Unsolved alpha,
                    Context.Unsolved beta,
                    Context.TypeVar "b"
                    ]
        let ctxOut = [
                        Context.TypeVar "a",
                        Context.Solved alpha (Type.MonoAtom (Type.Var "b")),
                        Context.Solved beta (Type.MonoAtom Type.Unit),
                        Context.TypeVar "b"
                        ]
        let t1 = Type.PolyArrow (Type.PolyAtom (Type.Ext alpha)) (Type.PolyAtom Type.Unit)
        let t2 = Type.PolyArrow (Type.PolyAtom (Type.Var "b")) (Type.PolyAtom (Type.Ext beta))
        runSubtyping ctx t1 t2 `shouldBe` Right ctxOut

testInstL = do
    context "when subtyping ext any" $ do
        it "handles occurs" $ do
            let alpha = 1
            let ctx = [ Context.Unsolved alpha ]
            let t1 = Type.PolyAtom (Type.Ext alpha)
            let t2 = Type.PolyArrow t1 t1
            runSubtyping ctx t1 t2 `shouldSatisfy` isLeft
        it "handles good case" $ do
            let alpha = 1
            let ctx = [
                        Context.TypeVar "a",
                        Context.Unsolved alpha,
                        Context.TypeVar "b"
                        ]
            let ctxOut = [
                            Context.TypeVar "a",
                            Context.Solved alpha (Type.MonoAtom Type.Unit),
                            Context.TypeVar "b"
                            ]
            let t1 = Type.PolyAtom (Type.Ext alpha)
            let t2 = Type.PolyAtom Type.Unit
            runSubtyping ctx t1 t2 `shouldBe` Right ctxOut

testInstR = do
    context "when subtyping any ext" $ do
        it "handles occurs" $ do
            let alpha = 1
            let ctx = [ Context.Unsolved alpha ]
            let t1 = Type.PolyAtom (Type.Ext alpha)
            let t2 = Type.PolyArrow t1 t1
            runSubtyping ctx t2 t1 `shouldSatisfy` isLeft
        it "handles good case" $ do
            let alpha = 1
            let ctx = [
                        Context.TypeVar "a",
                        Context.Unsolved alpha,
                        Context.TypeVar "b"
                        ]
            let ctxOut = [
                            Context.TypeVar "a",
                            Context.Solved alpha (Type.MonoAtom Type.Unit),
                            Context.TypeVar "b"
                            ]
            let t1 = Type.PolyAtom (Type.Ext alpha)
            let t2 = Type.PolyAtom Type.Unit
            runSubtyping ctx t2 t1 `shouldBe` Right ctxOut

testForallR = do
    it "handles any forall" $ do
        let t1 = Type.PolyAtom Type.Unit
        let t2 = Type.Forall "a" t1
        let ctx = [ Context.TypeVar "c" ]
        runSubtyping ctx t1 t2 `shouldBe` Right ctx

testForallL = do
    it "handles forall any" $ do
        let t1 = Type.PolyAtom Type.Unit
        let t2 = Type.Forall "a" t1
        let ctx = [ Context.TypeVar "c" ]
        runSubtyping ctx t2 t1 `shouldBe` Right ctx
