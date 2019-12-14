module SubtypingTests where

import Test.Hspec
import Control.Monad.State (evalStateT)
import Data.Either (isLeft)

import Subtyping
import qualified Context as Context
import Type as Type

runSubtyping :: Context.Ctx -> Type.Poly -> Type.Poly -> Either String Context.Ctx
runSubtyping ctx t1 t2 = evalStateT (subtype ctx t1 t2) 10

tests = hspec $ do
    describe "Subtyping" $ do
        testUnitUnit
        testVarVar
        testExtExt

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
            let ctx = [Context.TypeVar "b", Context.UnsolvedExt alpha, Context.TypeVar "c"]
            runSubtyping ctx t t `shouldBe` Right ctx
