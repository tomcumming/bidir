module CheckingTests where

import Test.Hspec
import Control.Monad.State (evalStateT)
import Data.Either (isLeft)

import Bidir.Checking
import qualified Bidir.Context as Context
import Bidir.Type as Type
import Bidir.Expr as Expr

runChecking :: Context.Ctx -> Expr.Expr -> Type.Poly -> Either String Context.Ctx
runChecking ctx e t = evalStateT (check ctx e t) 10

tests = hspec $ do
    describe "Typechecking" $ do
        testUnit
        testArrow
        testForall

testUnit = do
    it "handles unit" $ do
        runChecking [] Expr.Unit (Type.PolyAtom Type.Unit)
            `shouldBe` Right []

testArrow = do
    context "when checking arrow" $ do
        it "handles \\x -> ()" $ do
            let ctx = [ Context.TypeVar "a" ]
            let e = Expr.Abs "x" (Expr.Unit)
            let t = Type.PolyArrow (Type.PolyAtom Type.Unit) (Type.PolyAtom Type.Unit)
            runChecking ctx e t `shouldBe` Right ctx
        it "handles id" $ do
            let ctx = [ Context.TypeVar "a" ]
            let e = Expr.Abs "x" (Expr.Var "x")
            let t = Type.PolyArrow (Type.PolyAtom Type.Unit) (Type.PolyAtom Type.Unit)
            runChecking ctx e t `shouldBe` Right ctx

testForall = do
    context "when checking forall" $ do
        it "handles checking polymorphic id" $ do
            let ctx = [ Context.TypeVar "c" ]
            let e = Expr.Abs "x" (Expr.Var "x")
            let t = Type.Forall
                        "a"
                        (Type.PolyArrow
                            (Type.PolyAtom (Type.Var "a"))
                            (Type.PolyAtom (Type.Var "a")))
            runChecking ctx e t `shouldBe` Right ctx
