module InferenceTests where

import Test.Hspec
import Control.Monad.State (evalStateT)
import Data.Either (isLeft)

import Bidir.Checking
import qualified Bidir.Context as Context
import Bidir.Type as Type
import Bidir.Expr as Expr

runInference :: Context.Ctx -> Expr.Expr -> Either String (Context.Ctx, Type.Poly)
runInference ctx e = evalStateT (infer ctx e) 10

tests = hspec $ do
    describe "Inference" $ do
        testUnit
        testAnn
        testLambda

testUnit = do
    it "Handles Unit" $ do
        let ctx = [ Context.TypeVar "a" ]
        runInference ctx Expr.Unit `shouldBe` Right (ctx, Type.PolyAtom Type.Unit)

testAnn = do
    it "Infer the type of an annotated expression" $ do
        let alpha = 1
        let ctx = [
                    Context.TermVar "x" (Type.PolyAtom (Type.Ext alpha)),
                    Context.Unsolved alpha,
                    Context.TypeVar "a"
                    ]
        let ctxOut = [
                        Context.TermVar "x" (Type.PolyAtom (Type.Ext alpha)),
                        Context.Solved alpha (Type.MonoAtom Type.Unit),
                        Context.TypeVar "a"
                        ]
        runInference ctx (Expr.Ann (Expr.Var "x") (Type.PolyAtom Type.Unit))
            `shouldBe` Right (ctxOut, Type.PolyAtom Type.Unit)

testLambda = do
    it "Infers the type of a lambda" $ do
        let alpha = 10
        let beta = 11
        let ctx = [ Context.TypeVar "a" ]
        let ctxOut = [
                        Context.Solved beta (Type.MonoAtom Type.Unit),
                        Context.Unsolved alpha,
                        Context.TypeVar "a"
                        ]
        let e = Expr.Abs "x" Expr.Unit
        let t = Type.PolyArrow
                    (Type.PolyAtom (Type.Ext alpha))
                    (Type.PolyAtom (Type.Ext beta))
        runInference ctx e `shouldBe` Right (ctxOut, t)
