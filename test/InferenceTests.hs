module InferenceTests where

import Test.Hspec
import Control.Monad.State (evalStateT)
import Data.Either (isLeft)

import Checking
import qualified Context as Context
import Type as Type
import Expr as Expr

runInference :: Context.Ctx -> Expr.Expr -> Either String (Context.Ctx, Type.Poly)
runInference ctx e = evalStateT (infer ctx e) 10

tests = hspec $ do
    describe "Inference" $ do
        testUnit
        testAnn

testUnit = do
    it "Handles Unit" $ do
        let ctx = [ Context.TypeVar "a" ]
        runInference ctx Expr.Unit `shouldBe` Right (ctx, Type.PolyAtom Type.Unit)

testAnn = do
    it "Infer the type of an annotated expression" $ do
        let alpha = 1
        let ctx = [
                    Context.TermVar "x" (Type.PolyAtom (Type.Ext alpha)),
                    Context.UnsolvedExt alpha,
                    Context.TypeVar "a"
                    ]
        let ctxOut = [
                        Context.TermVar "x" (Type.PolyAtom (Type.Ext alpha)),
                        Context.SolvedExt alpha (Type.MonoAtom Type.Unit),
                        Context.TypeVar "a"
                        ]
        runInference ctx (Expr.Ann (Expr.Var "x") (Type.PolyAtom Type.Unit))
            `shouldBe` Right (ctxOut, Type.PolyAtom Type.Unit)

