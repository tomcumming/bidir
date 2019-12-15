module InferenceTests where

import Test.Hspec
import Control.Monad.State (evalStateT)
import Data.Either (isLeft)

import Inference
import qualified Context as Context
import Type as Type
import Expr as Expr

runInference :: Context.Ctx -> Expr.Expr -> Either String (Context.Ctx, Type.Poly)
runInference ctx e = evalStateT (infer ctx e) 10

tests = hspec $ do
    describe "Inference" $ do
        testUnit

testUnit = do
    it "Handles Unit" $ do
        let ctx = [ Context.TypeVar "a" ]
        runInference ctx Expr.Unit `shouldBe` Right (ctx, Type.PolyAtom Type.Unit)
