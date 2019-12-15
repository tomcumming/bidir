module InferApTests (tests) where

import Test.Hspec
import Control.Monad.State (evalStateT)
import Data.Either (isLeft)

import Bidir.Checking
import qualified Bidir.Context as Context
import Bidir.Type as Type
import Bidir.Expr as Expr

runInference :: Context.Ctx -> Type.Poly -> Expr.Expr -> Either String (Context.Ctx, Type.Poly)
runInference ctx t e = evalStateT (inferAp ctx t e) 10

tests = hspec $ do
    describe "Infer applications" $ do
        testForall

testForall = do
    it "handles forall" $ do
        let alpha = 10
        let ctx = [ Context.TypeVar "c" ]
        let ctxOut = Context.Solved alpha (Type.MonoAtom Type.Unit):ctx
        let t = Type.Forall
                    "a"
                    (Type.PolyArrow
                        (Type.PolyAtom (Type.Var "a"))
                        (Type.PolyAtom (Type.Var "a")))
        runInference ctx t Expr.Unit `shouldBe` Right (ctxOut, Type.PolyAtom (Type.Ext alpha))
