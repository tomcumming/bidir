module SubtypingTests where

import Test.Hspec
import Control.Monad.State (evalStateT)

import Subtyping
import qualified Context as Context
import Type as Type

runSubtyping :: Context.Ctx -> Type.Poly -> Type.Poly -> Either String Context.Ctx
runSubtyping ctx t1 t2 = evalStateT (subtype ctx t1 t2) 10

tests = hspec $ do
    describe "Subtyping" $ do
        testUnitUnit

testUnitUnit = it "handles unit unit" $ do
    let ctx = [Context.TypeVar "a"]
    runSubtyping ctx (Type.PolyAtom Type.Unit) (Type.PolyAtom Type.Unit)
        `shouldBe` Right ctx
