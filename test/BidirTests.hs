module BidirTests (tests) where

import Test.Hspec
import Data.Either (isLeft)

import Bidir.Type as Type
import Bidir.Expr as Expr
import Bidir.Context as Context
import Bidir

tests = hspec $ do
    describe "Types example programs correctly" $ do
        testIdFunction
        testForallEscape

testIdFunction = do
    it "Types the id function" $ do
        let isId t = case t of
                        Right (Type.PolyArrow
                            (Type.PolyAtom (Type.Ext x))
                            (Type.PolyAtom (Type.Ext y))) | x == y -> True
                        _ -> False
        inferType [] (Expr.Abs "x" (Expr.Var "x"))
            `shouldSatisfy` isId

testForallEscape = do
    it "Does not allow variables to escape forall quantifier" $ do
        -- f : forall a. (forall b. b -> a) -> a
        let t1 = Type.Forall "a" $
                    Type.PolyArrow
                        (Type.Forall "b" $
                            Type.PolyArrow
                                (Type.PolyAtom (Type.Var "b"))
                                (Type.PolyAtom (Type.Var "a")))
                        (Type.PolyAtom (Type.Var "a"))

        let ctx = [ Context.TermVar "f" t1 ]
        let idExpr = Expr.Abs "x" (Expr.Var "x")
        let e = Expr.Ap (Expr.Var "f") idExpr
        inferType ctx e `shouldSatisfy` isLeft
