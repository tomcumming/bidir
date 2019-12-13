module InstantiationTests where

import Test.Hspec

import Instantiation
import qualified Context as Context
import Type as Type


tests = hspec $ do
    describe "Instantiate Left" $ do
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
            instLeft ctx alpha (Type.PolyAtom (Type.Ext beta)) `shouldBe` Right ctxOut

