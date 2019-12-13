import Test.Hspec

import Bidir

import qualified InstantiationTests as InstantiationTests

main :: IO ()
main = do
    InstantiationTests.tests
