import Test.Hspec

import Bidir

import qualified InstantiationTests as InstantiationTests
import qualified SubtypingTests as SubtypingTests

main :: IO ()
main = do
    InstantiationTests.tests
    SubtypingTests.tests
