import Test.Hspec

import Bidir

import qualified InstantiationTests as InstantiationTests
import qualified SubtypingTests as SubtypingTests
import qualified CheckingTests as CheckingTests

main :: IO ()
main = do
    InstantiationTests.tests
    SubtypingTests.tests
    CheckingTests.tests
