import           Text.Dot.Types.Arbitrary
import           Text.Dot.Types.Internal

import           Control.Exception        (evaluate)
import           Control.Monad            (forM_)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes  (monoid)


main :: IO ()
main = hspec $ do
    describe "Dot" $ do
        batch "is a Monoid" $ monoid (undefined :: Dot)

-- Recall:
-- type TestBatch = (String, [Test])
-- type Test = (String, Property)
batch :: String -> TestBatch -> Spec
batch str (name, ts) =
    describe str $
        forM_ ts $ \(tname, p) ->
            it tname $ property p


