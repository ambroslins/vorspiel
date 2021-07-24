module ListSpec where

import qualified List
import Test.Hspec
import Prelude (Int, Maybe (..), ($))

spec :: Spec
spec = do
  describe "head" $ do
    it "returns the first element" $ List.head [1, 2, 3] `shouldBe` (Just 1 :: Maybe Int)
    it "returns Nothing if the list is empty" $ List.head [] `shouldBe` (Nothing :: Maybe Int)
