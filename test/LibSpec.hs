module LibSpec where

import           Test.Hspec

spec :: Spec
spec =
  describe "Lib" $
    it "works" $
      True `shouldBe` True
