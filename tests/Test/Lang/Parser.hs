
module Test.Lang.Parser
where

import Lang.Parser
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "parseLet" $ do
    it "does not parse lets with numeric identifiers" $ do
      (parseMaybe parseLet "(let ((y 2)) x)") `shouldBe` Nothing
      
