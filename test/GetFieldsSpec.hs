module GetFieldsSpec where 

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "start GetFields module test" $ do
    describe "sample test" $ do
      it "add one to one is two" $ 
        2 `shouldBe` 2