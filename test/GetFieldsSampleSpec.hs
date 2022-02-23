module GetFieldsSampleSpec where 

import Test.Hspec

import GetFieldsSample 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "start GetFields module test" $ do
    describe "getFields test" $ do
      it "all field is ..." $ 
        getFieldsSample  `shouldBe` ["name","authors","date","isbm","price"]