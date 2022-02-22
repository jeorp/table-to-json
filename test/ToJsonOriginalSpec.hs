{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ToJsonOriginalSpec where

import Control.Lens hiding ((:>))

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B 

import Data.Aeson
import Data.Extensible
import Default
import ToJsonOriginal

import Test.Hspec

type Obj = Record 
 '[
   "x" :> Int,
   "y" :> Int,
   "name" :> T.Text,
   "isActive" :> Bool
  ]

instance ToJSONOriginal Int where
  toJsonOriginal s = 
    let mv = decodeStrict (B.pack s) :: Maybe Value
    in case mv of 
        Just x -> fromJSON x 
        Nothing -> Error "not int"


instance ToJSONOriginal T.Text where
  toJsonOriginal s = 
    let mv = if s == "null" 
        then Just Null 
        else decodeStrict (T.encodeUtf8 $ T.pack $ "\"" <> s <> "\"") :: Maybe Value
    in case mv of 
        Just x -> fromJSON x 
        Nothing -> Error "not text"

instance ToJSONOriginal Bool where
  toJsonOriginal s = 
    let validate = (\case "t" -> "true"; "f" -> "false"; _ -> "null")
        mv = if s == "null" 
          then Just Null
          else decodeStrict (B.pack $ validate s) :: Maybe Value
    in case mv of 
        Just x -> fromJSON x 
        Nothing -> Error "not bool"

toObject :: [(String, String)] -> Result Obj
toObject = fromJsonRecord

field = ["x", "y", "name", "isActive"]

objectTest1 = toObject $ zip field ["1", "2", "hoge", "t"]

objectTest2 = toObject $ zip field ["sss", "2", "hoge", "t"]

objectTest3 = toObject $ zip field ["1", "2", "hoge", "t___"]

objectTest4 = toObject $ zip field ["null", "2", "hoge", "t___"]

objectTest5 = toObject $ zip field ["1", "2", "hoge", "null"]

getX :: Lookup s "x" Int => Result (Record s) -> Int
getX (Success r) =  r ^. #x
getX (Error e) = 0

getName :: Lookup s "name" T.Text => Result (Record s) -> T.Text
getName (Success r) =  r ^. #name
getName (Error e) = "error"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "start ToJsonOriginal module test !" $ do
    describe "test objectTest1" $ do
      it "x's value is 1" $ do
        getX objectTest1 `shouldBe` 1

      it "name's value is hoge" $ do
        getName objectTest1 `shouldBe` "hoge"

    describe "test objectTest2" $ do
      it "x's value is 2" $ do
        getX objectTest2 `shouldBe` 0

      it "objectTest2 is error obj" $ do
        objectTest2 `shouldBe` Error "not int"

    describe "test objectTest3" $ do
      it "objectTest3 is error obj" $ do
        objectTest3 `shouldBe` Error "expected Bool, but encountered Null"

    describe "test objectTest4" $ do
      it "objectTest4 is error obj" $ do
        objectTest4 `shouldBe` Error "parsing Int failed, expected Number, but encountered Null"
    
    describe "test objectTest5" $ do
      it "objectTest5 is error obj" $ do
        objectTest5 `shouldBe` Error "expected Bool, but encountered Null"