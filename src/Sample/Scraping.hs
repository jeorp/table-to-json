{-# LANGUAGE LambdaCase #-}
module Sample.Scraping where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B

import ToJsonOriginal
import Sample.DownloadHtml
import Sample.Model

instance ToJSONOriginal Integer where
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

instance ToJSONOriginal a => ToJSONOriginal (Maybe a) where
  toJsonOriginal = f . toJsonOriginal 
    where
      f (Success a) = Success $ Just a
      f (Error a) = Success Nothing



----------------------------  scraping statrt  --------------------------------------







        