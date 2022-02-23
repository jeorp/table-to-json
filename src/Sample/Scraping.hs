{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
module Sample.Scraping where

import Data.Extensible
import Data.Proxy
import GHC.TypeLits (symbolVal)

import Data.Aeson
import Data.Aeson.Types (parse, Result(..))
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import ToJsonOriginal
import Sample.DownloadHtml
import Sample.ScrapingHxt
import Sample.Model

instance ToJSONOriginal Int where
  toJsonOriginal s = 
    let mv = decodeStrict (B.pack s) :: Maybe Value
    in case mv of 
        Just x -> fromJSON x 
        Nothing -> Error "not int"

instance ToJSONOriginal Integer where
  toJsonOriginal s = 
    let mv = decodeStrict (B.pack s) :: Maybe Value
    in case mv of 
        Just x -> fromJSON x 
        Nothing -> Error "not integer"

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



----------------------------  scraping start  --------------------------------------


getStrings :: String -> IO [[String]] -- scraping from table return objects like a [[String]] 
getStrings query_path = getHtml query_path >>= fmap tail . flip scraping extract

catResultsIO :: [Result a] -> IO [a]
catResultsIO = loop
  where
    loop ((Error s): xs) = putStrLn s >> loop xs
    loop ((Success a) : xs) = (a:) <$> loop xs
    loop _ =  pure []

getResultsWithoutError :: Forall (KeyTargetAre KnownSymbol ToJSONOriginal) xs => (String, [String]) -> IO [Record xs]
getResultsWithoutError set = getStrings (fst set) >>= catResultsIO . map (fromJsonRecord . zip (snd set)) 

getBrandList :: IO [Brand]
getBrandList = getResultsWithoutError brandlistSet

brandlistSet = ("src/Sample/sql/brandlistAll.sql", getBrandFields)


        