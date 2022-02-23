{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sample.Model where

import Data.Text

import Data.Extensible
import Data.Proxy
import GHC.TypeLits

type Brand = Record BrandFields

type BrandFields = 
 '[
    "id" :> Integer,
    "brandname" :> Maybe Text, 
    "brandfurigana" :> Maybe Text,
    "url" :> Maybe Text,
    "kind" :> Text,
    "lost" :> Maybe Bool,
    "median" :> Maybe Int
  ]

getBrandFields :: [String]
getBrandFields = henumerateFor (Proxy :: Proxy (KeyIs KnownSymbol)) (Proxy @BrandFields) ((:) . symbolVal . proxyKeyOf) []