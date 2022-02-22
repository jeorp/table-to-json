{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TableToJson where

import Data.Default.Class

import Data.Aeson

import Data.Extensible
import Data.Proxy
import GHC.TypeLits (symbolVal)
import Control.Monad.Identity (Identity(..))


class ToJSONOriginal a where 
  toJsonOriginal :: String -> Result a

instance ToJSONOriginal a => ToJSONOriginal (Identity a) where
  toJsonOriginal = fmap pure . toJsonOriginal


toRecord :: Forall (KeyTargetAre KnownSymbol ToJSONOriginal) xs => [(String, String)] -> Result (Record xs)
toRecord kvs =  hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol ToJSONOriginal)) $ 
    \m -> let k = symbolVal (proxyKeyOf m)
              v = lookup k kvs
          in case v of
            Just x -> Field <$> toJsonOriginal x 
            Nothing -> Error "no field found ..."


