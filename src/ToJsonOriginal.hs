{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ToJsonOriginal where

import Data.Aeson

import Data.Extensible
import Data.Proxy
import GHC.TypeLits (symbolVal)
import Control.Monad.Identity (Identity(..))


class ToJSONOriginal a where 
  toJsonOriginal :: String -> Result a

instance ToJSONOriginal a => ToJSONOriginal (Identity a) where
  toJsonOriginal = fmap pure . toJsonOriginal


fromJsonRecord :: Forall (KeyTargetAre KnownSymbol ToJSONOriginal) xs => [(String, String)] -> Result (Record xs)
fromJsonRecord kvs =  hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol ToJSONOriginal)) $ 
    \m -> let k = symbolVal (proxyKeyOf m)
              v = lookup k kvs
          in case v of
            Just x -> Field <$> toJsonOriginal x 
            Nothing -> Error "no field found ..."


