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
import GHC.TypeLits
import Data.Default.Class (Default(..))
import Control.Monad.Identity (Identity(..))


class Default a => ToJSONOriginal a where 
  toJsonOriginal :: String -> Result a

instance ToJSONOriginal a => ToJSONOriginal (Identity a) where
  toJsonOriginal = fmap pure . toJsonOriginal

instance Default a => Default (Identity a) where
  def = Identity def

toRecord :: Forall (KeyTargetAre KnownSymbol ToJSONOriginal) xs => [(String, String)] -> Result (Record xs)
toRecord kvs =  hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol ToJSONOriginal)) $ 
    \m -> let k = symbolVal (proxyKeyOf m)
              v = lookup k kvs
          in case v of
            Just x -> Field <$> toJsonOriginal x 
            Nothing -> pure $ Field def


