{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Default where
import Data.Extensible
import Data.Proxy
import GHC.TypeLits
import Data.Default.Class (Default(..))
import Control.Monad.Identity (Identity(..))

instance Default a => Default (Identity a) where
  def = Identity def

instance Forall (KeyTargetAre KnownSymbol Default) xs => Default (Record xs) where
  def =  runIdentity $ hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol Default)) (const $ pure $ Field def)