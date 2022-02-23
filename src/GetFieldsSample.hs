{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GetFieldsSample where 

import Data.Text

import Data.Extensible
import Data.Proxy
import GHC.TypeLits

type Book = Record BookFields

type BookFields =
    '[ "name" >: String
     , "authors" >: [String]
     , "date" >: String
     , "isbm" >: String
     , "price" >: Float
     ]

getBrandFields :: [String]
getBrandFields = henumerateFor (Proxy :: Proxy (KeyIs KnownSymbol)) (Proxy @BookFields) ((:) . symbolVal . proxyKeyOf) []

