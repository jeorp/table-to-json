{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Sample.ScrapingHxt where

import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.XML.HXT.Core
import Text.XML.HXT.CSS

import Control.Arrow
import Data.Tree.NTree.TypeDefs

import Data.Aeson
import Data.Aeson.Types (parse, Result(..)) 

parseHTML = readString 
  [ withValidate no,
    withParseHTML yes,
    withWarnings no
  ]

scraping body parser = runX (parseHTML body >>> parser)


atTagCase tag = deep (isElem >>> hasNameWith ((== tag') . upper . localPart))
  where tag' = upper tag
        upper = map toUpper

onTd = proc r -> do
  td <- atTagCase "td" -< r
  (getText <<< deep isText) <<< arr 
     (\t@(NTree a xs) -> 
       if null xs 
         then NTree a [NTree (XText "null") []] 
         else t
     ) -< td

extract =
  atTagCase "tr"
  >>> proc r -> do
    xs <- listA onTd -< r
    returnA -< xs

