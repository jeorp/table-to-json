module Main where

import Sample.Scraping (getBrandList)

main :: IO ()
main = getBrandList >>= mapM_ print . take 10
