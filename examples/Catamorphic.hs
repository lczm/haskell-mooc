module Catamorphic where

import Data.List

-- subStringsOfLength :: Int -> String -> [String]
-- subStringsOfLength n string = map shorten (tails string)
--   where shorten s = take n s

-- whatFollows :: Char -> Int -> String -> [String]
-- whatFollows c k string = map tail (filter match (subStringsOfLength (k+1) string))
--   where match sub = take 1 sub == [c]

-- rewriting whatFollows with . and $

whatFollows :: Char -> Int -> String -> [String]
-- whatFollows c k = 
--   map tail . filter (\sub -> take 1 sub == [c]) . map (take (k+1)) . tails
whatFollows c k = 
  map tail . filter ((==[c]) . take 1) . map (take (k+1)) . tails

findSubString :: String -> String -> String
findSubString chars = takeWhile (\x -> elem x chars) 
                      . dropWhile (\x -> not $ elem x chars)


