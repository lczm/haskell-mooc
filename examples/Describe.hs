module Describe where

describe :: Int -> String
describe n
  | n == 2    = "Two"
  | even n    = "Even"
  | n == 3    = "Three"
  | n > 100   = "Big!!"
  | otherwise = "The number " ++ show n
