module Factorial where

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

factorialGuard n
  | n < 0     = -1
  | n == 0    = 1
  | otherwise = n * factorialGuard (n - 1)
