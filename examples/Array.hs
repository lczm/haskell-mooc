module Array where

import Data.Array

myArray :: Array Int String
myArray = array (7,11) [(7, "seven"), (8, "eight"), (9, "nine"), (10, "ten"), (11, "ELEVEN")]

myArray2 :: Array Int String
myArray2 = listArray (7, 11) ["seven", "eight", "nine", "ten", "ELEVEN"]
