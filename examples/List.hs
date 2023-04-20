module List where

descend 0 = []
descend n = n : descend(n-1)

iterate' f 0 x = [x]
iterate' f n x = x : iterate' f (n-1) (f x)

split :: Char -> String -> [String]
split c [] = []
split c xs = start : split c (drop 1 rest)
  where start = takeWhile (/=c) xs
        rest = dropWhile (/=c) xs

myhead :: [Int] -> Int
myhead [] = -1
myhead (first:rest) = first

mytail :: [Int] -> [Int]
mytail [] = []
mytail (first:rest) = rest

sumFirstTwo :: [Integer] -> Integer
-- used for lists with at least length two
sumFirstTwo (a:b:_) = a + b
-- equation gets used for all other lists
sumFirstTwo _ = 0

describeList :: [Int] -> String
describeList [] = "an empty list"
describeList [x] = "a list with one element"
describeList [x, y] = "a list with two elements"
describeList (x:y:z:xs) = "a list with at least three elements"

startsWithZero :: [Integer] -> Bool
startsWithZero (0:xs) = True
startsWithZero (x:xs) = False
startsWithZero [] = False

myMaximum :: [Int] -> Int
myMaximum [] = 0 -- this should be some error
myMaximum (x:xs) = go x xs
  where go biggest [] = biggest
        go biggest (x:xs) = go (max biggest x) xs

countNothings :: [Maybe a] -> Int
countNothings [] = 0
countNothings (Nothing : xs) = 1 + countNothings xs
countNothings (Just _  : xs) = countNothings xs

-- not tail recursive
doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs

-- tail recursive
doubleListTail :: [Int] -> [Int]
doubleListTail xs = go [] xs
  where go result [] = result
        go result (x:xs) = go (result ++ [2*x]) xs

-- map ghc implemention
-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x : xs) = f x : map f xs

-- filter ghc implemention
-- filter :: (a -> Bool) -> [a] -> [b]
-- filter _pred [] = []
-- filter pred (x:xs)
--  | pred x    = x : filter pred xs
--  | otherwise = filter pred xs

-- not tail recursive
sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x + sumNumbers xs

-- tail recursive
sumNumbersTail :: [Int] -> Int
sumNumbersTail xs = go 0 xs
  where go sum [] = sum
        go sum (x:xs) = go (sum+x) xs
