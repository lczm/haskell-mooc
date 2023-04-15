module Maybe where

perhapsMultiply :: Int -> Maybe Int -> Int
perhapsMultiply i Nothing = i
perhapsMultiply i (Just j) = i * j

intOrZero :: Maybe Int -> Int
intOrZero Nothing = 0
intOrZero (Just i) = i

safeHead :: [a] -> Maybe a
safeHead xs = if null xs 
                 then Nothing 
                 else Just (head xs)

-- if head is safe & if it is an int
-- return the head
headOrZero :: [Int] -> Int
headOrZero xs = intOrZero (safeHead xs)


