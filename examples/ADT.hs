module ADT where

data Report = ConstructReport Int String String

-- Pattern matching to get the content field
reportContents :: Report -> String
reportContents (ConstructReport id title contents) = contents

-- Setting the content field
setReportContents :: String -> Report -> Report
setReportContents contents (ConstructReport id title _contents) = ConstructReport id title contents

-- Constructors
-- A type can have constructors, and a constructor can have zero or more fields
data Card = Joker | Heart Int | Club Int | Spade Int | Diamond Int
  deriving Show

-- Understanding they the datatypes are algebraic
-- each datatype can be a sum of constructors,
-- and each constructor is a product of fields
data MyBool = MyTrue | MyFalse               -- corresponds to 1 + 1, has 2 possible values
data MyTwoBools = TwoBools MyBool MyBool -- corresponds to Bool * Bool, i.e. 2*2 = 4 possible values
data Complex = Two MyBool MyBool | One MyBool | None
  -- corresponds to Bool * Bool * Bool + 1 = (2*2) + (2*1)+1 = 7

-- Parameterized types
data MyMaybe a = Nothing | Just a

data Described a = Describe a String
getValue :: Described a -> a
getValue (Describe x _) = x

getDescription :: Described a -> String
getDescription (Describe _ description) = description

-- Recursive types
-- data IntList = Empty | Node Int IntList -- int = head, intlist = tail
--   deriving Show
--
-- ihead :: IntList -> Int
-- ihead (Node i _) = i
--
-- itail :: IntList -> IntList
-- itail (Node _ t) = t
--
-- ilength :: IntList -> Int
-- ilength Empty = 0
-- ilength (Node _ t) = 1 + ilength t

-- To allow for not just ints in the list, we can use type parameters
-- The Node constructor has two arguments
-- The first has type `a`
-- The second has type `List a`
--
-- data List a = Empty | Node a (List a)
--
-- lhead :: List a -> a
-- lhead (Node h _) = h
--
-- ltail :: List a -> List a
-- ltail (Node _ t) = t
--
-- lnull :: List a -> Bool
-- lnull Empty = True
-- lnull _     = False
--
-- llength :: List a -> Int
-- llength Empty = 0
-- llength (Node _ t) = 1 + llength t

-- Representing a binary tree
-- a binary tree has a node (itself) and its two children (left and right) 
-- or it could be empty
-- Node 4 (Empty) (Empty) as Empty can be a Tree
data Tree a = Node a (Tree a) (Tree a) | Empty

example :: Tree Int
example = (Node 0 (Node 1 (Node 2 Empty Empty)
                          (Node 3 Empty Empty))
                  (Node 4 Empty Empty))
-- note that node 4 is a leave with no children

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- look up a value in the binary tree
llookup :: Int -> Tree Int -> Bool
llookup x Empty = False
llookup x (Node y l r)
  | x < y = llookup x l
  | x > y = llookup x r
  | otherwise = True -- found case

iinsert :: Int -> Tree Int -> Tree Int
iinsert x Empty = Node x Empty Empty  -- insertion case, when traversing down it is empty
iinsert x (Node y l r) -- the node i am at
  | x < y = Node y (iinsert x l) r -- traverse down left 
  | x > y = Node y l (iinsert x r) -- traverse own right
  | otherwise = Node y l r -- already exists

-- Record syntax
-- data Person = MkPerson String Int String String String
--   deriving Show

data Person = MkPerson { name :: String
                       , age :: Int
                       , town :: String
                       , state :: String
                       , profession :: String}
  deriving Show

people :: [Person]
people = [ MkPerson "Jane Doe" 21 "Houston" "Texas" "Engineer"
         , MkPerson "Maija Meikäläinen" 35 "Rovaniemi" "Finland" "Engineer"
         , MkPerson "Mauno Mutikainen" 27 "Turku" "Finland" "Mathematician"
         ]

-- query :: [Person] -> [Person]
-- query [] = []
-- query ((MkPerson name age town state profession):xs)
--   | state == "Finland" && profession == "Engineer" =
--       (MkPerson name age town state profession) : query xs
--   | otherwise = query xs

query :: [Person] -> [Person]
query [] = []
query (x:xs)
  | state x == "Finland" && profession x == "Engineer" = x : query xs
  | otherwise = query xs

