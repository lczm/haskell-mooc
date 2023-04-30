module Class where

-- Make my own type a member of the Eq class
data Color = Black | White

instance Eq Color where
  Black == Black = True
  White == White = True
  _     == _     = False


class Size a where
  -- functions
  empty :: a
  size :: a -> Int
  sameSize :: a -> a -> Bool

-- input of (Maybe a) to said functions
instance Size (Maybe a) where
  empty = Nothing
  size Nothing = 0
  size (Just a) = 1

  sameSize x y = size x == size y

-- input of [a] to said functions
instance Size [a] where
  empty = []
  size xs = length xs
  sameSize x y = size x == size y

-- default implementations
class Example a where
  example :: a -- the main example for the type `a`
  examples :: [a] -- a short list of examples
  examples = [example] -- defaulting to just the main example

instance Example Int where
  example = 1
  examples = [0, 1, 2]

instance Example Bool where
  example = True

-- class Combine a where
--   combine :: a -> a -> a
--   combine3 :: a -> a -> a -> a
--   combine3 x y z = combine x (combine y z)

-- Combine class with default implementation moved out
class Combine a where
  combine :: a -> a -> a

combine3 :: Combine a => a -> a -> a -> a
combine3 x y z = combine x (combine y z)

data IntPair = IntPair Int Int
  deriving Show

instance Eq IntPair where
  IntPair a1 a2 == IntPair b1 b2 = a1==b1 && a2==b2

instance Ord IntPair where
  IntPair a1 a2 <= IntPair b1 b2
    | a1<b1 = True
    | a1>b1 = False
    | otherwise = a2<=b2

data Person = Dead | Alive String Int
  deriving (Show, Eq, Ord)

data Pair a = MakePair a a
  deriving Show

instance Eq a => Eq (Pair a) where
  (MakePair x y) == (MakePair a b) = x==a && y==b

class Check a where
  check :: a -> Bool

instance Check Int where
  check x = x > 0

instance Check a => Check [a] where
  check xs = and (map check xs)

checkAll :: Check a => [a] -> Bool
checkAll xs = and (map check xs)
