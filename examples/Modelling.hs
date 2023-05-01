module Modelling where

import Data.Ord
import Data.List
import Data.Semigroup
import qualified Data.List.NonEmpty as NE

data Plate = Plate String
  deriving (Show, Eq)

-- Placeholder function
correctPlateNumber :: String -> Bool
correctPlateNumber s = True

parsePlate :: String -> Maybe Plate
parsePlate string
  | correctPlateNumber string = Just (Plate string)
  | otherwise                 = Nothing

data Money = Money Int
  deriving Show

renderMoney :: Money -> String
renderMoney (Money cents) = show (fromIntegral cents / 100)

-- custom operator for money that functions the same as a +
(+!) :: Money -> Money -> Money
(Money a) +! (Money b) = Money (a+b)

scale :: Money -> Double -> Money
scale (Money a) x = Money $ round $ fromIntegral a * x

addVat :: Money -> Money
addVat m = m +! scale m 0.24

data Person = Person {name :: String, age :: Int}
  deriving Show

data SortOrder = Ascending | Descending
data SortField = Name | Age

sortByField :: SortField -> [Person] -> [Person]
sortByField Name ps = sortBy (comparing name) ps
sortByField Age ps = sortBy (comparing age) ps

sortPersons :: SortField -> SortOrder -> [Person] -> [Person]
sortPersons field Ascending ps = sortByField field ps
sortPersons field Descending ps = reverse $ sortByField field ps

persons = [Person "Fridolf" 73, Person "Greta" 60, Person "Hans" 65]

nonEmpty :: [a] -> Maybe (NE.NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (x:xs) = Just (x NE.:| xs)

neHead (x NE.:| _) = x
neLast (x NE.:| []) = x
neLast (_ NE.:| xs) = last xs

data MSum a = MSum a
instance Num a => Semigroup (MSum a) where
  MSum a <> MSum b = MSum (a+b)

data MProduct a = MProduct a
instance Num a => Semigroup (MProduct a) where
  MProduct a <> MProduct b = MProduct (a*b)

-- mcomcat = monoid concat
foldMap' :: Monoid m => (a -> m) -> [a] -> m
foldMap' f xs = mconcat (map f xs)

-- open vs closed abstractions
-- when should i use type classes?
-- ADTs solution is closed (i.e. all types are known at hand)
-- Type classes are open (i.e. open to for extensibility, and for more types to be added)

-- scenario : a vehicle can be either a car or an airplane
-- this can be modeled with ADTs, but they also type classes

-- ADTs
-- data Vehicle = Car String | Airplane String
--
-- sound :: Vehicle -> String
-- sound (Car _) = "brum brum"
-- sound (Airplane _) = "zooooom"

-- Type classes
-- note how every case gets its own datatype
data Car = Car String
data Airplane = Airplane String
data Bike = Bike String

class VehicleClass a where
  sound :: a -> String

instance VehicleClass Car where
  sound (Car _ ) = "brum brum"

instance VehicleClass Airplane where
  sound (Airplane _) = "zooooom"

instance VehicleClass Bike where
  sound (Bike _) = "whirrr"

-- a closed abstraction is good when we want to know that we've handled all cases
-- this would be hard to impement reliably in a class-based solution

-- canCollide :: Vehicle -> Vehicle -> Bool
-- canCollide (Car _) (Car _) = True
-- canCollide (Airplane _) (Airplane _) = True
-- canCollide _ _ = False
