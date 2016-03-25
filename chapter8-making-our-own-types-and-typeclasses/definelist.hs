module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where

import qualified Data.Map as Map
import Data.Maybe

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- *Main> surface $ Circle 10 20 10
-- 314.15927
-- *Main> surface $ Rectangle 0 0 100 100
-- 10000.0
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 -y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- It's Bad way
-- data Person = Person String String Int Float String String deriving (Show)
--
-- firstName :: Person -> String
-- firstName (Person firstName _ _ _ _ _) = firstName
--
-- lastName :: Person -> String
-- lastName (Person _ lastName _ _ _ _) = lastName
--
-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age
--
-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height
--
-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number
--
-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

data Person = Person { firstName :: String
, lastName :: String
, age :: Int
, height :: Float
, phoneNumber :: String
, flavor :: String
} deriving (Show)

data Maybe a = Nothing | Just a



data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

-- data Car a b c = Car { company :: a
-- , model :: b
-- , year :: c} deriving(Show)

tellCar :: Car -> String
tellCar Car {company = c, model = m, year = y} = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook


data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map  of
    Data.Maybe.Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Data.Maybe.Just (state, code) -> if state /= Taken then Right code else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers =
  Map.fromList
  [(100,(Taken,"ZD39I"))
  ,(101,(Free,"JAH3I"))
  ,(103,(Free,"IQSA9"))
  ,(105,(Free,"QOTSA"))
  ,(109,(Taken,"893JJ"))
  ,(110,(Taken,"99292"))]

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
