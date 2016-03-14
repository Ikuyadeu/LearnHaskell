module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where

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

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

data Maybe a = Nothing | Just a
