-- ghci> let multTwoWithNine = multThree 9
-- ghci> multTwoWithNine 2 3
-- 54
-- ghci> let multWithEighteen = multTwoWithNine 2
-- ghci> multWithEighteen 10
-- 180
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- ghci> compareWithHundred 99
-- GT
-- ghci> compareWithHundred 100
-- EQ
-- ghci> compareWithHundred 101
-- LT
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- ghci> applyTwice (+3) 10
-- 16
-- ghci> applyTwice (++ " HAHA") "HEY"
-- "HEY HAHA HAHA"
-- ghci> applyTwice ("HAHA " ++) "HEY"
-- "HAHA HAHA HEY"
-- ghci> applyTwice (multThree 2 2) 9
-- 144
-- ghci> applyTwice (3:) [1]
-- [3,3,1]
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
-- [6,8,7,9]
-- ghci> zipWith' max [6,3,2,1] [7,3,1,5]
-- [7,3,2,5]
-- ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
-- ["foo fighters","bar hoppers","baz aldrin"]
-- ghci> zipWith' (*) (replicate 5 2) [1..]
-- [2,4,6,8,10]
-- ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-- [[3,4,6],[9,20,30],[10,12,12]]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- ghci> flip' zip [1,2,3,4,5] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]
-- ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]
-- [5,4,3,2,1]
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x
-- flip' f = \x y -> f y x

-- -- ghci> map (+3) [1,5,3,1,6]
-- -- [4,8,6,4,9]
-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs

-- -- ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
-- -- [5,6,4]
-- -- ghci> filter (==3) [1,2,3,4,5]
-- -- [3]
-- -- ghci> filter even [1..10]
-- -- [2,4,6,8,10]
-- -- ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
-- -- [[1,2,3],[3,4,5],[2,2]]
-- -- ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
-- -- "uagameasadifeent"
-- -- ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"
-- -- "GAYBALLS"
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter p (x:xs)
-- 		| p x       = x : filter p xs
-- 		| otherwise = filter p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let
		smallerSorted = quicksort (filter (<=x) xs)
		biggerSorted = quicksort (filter (>x) xs)
		in smallerSorted ++ [x] ++ biggerSorted

-- ghci> largestDivisible
-- 99554
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
	where
		p x = x `mod` 3829 == 0

-- ghci> chain 10
-- [10,5,16,8,4,2,1]
-- ghci> chain 1
-- [1]
-- ghci> chain 30
-- [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
		| even n = n:chain (n `div` 2)
		| odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z
-- addThree = \x -> \y -> \z -> x + y + z

-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\acc x -> ((x==y) || acc)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 const

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
