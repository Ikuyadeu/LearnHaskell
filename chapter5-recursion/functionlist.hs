-- ghci> maximum [1,2,3,4,5]
-- 5
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- ghci> minimum [1,2,3,4,5]
-- 1
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "minimum of empty list"
minimum' [x] = x
mminimum' (x:xs) = min x (maximum' xs)

-- ghci> replicate 5 3
-- [3,3,3,3,3]
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i,Ord i) => i-> [a] -> [a]
take' n _
		| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- take x(repeat y) = replicate x y
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- ghci> zip' [1,2,3] ['a', 'b']
-- [(1,'a'),(2,'b')]
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

-- ghci> elem' 5 [1,2,3,4,5]
-- True
-- ghci> elem' 6 [1,2,3,4,5]
-- False
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
		| a == x = True
		| otherwise = a `elem` xs

-- ghci> quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- [1,2,2,3,3,4,4,5,6,7,8,9,10]
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let
	smallerSorted = quicksort [a | a <- xs, a <= x]
	biggerSorted = quicksort [a | a <- xs, a > x]
	in  smallerSorted ++ [x] ++ biggerSorted
