import Data.List
import qualified Data.Map as Map
import Data.Char
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
	let nlen = length needle
	in foldl (\acc x -> ((take nlen x == needle) || acc)) False (tails haystack)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

-- ghci> encode 3 "Heeeeey"
-- "Khhhhh|"
encode :: Int -> String -> String
encode shift msg =
	let
		ords = map ord msg
		shifted = map (+ shift) ords
		in map chr shifted

-- ghci> decode 3 "Heeeeey"
-- "Ebbbbbv"
decode :: Int -> String -> String
decode shift = encode (negate shift)

phoneBook =
		[("betty","555-2938")
		,("betty","342-2492")
		,("bonnie","452-2928")
		,("patsy","493-2928")
		,("patsy","943-2929")
		,("patsy","827-9162")
		,("lucille","205-2928")
		,("wendy","939-8282")
		,("penny","853-2492")
		,("penny","555-2111")
		]

-- ghci> findKey "penny" phoneBook
-- Just "853-2492"
-- ghci> findKey "betty" phoneBook
-- Just "555-2938"
-- ghci> findKey "wilma" phoneBook
-- Nothing
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
	then Just v
	else findKey key xs

-- ghci> Map.empty
-- fromList []
-- ghci> Map.insert 3 100 Map.empty
-- fromList [(3,100)]
-- ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))
-- fromList [(3,100),(4,200),(5,600)]
-- ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
-- fromList [(3,100),(4,200),(5,600)]
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
-- "827-9162, 943-2929, 493-2928"
-- ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
-- "939-8282"
-- ghci> Map.lookup "betty" $ phoneBookToMap phoneBook
-- "342-2492, 555-2938"
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (++) $ map (\(k,v) -> (k,[v]))
