import Data.List
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
	let nlen = length needle
	in foldl (\acc x -> ((take nlen x == needle) || acc)) False (tails haystack)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)
