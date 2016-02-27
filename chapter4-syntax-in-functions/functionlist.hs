lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
		| a > b = a
		| otherwise = b

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
		| bmi <= skinny = "You're underweight, you emo, you!"
		| bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
		| bmi <= fat    = "You're fat! Lose some weight, fatty!"
		| otherwise     = "You're a whale, congratulations!"
		where
			bmi = weight / height ^ 2
			(skinny, normal, fat) = (18.5, 25.0, 30.0)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
		where
			what [] = "empty."
			what [x] = "a singleton list."
			what xs = "a longer list."  
