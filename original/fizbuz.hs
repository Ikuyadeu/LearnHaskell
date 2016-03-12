fizzbuzz :: [String]
fizzbuzz = map toFizzbuzz [1..]
  where
  toFizzbuzz x
    |x `mod` 15 == 0 = "FizzBuzz"
    |x `mod`  3 == 0 = "Fizz"
    |x `mod`  5 == 0 = "Buzz"
    |otherwise       = show x


main = mapM_ putStrLn $ take 100 fizzbuzz
