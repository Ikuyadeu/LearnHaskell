import Prelude
import Data.Int
import Control.Parallel

isPrime :: Int32 -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2 .. toEnum (floor $ sqrt $ fromIntegral x)]

arr :: [Int32]
arr = [2 .. 1000000]

main :: IO ()
main = do
	let primes = reduceP (fromEnum . isPrime) (+) arr
	putStr "primes: " >> print primes
	--let arr' = map isPrime arr `using` parListChunk 256 rpar
	--putStr "primes: " >> print (length $ filter id arr')

reduceP :: (b -> a) -> (a -> a -> a) -> [b] -> a
reduceP f _ [x] = f x
reduceP f (<+>) xs = (ys `par` zs) `pseq` (ys <+> zs) where
	len = length xs
	(ys', zs') = splitAt (len `div` 2) xs
	ys = reduceP f (<+>) ys'
	zs = reduceP f (<+>) zs'
