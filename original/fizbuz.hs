import Text.Printf


fizbuz :: Int -> String
fizbuz x
	| x % 3 == 0 and x % 5 == 0 = "fizbuz"
	| x % 3 == 0 = "fiz"
	| x % 5 == 0 = "buz"

main :: IO()
main = take(5, fizbuz)
