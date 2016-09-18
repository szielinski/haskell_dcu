--Lab 3
--
--Szymon Zielinski

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == (reverse list)

shortest :: (Eq a) => [[a]] -> [a]
shortest list 
	| length list == 0 = []
	| length list == 1 = list !! 0
	| length (head list) < length (last list) = shortest (init list)
	| length (head list) > length (last list) = shortest (tail list)

sumPoly :: [Int] -> [Int] -> [Int]
sumPoly a b
	| (length a == 0 && length b == 0) = []
	| (length a == 0 && length b > 0) =  [head b] ++ sumPoly [] (tail b)
	| (length a > 0 && length b == 0) = [head a] ++ sumPoly (tail a) []
	| (length a > 0 && length b > 0) = [((head a) + (head b))] ++ sumPoly (tail a) (tail b)

evalPoly :: Int -> [Int] -> Int
evalPoly a list = if length list == 0 
		then 0
		else (last list * (a ^ (length list-1))) + evalPoly a (init list) 
--	| length list == 0 = 0
--	| (length list) > 0 = (last list * (a ^ (length list-1))) + evalPoly a (init list) 