--Lab 4
--
--Szymon Zielinski

myHead :: (Eq a) => [a] -> a
myHead [] = error "The list is empty!"
myHead list = list !! 0

myLast :: (Eq a) => [a] -> a
myLast [] = error "The list is empty!"
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: (Eq a) => [a] -> [a]
myTail [] = error "The list is empty!"
myTail (x:xs) = xs

myInit :: (Eq a) => [a] -> [a]
myInit [] = error "The list is empty!"
myInit [x] = []
myInit (x:xs) = x : myInit xs

myLength :: (Eq a) => [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

myReverse :: (Eq a) => [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myConcat :: (Eq a) => [[a]] -> [a]
myConcat [] = []
myConcat [[x]] = [x]
myConcat x = myHead x ++ myConcat (myTail x)

mySum :: Num a => [a] -> a
mySum [] = error "The list is empty!"
mySum [x] = x 
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [] = error "The list is empty!"
myProduct [x] = x 
myProduct (x:xs) = x * myProduct xs

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "The list is empty!"
myMaximum [x] = x 
myMaximum x = if myHead x < myLast x
		then myMaximum (myTail x)
		else myMaximum (myInit x)

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "The list is empty!"
myMinimum [x] = x 
myMinimum x = if myHead x > myLast x
		then myMinimum (myTail x)
		else myMinimum (myInit x)

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a [xs] = a == xs
myElem a x = if a == x !! 0
		then True
		else myElem a (myTail x)

myDelete :: Eq a => a -> [a] -> [a]
myDelete a [] = []
myDelete a [xs] = []
myDelete a x = if a == x !! 0
		then myTail x
		else ([myHead x] ++ myDelete a (myTail x))

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup [x] = [x]
remDup (x:xs) = if myElem x xs
		then remDup ([x] ++ myDelete x xs)
		else [x] ++ remDup xs

myUnion :: Eq a => [a] -> [a] -> [a]
myUnion [] [] = []
myUnion a [] = a
myUnion [] a = remDup a
myUnion (x:xs) a = if myElem x (remDup a)
			then [x] ++ myUnion xs (myDelete x (remDup a))
			else [x] ++ myUnion xs (remDup a)

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] [] = []
myIntersect a [] = []
myIntersect [] a = []
myIntersect (x:xs) a = if myElem x a
			then [x] ++ myIntersect xs a
			else [] ++ myIntersect xs a