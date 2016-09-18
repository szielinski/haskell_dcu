--Lab 5
--
--Szymon Zielinski

data BinTree a = Empty | Root a (BinTree a) (BinTree a) deriving (Show, Read, Eq)  

singleton :: a -> BinTree a
singleton x = Root x Empty Empty

addnode :: Ord a => a -> BinTree a -> BinTree a
addnode x Empty = singleton x
addnode x (Root a left right)
	| x == a = Root x left right
	| x < a = Root a (addnode x left) right
	| x > a = Root a left (addnode x right)

myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty)) (Root 7 Empty Empty)

maketree :: Ord a => [a] -> BinTree a
maketree [x] = singleton x
maketree x = addnode (head x) (maketree (tail x)) 

inorder :: BinTree a -> [a]
inorder Empty = []
inorder (Root a left right) = inorder left ++ [a] ++ inorder right

mpsort :: Ord a => [a] -> [a]
mpsort [] = []
mpsort x = inorder(maketree x)

hosort :: (a -> a -> Bool) -> [a] -> [a]
hosort _ [x] = [x]
hosort f (x:xs) = if (f x (head (hosort f xs)))
			then [x] ++ hosort f xs
			else (hosort f (xs++[x]))