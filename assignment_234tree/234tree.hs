-- 2-3-4 Tree Assignment
--
-- Szymon Zielinski 
-- 59539204

data Tree a = Empty | Root [a] [Tree a] deriving (Show, Read, Eq)  	-- 2-3-4 tree

leaf x = Root [x] [Empty]		-- leaf node

-- add a node to the given tree
addnode :: Ord a => a -> Tree a -> Tree a

-- base case - empty tree
addnode x Empty = leaf x

-- another base case - hit the lowest subtree
addnode x (Root a [Empty])
	| (length a == 3) = addnode x (split (Root a [Empty]))
	| elem x a = Root a [Empty]
	| (length a == 1) = onecase x (Root a [Empty])
	| (length a == 2) = twocase x (Root a [Empty])

-- general case
addnode x (Root a b)
	| (length a == 3) = addnode x (split (Root a b))	-- if root node has 3 data elements, split it
	
	-- decide how to split a node with 3 data elements and add the new node based on the amount of subtrees present within the tree
	| (length b == 1 && getNoRoots (b !! 0) == 3) = addnode x (splitandcombine (Root a [Empty]) (b !! 0))
	| (length b == 2 && getNoRoots (b !! 0) == 3) = addnode x (splitandcombine (Root a [b !! 1]) (b !! 0))
	| (length b == 2 && getNoRoots (b !! 1) == 3) = addnode x (splitandcombine (Root a [b !! 0]) (b !! 1))
	| (length b == 3 && getNoRoots (b !! 0) == 3) = addnode x (splitandcombine (Root a (tail b)) (b !! 0))
	| (length b == 3 && getNoRoots (b !! 1) == 3) = addnode x (splitandcombine (Root a ([b !! 0] ++ [b !! 2])) (b !! 1))
	| (length b == 3 && getNoRoots (b !! 2) == 3) = addnode x (splitandcombine (Root a (init b)) (b !! 2))

	-- if no nodes with 3 data elements present, decide where to add the new node
	| (length a == 1 && x<(head a)) = (Root a ([(addnode x (head b))]++ (tail b)))
	| (length a == 1 && x>(head a)) = (Root a ((init b)++[(addnode x (last b))]))
	| (length a == 2 && x<(head a)) = (Root a ([(addnode x (head b))]++(tail b)))
	| (length a == 2 && x>(head a)&& x<(last a) && length b == 3) = (Root a ([head b]++[(addnode x (b!!1))]++[last b]))
	| (length a == 2 && x>(head a)&& x<(last a) && length b == 2) = (Root a ([head b]++[(addnode x Empty)]++[last b]))
	| (length a == 2 && x>(last a)) = (Root a ((init b)++[(addnode x (last b))]))

-- a fucntion that splits a node with 3 data elements
split :: Ord a => Tree a -> Tree a
split (Root a [Empty]) = Root [(a !! 1)] ( [leaf (head a)]++[leaf (last a)] )
split (Root a b) = Root [(a !! 1)] ( [Root [(head a)] (take 2 b)] ++ [Root [(last a)] (drop 2 b)] )

-- a function that takes a root node and a child to be split, splits and combines them, where:
-- a b - parent, x z - child to be split
splitandcombine :: Ord a => Tree a -> Tree a -> Tree a
	
-- case where neither the parent nor the child have any subtrees
splitandcombine (Root a [Empty]) (Root x [Empty])
	| (length a == 1 && (head a) < (x !! 1)) = Root ( a ++ [(x !! 1)]) ([(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])])	
	| (length a == 1 && (head a) > (x !! 1)) = Root ([(x !! 1)]++a) ([(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])])
	| (length a == 2 && (head a) > (x !! 1)) = Root ([(x !! 1)]++a) ([(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])])	
	| (length a == 2 && (head a) < (x !! 1) && (last a) > (x !! 1)) = Root ([head a]++[x !! 1]++[last a]) ([(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])])	
	| (length a == 2 && (last a) < (x !! 1)) = Root ( a ++ [(x !! 1)]) ([(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])])

-- case where the parent has no subtrees
splitandcombine (Root a [Empty]) (Root x z)
	| (length a == 1 && (head a) < (x !! 1)) = Root ( a ++ [(x !! 1)]) ([(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))])
	| (length a == 1 && (head a) > (x !! 1)) = Root ([(x !! 1)]++a) ([(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))])
	| (length a == 2 && (head a) > (x !! 1)) = Root ([(x !! 1)]++a) ([(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))])	
	| (length a == 2 && (head a) < (x !! 1) && (last a) > (x !! 1)) = Root ([head a]++[x !! 1]++[last a]) ([(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))])	
	| (length a == 2 && (last a) < (x !! 1)) = Root ( a ++ [(x !! 1)]) ([(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))])

-- case where the child has no subtrees	
splitandcombine (Root a b) (Root x [Empty])
	| (length a == 1 && (head a) < (x !! 1)) = Root ( a ++ [(x !! 1)]) ( [b !! 0] ++ [(Root [(x !! 0)] ([Empty]))] ++ [(Root [(x !! 2)] [Empty])])	
	| (length a == 1 && (head a) > (x !! 1)) = Root ([(x !! 1)]++a) ([(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])]++ [head b])	
	| (length a == 2 && (head a) > (x !! 1)) = Root ([(x !! 1)]++a) ([(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])]++b)	
	| (length a == 2 && (head a) < (x !! 1) && (last a) > (x !! 1)) = Root ([head a]++[x !! 1]++[last a]) ([head b] ++ [(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])] ++ [last b])	
	| (length a == 2 && (last a) < (x !! 1)) = Root ( a ++ [(x !! 1)]) (b++[(Root [(x !! 0)] [Empty])] ++ [(Root [(x !! 2)] [Empty])])	

-- general case where both have subtrees
splitandcombine (Root a b) (Root x z)
	| (length a == 1 && (head a) < (x !! 1)) = Root ( a ++ [(x !! 1)]) ([head b]++[(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))])
	| (length a == 1 && (head a) > (x !! 1)) = Root ([(x !! 1)]++a) ([(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))]++ [head b])
	| (length a == 2 && (head a) > (x !! 1)) = Root ([(x !! 1)]++a) ([(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))]++b)	
	| (length a == 2 && (head a) < (x !! 1) && (last a) > (x !! 1)) = Root ([head a]++[x !! 1]++[last a]) ([head b] ++ [(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))] ++ [last b])	
	| (length a == 2 && (last a) < (x !! 1)) = Root ( a ++ [(x !! 1)]) (b++[(Root [(x !! 0)] (take 2 z))] ++ [(Root [(x !! 2)] (drop 2 z))])

-- display the 2-3-4 tree as an ordered list (lowest to highest value)
display:: Ord a => Tree a -> [a]
display (Root a [Empty]) = a
display (Root a b)
	-- case: n-node with 1 subtree
	| length b == 1 && (getRoot(b!!0)!!0) < (a !! 0) = display (b !! 0) ++ a	
	| length a == 1 && length b == 1 && (getRoot(b!!0)!!0) > (a !! 0) = a ++ display (b !! 0)
	| length a > 1 && length b == 1 && (getRoot(b!!0)!!0) > (head a) && (getRoot(b!!0)!!0) < (last a) = [head a] ++ display (b !! 0) ++ [last a]
	| length a > 1 && length b == 1 && (getRoot(b!!0)!!0) > (last a) = a ++ display (b !! 0)
	
	-- case: 1-node with 2 subtrees
	| length a == 1 && length b == 2= display (b !! 0) ++ [a !! 0] ++ display (b!!1)  
	
	-- case: 2-node with 2 subtrees
	| length a == 2 && length b == 2 && (getRoot(b!!0)!!0) < (a !! 0) && (getRoot(b!!1)!!0) < (a !! 1) = display (b !! 0) ++ [a !! 0] ++ display (b!!1) ++ [a !! 1]
	| length a == 2 && length b == 2 && (getRoot(b!!0)!!0) > (a !! 0) && (getRoot(b!!1)!!0) > (a !! 1) =  [a !! 0] ++ display (b !! 0) ++ [a !! 1] ++ display (b!!1) 
	| length a == 2 && length b == 2 && (getRoot(b!!0)!!0) < (a !! 0) && (getRoot(b!!1)!!0) > (a !! 1) = display (b !! 0) ++ [a !! 0] ++ [a !! 1] ++ display (b!!1) 
	
	-- case: 2-node with 3 subtrees
	| length a == 2 && length b == 3 = display (b !! 0) ++ [a !! 0] ++ display (b !! 1) ++ [a !! 1] ++ display (b !! 2)
	
	-- case: 3-node with 2 subtrees
	| length a == 3 && length b == 2 && (getRoot(b!!0)!!0) < (a !! 0) && (getRoot(b!!1)!!0) < (a !! 1) = display (b !! 0) ++ [a !! 0] ++ display (b !! 1) ++ [a !! 1] ++ [a !! 2]
	| length a == 3 && length b == 2 && (getRoot(b!!0)!!0) < (a !! 0) && (getRoot(b!!1)!!0) < (a !! 2) = display (b !! 0) ++ [a !! 0] ++ [a !! 1] ++ display (b !! 1) ++ [a !! 2]
	| length a == 3 && length b == 2 && (getRoot(b!!0)!!0) < (a !! 0) && (getRoot(b!!1)!!0) > (a !! 2) = display (b !! 0) ++ [a !! 0] ++ [a !! 1] ++ [a !! 2] ++ display (b !! 1) 
	| length a == 3 && length b == 2 && (getRoot(b!!0)!!0) < (a !! 1) && (getRoot(b!!1)!!0) < (a !! 2) = [a !! 0] ++ display (b !! 0) ++ [a !! 1] ++ display (b !! 1) ++ [a !! 2] 
	| length a == 3 && length b == 2 && (getRoot(b!!0)!!0) < (a !! 1) && (getRoot(b!!1)!!0) > (a !! 2) = [a !! 0] ++ display (b !! 0) ++ [a !! 1] ++ [a !! 2] ++ display (b !! 1) 
	| length a == 3 && length b == 2 && (getRoot(b!!0)!!0) < (a !! 2) && (getRoot(b!!1)!!0) > (a !! 2) = [a !! 0] ++ [a !! 1] ++ display (b !! 0) ++ [a !! 2] ++ display (b !! 1) 
	
	-- case: 3-node with 3 subtrees
	| length a == 3 && length b == 3 && (getRoot(b!!0)!!0) < (a !! 0) && (getRoot(b!!1)!!0) < (a !! 1) && (getRoot(b!!2)!!0) < (a !! 2) = display (b !! 0) ++ [a !! 0] ++ display (b !! 1) ++ [a !! 1] ++ display (b !! 2) ++ [a !! 2]
	| length a == 3 && length b == 3 && (getRoot(b!!0)!!0) < (a !! 0) && (getRoot(b!!1)!!0) < (a !! 1) && (getRoot(b!!2)!!0) > (a !! 2) = display (b !! 0) ++ [a !! 0] ++ display (b !! 1) ++ [a !! 1] ++ [a !! 2] ++ display (b !! 2)
	| length a == 3 && length b == 3 && (getRoot(b!!0)!!0) < (a !! 0) && (getRoot(b!!1)!!0) < (a !! 2) && (getRoot(b!!2)!!0) > (a !! 2) = display (b !! 0) ++ [a !! 0] ++ [a !! 1] ++ display (b !! 1) ++ [a !! 2] ++ display (b !! 2)  
	| length a == 3 && length b == 3 && (getRoot(b!!0)!!0) < (a !! 1) && (getRoot(b!!1)!!0) < (a !! 2) && (getRoot(b!!2)!!0) > (a !! 2) = [a !! 0] ++ display (b !! 0) ++ [a !! 1] ++ display (b !! 1) ++ [a !! 2] ++ display (b !! 2) 
	
	-- case: 3-node with 4 subtrees
	| length a == 3 && length b == 4 = display (b !! 0) ++ [a !! 0] ++ display (b !! 1) ++ [a !! 1] ++ display (b !! 2) ++ [a !! 2] ++ display (b !! 3) 

-- returns, as a list, data elements that the given node has
getRoot:: Ord a => Tree a -> [a]
getRoot (Root a b) = a
	
-- returns the number of data elements that the given node has
getNoRoots:: Ord a => Tree a -> Int
getNoRoots (Root a b) = length a

-- add a number to a 1-data node (make a 2-data node)
onecase :: Ord a => a -> Tree a -> Tree a
onecase x (Root a [Empty]) = if (x < (head a))
			then Root ([x]++[(head a)]) [Empty]
			else Root ([(head a)]++[x]) [Empty]

-- add a number to a 2-data node (make a 3-data node)
twocase:: Ord a => a -> Tree a -> Tree a
twocase x (Root a [Empty]) 
	| x < (head a) = Root ([x]++a) [Empty]
	| x > (head a) && x < (last a) = Root ([(head a)]++[x]++[(last a)]) [Empty]
	| x > (last a) = Root (a++[x]) [Empty]
	
-- make a 2-3-4 tree from a list
makeTree:: Ord a => [a] -> Tree a
makeTree [x] = leaf x
makeTree x = addnode (last x) (makeTree (init x))	