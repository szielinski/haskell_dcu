--Lab 2
--
--Szymon Zielinski

diff :: Int -> Int -> Int
diff x y = abs (x-y)

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt(s * (s - a) * (s - b) * (s - c))
	     where s = (a+b+c)/2

isSum :: Int -> Int -> Int -> Bool
isSum a b c = ((a + b) == c) || ((a + c) == b) || ((c + b) == a)

isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c = not((a + b) <= c) || ((a + c) <= b) || ((c + b) <= a)

correctTriangleArea :: Float -> Float -> Float -> Float
correctTriangleArea a b c = if (isTriangle a b c) 
			    then (triangleArea a b c) 
		            else error "Not a triangle!"