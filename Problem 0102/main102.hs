{-
Three distinct points are plotted at random on a Cartesian plane, for which -1000 >= x, y <=1000, 
such that a triangle is formed.

Consider the following two triangles:

A(-340,495), B(-153,-910), C(835,-947)

X(-175,41), Y(-421,-714), Z(574,-645)

It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.

Using triangles.txt, a 27K text file containing the co-ordinates of one thousand "random" triangles, 
find the number of triangles for which the interior contains the origin.

NOTE: The first two examples in the file represent the triangles in the example given above.
-}

import Data.List

-- First, define a few key data classes, functions, and typeclasses

data Point = Point {coordX :: Float, coordY :: Float} deriving (Eq, Show) 
data Triangle = Triangle {point1 :: Point, point2 :: Point, point3 :: Point} deriving (Show)
data Line = Line Point Point deriving (Show)

-- Create a function to determine is a triangle is rightside-up, upside-down, or neither
triangleType :: Triangle -> String
triangleType (Triangle (Point a1 a2) (Point b1 b2) (Point c1 c2))
	| (length $ filter (<0) ys) == 2 = "Rightside-up"
	| (length $ filter (>=0) ys) == 2 = "Upside-down"
	| otherwise = "Neither"
	where ys = [a2,b2,c2]
	
-- Create a function to calculate the x-coordinate of the x-intercept of a line
xIntercept :: Line -> Float
xIntercept (Line (Point x0 y0) (Point x1 y1)) = (-y0) * (x1-x0)/(y1-y0) + x0

-- Create a function that takes in a triangle and determines if it contains 0
-- containsZero :: Triangle -> Bool
hasZero t@(Triangle (Point a1 a2) (Point b1 b2) (Point c1 c2))
	| triangleType t == "Neither" = False
	| parity <= 0 = True
	| parity > 0 = False
	| otherwise = error "Cannot determine if has zero"
	where
		ps = [Point a1 a2, Point b1 b2, Point c1 c2]
		pivot
			| triangleType t == "Rightside-up" = head $ filter (\p -> coordY p >=0) ps 
			| triangleType t == "Upside-down" = head $ filter (\p -> coordY p < 0) ps
			| otherwise = error "Cannot determine pivot"
		rest = ps \\ [pivot]
		inter1 = xIntercept $ Line pivot (rest !! 0)
		inter2 = xIntercept $ Line pivot (rest !! 1)
		parity = inter1 * inter2
		
-- Some test cases
test1 = hasZero (Triangle (Point (-340) 495) (Point (-153) (-910)) (Point 835 (-947)))
test2 = hasZero (Triangle (Point (-175) 41) (Point (-421) (-714)) (Point 574 (-645)))

-- Create a function that takes in a list of 6 floats and returns a triangle
toTri posns@[a1,a2,b1,b2,c1,c2] =  Triangle (Point a1 a2) (Point b1 b2) (Point c1 c2)
	
-- Print and write out the answer
main = do
		rawContents <- fmap lines $ readFile "triangles.txt"
		let contents = map (read . ('[' :) . (++ "]")) rawContents :: [[Float]]
		let ans = length [zs | zs <- contents, (hasZero . toTri) zs]
		print ans