
					-- Problem 6 --

diffSumSquaredAndSumSquares x =
	truncate $
	sum [1..x] ** 2 -
	foldl (\acc x -> acc + (x**2)) 1 [2..x]
