
					-- Problem 5 --

smallestNum xs = 
	sN (filter isPrime xs) (filter (\x -> not $ isPrime x) xs)

--isPrime 2 = True
--isPrime x
--	| x < 2 = False
--	| x `mod` 2 == 0 = False
--	| otherwise = isPrime' x 3

--isPrime' x n
--	| x <= n = True
--	| x `mod` n == 0 = False
--	| otherwise = isPrime' x (n+2)

sN ps [] = foldl (\acc x -> acc*x) 1 ps
sN ps (n:ns)
	| ok == 1 = sN ps ns
	| otherwise = sN (ps ++ [ok]) ns
	where ok = checkGood ps n

checkGood _ 1 = 1
checkGood [] n = n
checkGood (p:ps) n
	| n `mod` p == 0 = checkGood ps (n `div` p)
	| otherwise = checkGood ps n
