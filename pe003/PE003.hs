module PE003 
( isPrime
, primes
) where

--prime n = prime' n [1..]
--prime' 0 x:xs = x:xs
--prime' n (x:xs) = if x % 2 == 0 then prime' n-1 (x+1):xs

--checkPrime :: (Integral a) => a -> [a] -> Bool
--checkPrime _ [] = True
--checkPrime c (x:xs)
--	| (c `mod` x) == 0 = False
--	| otherwise = checkPrime c xs
	isPrime :: (Integral a) => a -> Bool
	isPrime x
		| x < 2 = False
		| x <= 3 = True
		| otherwise = isPrime' 5 x

	isPrime' :: (Integral a) => a -> a -> Bool
	isPrime' k x
		| x `mod` 2 == 0  = False
		| x `mod` 3 == 0  = False
		| k*k > x = True
		| x `mod` k == 0 = False
		| x `mod` (k + 2) == 0 = False
		| otherwise = isPrime' (k+6) x

	primes :: (Integral a) => Int -> [a]
	primes c 
		| c > 0     = take c $ filter isPrime [1..]
		| otherwise = []


--filter (\x -> (600851475143 `mod` x) == 0) (prime 1000)
-- > [71,839,1471,6857]
-- 600851475143/71/839/1471/6857
-- > 1.0
