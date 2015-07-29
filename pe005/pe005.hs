import PE.Primes

main = do
	print . product . foldl primeOrNotCovered ps $ [1..n]
	where
		primeOrNotCovered xs x
			| isPrime x = xs
			| otherwise = foldl notCovered x xs:xs
		notCovered 1 _ = 1
		notCovered x p
			| x `mod` p == 0 = x `div` p
			| otherwise = x
		isPrime x = x `elem` ps
		ps = takeWhile (<=n) primes
		n = 20
