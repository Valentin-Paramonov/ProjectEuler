import PE.Primes

main = do
	print . foldl1 max . primeFactors $ 600851475143
