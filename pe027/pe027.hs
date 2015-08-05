import PE.Primes

main = print . uncurry (*) . fst . foldl1 max' . map conseccutivePrimeCount $ abPairs where
    max' a b
        | snd a >= (snd b) = a
        | otherwise = b
    conseccutivePrimeCount = length' . primeSequence . quadratic'
    length' (ab,ns) = (ab, length ns)
    primeSequence (ab,q) = (ab, takeWhile isPrime . values $ q)
    values = flip fmap [0..]
    isPrime n = n `elem` takeWhile (<= n) primes
    quadratic' t = (t, uncurry quadratic t)
    quadratic a b n = n*(n + a) + b
    abPairs = [(a,b) | a <- a, b <- b]
    a = [-999..999]
    b = [-999..999]
