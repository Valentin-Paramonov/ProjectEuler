module PE.Primes (
    primes, primeFactors, pds
) where

primes = 2:3:ps [6,12..]

ps (x:xs)
    | lxIsPrime && rxIsPrime = lx:rx:rest
    | lxIsPrime && not rxIsPrime = lx:rest
    | not lxIsPrime && rxIsPrime = rx:rest
    | otherwise = rest
    where isPrime' = isPrime primes
          lx = x - 1
          rx = x + 1
          lxIsPrime = isPrime' lx
          rxIsPrime = isPrime' rx
          rest = ps xs

isPrime [] _ = True
isPrime (p:ps) n
    | p * p > n = True
    | mod n p == 0 = False
    | otherwise = isPrime ps n

pds n = primeProd n - n

primeProd n = product . map (\(f,p) -> 1 + (sum $ map (f^) [1..p])) $ pFactors n

pFactors n = pFactors' n primes []

pFactors' n pps@(p:ps) fs
    | p > n = fs
    | mod n p == 0 = pFactors' (n `div` p) pps (insertFactor p fs)
    | otherwise = pFactors' n ps fs

insertFactor f [] = [(f,1)]
insertFactor f ffs@((pf,power):fs)
    | f == pf = (f,power + 1):fs
    | otherwise = (f,1):ffs

primeFactors = concat . map (factorList) . reverse . pFactors
    where factorList (f,p) = zipWith (^) [f,f..] [1..p]
