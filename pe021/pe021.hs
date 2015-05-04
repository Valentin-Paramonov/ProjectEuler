import PE.Primes

primes' = takeWhile (< 1620) primes

divisors n = filter (\p -> mod n p == 0) $ primes'

properDivisorSum n = (product . map series $ primeFact n) - n
    where series (p,i) = div (p^(i + 1) - 1) (p - 1)

primeFact n = zip divs $ primeFact' n divs [0]
    where divs = divisors n

primeFact' _ [] (f:fs) = reverse fs
primeFact' n pss@(p:ps) fss@(f:fs)
    | mod n p == 0 = primeFact' (div n p) pss $ (f + 1):fs
    | otherwise = primeFact' n ps (0:fss)

main = do
    let isAmicable a = properDivisorSum b == a && a /= b
            where b = properDivisorSum a
        amicable = sum $ filter isAmicable [2..9999]
    print $ amicable
