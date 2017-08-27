import PE.Primes
import PE.Number

-- if a prime contains 0,2,4,5,6,8 then some rotations won't be prime
ps = filter (all (\n -> odd n && n /= 5) . toDigits) $ takeWhile (< 1000000) primes
isCircular n = 
    let 
        l = length $ toDigits n
        isPrime n = elem n ps
        allTrue = not . any (== False)
    in 
        if allTrue $ map isPrime [rotate m n | m <- [0..l-1]]
        then 1
        else 0

main = do
    -- 2 and 5 are excluded in the filter, therefore + 2
    print . (+ 2) . sum . (map isCircular) $ ps

