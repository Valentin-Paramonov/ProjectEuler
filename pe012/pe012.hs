factors :: (Integral a) => a -> [a]
factors x = factors' x 2 []

factors' :: (Integral a) => a -> a -> [a] -> [a]
factors' 1 _ xs = xs
factors' x 2 xs
    | mod x 2 == 0 = factors' (div x 2) 2 (xs ++ [2])
    | otherwise = factors' x 3 xs
factors' x 3 xs
    | mod x 3 == 0 = factors' (div x 3) 3 (xs ++ [3])
    | otherwise = factors' x 5 xs
factors' x k xs
    | k*k > x = xs ++ [x]
    | mod x k == 0 = next k
    | mod x (k + 2) == 0 = next (k + 2)
    | otherwise = factors' x (k + 6) xs
    where
        next f = 
            factors' (div x f) k (xs ++ [f])

countOccurs [] = []
countOccurs (x:xs) = countOccurs' xs [(x,1)]

countOccurs' [] ps = ps
countOccurs' (x:xs) ((e,n):ps)
    | x == e = countOccurs' xs ((e, n + 1):ps)
    | otherwise = countOccurs' xs ((x, 1):(e, n):ps)

numFactors 0 = 0
numFactors 1 = 1
numFactors x = 
    product [n+1 | (_,n) <- countOccurs $ factors x]
    
sumN n = n * (n + 1) `div` 2

-- head $ filter (\x -> numFactors x > 500) (map sumN [1..])