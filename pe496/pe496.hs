import Data.List

main = do
    let l = 1000000000
        t = [m*n | n <- [2..intSqrt l], m <- [n+1..2*n-1], gcd m n == 1]
        intSqrt x = toInteger . truncate . sqrt $ fromIntegral x
        sumOfProportionals x = x*(n*(n+1) `div` 2)
            where n = l `div` x
    print $ foldl' (\acc a -> sumOfProportionals a + acc) 0 t

