import PE.Primes

prime n = primes !! n

update [] d = [d]
update (d:ds) n
    | d == n = d * d * d:ds
    | otherwise = d:(update ds n)

f divisors n
    | n == 1 = product divisors
    | otherwise = 
        let 
            p = prime $ length divisors
            updatePossibility = dropWhile (\d -> d*d > p) divisors
        in
            if updatePossibility /= []
            then
                f (update divisors $ head updatePossibility) (n - 1)
            else
                f (update divisors p) (n - 1)

main = do
    print $ mod (f [2] 500500) 500500507

