import PE.Primes

ps = take 500500 primes
prime n = ps !! n

f i 0 = 1
f i d =
    minimum $ map (\(c,n) -> pi^(c-1) * (f (i + 1) (d - n))) candidates
    where
        k = d
        p = prime $ k + i - 1 - 1
        pi = prime $ i - 1
        m = floor (log (fromIntegral p) / log (fromIntegral pi))
        powersOf2 = [(2^a,a+1) | a <- [1..]]
        candidates = 
            let 
                cs = takeWhile ((<= m) . fst) powersOf2
            in
                if cs /= [] then map (\(c,n) -> (2*c,n)) cs else [(2,1)] 

main = do
    print $ f 1 500500 `mod` 500500507

