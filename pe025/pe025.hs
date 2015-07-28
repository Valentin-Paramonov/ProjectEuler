fib = fib' 1 1
fib' a b = a:fib' b (a+b)

main = do
    let indexed = zip [1..] fib
        tdigits = takeWhile (\(_, f) -> f >= 1e9999) indexed
    print $ fst $ head tdigits
