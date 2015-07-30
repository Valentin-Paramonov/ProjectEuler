main = print $ sumSquared - sumOfSqures
    where
        sumSquared = sum [1..n] ^ 2
        sumOfSqures = sum . map (^2) $ [1..n]
        n = 100
