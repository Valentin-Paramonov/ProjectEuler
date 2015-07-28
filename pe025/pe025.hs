import PE.Math

main = do
    let indexed = zip [1..] fibs
        tdigits = filter (\(_, f) -> f >= 10^999) indexed
    print . fst . head $ tdigits
