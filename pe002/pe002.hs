import PE.Math

main = do
    let limit = 4*10^6
    print . sum . takeWhile (<=limit) . filter (even) $ fibs
