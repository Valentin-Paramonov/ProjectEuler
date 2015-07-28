main = do
    let limit = 1000
    print . sum $ [ n | n <- [1..limit - 1], n `mod` 3 == 0 || n `mod` 5 == 0 ]
