main = print . multiply . head $ sides
    where
        multiply (a,b,c) = a*b*c
        sides = [(a,b,c) |
            c <- [1..limit],
            b <- [1..c],
            a <- [1..b],
            a + b + c == limit,
            a^2 + b^2 == c^2]
        limit = 1000
