diagonalSum n = sum . map d $ [0..l] where
    l = (n - 1) `quot` 2

d 0 = 1
d i = 4*n^2 - 6*(n - 1) where
    n = 2*i + 1

main = print . diagonalSum $ 1001
