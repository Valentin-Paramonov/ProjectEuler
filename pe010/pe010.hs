import PE.Primes

main = print . sum . takeWhile (< 2*10^6) $ primes
