module PE.Primes (
    primes
) where

primes = 2:3:ps [i| i <- [6,12..]]

ps (x:xs) 
    | lxIsPrime && rxIsPrime = lx:rx:rest
    | lxIsPrime && not rxIsPrime = lx:rest
    | not lxIsPrime && rxIsPrime = rx:rest
    | otherwise = rest
    where isPrime' = isPrime primes
          lx = x - 1
          rx = x + 1
          lxIsPrime = isPrime' lx
          rxIsPrime = isPrime' rx
          rest = ps xs

isPrime [] _ = True
isPrime (p:ps) n
    | p * p > n = True
    | mod n p == 0 = False
    | otherwise = isPrime ps n

