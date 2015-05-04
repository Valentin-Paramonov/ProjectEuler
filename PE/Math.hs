module PE.Math where

import Data.List

fact :: (Integral a) => a -> a
fact 0 = 1
fact 1 = 1
fact 2 = 2
fact 3 = 6
fact 4 = 24
fact 5 = 120
fact 6 = 720
fact 7 = 5040
fact 8 = 40320
fact 9 = 362880
fact 10 = 3628800
fact 11 = 39916800
fact 12 = 479001600
fact 13 = 6227020800
fact 14 = 87178291200
fact 15 = 1307674368000
fact 16 = 20922789888000
fact 17 = 355687428096000
fact 18 = 6402373705728000
fact 19 = 121645100408832000
fact 20 = 2432902008176640000
fact n = foldl1' (*) [1..n]

replicate' :: (Integral b) => a -> b -> [a]
replicate' _ 0 = []
replicate' n k = n:(replicate' n $ k-1)

c :: (Integral a) => a -> a -> a
c k n
    | k > n = 0
    | k == n = 1
    | otherwise = foldl1' (*) [k+1..n] `div` (fact $ n-k)

pow :: (Integral a) => a -> a -> a
pow _ 0 = 1
pow n k = foldl1' (*) $ replicate' n k

