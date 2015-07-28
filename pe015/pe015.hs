module Main where

main = do
    print $ p 20

p :: (Integral a) => a -> a
p n = let f = fact n in
    fact (2 * n) `div` (f * f)
    
fact n = foldl (\x acc -> acc * x) 1 [1..n]
