import System.IO
import Data.List

main = do
    file <- readFile "./pe018.data"
    let removeSpaces = filter (\(a:_) -> a /= ' ')
        splitOnSpace = groupBy (\a b -> a /= ' ' && b /= ' ')
        split = removeSpaces . splitOnSpace
        stringToInt = (\a -> read a :: Int)
     
    let numbers = map (map stringToInt . split) $ lines file
    print $ maxPath numbers

maxPath :: [[Int]] -> Int
maxPath ns = maxPath' 0 ns

maxPath' _ [] = 0
maxPath' i (x:xs) =
    x !! i + (max (maxPath' i xs) (maxPath' (i + 1) xs))
