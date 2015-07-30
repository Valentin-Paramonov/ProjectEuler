import System.IO
import Data.Char

largestProd _ mx [] = mx
largestProd w mx (x:xs)
    | nmx > mx = largestProd w nmx xs
    | otherwise = largestProd w mx xs
    where nmx = product . take w $ xs

main = do
    raw <- readFile "pe008/pe008.data"
    let
        d:ds = map digitToInt . foldl1 (++) . lines $ raw
        window = 13
    print $ largestProd window d ds
