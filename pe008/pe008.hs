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
        digits = map digitToInt . foldl1 (++) . lines $ raw
        window = 13
        d:ds = digits
    print $ largestProd window d ds
