import PE.Math
import PE.Number

f n = 
    let factSum = sum . (map fact) $ toDigits n 
    in if n == factSum
    then n 
    else 0

main = do
    print $ sum . map f $ [3..99999]
