import Data.Char

main = print . sum . filter (\n -> digitPowerSum 5 n == n) $ [11..limit] where
    digitPowerSum p = sum . map ((^p) . digitToInt) . show
    limit = 6*9^5
