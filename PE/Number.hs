module PE.Number where

import Data.Char

toNumber :: [Int] -> Integer
toNumber = read . map intToDigit

toDigits :: Integer -> [Int]
toDigits = map digitToInt . show
