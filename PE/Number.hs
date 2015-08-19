module PE.Number where

import Data.Char

toNumber :: (Integral a, Read a) => [Int] -> a
toNumber = read . map intToDigit

toDigits :: (Integral a, Show a) => a -> [Int]
toDigits = map digitToInt . show
