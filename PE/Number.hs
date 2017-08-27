module PE.Number where

import Data.Char

toNumber :: (Integral a, Read a) => [Int] -> a
toNumber = read . map intToDigit

toDigits :: (Integral a, Show a) => a -> [Int]
toDigits = map digitToInt . show

rotate times n =
    let digits = toDigits n
    in toNumber $ take (length digits) $ drop times $ cycle digits

