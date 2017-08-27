module PE.Number (
    toNumber, toDigits, numDivisors
) where

import Data.Char
import PE.Primes

toNumber :: (Integral a, Read a) => [Int] -> a
toNumber = read . map intToDigit

toDigits :: (Integral a, Show a) => a -> [Int]
toDigits = map digitToInt . show

rotate times n =
    let digits = toDigits n
    in toNumber $ take (length digits) $ drop times $ cycle digits

countDivisors [] = 1
countDivisors divisors@(d:ds) =
    let 
        isPower n = mod n d == 0
        xPowers = takeWhile isPower divisors
        rest = dropWhile isPower divisors
    in
        (length xPowers + 1) * (countDivisors rest)
    
numDivisors = countDivisors . primeFactors
