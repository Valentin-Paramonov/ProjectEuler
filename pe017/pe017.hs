module Main where
import qualified Data.Char as Char

main = do
    print $ foldl (+) 0 $ map countLetters $ map toWord [1..1000]

toWord :: (Integral a) => a -> [Char]
toWord 0 = "zero"
toWord 1 = "one"
toWord 2 = "two"
toWord 3 = "three"
toWord 4 = "four"
toWord 5 = "five"
toWord 6 = "six"
toWord 7 = "seven"
toWord 8 = "eight"
toWord 9 = "nine"
toWord 10 = "ten"
toWord 11 = "eleven"
toWord 12 = "twelve"
toWord 13 = "thirteen"
toWord 14 = "fourteen"
toWord 15 = "fifteen"
toWord 16 = "sixteen"
toWord 17 = "seventeen"
toWord 18 = "eighteen"
toWord 19 = "nineteen"
toWord 20 = "twenty"
toWord 30 = "thirty"
toWord 40 = "forty"
toWord 50 = "fifty"
toWord 60 = "sixty"
toWord 70 = "seventy"
toWord 80 = "eighty"
toWord 90 = "ninety"
toWord 1000 = "one thousand"
toWord n 
    | n > 1000 = "?"
    | otherwise = toWord' (show n)

toWord' (h:d:t:[])
    | (d:[t]) == "00" = hundreds ++ " hundred"
    | otherwise = hundreds ++ " hundred and " ++ (toWord' (d:[t]))
    where hundreds = toWord $ toInt [h] 
toWord' (d:t:[])
    | t == '0' = tens
    | d == '0' || d == '1' = toWord $ toInt (d:[t])
    | otherwise = tens ++ "-" ++ (toWord $ toInt [t])
    where tens = toWord $ toInt (d:"0")

toInt x = read x :: Int

countLetters x = length $ filter (Char.isAlpha) x
